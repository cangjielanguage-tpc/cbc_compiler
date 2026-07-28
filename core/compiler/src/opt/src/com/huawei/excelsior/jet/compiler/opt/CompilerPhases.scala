/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.{CodeUnit, Environment, Stage, Stats}
import com.huawei.excelsior.jet.compiler.StatsKind.*
import com.huawei.excelsior.jet.compiler.Pass.Backend
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition}
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.JBCParser
import com.huawei.excelsior.jet.compiler.opt.frontend.cangjie.{CHIRParser, CangjieLLVMIRParser}
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, LogsKind, Nodes, Universe}
import com.huawei.excelsior.jet.compiler.opt.lowering.{Lowering, NewArrayAllocations}
import com.huawei.excelsior.jet.compiler.opt.middle.escape.{StackAllocAnalysis, StackAllocOptimization}
import com.huawei.excelsior.jet.compiler.opt.middle.patterns.BytecodePatterns
import com.huawei.excelsior.jet.compiler.opt.middle.{ClinitAnalysis, CompensatoryRecordZeroing, EvacuateAnalysis, FieldsTypeAnalysis, GCPointsInserting, GlobalInitFieldsAnalysis, Optimize, SingletonObjectsReplace}
import com.huawei.excelsior.jet.compiler.opt.middle.inline.{InlineFromBytecode, InlineIRInfo, InlineOptimization}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.LoopPeeling
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.opt.serialization.{OptExtraInfo, SerializerLayerComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.{HugeClinitsLimit, HugeMethodsLimit, HugeSizeLimitAfterParsingForO1, SmallSizeLimitAfterParsingForO1}
import com.huawei.excelsior.jet.compiler.options.StrOption
import com.huawei.excelsior.jet.compiler.serialization.ExtraInfo.*
import com.huawei.excelsior.jet.util.ScalaCollections.OrderedEnum
import xscala.io.TextOutput

import scala.collection.mutable

object CompilerPhases {

  /** Compilation phases order */
  enum CompilerPhase extends OrderedEnum[CompilerPhase] {
    case ZeroPhase // used in unit-tests

    // -- Front --
    case IRParsing
    case PreInline
    case Serialization
    case InterProceduralAnalysis

    // -- Deserialization --
    case Deserialization

    // -- PGO global planning --
    case PGOStaticAnalysis

    // -- Back --
    case PostInline
    case PreLowering
    case Lowering
    case Preparation
    case BackEnd
  }

  /** This sandbox is needed to separate IR creation and error handling.
    * Such separation allows to free all IR resources while error handling in case of OOM.
    */
  class IRSandbox(val codeUnit: CodeUnit, val env: Environment, val stats: Stats) {
    def run(body: IRSandbox => Unit): Unit = {
      try {
        body(this)
      } catch {
        case e: Throwable =>
          Opt.reportCrash(env, "New compiler", codeUnit, e)
          if (stats.isEnabled(Crashes)) {
            val message = e.getMessage
            stats.count(Crashes, if (message != null) message else e.toString)
          }
          throw e
      }
    }
  }

  abstract class IR(val platformConfig: PlatformConfig, val codeUnit: CodeUnit,
                    override val env: Environment, val statsGlobal: Stats, val parent: Universe)
    extends Universe {

    def this(platformConfig: PlatformConfig, sandbox: IRSandbox, parent: Universe = null) =
      this(platformConfig, sandbox.codeUnit, sandbox.env, sandbox.stats, parent)
  }

  // Used for verbose logging.
  var nestLevel = 0

  trait Phase { self: Universe =>
    private val phases = new mutable.ListBuffer[() => Unit]

    protected def register(body: => Unit): Unit = {
      phases += (() => body)
    }

    protected def registerVerbose(phaseName: String, st: Stage)(body: => Unit): Unit = {
      def report(action: String): Unit = {
        if (env.enabled(VerboseProgress)) {
          env.println((" " * nestLevel) + s"New compiler $action $phaseName for $codeUnit")
        }
      }

      phases += (() => {
        report("started")
        try {
          nestLevel += 1
          try {
            stage(st) { body }
          } finally {
            nestLevel -= 1
          }
        } catch {
          case e: Throwable =>
            report("failed")
            throw e
        }
        report("passed")
      })
    }

    /** Returns `true` iff all phases completed successfully. */
    final def run(): Unit = stage(Stage.OptRunPhases) {
      phases foreach { _() }
      closeDbgPrinter()
    }
  }


  trait FrontOnly extends FrontPhase
    { self: Universe => }

  trait BackOnly extends IRDeserializationPhase with BackPhase
    { self: Universe with BackEnd => }

  trait ComboFrontBack extends FrontPhase with BackPhase
    { self: Universe with BackEnd => }


  trait FrontPhase extends Phase with Nodes
    with CangjieLLVMIRParser with CHIRParser
    with JBCParser with BytecodePatterns with InlineOptimization with InlineFromBytecode with Optimize with SerializerLayerComponent
    with OptExtraInfo with InlineIRInfo with FieldsTypeAnalysis with GlobalInitFieldsAnalysis
    with CompensatoryRecordZeroing.InFrontEnd { self: Universe =>

    registerVerbose("front", Stage.OptFront) {
      startPhase(CompilerPhase.IRParsing)
      createDbgPrinter(LogsKind.IRBuild)

      assert(!codeUnit.isVersionedMethod) // we don't want to serialize any information about versioned methods
      assert(!rootMethod.isNative || rootMethod.isAJReplaced)
      assert(currentInlineContext == null)

      val (scope, rtPartsInfo) = withInlineContext(InlineContext.newRoot(rootMethod)) {
        createScope(Scope.createAnchor(entryBlock), BytecodePosition(currentInlineContext), None) {
          if (rootDeclaringClass.isCangjieType) {
            val source = rootDeclaringClass.getCangjiePackage.getInputFile.toString
            if (source.endsWith(".chir")) {
              loadCHIRMethod(rootMethod, rootMethodParams)
            } else {
              loadHLIRMethod(rootMethod, rootMethodParams)
            }
          } else {
            loadJBCMethod(rootMethod, rootMethodParams)
          }
        }
      }
      scope.merge()

      if (allNodesCount > env.valueOf(HugeSizeLimitAfterParsingForO1)) {
        switchToO1()
      } else if (allNodesCount < env.valueOf(SmallSizeLimitAfterParsingForO1)) {
        switchToO1()
      }

      isDirtyForClassGC = rtPartsInfo.isDirtyForClassGC

      dbgPrinter.debugNodes("All graph after parsing")

      insertZeroingForAJRecordInitializers()

      if (isO1Compiled) {
        optimizeBytecodePatternsO1()
      } else {
        optimizeBytecodePatternsO2()
        simplifyIR() // simplify IR before any optimizations to avoid some unwanted transformations
        dbgPrinter.debugNodes("All graph after simplify")
      }

      checkGraphConsistency(CheckLevels.Important, cfg)
      checkIRConsistency(CheckLevels.Important)

      startPhase(CompilerPhase.PreInline)

      inlineAll()
      optimize()

      if (rootMethod.shouldBeSerialized) {
        startPhase(CompilerPhase.Serialization)

        serialization.serialize(rootMethod)

        analyzeLocalFieldsTypeStores()
        analyzeLocalFieldsStoresInGlobalInit()

        saveMethodLocalAnalysisResults(rootMethod, MethodExtraInfoLocal(
          cfi = InlineIRInfo.isCFI(guarded = false),
          cfiWithGuard = InlineIRInfo.isCFI(guarded = true),
          bodyWeight = InlineIRInfo.inlinedBodyWeight,
          bodyDuration = InlineIRInfo.inlinedBodyDuration,
          leaf = InlineIRInfo.leaf,
          isScalarMethod = InlineIRInfo.isScalarMethod,
          isDirtyForClassGC = isDirtyForClassGC,
          isUnstructuredLocking = isUnstructuredLocking,
          syncedParams = InlineIRInfo.synchronizedParams,
          bodySyncOperationsWeight = InlineIRInfo.bodySyncOperationsWeight,
          badForCBC = InlineIRInfo.badForCBC,
          alwaysEvacuatedParams = InlineIRInfo.alwaysEvacuatedParams,
          isO1Compiled = isO1Compiled,
          isNoReturn = {
            if (all[Return].isEmpty) {
              stats.count(NoReturn, s"Method set as 'NoReturn'")
              true
            } else {
              false
            }
          },
        ))

        def saveStubInGlobalAnalysisResults(): Unit = {
          saveMethodGlobalAnalysisResults(rootMethod, MethodExtraInfoGlobal(
            isCleanClinit = false,
            returnType = None,
            generalizedNewTypes = Nil,
            paramsEscape = None
          ))
        }

        if (rootMethod.isInlineAllAndRemove) {
          saveStubInGlobalAnalysisResults()

        } else {
          try {
            // After serialization phase we can calculate more accurate type for global dependent nodes.
            invalidateGlobalDependentNodeTypes()

            startPhase(CompilerPhase.InterProceduralAnalysis)

            val isCleanClinit = analyzeClinit()

            // Note that fields are analyzed before calculation refined return type.
            analyzeCurrentClassFieldTypes()
            analyzeCurrentClassGlobalInitFields()

            saveMethodGlobalAnalysisResults(rootMethod, MethodExtraInfoGlobal(
              isCleanClinit = isCleanClinit,
              returnType = calcRefinedReturnType(),
              generalizedNewTypes = calcGeneralizedNewTypes(),
              paramsEscape = calcRefinedParamsEscape()
            ))
          } catch {
            case e: (OutOfMemoryError | StackOverflowError) if !env.enabled(FailOnOOMInInterProcAnalysis) =>
              saveStubInGlobalAnalysisResults()

            case e: Throwable => throw e
          }
        }
      }
    }
  }

  trait IRDeserializationPhase extends Phase with SerializerLayerComponent with OptExtraInfo { self: Universe =>
    register {
      startPhase(CompilerPhase.Deserialization)
      createDbgPrinter(LogsKind.IRDeser)

      val (scope, rtPartsInfo) = createScope(Scope.createAnchor(entryBlock), NoPosition, None) {
        serialization.loadMethod(rootMethod, rootMethodParams)
      }
      scope.merge()

      if (locallyAnalyzeMethod(rootMethod).get.isO1Compiled) {
        switchToO1()
      }
      isUnstructuredLocking = locallyAnalyzeMethod(rootMethod).get.isUnstructuredLocking

      isDirtyForClassGC = rtPartsInfo.isDirtyForClassGC

      dbgPrinter.debugNodes("All graph after deserialization")
      dbgPrinter.debugNodes("All graph after deserialization with positions", { "(" + _.pos.toString + ")" })
      checkGraphConsistency(CheckLevels.Important, cfg)
      checkIRConsistency(CheckLevels.Important)
    }
  }

  trait BackPhase extends Phase
    with InlineOptimization with Optimize with StackAllocOptimization with Lowering
    with IRTransformationsCollection with LoopPeeling with GCPointsInserting with NewArrayAllocations
    with FieldsTypeAnalysis with EvacuateAnalysis with SingletonObjectsReplace
    with CompensatoryRecordZeroing.InBackEnd { self: Universe with BackEnd =>

    registerVerbose("back", Stage.OptBack) {
      if (env.enabled(NoO1ForLongLongTime) || rootDeclaringClass.isJetRuntimeClass) {
        // Use O2 even for huge methods

      } else if (rootMethod.isClinit) {
        if (allNodesCount > env.valueOf(HugeClinitsLimit)) {
          stats.value(HugeClinitsSize, null, allNodesCount)
          switchToO1()
        }

      } else {
        if (allNodesCount > env.valueOf(HugeMethodsLimit)) {
          stats.value(HugeMethodsSize, null, allNodesCount)
          switchToO1()
        }
      }

      startPhase(CompilerPhase.PostInline)

      if (env.getPass == Backend) {
        // need to clear local information of FieldsTypeAnalysis for stabilize analyze result in parallel compilation mode of JET
        // see JET-13463
        resetFieldsTypeAnalysis()
      }

      // This transformation cannot be called more than once because it may create guards under guards under guards...
      inlineAll()

      optimize()

      val versioningPointsEliminated = GradientVersioningPoint.eliminateAll()
      val loopsPeeled = env.enabled(PeelAllOuterLoops) && peelAllOuterLoops()
      if (versioningPointsEliminated || loopsPeeled) {
        optimize()
      }

      startPhase(CompilerPhase.PreLowering)

      stage(Stage.PreLowering) {
        collectOptimizationFailStats()
        // Allocation of objects on stack transformation cannot be called more than once because it may create guards under guards under guards...
        // Lowering of computable instance types should be called before newarray allocations optimization.
        val loweredCompileTimeComputable = redirectNotComputedAtCompileTimeIntrinsicsToRT()
        if (loweredCompileTimeComputable) {
          // clear unreachable computable instance type branches for correcting IR before optimizations
          eliminateConstBranches()
          eliminateUnreachableCode()
        }

        replaceSingletonObjects()

        val allocatedObjects = allocateObjectsOnStack()
        val newArrayFillOptimized = eliminateAJArrayFillZeroing()
        val newArraysOptimized = optimizeNewArrayAllocations()
        val evacuationPlaced = placeEvacuation()

        if (allocatedObjects || newArrayFillOptimized || newArraysOptimized || evacuationPlaced) {
          optimize()
        }
        checkIRConsistency(CheckLevels.Important)
        optimizeNopMemoryBarriers()
        analyzeForLowering()
        transformForLowering()
        markWarmBlocks()
        markColdBlocks()
        ContextTypesMap.convertContextTypes4Lowering()
      }

      trials.decideAndThen {
        startPhase(CompilerPhase.Lowering)
        doLowering()

        startPhase(CompilerPhase.Preparation)
        addGCPoints()
        insertZeroingInRecordConstructor()
        createDbgPrinter(LogsKind.CodeGen)
        prepareIR()

        startPhase(CompilerPhase.BackEnd)
        backEnd()

        stats.count(CompilationMode, if isO1Compiled then "O1" else "O2")
      }
    }
  }

}
