/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.abi.ABI.makeABISignature
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, BytecodePosition}
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{CONSTRUCTOR_NAME, EXPORTED_SYMBOL_PREFIX, STD_CORE_PACKAGE_NAME, isPackageInit}
import com.huawei.excelsior.jet.compiler.coverage.JcnoFileGenerator
import com.huawei.excelsior.jet.compiler.hlir.HLIRErrorReporter.withErrorReporter
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Tag.*
import com.huawei.excelsior.jet.compiler.hlir.{HLIRErrorReporter, HLIRMetadata, HLIRSymLevelResolver}
import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, ColumnNumber, LexicalBlock, LineNumber}
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.TypeVariableType
import com.huawei.excelsior.jet.compiler.opt.frontend.AJReplacedLoading
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, ConstBranchElimination, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.patterns.Arrays
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.compiler.opt.middle.{ContextTypesRecalculation, DCEComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.STATIC_VIRTUAL
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParamSet.Position.Start
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.{MutObject, MutRecord, Receiver, RetByVal}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{InstantiatedReference, fromSymType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, BitcodeMethodReference, CallKind, ClassType, ConstraintCallMethodReference, Field, InstantiatedMethodReference, Method, MethodReference, MethodSignature, MethodType, SignatureType, TypeKind, UniversalGenericMethodReference, MethodReferenceAccessKind as MAK, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.matching.Regex
import xscala.util.MathUtils
import xscala.util.StringOps.{asciiCapitalize, r}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

/**
  * Cangjie language LLVM-based IR parser.
  *
  * @author cypok
  */
trait CangjieLLVMIRParser
    extends UCEComponent
       with ConstBranchElimination
       with CangjieParsingCleanup
       with DCEComponent
       with HLIRNodes
       with ContextTypesRecalculation
       with XiTransform
       with AJReplacedLoading
       with DebugSupport
       with Arrays { self: Universe =>

  import CangjieLLVMIRParser.Regex.*

  private val genDebug = env.enabled(GenDebug)

  private def loadNormal(method: Method, args: Seq[Node]): Return = {
    withFreeUnreachableBlocks {
      allowDifferentXHandlersInBlock {
        Node.withImplicitArgConversion(enrichArg()) {
          parseMethod(method, args)
        }

        // Exception handling tweaks for IR consistency.
        if (currentScope.hasXEdges) {
          // We still don't like different handlers in single block
          if (splitTryBlocksByHandledRegions()) {
            dbgPrinter.debugNodes("All graph after try-blocks splitting")
          }

          // We still don't like extra code in XBlock, move it out.
          all[XBlock] foreach (Block.splitAfter(_))
          dbgPrinter.debugNodes("All graph after xblock unification")
        }
      }
    }

    checkIRConsistency(CheckLevels.Desirable)

    if (!isO1Compiled && env.enabled(HLIRParsingOptimizations)) {
      eliminateConstBranches() // Cangjie likes to generate pattern: if (false) { unreachable }
      eliminateUnreachableCode()
      eliminateDeadCode() // Cangjie has a lot of dead nodes for function & global definitions.
      dbgPrinter.debugNodes("All graph after const branch elimination, UCE, DCE")

      // TODO: eliminate heavy NoValue usage in parser and use "lazy nodes" which should trigger errors on first use
      assert(NoValue.unique.isEmpty || !currentScope.contains(NoValue.unique.get), "there should be no alive NoValue nodes after parsing")
    } else {
      // poor man's DCE to ensure correctness
      allNodes foreach {
        case n @ (_: GetElementPtr | _: FieldAddr | _: DeferredGetElementPtr | _: GenericGetElementPtr) if n.uses.isEmpty =>
          decommit(n)
        case _ =>
      }
    }

    assert(all[BCGlobalOrFunction].isEmpty, "there should be no alive BCGlobal or BCFunction after parsing")
    assert(all[DeferredGetElementPtr].isEmpty, "there should be no alive DeferredGetElementPtr after parsing")
    assert(all[GenericGetElementPtr].isEmpty, "there should be no alive GenericGetElementPtr after parsing")

    // The only valid use of FieldAddr and GetElementPtr is passing by-reference field of C type to C function.
    // TODO: check records with traceable fields as well
    assert(!all[GetElementPtr].exists(_.field.getType.isTraceableReference),
      "there should be no alive traceable GetElementPtr nodes after parsing")
    assert(!all[FieldAddr].exists(_.field.getType.isTraceableReference),
      "there should be no alive traceable FieldAddr nodes after parsing")

    if (!isO1Compiled && env.enabled(HLIRParsingOptimizations)) {
      recalculateContextTypes() // Cangjie has a lot of preparation checks for function & global definitions.
      dbgPrinter.debugNodes("All graph after const branch elimination, UCE, DCE, context types recalculation")

      // cleanup before local variable simplification (see JET-14909)
      eliminateUnreachableCode()
      eliminateDeadCode()
      dbgPrinter.debugNodes("All graph after UCE, DCE")
    }

    if (simplifyLocalVariables()) {
      dbgPrinter.debugNodes("All graph after local variables simplification")
      completeSSA()
      dbgPrinter.debugNodes("All graph after variables to SSA conversion")
    }

    // Must be done after all parsing optimizations that might accidentally remove this unified return (see JET-16162).
    val ret = processReturns(method)
    dbgPrinter.debugNodes("All graph after returns unification")

    ret
  }

  private def splitTryBlocksByHandledRegions(): Boolean = {
    var changed = false
    for {
      b <- all[Block].toList if b.hasXHandlers
      List(prev, next) <- b.xpoints.toList.sliding(2)
      if prev.handlerOrNull != next.handlerOrNull
    } {
      Block.splitBefore(next.owner)
      // TODO: remove whole transformation when we can handle multiple handlers in block
      assert(prev.block.singleXHandlerOrNull == prev.handlerOrNull)
      changed = true
    }
    changed
  }

  private def withRecordConversion[T](action: => T): T = {
    def convertRecord(tpe: Type, n: Node): Node = (n.tpe, tpe) match {
      case (from, to) if from == to => n

      case (RecordAddrType(x), RecordAddrType(y)) if x.isArraySliceLike && y.isArraySliceLike => n

      case (from: RecordAddrType, to: RecordAddrType) =>
        // Such casts can happen when the same record is instantiated in different packages with different mangled names.
        // TODO: check actual layout of records or better -- prohibit such casts at all
        assert(from.sigType.getRawObjectSize == to.sigType.getRawObjectSize, s"inconsistent record type size: cast $from -> $to")
        ReinterpretCast(from, to)(n)

      case (from@(_: RecordAddrType | AddrType), to@(_: RecordAddrType | AddrType)) =>
        // Such casts are needed to convert @C structs to/from C pointers.
        ReinterpretCast(from, to)(n)

      case _ => n
    }

    Node.withImplicitArgConversion(convertRecord) {
      action
    }
  }

  private def loadReplaced(method: Method, args: Seq[Node]) = {
    withRecordConversion {
      loadAJReplaced(method, args)
    }
  }

  def loadHLIRMethod(method: Method, args0: Seq[Node]): RTPartsInfo = stage(Stage.LoadHLIR) {
    val args = depriveMethodArgs(method.getMethodType, args0)
    val (ret, message) = method match {
      case _ if method.isAJReplaced     => (loadReplaced(method, args),   "after load AJReplaced)")
      case _                            => (loadNormal(method, args),     "after load normal")
    }
    dbgPrinter.debugNodes(message)
    dbgPrinter.debugGraphs(message)
    currentScope.setResult(ret)

    RTPartsInfo(isDirtyForClassGC = false)
  }

  private def processReturns(method: Method): Return = {
    val retValType = ValueType.fromSig(method.getReturnType, instantiateRich = true)
    unifyReturns(retValType)
  }

  private def parseMethod(method: Method, args: Seq[Node]): Unit = stage(Stage.CangjieFunctionParsing) {
    val source = method.getDeclaringClass.getCangjiePackage.getInputFile.toString
    withErrorReporter(source) { implicit reporter =>
      val cb = if (method.getDeclaringClass.isCangjieJavaHelper) {
        parseJavaHelperMethod(method, args)
      } else {
        parseNormalMethod(method, args, method.getLLVMIndex)
      }
      cb.cleanupGlobalsAndFunctions()
      if (genDebug) {
        updateDebugInfoAfterParsing(cb)
      }

      if (env.enabled(BoolOption.GenCoverageInCBC) && method.getSourceFile != null) {
        val sourceFile = method.getSourceFile.toString
        JcnoFileGenerator.sendSourceLines(sourceFile, method.getSourceLine)
        for ((lineNum, _, _) <- cb.instrNumberToLineColAndScopeId.values) {
          JcnoFileGenerator.sendSourceLines(sourceFile, lineNum)
        }
      }
    }
  }

  private def parseJavaHelperMethod(method: Method, args: Seq[Node])(implicit reporter: HLIRErrorReporter): CB = {
    val methodName = method.getName

    def isPreInitHelper: Boolean = methodName.equals(CangjieSymLevelMaker.JAVA_HELPER_PREINIT)
    def isPostInitHelper: Boolean = methodName.equals(CangjieSymLevelMaker.JAVA_HELPER_POSTINIT)

    /** For a given $preInit/$postInit method of a JavaHelper class, finds the corresponding $init method */
    def findInitFunction() = {
      val sig = methodName match {
        case CangjieSymLevelMaker.JAVA_HELPER_POSTINIT => method.getSignature
        case CangjieSymLevelMaker.JAVA_HELPER_PREINIT =>
          val preInitSig = method.getSignature

          assert(cond(preInitSig.returnType) {
            case SignatureType.JavaArray(baseType, 1) => baseType.symType.isJavaLangObject
          }, s"unexpected return type in ${preInitSig.toJETSignature} (expected JA1Cjava/lang/Object;)")

          val helperClassName = method.getDeclaringClass.getName
          assert(helperClassName.endsWith(CangjieSymLevelMaker.JAVA_HELPER_SUFFIX))

          val javaClassName = helperClassName.substring(0, helperClassName.length - CangjieSymLevelMaker.JAVA_HELPER_SUFFIX.length)
          val javaClassSig = SignatureType.JBCReference(javaClassName)
          preInitSig.copy(returnType = SignatureType.Void, javaClassSig +: preInitSig.parameterTypes)

        case x => shouldNotReachHere(x)
      }
      method.getDeclaringClass.findDeclaredMethod(XString.ascii(CangjieSymLevelMaker.JAVA_HELPER_INIT), sig)
    }

    var argsForParsing = args
    if (isPreInitHelper) {
      // in this case, arguments do not match between the method signature and the current function code.
      // Need to add one "fake" argument to fix it
      val fakeReceiver = withinScope(currentScope.outer) {
        NoValue()
      }
      argsForParsing = fakeReceiver +: args
    }

    // $preInit/$postInit are generated from the code if $init and do not have separate llvmIdx
    val bitCodeMethod = if (isPreInitHelper || isPostInitHelper) findInitFunction() else method
    val fnIdx = bitCodeMethod.getLLVMIndex
    val cb = parseNormalMethod(method, argsForParsing, fnIdx)

    if (isPostInitHelper || isPreInitHelper) {
      // Constructor for @java class is mapped to `$init` in the helper and has the following structure,
      // from which implementations for preInit and postInit are extracted below:
      //       entry
      //         |
      //      preInit
      //         |
      //     constrCall
      //         |
      //      postInit with returns
      def isJavaSelfInitCall(n: Call) = n.akind == MAK.SPECIAL && n.receiver == argsForParsing(n.methodType.getReceiverArgIdx) &&
        n.targetRef.methodName == xstr(CangjieSymLevelMaker.CONSTRUCTOR_NAME)
      def isCangjieSelfInitCall(n: Call) = n.akind == MAK.STATIC && n.invokeArgs.headOption.orNull == argsForParsing(0) &&
        n.targetRef.methodName == xstr(CangjieSymLevelMaker.JAVA_HELPER_INIT)

      // find Call instruction of superconstructor
      val candidateConstrCalls = all[Call] filter { n => isJavaSelfInitCall(n) || isCangjieSelfInitCall(n) }
      val constrCall = ScalaCollections.singleElement(candidateConstrCalls)

      // NOTE: super()/this() call post-dominates entry in "normal" cases, but does not post-dominate when there is a
      //  throw before that call, so we cannot enable this assertion:
      //  assert(PostDominators.augmented(cfg).postDominates(constrCall.block, entryBlock))

      val constrMethodType = constrCall.targetRef.methodType

      if (isPreInitHelper) {
        // Extract preInit:
        //       entry                entry
        //         |                    |
        //      preInit              preInit
        //         |                    |
        //     constrCall   --->   Object[] args = makeBoxedArgs
        //         |                    |
        //         |                  return args
        //         |
        //      postInit
        //         |
        //       return
        val gotoConstrCall = Block.splitBefore(constrCall)
        val arrayType = SignatureType.fromSymType(typeProvider.getArrayType(typeProvider.getObjectType, 1))
        val newArr = insertCodeBefore(gotoConstrCall, useDefaultHandler = true) {
          ensurePrepared(PreparationRequired.forType(arrayType))
          val paramsCountExceptReceiver = IConst(constrMethodType.parameterCount - 1)
          val array = NewArray(arrayType)(paramsCountExceptReceiver)

          for (i <- 1 until constrMethodType.parameterCount) {
            val paramType = constrMethodType.parameterType(i)
            val param = constrCall.invokeArgs(i)
            val boxedParam = if (paramType.isPrimitive) {
              // boxing is required
              val boxType = Java.Support.BoxType(paramType.jbcKind)
              ensurePrepared(PreparationRequired.forType(boxType.symType))
              val mref = new MethodReference(boxType.valueOf, MAK.STATIC)
              Invoke(mref)(param)
            } else {
              param
            }
            ArrayPut(arrayType)(array, IConst(i - 1), boxedParam)
          }

          array
        }

        // replaces goto by return (similarly to Toolbox.replaceByHalt)
        val block = gotoConstrCall.block
        block.blockEnd = Return(gotoConstrCall.inCtrl, gotoConstrCall.inMemory, newArr)
        gotoConstrCall.makeUsesUnreachable()
        decommit(gotoConstrCall)

      } else {
        assert(isPostInitHelper)
        // Extract postInit:
        //       entry               entry
        //         |                   |
        //      preInit                |
        //         |                   |
        //     constrCall   --->       |
        //         |                   |
        //      postInit            postInit
        //         |                   |
        //       return              return
        val targetBlock = Block.splitBefore(constrCall).target
        // preserve controlled nodes in entry block
        withIncrementalGCM {
          eliminateCrossBlockMemoryEdges()
        }
        replaceValueUsesByNoValueAndStrikeOut(constrCall)
        val entryGoto = Block.splitAfter(entryBlock, keepControlled = true)

        xiTransform { scheduler =>
          scheduler.extract(targetBlock)
          scheduler.unsafe.redirect(entryGoto.targetEdge, _ => targetBlock)
        }
        completeSSA()
      }

      dbgPrinter.debugGraphs("Graph after postInit/preInit split")
    }

    cb
  }

  private def parseNormalMethod(method: Method, args: Seq[Node], fnIdx: Int)(implicit reporter: HLIRErrorReporter): CB = withRecordConversion {
    val source = method.getDeclaringClass.getCangjiePackage.getInputFile.toString
    val resolver = CangjieSymLevelMaker.getHLIRResolver(source)(env)
    val parsedModule = resolver.hlir.module
    val cb = new CB(method, args)(resolver, reporter)
    try {
      withPosFactory(cb.currentPosition) {
        Bitcode.parseFunctionBody(source, parsedModule, fnIdx, cb, false)
        assert(!currentScope.hasState)
      }
    } catch {
      case e: Exception =>
        throw new RuntimeException(e)
    }

    // Connect parsed graph to entryBlock.
    cb.bblockByIdx(0).addArg(Goto(entryBlock, entryMemory))

    // Set line numbers.
    if (cb.instrNumberToLineColAndScopeId.nonEmpty) {
      for (n <- allNodes) {
        n.pos match {
          case pos: BytecodePosition =>
            for ((lineNum, colNum, scopeId) <- cb.instrNumberToLineColAndScopeId get pos.offset) {
              val instScope = if (genDebug) convertLB(scopeId, cb) else null // we need lexical blocks only for debugger
              n.pos = pos.copy(lineNumber = lineNum, columnNumber = colNum, scope = instScope)
            }

          case _ =>
        }
      }
    }

    dbgPrinter.debugNodes("All graph after parsing with positions", { "(" + _.pos.toString + ")" })
    dbgPrinter.debugGraphs("Graph after parsing")
    cb
  }

  private def convertLB(id: Long, cb: CB): LexicalBlock = {
    cb.diLexBlocks.get(id)
      .map(lb => cb.convertedLexBlocks.getOrElseUpdate(id, new LexicalBlock(null, lb.line, lb.column, convertLB(lb.scopeId, cb))))
      .orNull
  }

  // cypok: I really don't remember why it's called CB. :)
  private[cangjie] class CB(method: Method, args: Seq[Node])(implicit resolver: HLIRSymLevelResolver, reporter: HLIRErrorReporter) extends Bitcode.InstructionConsumer[Node] {
    private val hlir = resolver.hlir
    private val parsedModule = hlir.module
    val module = method.getDeclaringClass.getCangjiePackage

    private val blocksMap = mutable.HashMap.empty[Int, Block]
    def blockByIdx(idx: Int, newBlock: () => Block) = {
      blocksMap.getOrElseUpdate(idx, {
        // prevent spoiling of current state
        val old = currentScope.swapState(null)
        try {
          newBlock()
        } finally {
          currentScope.swapState(old)
        }
      })
    }
    def bblockByIdx(idx: Int) = blockByIdx(idx, () => BBlock()).asInstanceOf[BBlock]
    def xblockByIdx(idx: Int) = blockByIdx(idx, () => XBlock()).asInstanceOf[XBlock]
    def prevBlockByIdx(idx: Int) = blockByIdx(idx, () => shouldNotReachHere("unknown previous block"))

    val debugInfoForVariables = mutable.LinkedHashMap.empty[StackAlloc, Bitcode.DILocalVariable]

    var instrNumber = -1

    val instrNumberToLineColAndScopeId = mutable.HashMap.empty[Int, (Int, Int, Long)]
    val convertedLexBlocks = mutable.HashMap.empty[Long, LexicalBlock]
    val diLexBlocks = mutable.HashMap.empty[Long, Bitcode.DILexicalBlock]

    var curBlockIdx = 0
    var curBlock: Block = null

    val preparationKind: PreparationKind = if (method.isExported) {
      // Explicit preparation check is needed here to invoke exported methods directly from runtime.
      // FIXME: JET-12025
      PreparationKind.PROLOGUE_PREPARATION
    } else if (env.enabled(PreparationAsserts) && method.getDeclaringClass.preparationRequired) {
      PreparationKind.PROLOGUE_ASSERTION
    } else {
      null
    }

    if (preparationKind != null) {
      spinalAny {
        ensurePrepared(rootMethod.getDeclaringClass, kind = preparationKind)
      }
    }

    private def floating[A <: Node](action: => A) = {
      adjustEopType(action)
    }

    private def initCurBlock(blockByIdx: Int => Block): Unit = {
      assert(curBlock == null)
      curBlock = blockByIdx(curBlockIdx)
      class _State extends Scope.State(curBlock, curBlock, if (env.enabled(ContextTypesInParsing)) new ContextTypesMap() else null) {
        protected type This = _State
      }
      val old = currentScope.swapState(new _State)
      assert(old == null)
    }

    private def spinalAny[A](action: => A) = {
      if (curBlock == null) {
        // Note that we rely on the fact that XBlock is always started using startXBlock()
        // which explicitly creates XBlock.
        initCurBlock(bblockByIdx)
      }
      action
    }

    private def spinal[A <: Node](action: => A) = spinalAny {
      adjustEopType(action)
    }

    private def terminator[A](action: => A): Unit = {
      spinalAny {
        action
      }
      curBlockIdx += 1
      curBlock = null
      currentScope.swapState(null)
    }

    private def adjustEopType(node: Node): Node = {
      if (node != null) {
        depriveIfNeeded(node)
      } else {
        node
      }
    }


    override def emptyValuesArray(length: Int) = new Array[Node](length)

    override def startFunction(fn: Bitcode.Function): Unit = { }
    override def endFunction(): Unit = { }

    def currentPosition() = {
      val bcOffsetLike = if (instrNumber != -1) instrNumber else BytecodeOffset.SYNTHETIC
      BytecodePosition(bcOffsetLike, currentInlineContext)
    }

    // Cangjie FE generates synthetic "compile added code" with positions starting from 90 000 (as far as we understand).
    private val LINE_FOR_COMPILE_ADDED_CODE = 90_000

    private val methodSourceFile = {
      val file = method.getSourceFile
      if (file != null) file.toString else null
    }

    override def lexicalBlock(id: Long, lb: Bitcode.DILexicalBlock, lineNumber: Int, columnNumber: Int): Unit = {
      assert(id > 0)
      diLexBlocks.getOrElseUpdate(id, lb).recognizeInst(lineNumber, columnNumber)
    }

    override def instructionLocation(instrNumber: Int, file: Bitcode.DIFile, lineNumber: Int, columnNumber: Int, scopeId: Long): Unit = {
      if (lineNumber >= LINE_FOR_COMPILE_ADDED_CODE) {
        // Just skip these positions ignoring non-synthetic huge source files.
        // TODO: can we do better?
        return
      }

      assert(LineNumber.isValid(lineNumber) && ColumnNumber.isValid(columnNumber))

      instrNumberToLineColAndScopeId(instrNumber) =
        if (file.fullPath == methodSourceFile) {
          (lineNumber, columnNumber, scopeId)
        } else {
          // This location would be leading to another file, it's better to completely ignore it.
          (LineNumber.UNREPRESENTABLE, ColumnNumber.UNKNOWN, scopeId)
        }
    }

    override def startInstruction(instrNumber: Int): Unit = {
      this.instrNumber = instrNumber

      if (genDebug) {
        spinal {
          DebugTextPosBreakpoint()
        }
      }
    }

    override def endInstruction(): Unit = {
      instrNumber = -1
    }

    override def startXBlock(instrNumber: Int): Unit = {
      // We should first initialize XBlock and only then generate line number marker.
      // Otherwise it would create BBlock at this position.
      assert(curBlock == null)
      initCurBlock(xblockByIdx)
      startInstruction(instrNumber)
    }

    override def noValue() = floating {
      NoValue()
    }

    /** Inserts package initialization check before load of global or static variable from another package. */
    private def packageInitCheck(klass: ClassType): Unit = {
      val anotherPackage = klass.getCangjiePackage
      val thisPackage = method.getDeclaringClass.getCangjiePackage
      if (anotherPackage != null && anotherPackage != thisPackage) {
        PackageInitCheck(anotherPackage)()
      }
    }

    override def global(g: Bitcode.Global) = spinal {
      BCGlobal(g)
    }

    override def function(fn: Bitcode.Function) = spinal {
      BCFunction(fn)
    }

    def fieldAddr(f: Field): Node = {
      assert(f.isStatic)
      ensurePrepared(PreparationRequired.forGetStatic(f))
      if (f.isAJFlat) {
        packageInitCheck(f.getDeclaringClass)
        GetStatic(f)
      } else {
        FieldAddr(f)()
      }
    }

    def functionAddr(m: Method): Node = {
      // FIXME: should the preparation be done later?
      val declaringClass = m.getDeclaringClass
      if (m.isCAnnotated) {
        ensurePrepared(PreparationRequired.forType(declaringClass))
        CFuncWrapperAddr(m)()
      } else {
        assert(!declaringClass.isCangjieJavaHelper)
        if (m.isStatic) {
          ensurePrepared(PreparationRequired.forMethodAddr(m))
        }
        SymbolAddress(m)
      }
    }

    override def cstIntegral(ty: Bitcode.Type, numericValue: Long) = floating {
      IntegralConst(ValueType(ty2asm(ty)))(numericValue)
    }

    override def cstFloatingPoint(ty: Bitcode.Type, bits: Long) = floating {
      if (ty == Bitcode.Types.FLOAT) {
        assert(MathUtils.isNBits(bits, 32))
        FConst(java.lang.Float.intBitsToFloat(bits.toInt))
      } else if (ty == Bitcode.Types.DOUBLE) {
        DConst(java.lang.Double.longBitsToDouble(bits))
      } else {
        assert(ty == Bitcode.Types.HALF)
        // bits are encoded as unsigned, however FP types have sign bit and it's more convenient to keep them sign-extended
        assert(MathUtils.isNBits(bits, 16))
        IConst(bits.toShort)
      }
    }

    override def cstNullPointer(ty: Bitcode.Type) = cstIntegral(ty, 0)

    override def metadata(md: Bitcode.MDItem) = {
      BCNode(md)
    }

    override def getMDValue(value: Node) = {
      value match {
        case bcNode: BCNode => bcNode.value match {
          case mdValue: Bitcode.MDValue => mdValue
          case _ => null
        }
        case _ => null
      }
    }

    /** Returns param by index from bitcode parameters.
      * Bitcode parameters contain elements from `source` signature and a receiver at index 0. 
      */
    override def param(ty: Bitcode.Type, idx: Int) = {
      val bitcodeReceiverIndex = 0
      val withMut = method.isCangjieMut
      val withReceiver = method.hasReceiverParameter
      if (withMut && idx == bitcodeReceiverIndex) {
        MutParam(method, args)
      } else if (withReceiver && idx == bitcodeReceiverIndex) {
        args(method.getReceiverArgIdx)
      } else if (withReceiver || withMut) {
        assert(Receiver.position == Start && MutObject.position == Start && MutRecord.position == Start)
        args(method.startSpecialParamsCount + idx - 1)
      } else {
        args(method.startSpecialParamsCount + idx)
      }
    }

    override def ret(ty: Bitcode.Type, value: Node) = terminator {
      def returnType: SignatureType = method.getSignature.returnType

      val retValue = ty match {
        case ty: (Bitcode.StructType | Bitcode.ArrayType) =>
          val retByValArg = args(method.getRetByValArgIdx)
          copy(ty, retByValArg, value)
          retByValArg
        case _ if returnType.isVariableSizeType =>
          UniversalGeneric.CopyResultVST(returnType)(value, args(method.getRetByValArgIdx))
        case _ => value
      }

      // workaround for JET-14959
      // TODO: consider to improve `$JavaHelper.$preInit` generation
      val retType = if (method.getName.equals(CangjieSymLevelMaker.JAVA_HELPER_PREINIT)) {
        VoidType
      } else {
        // Workaround for JET-14374
        val overridesSameRetType = !MethodTables.canBeInMethodTable(rootMethod) || rootDeclaringClass.getDeclaredSuperTypes
          // Find all methods in supertypes which can be overridden by current method.
          .flatMap(_.getDeclaredMethods)
          .filter(_.overridesNameAndSig(rootMethod))
          // Check if all of them have the same return type.
          .forall(_.getSignature.returnType == rootMethod.getSignature.returnType)
        // Otherwise we override function with less precise type
        // (which is a different interface if our return type is interface).
        // So we cannot use enrichment here.
        ValueType.fromSig(method.getReturnType, eopTypeForInterfaces = overridesSameRetType, instantiateRich = true)
      }
      Return.proto(retType)(retValue)
    }

    override def ret() = ret(Bitcode.Types.VOID, Void())

    override def br(bb: Int) = terminator {
      val e = Goto()
      bblockByIdx(bb).addArg(e)
    }

    override def br(cond: Node, trueBB: Int, falseBB: Int) = terminator {
      val e = If(Cmp(IntType, Condition.NE)(cond, IConst(0)))
      bblockByIdx(trueBB).addArg(e.trueExit)
      bblockByIdx(falseBB).addArg(e.falseExit)
    }

    override def unreachable() = terminator {
      Halt.explained("bitcode parsing")()
    }

    def phi(values: Array[Node], predBBs: Array[Int]) = spinal {
      assert(values.length == predBBs.length)
      if (values.isEmpty) {
        noValue()
      } else {
        val tpe = {
          val mergedType = values map (_.tpe) reduce (_ | _)
          // Pretend that result is TRef if all values are null.
          if (mergedType == EopType.Null) EopType.Plain else mergedType
        }
        val blockToValue = ((predBBs map prevBlockByIdx) zip values).toMap[Block, Node]
        Phi(tpe)(curBlock +: curBlock.predBlocks.map(blockToValue).toSeq: _*)
      }
    }

    override def alloca(allocTy: Bitcode.Type, count: Node): Node = spinal {
      assert(count == IConst(1))
      if (allocTy.isZST) {
        allocOneZST()
      } else {
        allocOne(allocTy)
      }
    }

    private def allocOneZST() =
      StackAlloc.Local(SignatureType.Void)

    private def allocOne(allocTy: Bitcode.Type) = {
      val structOpt = for {
        case ty: Bitcode.StructType <- Option(allocTy)
        case t: Ref.Type <- hlir.ref(ty.name)
      } yield resolver.refSignature(t)

      val sig = structOpt getOrElse ty2sig(allocTy)
      StackAlloc.Local(sig)
    }

    override def extractValue(baseTy: Bitcode.Type, baseVal: Node, indices: Array[Int]): Bitcode.TypedV[Node] = {
      // struct values are represented as pointers, so baseVal == basePtr
      val basePtr = baseVal
      // extractValue has constant indices as integers, getElementPtr may have non-constant indices as nodes, convert them.
      // Also add first zero index which is omitted in case of extractValue.
      val indicesAsNodes = (0 +: indices) map { i => IConst.apply(i) : Node }
      val ptr = getElementPtr(baseTy, basePtr, indicesAsNodes, inbounds = true) // always inbounds
      ptr.ty match {
        case ptrTy: Bitcode.PointerType =>
          val valTy = ptrTy.pointee
          Bitcode.TypedV(valTy, load(valTy, ptr.v))

        case ty =>
          shouldNotReachHere(ty)
      }
    }

    override def getElementPtr(baseTy: Bitcode.Type, basePtr: Node, indices: Array[Node], inbounds: Boolean): Bitcode.TypedV[Node] = spinalAny {
      def error(msg: String) = {
        val inboundsStr = if (inbounds) " inbounds" else ""
        shouldNotReachHere(s"$msg in getelementptr$inboundsStr: ${indices.mkString("[", ",", "]")}")
      }

      def tySize(ty: Bitcode.Type): Int = {
        val elemType = ty2sig(ty).symType
        if (elemType.isRecord) {
          elemType.getRawObjectSize
        } else {
          elemType.size
        }
      }

      val (ptr, resTy) = if (inbounds) {
        // Access to record field(s)
        indices match {
          case Array(IntegralConst(0), fieldIndices @ _*) if fieldIndices.nonEmpty =>
            fieldIndices.foldLeft((basePtr, baseTy)) {
              case ((ptr, ty: Bitcode.StructType), IntegralConst(elementIdxLong))
                if 0 <= elementIdxLong && elementIdxLong < ty.elements.length =>

                val elementIdx = elementIdxLong.toInt
                val hostType = ty2sig(ty)
                val fieldTy = ty.elements(elementIdx)
                val fieldPtr = if (fieldTy.isZST) {
                  Void()
                } else {
                  val classType = asClassType(hostType)
                  val field = classType.findDeclaredFieldOrNull(elementIdx)
                  if (field != null) {
                    if (field.isAJFlat) {
                      GetField(field)(ptr)
                    } else {
                      val refType = hlir.ref(ty.name).get
                      refType match {
                        case refType: HLIRMetadata.Ref.InstantiatedRecord =>
                          val instantiatedRefType: SignatureType.InstantiatedRecord = resolver.refSignature(refType) match {
                            case x: SignatureType.InstantiatedRecord => x
                            case _ => error("unsupported ref type")
                          }
                          val instantiatedFieldType = fieldTy match {
                            case x: Bitcode.StructType => resolver.refSignature(hlir.ref(x.name).get.asInstanceOf[HLIRMetadata.Ref.Type])
                            case x: Bitcode.TypeVariableType => resolver.refSignature(hlir.ref(x.name).get.asInstanceOf[HLIRMetadata.Ref.Type])
                            case x: Bitcode.NBitsScalarType => ty2sig(fieldTy)
                            case _ => error("unsupported field type")
                          }
                          GenericGetElementPtr(field, instantiatedRefType, instantiatedFieldType)(ptr)
                        case _ =>
                          GetElementPtr(field)(ptr)
                      }
                    }
                  } else {
                    assert(hostType.isDeferred)
                    val linkageName = ty.name + "%" + elementIdx
                    val ref = hlir.ref(linkageName).get.asInstanceOf[HLIRMetadata.Ref.InstanceField]
                    DeferredGetElementPtr(ref, hostType)(ptr)
                  }
                }
                (fieldPtr, fieldTy)

              case ((ptr, ty: Bitcode.ArrayType), idx) =>
                locally { // TODO: JET-16641
                  val lowerBound = If(Cmp(LongType, Condition.GE)(idx, LConst(0)))

                  continue(lowerBound.trueExit)
                  val upperBound = If(Cmp(LongType, Condition.LT)(idx, LConst(ty.length)))

                  continue(upperBound.falseExit, lowerBound.falseExit)
                  ErrorRTSCall(RTSProc.JR_ThrowCJIndexOutOfBoundsException)()
                  Halt.afterRTSCall(RTSProc.JR_ThrowCJIndexOutOfBoundsException, "array index check failed")()

                  continue(upperBound.trueExit)
                }
                val valuePtr = Add(
                  ptr,
                  Mul(idx, IntegralConst(AddrType)(tySize(ty.element))) // TODO: JET-16640
                )
                (valuePtr, ty.element)

              case ((_, ty), idx) => error(s"unexpected element index $idx in type $ty")
            }

          case _ => error("unexpected indices")
        }
      } else {
        // Access to c-like array (i.e. through raw pointer)
        indices match {
          case Array(idx) =>
            val ptr = Add(
              basePtr,
              Mul(
                signExtendToAddr(idx),
                IntegralConst(AddrType)(tySize(baseTy)))
            )
            (ptr, baseTy)

          case _ => error("unexpected indices")
        }
      }

      Bitcode.TypedV(Bitcode.Types.ptrTo(resTy), ptr)
    }

    private def copy(ty: Bitcode.StructType | Bitcode.ArrayType | Bitcode.TypeVariableType, to: Node, from: Node) = ty match {
      // FIXME-UG support VST records
      case ty: Bitcode.TypeVariableType =>
        val tvRef = hlir.ref(ty.name).get.asInstanceOf[Ref.TypeVariable]
        UniversalGeneric.CopyUniversalVariable(resolver.refSignature(tvRef))(to, from)
      case _ =>
        CopyStructure(ty2sig(ty))(to, from)
    }

    override def store(ty: Bitcode.Type, mem: Node, value: Node): Unit = spinalAny {
      if (ty.isZST) {
        return // nop
      }

      ty match {
        case ty: (Bitcode.StructType | Bitcode.ArrayType) =>
          val addr = obtainValueOrAddr(ty, mem, optimizeConst = false)
          copy(ty, addr, value)
        case _ =>
          import HLIRMetadata.Ref
          mem match {
            case BCGlobal(g: Bitcode.Global) =>
              val ref = hlir.ref(g.name).get
              val symRefType = resolver.symRefType(ref).get
              val refType = SignatureType.fromSymType(symRefType)
              val name = resolver.symName(ref.asInstanceOf[Ref.HasName])
              val sig = resolver.typeSignature(ref.asInstanceOf[Ref.HasSignature])

              if (refType.isJavaReference) {
                Clinit(symRefType)()
              }
              ensurePrepared(symRefType)

              if (refType.isDeferred) {
                val fieldRef = BitcodeFieldReference(refType, sig, xstr(name), isWrite = true, isStatic = true)
                BitcodeDeferred.PutField.static(fieldRef)(value)
              } else {
                val f = symRefType.findDeclaredFieldOrNull(xstr(name), sig) ensuring
                  (_ != null, s"cannot find field '$name' with signature '${sig.toJETSignature}' in class '${symRefType.getName}'")
                assert(!f.isAJFlat)
                if (env.enabled(GenerateWriteBarriers) && f.getType.isTraceableReference) {
                  assert(!f.getDeclaringClass.isRecord)
                  if (currentInlineContext.method.isManaged) {
                    inlinedCall(RT.WriteBarriers.static)(value)
                  } else {
                    VerificationStaticWriteBarrier(value)
                  }
                }
                PutStatic(f)(value)
              }

            case GetElementPtr(field, base) =>
              assert(!field.isAJFlat)
              if (env.enabled(GenerateWriteBarriers) && field.getType.isTraceableReference) {
                assert(!field.isStatic)
                assert(field.getDeclaringClass.isRecord)
                if (currentInlineContext.method.isManaged) {
                  inlinedCall(RT.WriteBarriers.record)(value)
                } else {
                  VerificationStaticWriteBarrier(value)
                }
              }
              PutField(field)(base, value)

            case DeferredGetElementPtr(ref, recordType, base) =>
              val name = resolver.symName(ref)
              val signatureType = resolver.typeSignature(ref)
              val fieldRef = BitcodeFieldReference(recordType, signatureType, xstr(name), isWrite = true, isStatic = false)
              BitcodeDeferred.PutField.instance(fieldRef)(base, value)

            case GenericGetElementPtr(field, instantiatedRefType, instantiatedFieldType, base) =>
              val convertedValue = if (instantiatedFieldType.containsTypeVariables) {
                UniversalGeneric.convertHolder(from = instantiatedFieldType, to = field.getSignature)(value)
              } else {
                value
              }
              UniversalGeneric.PutField(field, instantiatedRefType, instantiatedFieldType)(base, convertedValue)

            case _ =>
              ty match {
                case ty: Bitcode.TypeVariableType =>
                  val tvRef = hlir.ref(ty.name).get.asInstanceOf[Ref.TypeVariable]
                  val tv = resolver.refSignature(tvRef)
                  val src = copy(ty, StackAlloc.OffHeapMemory(tv), value)
                  StoreMemory(ty2asm(ty), tv, atomic = false)(mem, src)
                case _ =>
                  StoreMemory(ty2asm(ty), ty2sig(ty), atomic = false)(mem, value)
              }
          }
      }
    }

    private def obtainValueOrAddr(ty: Bitcode.Type, mem: Node, optimizeConst: Boolean): Node = {
      import HLIRMetadata.Ref

      def byRef(ref: Ref) = {
        val symRefType = resolver.symRefType(ref).get
        val refType = SignatureType.fromSymType(symRefType)
        val name = resolver.symName(ref.asInstanceOf[Ref.HasName])
        val sig = resolver.typeSignature(ref.asInstanceOf[Ref.HasSignature])

        if (refType.isJavaReference) {
          Clinit(symRefType)()
        }
        ensurePrepared(symRefType)
        packageInitCheck(symRefType)

        if (refType.isDeferred) {
          val fieldRef = BitcodeFieldReference(refType, sig, xstr(name), isWrite = false, isStatic = true)
          val fieldOp = BitcodeDeferred.GetField.static(fieldRef)()
          workaroundJET15803(sig, fieldOp)
        } else {
          val f = symRefType.findDeclaredFieldOrNull(xstr(name), sig) ensuring
            (_ != null, s"cannot find field '$name' with signature '${sig.toJETSignature}' in class '${symRefType.getName}'")
          if (optimizeConst && f.isFinal && f.getType.isPrimitive && f.hasInitialValue) {
            f.getInitialValue match {
              case v: IntValue => IConst(v.value)
              case v: LongValue => LConst(v.value)
              case v: FloatValue => FConst(v.value)
              case v: DoubleValue => DConst(v.value)
              case v => shouldNotReachHere(s"unexpected const value: $v")
            }
          } else {
            GetStatic(f)
          }
        }
      }

      mem match {
        case BCGlobal(g: Bitcode.Global) =>
          byRef(hlir.ref(g.name).get)
        case GetElementPtr(field, base) =>
          GetField(field)(base)
        case DeferredGetElementPtr(ref, recordType, base) =>
          val name = resolver.symName(ref)
          val signatureType = resolver.typeSignature(ref)
          val fieldRef = BitcodeFieldReference(recordType, signatureType, xstr(name), isWrite = false, isStatic = false)
          val fieldOp = BitcodeDeferred.GetField.instance(fieldRef)(base)
          workaroundJET15803(signatureType, fieldOp)
        case GenericGetElementPtr(field, instantiatedRefType, instantiatedFieldType, base) =>
          if (instantiatedFieldType.isVariableSizeType) {
            val getField = UniversalGeneric.GetFieldOHM(field, instantiatedRefType, instantiatedFieldType)(base, StackAlloc.OffHeapMemory(instantiatedFieldType))
            UniversalGeneric.convertHolder(from = field.getSignature, to = instantiatedFieldType)(getField)
          } else {
            UniversalGeneric.GetField(field, instantiatedRefType, instantiatedFieldType)(base)
          }

        case _ => ty match {
          case _: Bitcode.StructType | _: Bitcode.ArrayType  =>
            mem

          case _ =>
            LoadMemory(ty2asm(ty), ty2sig(ty), atomic = false)(mem)
        }
      }
    }

    override def load(ty: Bitcode.Type, mem: Node): Node = spinal {
      if (ty.isZST) {
        return Void()
      }

      val valueOrAddr = obtainValueOrAddr(ty, mem, optimizeConst = true)

      (ty, valueOrAddr) match {
        case (ty: Bitcode.StructType, addr) =>
          val value = allocOne(ty)
          copy(ty, value, addr)
          value

        case (_, value) => value
      }
    }

    override def cast(op: Int, toTy: Bitcode.Type, fromTy: Bitcode.Type, value: Node): Node = spinal {
      import BitFieldExtract.*

      def prepareHalfValue(fromAsm: AsmType, value: Node): (AsmType, Node) = {
        if (fromAsm == F16) {
          (F32, ValueConvert(F16, F32)(value))
        } else {
          (fromAsm, value)
        }
      }
      def processHalfResult(isHalf: Boolean, value: Node) = {
        if (isHalf) {
          ValueConvert(F32, F16)(value)
        } else {
          value
        }
      }

      val fromAsm = ty2asm(fromTy)
      val toAsm = ty2asm(toTy)

      val fromTpe = ValueType(fromAsm)
      val toTpe = ValueType(toAsm)

      def fromBits = fromTy.getIntegerBitsNum
      def toBits = toTy.getIntegerBitsNum

      def shortToInt(rtk: AsmType) =
        if (rtk.isShortIntegral) I32 else rtk

      def assertNonBooleanCast(): Unit =
        assert(!toTy.isBoolean && !fromTy.isBoolean, s"unsupported cast ($op: $fromTy -> $toTy) for i1")

      // TODO: all these conversions are not tested enough,
      //       read Cangjie specification, check LLVM IR and maybe fix the code here

      op match {
        case 0 => // CAST_TRUNC
          assert(toBits < fromBits)
          // sign-extension has no motivation, it's just a default extension
          BFX(toTpe, 0, toBits, signExtension = true, value)

        case 1 => // CAST_ZEXT
          assert(toBits > fromBits)
          BFX(toTpe, 0, fromBits, signExtension = false, value)

        case 2 => // CAST_SEXT
          assert(toBits > fromBits)
          BFX(toTpe, 0, fromBits, signExtension = true, value)

        case 3 => // CAST_FPTOUI
          assertNonBooleanCast()
          // TODO: add support for unmanaged value conversions to Cast node.
          val (fpFromAsm, fpValue) = prepareHalfValue(fromAsm, value)
          if (toBits == 64) {
            val proc = fpFromAsm match {
              case F32 => RTSProc.JR_f2ul
              case F64 => RTSProc.JR_d2ul
              case _ => shouldNotReachHere(fromTy)
            }
            RTSCall(proc)(fpValue)

          } else {
            assert(toBits < 64)
            // Value could be converted to bigger signed type and then zero-extended to target type.
            assert(toBits < 64)
            if (toBits < 32) {
              ValueConvert(fpFromAsm, I32)(fpValue)
              // TODO: do we need some truncation?

            } else {
              assert(toBits == 32)
              val i64 = ValueConvert(fpFromAsm, I64)(fpValue)
              BitFieldExtract.Truncate(i64)
            }
          }

        case 4 => // CAST_FPTOSI
          assertNonBooleanCast()
          val (fpFromAsm, fpValue) = prepareHalfValue(fromAsm, value)
          ValueConvert(fpFromAsm, shortToInt(toAsm))(fpValue)
          // TODO: do we need some truncation?

        case 5 => // CAST_UITOFP
          assertNonBooleanCast()
          // TODO: add support for unmanaged value conversions to Cast node.
          val fpToAsm = if (toAsm == F16) F32 else toAsm
          val res = if (fromBits == 64) {
            val proc = fpToAsm match {
              case F32 => RTSProc.JR_ul2f
              case F64 => RTSProc.JR_ul2d
              case _ => shouldNotReachHere(toTy)
            }
            RTSCall(proc)(value)

          } else {
            // Non-long values could be zero-extended to bigger signed type and then converted to float.
            assert(fromBits < 64)
            if (fromBits < 32) {
              val i32 = BFX(IntType, 0, fromBits, signExtension = false, value)
              ValueConvert(I32, fpToAsm)(i32)

            } else {
              val i64 = BFX(LongType, 0, fromBits, signExtension = false, value)
              ValueConvert(I64, fpToAsm)(i64)
            }
          }
          processHalfResult(toAsm == F16, res)

        case 6 => // CAST_SITOFP
          assertNonBooleanCast()
          val intOrLongValue = if (fromAsm.isShortIntegral) {
            BFX(IntType, 0, fromBits, signExtension = true, value)
          } else {
            value
          }
          val fpToAsm = if (toAsm == F16) F32 else toAsm
          val res = ValueConvert(shortToInt(fromAsm), fpToAsm)(intOrLongValue)
          processHalfResult(toAsm == F16, res)

        case 7 |  // CAST_FPTRUNC
             8 => // CAST_FPEXT
          assertNonBooleanCast()
          ValueConvert(fromAsm, toAsm)(value)

        case 9 | 10 => // CAST_PTRTOINT | CAST_INTTOPTR
          assert(toAsm == fromAsm, s"cast $toTy <- $fromTy expects types with equals width")
          // pointers are already represented as integers, nothing to do
          value

        case 11 => // CAST_BITCAST
          assert(fromAsm.sizeInBits == toAsm.sizeInBits, s"unexpected bitcast: $toTy <- $fromTy")
          ReinterpretCast(fromTpe, toTpe)(value)

        case 12 => // CAST_ADDRSPACECAST
          shouldNotReachHere("CAST_ADDRSPACECAST")

        case _ =>
          shouldNotReachHere(op)
      }
    }


    override def unOp(ty: Bitcode.Type, op: Int, value: Node): Node = spinal {
      val tpe = if (ty == Bitcode.Types.HALF) FloatType else value.tpe
      op match {
        case 0 => Neg(tpe)(value)
        case _ =>
          shouldNotReachHere(s"unsupported unary operation $op")
      }
    }

    override def binOp(ty: Bitcode.Type, op: Int, l: Node, r: Node): Node = spinal {
      if (ty.isBoolean) {
        val node = op match {
          case 10 => And(l, r)
          case 11 => Or(l, r)
          case 12 => Xor(l, r)
          case _ =>
            shouldNotReachHere(s"unsupported binary operation $op for i1")
        }
        return node
      }

      val tpe = if (ty == Bitcode.Types.HALF) FloatType else l.tpe

      // TODO: replace `proto` with `apply`
      val proto = op match {
        case 0 => Add.proto(tpe)
        case 1 => Sub.proto(tpe)
        case 2 => Mul.proto(tpe)
        case 3 => UDiv(tpe)
        case 4 => if (tpe.isFloatingPointType) FDiv(tpe) else IDiv(tpe)
        case 5 => URem(tpe)
        case 6 => if (tpe.isFloatingPointType) FRem(tpe) else IRem(tpe)
        case 7 => Shift.proto(tpe, ArithOp.LSL)
        case 8 => Shift.proto(tpe, ArithOp.LSR)
        case 9 => Shift.proto(tpe, ArithOp.ASR)
        case 10 => And.proto(tpe)
        case 11 => Or.proto(tpe)
        case 12 => Xor.proto(tpe)
        case _ => shouldNotReachHere(op)
      }
      proto match {
        case proto: Shift.Proto =>
          val rTruncated = r.tpe match {
            case LongType =>
              // Offset of shift should always be less than or equal to bits number
              // so it should be safe to truncate it to 32-bit value.
              BitFieldExtract.Truncate(r)
            case IntType =>
              r
            case t => shouldNotReachHere(t)
          }
          val lAdjusted = proto.op match {
            case ArithOp.LSR => zeroExtendShortIntegral(ty, l)
            case ArithOp.ASR => signExtendShortIntegral(ty, l)
            case ArithOp.LSL => l
            case x => shouldNotReachHere(x)
          }
          proto(lAdjusted, rTruncated)

        case proto: IDivRemOp.Proto =>
          def extend(x: Node) = extendShortIntegral(ty, x, signExtension = !proto.isUnsigned)
          proto(extend(l), extend(r))

        case _ if ty == Bitcode.Types.HALF =>
          def h2f(x: Node) = ValueConvert(F16, F32)(x)
          ValueConvert(F32, F16)(proto(h2f(l), h2f(r)))

        case _ =>
          proto(signExtendShortIntegral(ty, l), signExtendShortIntegral(ty, r))
      }
    }

    override def cmp(ty: Bitcode.Type, op: Int, l: Node, r: Node): Node = floating {
      import Condition.*

      if (ty.isBoolean) {
        val cond = op match {
          case 32 => EQ
          case 33 => NE
          case _ => shouldNotReachHere(s"unsupported comparison operation $op for i1")
        }
        return CondVal(Cmp(l.tpe, cond)(l, r))
      }

      val cond = op match {
        // floating-point               U L G E
        case 1  => EQ              ///< 0 0 0 1    True if ordered and equal
        case 2  => GT              ///< 0 0 1 0    True if ordered and greater than
        case 3  => GE              ///< 0 0 1 1    True if ordered and greater than or equal
        case 4  => LT              ///< 0 1 0 0    True if ordered and less than
        case 5  => LE              ///< 0 1 0 1    True if ordered and less than or equal
        case 6 |                   ///< 0 1 1 0    True if ordered and operands are unequal
             7 |                   ///< 0 1 1 1    True if ordered (no nans)
             8 |                   ///< 1 0 0 0    True if unordered: isnan(X) | isnan(Y)
             9                     ///< 1 0 0 1    True if unordered or equal
           => shouldNotReachHere("unexpected FP comparison op " + op)
        case 10 => GT_OR_UNORDERED ///< 1 0 1 0    True if unordered or greater than
        case 11 => GE_OR_UNORDERED ///< 1 0 1 1    True if unordered, greater than, or equal
        case 12 => LT_OR_UNORDERED ///< 1 1 0 0    True if unordered or less than
        case 13 => LE_OR_UNORDERED ///< 1 1 0 1    True if unordered, less than, or equal
        case 14 => NE              ///< 1 1 1 0    True if unordered or not equal

        // integral
        case 32 => EQ
        case 33 => NE
        case 34 => UGT
        case 35 => UGE
        case 36 => ULT
        case 37 => ULE
        case 38 => GT
        case 39 => GE
        case 40 => LT
        case 41 => LE

        case _ => shouldNotReachHere("unexpected comparison op " + op)
      }
      def adjust(x: Node) = if (cond.isUnsigned) {
        zeroExtendShortIntegral(ty, x)
      } else {
        signExtendShortIntegral(ty, x)
      }
      def h2f(x: Node) = ValueConvert(F16, F32)(x)

      if (ty == Bitcode.Types.HALF) {
        CondVal(Cmp(FloatType, cond)(h2f(l), h2f(r)))
      } else {
        CondVal(Cmp(l.tpe, cond)(adjust(l), adjust(r)))
      }
    }

    private def javaNullCheck(obj: Node): Unit = {
      NullCheck(trusted = method.noNullCheck(env), domain = Domain.JAVA)(obj)
    }

    private def javaArrayStoreCheck(array: Node, value: Node, arrayType: SignatureType): Unit = {
      val needStoreCheck = (value.tpe.isTraceableRefType) && (value match {
        case _: AnyNull => false
        case _ => true
      })
      if (needStoreCheck) {
        ArrayStoreCheck(arrayType, trusted = method.noArrayStoreCheck(env))(array, value)
      }
    }

    private def arrayIndexCheck(array: Node, idx: Node, len: Node, arrayType: SignatureType, trusted: Boolean): Unit = {
      nullCheckWorkaround(array)
      ArrayIndexCheck(arrayType, trusted || method.noArrayIndexCheck(env))(array, idx, len)
    }

    private def arrayIndexCheck(array: Node, idx: Node, len: Node, arrayType: SignatureType): Unit = arrayIndexCheck(array, idx, len, arrayType, false)

    private def arrayIndexCheck(array: Node, idx: Node, arrayType: SignatureType, trusted: Boolean): Unit =
      arrayIndexCheck(array, idx, ArrayLength(arrayType)(array), arrayType, trusted)

    private def arrayIndexCheck(array: Node, idx: Node, arrayType: SignatureType): Unit = arrayIndexCheck(array, idx, arrayType, false)

    private def arrayNewOp(arrayType: SignatureType, len: Node): Node = {
      ensurePrepared(PreparationRequired.forType(arrayType.symType))
      if (arrayType.isDeferred) {
        BitcodeDeferred.NewArray(arrayType)(len)
      } else {
        NewArray(arrayType)(len)
      }
    }

    private def arrayCopyOf(src: Node, fromIdx: Node, length: Node)(smallCopyProc: => Node): Node = {
      val srcTD = InstanceDescriptorBy(src)
      // get values of elemSize offset and array specialization size using raw memory operations,
      // because RTStruct fields are not serializable
      val elemSizeAddr = Add(srcTD, IntegralConst(AddrType)(RTConst.CangjieInstanceDescriptor.elemSize.offset))
      val elemSize = LoadMemory(I32, SignatureType.Int32, atomic = false)(elemSizeAddr)
      val actualSize = Mul(length, BitFieldExtract.SignExtend(elemSize))
      val maxSize = IntegralConst(length.tpe)(RTConst.SmallCangjieAllocator.MAX_LENGTH_OF_SPECIALIZED_PRIM_ARRAY.intValue)
      val sizeCheck = If(Cmp(length.tpe, Condition.LE)(actualSize, maxSize))

      continue(sizeCheck.trueExit)
      val smallCopy = smallCopyProc at Goto()

      continue(sizeCheck.falseExit)
      val copyLength = length
      val normalCopy = RTSCall(RTSProc.CJ_ArrayCopyGeneric)(src, fromIdx, length, copyLength, IConst(Domain.CANGJIE.ordinal)) at Goto()

      join(smallCopy, normalCopy)
    }


    private val ArraySliceBaseFieldName  = XString("base")
    private val ArraySliceStartFieldName = XString("start")
    private val ArraySliceSizeFieldName  = XString("size")

    private def arraySliceBaseField (arraySliceType: SignatureType) = asClassType(arraySliceType).findField(ArraySliceBaseFieldName)
    private def arraySliceStartField(arraySliceType: SignatureType) = asClassType(arraySliceType).findField(ArraySliceStartFieldName)
    private def arraySliceSizeField (arraySliceType: SignatureType) = asClassType(arraySliceType).findField(ArraySliceSizeFieldName)

    private def eraseJavaArrayType(t: SymType) = {
      if (t.getArrayElemType.symType.isJavaReference) {
        typeProvider.get1DimArrayType(TypeKind.CLASS)
      } else {
        t
      }
    }

    /** Return value of array scalar element or address of array aggregate element. */
    private def arrayGetOrGetAddr(arrayType: SignatureType, elemType: SignatureType, array: Node, index: Node) = {
      if (arrayType.getArrayElemType.isZST) {
        Void()

      } else {
        ArrayGet(arrayType, obtainEnrichedElemType(arrayType, elemType))(array, index)
      }
    }

    /** Put value to array scalar or aggregate element. */
    private def arrayPut(arrayType: SignatureType, elemType: SignatureType, array: Node, index: Node, value: Node): Unit = {
      val elemTypeSig = arrayType.getArrayElemType
      if (elemTypeSig.isZST) {
        // nop

      } else if (elemTypeSig.isRecord) {
        val addr = ArrayGet(arrayType)(array, index)
        CopyStructure(elemTypeSig)(addr, value)

      } else {
        val enrichedElemType = obtainEnrichedElemType(arrayType, elemType)
        if (env.enabled(GenerateWriteBarriers) && enrichedElemType.isTraceableReference) {
          if (currentInlineContext.method.isManaged) {
            inlinedCall(RT.WriteBarriers.instance)(array, value)
          } else {
            VerificationInstanceWriteBarrier(array, value)
          }
        }
        if (!collectArrayAggregate(arrayType, array, index, value)) {
          ArrayPut(arrayType, enrichedElemType)(array, index, value)
        }
      }
    }

    private def obtainEnrichedElemType(arrayType: SignatureType, elemType: SignatureType): SignatureType = {
      if (arrayType.symType.isJavaArray) {
        TypedArrayOperation.enrichedElemType(arrayType)

      } else {
        val elemTypeSig = arrayType.getArrayElemType
        if (elemTypeSig.isZST) {
          shouldNotReachHere((arrayType, elemType))

        } else {
          if (elemTypeSig.isRecord || elemTypeSig.isPrimitive) {
            elemTypeSig

          } else {
            elemType
          }
        }
      }
    }

    private def createSlice(arraySliceType: SignatureType, base: Node, start: Node, size: Node, originalStart: Node, originalSize: Node) = {
      // Check the following bounds to guarantee that the slice is inside of the base array (or slice):
      //   0 <= start <= originalSize
      //   0 <= size  <= originalSize - start

      // Note that AIC(_, i, len) is equivalent to the following check with exclusive upper bound:
      //   0 <= i < len
      // So to make the upper bound inclusive we need to increment it by one.

      val inclusiveOriginalSize = Add(originalSize, LConst(1))
      val arrayType = SignatureType.CangjieArray(arraySliceType.getArrayElemType) // array is always non-null in slice
      arrayIndexCheck(base, start, inclusiveOriginalSize, arrayType)
      arrayIndexCheck(base, size, Sub(inclusiveOriginalSize, start), arrayType)

      val newStart = Add(originalStart, start)

      val arraySlice = StackAlloc.Local(arraySliceType)

      if (env.enabled(GenerateWriteBarriers)) {
        if (currentInlineContext.method.isManaged) {
          inlinedCall(RT.WriteBarriers.record)(base)
        } else {
          VerificationStaticWriteBarrier(base)
        }
      }

      PutField(arraySliceBaseField (arraySliceType))(arraySlice, base)
      PutField(arraySliceStartField(arraySliceType))(arraySlice, newStart)
      PutField(arraySliceSizeField (arraySliceType))(arraySlice, size)
      arraySlice
    }

    private def arraySliceBase (arraySlice: Node) = GetField(Cangjie.Support.ArraySlice.base )(arraySlice)
    private def arraySliceStart(arraySlice: Node) = GetField(Cangjie.Support.ArraySlice.start)(arraySlice)
    private def arraySliceSize (arraySlice: Node) = GetField(Cangjie.Support.ArraySlice.size )(arraySlice)
    private def arraySliceBase (arraySliceType: SignatureType, arraySlice: Node) = GetField(arraySliceBaseField (arraySliceType))(arraySlice)
    private def arraySliceStart(arraySliceType: SignatureType, arraySlice: Node) = GetField(arraySliceStartField(arraySliceType))(arraySlice)
    private def arraySliceSize (arraySliceType: SignatureType, arraySlice: Node) = GetField(arraySliceSizeField (arraySliceType))(arraySlice)

    private def resolveField(refClass: SignatureType, cjName: XString) = {
      val name = XString.ascii(cjName.toString)
      asClassType(refClass).findField(name) ensuring (_ != null, s"cannot find field '$cjName' in class '$refClass'")
    }

    /** According to the CJDB test-suite some constructs are not supposed to be statements (e.g. variable declaration
      * or string literal), but CJ front generates debug information for these constructs anyway, so we clear them
      * manually here.
      *
      * TODO-DWARF: reconsider this decision later (clear them in front or make them statements).
      */
    private def clearDebugPositionForNonStatementIntrinsic(): Unit = {
      if (genDebug) {
        @tailrec
        def findBreakpoint(n: ControlNode): DebugTextPosBreakpoint = n match {
          case n: DebugTextPosBreakpoint => n
          case n: BCFunction => findBreakpoint(n.inCtrl)
          case n: PreparationCheck => findBreakpoint(n.inCtrl)
          case n => shouldNotReachHere(s"unexpected control node before intrinsic $n")
        }
        strikeOut(findBreakpoint(currentCtrl))
      }
    }

    private object HLIRIntrinsic {
      def unapply(x: String) = hlir.extractHLIRIntrinsic(x)
    }

    private def intrinsic(name: String, args: Array[Node], argTys: Array[Bitcode.Type], retTy: Bitcode.Type): Node = {

      name match {
        case HLIRMetaIntrinsic =>
          val Array(mdArg) = args
          clearDebugPositionForNonStatementIntrinsic()
          mdArg

        case HLIRIntrinsic(subName) =>
          jetIntrinsic(subName, args, argTys, retTy)

        case ArithmeticWithOverflowLLVMIntrinsic() =>
          ErrorRTSCall(RTSProc.JR_FatalError)(AJString.bstr(ascii(s"LLVM intrinsic $name is not supported")))
          // Return value is a tuple, but it's actually unreachable.
          // Halt might be inserted, but it's not trivial.
          // Halt after all no-return calls might be inserted after parsing.
          IntegralConst(AddrType)(0)

        case "llvm.dbg.declare" =>
          val Array(valueAddr, BCNode(diLocalVar: Bitcode.DILocalVariable), BCNode(Bitcode.DIExpressionEmpty)) = args
          val Array(valueAddrTy, _, _) = argTys

          // Note that assignment to this variable (on the same position or later) would be generated
          // with its own position marker.
          clearDebugPositionForNonStatementIntrinsic()

          val valueTy = valueAddrTy match {
            case valueAddrTy: Bitcode.PointerType => valueAddrTy.pointee
            case _ => shouldNotReachHere(s"argument must be an address of a variable but got $valueAddrTy")
          }

          valueAddr match {
            case valueAddr: StackAlloc =>
              val oldDI = debugInfoForVariables.put(valueAddr, diLocalVar)
              assert(oldDI.isEmpty, s"double debug info for ${diLocalVar.name}")

            case _: Param =>
              // Only pointer to record parameter could be treated as address of variable.
              // However it's not clear, if it's correct, clang doesn't behave like this.
              // Return to this topic when we support non-scalar variables.
              // Currently we ignore this variable.
              assert(valueTy.isStruct)

            case _ => shouldNotReachHere(s"unexpected variable address: $valueAddr")
          }

          assert(retTy == Bitcode.Types.VOID)
          noValue()

        case _ =>
          assert(hlir.isLLVMIntrinsic(name))
          notImplemented(s"LLVM intrinsic: $name")
      }
    }

    private def nullCheckWorkaround(obj: Node) = {
      if (env.enabled(IgnoreNonNullSignatureInfo)) {
        NullCheck(trusted = true)(obj)
      }
    }

    private def jetIntrinsic(name: String, args: Array[Node], argTys: Array[Bitcode.Type], retTy: Bitcode.Type): Node = {
      import HLIRMetadata.*

      def parseArgs[T](pf: PartialFunction[Array[Node], T]): T = pf.applyOrElse(args, { (args: Array[Node]) =>
        shouldNotReachHere(s"unexpected arguments of call hlir.$name: ${args.mkString("[", ",", "]")}")
      })

      def stackAllocStruct(structTy: Bitcode.StructType, fields: Seq[(Bitcode.Type, Node)]): Node = {
        assert(structTy.elements.length == fields.size, s"$structTy elements count mismatch (expected ${fields.size}, actual ${structTy.elements.length})")
        allocOne(structTy) tap { struct =>
          for (((fieldTy, fieldValue), i) <- fields.zipWithIndex) {
            assert(structTy.elements(i) == fieldTy, s"$structTy element #$i mismatch (expected $fieldTy, actual ${structTy.elements(i)})")
            val structFieldAddr = getElementPtr(structTy, struct, Array(IConst(0), IConst(i)), inbounds = true).v
            store(fieldTy, structFieldAddr, fieldValue)
          }
        }
      }

      def loadStructFields(structTy: Bitcode.StructType, fieldTys: Seq[Bitcode.Type], struct: Node): Seq[Node] = {
        assert(structTy.elements.length == fieldTys.size, s"$structTy elements count mismatch (expected ${fieldTys.size}, actual ${structTy.elements.length})")
        for ((fieldTy, i) <- fieldTys.zipWithIndex) yield {
          assert(structTy.elements(i) == fieldTy, s"$structTy element #$i mismatch (expected $fieldTy, actual ${structTy.elements(i)})")
          val structFieldAddr = getElementPtr(structTy, struct, Array(IConst(0), IConst(i)), inbounds = true).v
          load(fieldTy, structFieldAddr)
        }
      }

      def parseArrayPut(trusted: Boolean): Node = {
        val (arrayType, elemType, array, idx, value) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Array)), array, idx, value) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), array, idx, value)
        }
        arrayIndexCheck(array, idx, arrayType, trusted)
        arrayPut(arrayType, elemType, array, idx, value)
        noValue()
      }

      def parseArrayGet(trusted: Boolean) = {
        val (arrayType, elemType, array, idx) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Array)), array, idx) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), array, idx)
        }
        arrayIndexCheck(array, idx, arrayType, trusted)
        arrayGetOrGetAddr(arrayType, elemType, array, idx)
      }

      def parseArraySliceGet(trusted: Boolean) = {
        val (arraySliceType, elemType, arraySlice, idx) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.ArraySlice)), arraySlice, idx) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), arraySlice, idx)
        }

        val arrayType = SignatureType.CangjieArray(arraySliceType.getArrayElemType) // array can't be null in ArraySlice
        val base = arraySliceBase(arraySliceType, arraySlice)
        val start = arraySliceStart(arraySliceType, arraySlice)
        val size = arraySliceSize(arraySliceType, arraySlice)

        arrayIndexCheck(base, idx, size, arrayType, trusted)

        val baseIdx = Add(start, idx)
        arrayGetOrGetAddr(arrayType, elemType, base, baseIdx)
      }

      def parseArraySlicePut(trusted: Boolean) = {
        val (arraySliceType, elemType, arraySlice, idx, value) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.ArraySlice)), arraySlice, idx, value) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), arraySlice, idx, value)
        }
        val arrayType = SignatureType.CangjieArray(arraySliceType.getArrayElemType) // array can't be null in ArraySlice
        val base = arraySliceBase(arraySliceType, arraySlice)
        val start = arraySliceStart(arraySliceType, arraySlice)
        val size = arraySliceSize(arraySliceType, arraySlice)

        arrayIndexCheck(base, idx, size, arrayType, trusted)

        val baseIdx = Add(start, idx)
        arrayPut(arrayType, elemType, base, baseIdx, value)
        noValue()
      }

      if (name == "null") {
        Null()

      } else if (name == "is.null") {
        val obj = parseArgs { case Array(obj) => obj }
        CondVal(Cmp(TRefType, Condition.EQ)(obj, Null()))

      } else if (name == "nullable") {
        val obj = parseArgs { case Array(obj) => obj }
        obj

      } else if (name == "require.nonnull") {
        val obj = parseArgs { case Array(obj) => obj }
        NullCheck(trusted = false, domain = Domain.CANGJIE)(obj)
        obj

      } else if (name == "java.require.nonnull") {
        val obj = parseArgs { case Array(obj) => obj }
        NullCheck(trusted = false, domain = Domain.JAVA)(obj)
        obj

      } else if (name == "ref.cmp") {
        val (obj1, obj2) = parseArgs { case Array(obj1, obj2) => (obj1, obj2) }
        CondVal(Cmp(TRefType, Condition.EQ)(obj1, obj2))

      } else if (name startsWith "uninitialized.") {
        parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Type))) =>

            @tailrec
            def genUninitialized(ref: Ref.Type): Node = ref match {
              case ref: Ref.Primitive =>
                val kind = ref.asSignatureType.symKindErased
                if (kind.isVoid) {
                  noValue()
                } else {
                  ZeroValueNode(ValueType(kind))
                }

              case _: Ref.CPointer | Ref.CString =>
                ZeroValueNode(AddrType)

              case _: Ref.HasRecordDef | _: Ref.ArraySlice | _: Ref.VArray =>
                // Must zero uninitialized memory here, because it can be directly copied to heap object (e.g. record array).
                // See JET-15875.
                StackAlloc.Local(resolver.refSignature(ref), workaroundForNonZeroedTraceableRecords = true)

              case _: Ref.Array | _: Ref.JavaArray | _: Ref.HasClassDef | _: Ref.HasInterfaceDef | _: Ref.Nullable =>
                Null()

              case ref: Ref.RawEnum =>
                genUninitialized(ref.baseType)

              case _: (Ref.Instantiated[?] | Ref.TypeVariable | Ref.OwnTypeVariable) =>
                shouldNotReachHere(s"FIXME-UG: ${ref.md}") // FIXME-UG
            }

            genUninitialized(ref)
        }

      } else if (name startsWith "identity.hash.code") {
        RTSCall(RTSProc.CJ_IdentityHashCode)(args.toSeq: _*)

      } else if (name startsWith "reference.hash.code.") {
        require(hlir.version.hasReferenceHashCode, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")

        parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Type)), value) =>
            if (resolver.symType(ref).get.isTraceableReference) {
              RTSCall(RTSProc.CJ_IdentityHashCode)(args.toSeq: _*)
            } else {
              LConst(0)
            }
        }

      } else if (name == "alloc") {
        val classType = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Type))) =>
            resolver.refSignature(ref)
        }
        val symClassType = asClassType(classType)
        if (classType.isJavaReference) {
          Clinit(symClassType)()
        }
        ensurePrepared(PreparationRequired.forType(classType))
        if (classType.isDeferred || classType.hasDeferredSuper) {
          BitcodeDeferred.New(classType)()
        } else {
          New(classType)()
        }

      } else if (name startsWith "getfield.") {
        // TODO: use fieldSig
        val ty = retTy
        assert(!ty.isStruct, "record get/put fields should use pointers")
        if (ty.isZST) {
          Void()
        } else {
          val (refClass, refType, fieldName, fieldSig, obj) = parseArgs {
            case Array(BCNode(HLIR(ref @ Ref.InstanceField(refType: Ref.Type, _, _))), obj) =>
              (resolver.refSignature(refType), refType, xstr(ref.name), resolver.typeSignature(ref), obj)
          }
          if (refClass.isDeferred || refClass.hasDeferredSuper) {
            assert(!refClass.isInstanceOf[InstantiatedReference], s"unexpected getfield from deferred universal generic type")
            val field = BitcodeFieldReference(refClass, fieldSig, fieldName, isWrite = false, isStatic = false) // TODO: JET-14591
            if (refClass.isJavaReference) {
              javaNullCheck(obj)
            }
            val fieldOp = BitcodeDeferred.GetField.instance(field)(obj)
            workaroundJET15803(fieldSig, fieldOp)
          } else {
            nullCheckWorkaround(obj)
            val field = resolveField(refClass, fieldName)
            refClass match {
              case refClass: InstantiatedReference =>
                if (fieldSig.isVariableSizeType) {
                  val getField = UniversalGeneric.GetFieldOHM(field, refClass, fieldSig)(obj, StackAlloc.OffHeapMemory(fieldSig))
                  UniversalGeneric.convertHolder(from = field.getSignature, to = fieldSig)(getField)
                } else {
                  UniversalGeneric.GetField(field, refClass, fieldSig)(obj)
                }
              case _ =>
                GetField(field)(obj)
            }
          }
        }

      } else if (name startsWith "putfield.") {
        val ty = argTys.last
        assert(!ty.isStruct, "record get/put fields should use pointers")
        if (ty.isZST) {
          // nothing to do
          noValue()
        } else {
          val (refClass, fieldName, fieldSig, obj, value) = parseArgs {
            case Array(BCNode(HLIR(ref @ Ref.InstanceField(refType: Ref.Type, _, _))), obj, value) =>
              (resolver.refSignature(refType), xstr(ref.name), resolver.typeSignature(ref), obj, value)
          }
          if (refClass.isDeferred || refClass.hasDeferredSuper) {
            val field = BitcodeFieldReference(refClass, fieldSig, fieldName, isWrite = true, isStatic = false) // TODO: JET-14591
            if (refClass.isJavaReference) {
              javaNullCheck(obj)
            }
            BitcodeDeferred.PutField.instance(field)(obj, value)
          } else {
            nullCheckWorkaround(obj)
            val field = resolveField(refClass, fieldName)
            if (env.enabled(GenerateWriteBarriers) && field.getType.isTraceableReference) {
              assert(!field.isStatic)
              if (currentInlineContext.method.isManaged) {
                inlinedCall(RT.WriteBarriers.instance)(obj, value)
              } else {
                VerificationInstanceWriteBarrier(obj, value)
              }
            }

            refClass match {
              case refClass: InstantiatedReference =>
                val convertedValue = if (fieldSig.containsTypeVariables) {
                  UniversalGeneric.convertHolder(from = fieldSig, to = field.getSignature)(value)
                } else {
                  value
                }
                UniversalGeneric.PutField(field, refClass, fieldSig)(obj, convertedValue)
              case _ =>
                PutField(field)(obj, value)
            }
          }
        }

      } else if (name startsWith "invokevirtual.") {
        parseArgs {
          case Array(BCNode(HLIR(ref: (Ref.InstanceMethod | Ref.InstantiatedInstanceMethod))), invokeArgs*) =>
            val refType = asClassType(resolver.symType(ref.refType).get)
            val mak = if (refType.isInterface) MAK.INTERFACE else MAK.VIRTUAL
            val name = resolver.symName(ref)
            val sig = resolver.functionSignature(ref, vararg = false)

            if (refType.isDeferred || refType.hasDeferredSuper) {
              val sourceMT = MethodType(sig)
              val (abiSig, specialParams) = makeABISignature(sig, Some(fromSymType(refType)))
              val mt = MethodType(abiSig, specialParams)
              val target = new BitcodeMethodReference(mt, sourceMT, mak, CompiledType(refType), xstr(name))
              callImpl(target, invokeArgs)

            } else {
              def msg: String = s"cannot find method '$name' with signature '${sig.toJETSignature}' in class '${refType.getName}'"

              ref.refType match {
                case inst: Ref.Instantiated[?] =>
                  val instantiatedTypeParameters = resolver.instantiatedTypeParameterSignatures(inst)
                  val sigEq = MethodSignature.equalInstantiatedLegacy(instantiatedTypeParameters)
                  val method = refType.findMethodOrNullWithSigEq(xstr(name), sig, sigEq) ensuring (_ != null, msg)
                  val target = new MethodReference(method, mak, CompiledType(refType))

                  val genericSig = method.getSignature
                  val refTypeSig = resolver.refSignature(inst.asInstanceOf[Ref.Type])

                  val targetWithUGContext = target.toInstantiatedMethodReference(instantiatedTypeParameters, refTypeSig)
                  universalGenericCall(targetWithUGContext, invokeArgs, genericSig, sig)

                case _ =>
                  val method = refType.findMethodOrNull(xstr(name), sig) ensuring (_ != null, msg)
                  val target = new MethodReference(method, mak, CompiledType(refType))
                  callImpl(target, invokeArgs)
              }
            }
        }

      } else if (name startsWith "invokestatic.") {
        parseArgs {
          case Array(BCNode(HLIR(typeRef: (Ref.Type | Ref.ThisType.type))), BCNode(HLIR(methodRef: (Ref.StaticMethod | Ref.InstantiatedStaticMethod))), invokeArgs*) =>
            require(hlir.version.hasUniversalGenericsForFusion, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
            require(typeRef.tag.in(ClassRef, InstantiatedClassRef, MonomorphicClassRef, InterfaceRef, InstantiatedInterfaceRef, MonomorphicInterfaceRef,
              RecordRef, InstantiatedRecordRef, MonomorphicRecordRef, TupleRef, InterfaceExtensionRef, InstantiatedInterfaceExtensionRef, TypeVariableRef, ThisTypeRef),
              s"unexpected type ref for hlir.$name: $typeRef")
            val (thisTypeInfo, mak) = typeRef match {
              case Ref.ThisType =>
                // figure out ThisTypeInfo by context
                val ti = if (rootMethod.hasThisTypeInfoParameter) {
                  // intrinsic called from another static method: forward ThisTypeInfo received as parameter
                  rootMethodParam(rootMethod.getThisTypeInfoArgIdx) ensuring (_.tpe == LongType)
                } else {
                  require(!rootMethod.isStatic)
                  // intrinsic called from instance method: obtain ThisTypeInfo from this object
                  val receiver = rootMethodParam(rootMethod.getReceiverArgIdx)
                  ThisTypeInfoBy(receiver ensuring (_.tpe.isTraceableRefType, s"$receiver"))
                }
                (ti, MAK.STATIC_VIRTUAL)
              case tpe: Ref.Type =>
                // direct or generic call with ThisTypeInfo param
                val classType = resolver.refSignature(tpe)
                ensurePrepared(PreparationRequired.forType(classType))
                (ThisTypeInfo(classType), MAK.STATIC)
            }
            val sig = resolver.functionSignature(methodRef, vararg = false)
            val refType = asClassType(resolver.symType(methodRef.refType).get)
            val methodName = resolver.symName(methodRef)
            val method = refType.findMethodOrNull(xstr(methodName), sig)
              .ensuring(_ != null, s"cannot find method '$methodName' with signature '${sig.toJETSignature}' in class '${refType.getName}'")
            val target = new MethodReference(method, mak, CompiledType(refType))

            callImpl(target, invokeArgs :+ thisTypeInfo)
        }

      } else if (name startsWith "type.variable.invokevirtual.") {
        parseArgs {
          case Array(BCNode(HLIR(ref: Ref.InstanceMethod)), invokeArgs*) =>
            // Obtain type variable ref from bitcode type
            val tvName = argTys(1).asInstanceOf[Bitcode.TypeVariableType].name
            val tvRef = hlir.ref(tvName).get.asInstanceOf[Ref.TypeVariable]
            val tv = resolver.refSignature(tvRef)

            val sig = resolver.functionSignature(ref, vararg = false)
            val refType = asClassType(resolver.symType(ref.refType).get)
            val (abiSig, specialParams) = makeABISignature(sig, Some(tv))
            val mt = MethodType(abiSig, specialParams)

            val name = resolver.symName(ref)

            val target = ConstraintCallMethodReference(sig, mt, xstr(name), CompiledType(refType))

            universalGenericCallImpl(target, invokeArgs, sig.returnType)
        }

      } else if (name == "type.variable.is.reference") {
        parseArgs {
          case Array(BCNode(HLIR(ref: Ref.TypeVariable))) =>
            require(hlir.version.hasUniversalGenericsForFusion, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
            UniversalGeneric.TypeVarIsRef(resolver.refSignature(ref).asInstanceOf[SignatureType.TypeVariable])

        }

      } else if (name startsWith "box.") {
        parseArgs {
          case Array(BCNode(HLIR(ref: (Ref.Box | Ref.InterfaceExtension | Ref.InstantiatedInterfaceExtension))), value) =>
            require(hlir.version.hasUniversalGenericsForFusion, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
            // TODO: support type variable
            val boxType = resolver.refSignature(ref)
            val boxSymType = asClassType(boxType.symType)
            val boxField = ScalaCollections.singleElement(boxSymType.getFields)
            ensurePrepared(PreparationRequired.forType(boxType))
            val box = New(boxType)()

            val boxedType = boxField.getType
            if (boxedType.isZST) {
              // nop

            } else if (boxedType.isRecord) {
              val addr = GetField(boxField)(box)
              CopyStructure(boxedType)(addr, value)


            } else {
              if (env.enabled(GenerateWriteBarriers) && boxField.getType.isTraceableReference) {
                if (currentInlineContext.method.isManaged) {
                  inlinedCall(RT.WriteBarriers.instance)(box, value)
                } else {
                  VerificationInstanceWriteBarrier(box, value)
                }
              }

              PutField(boxField)(box, value)
            }

            box
        }

      } else if (name startsWith "unbox.") {
        parseArgs {
          case Array(BCNode(HLIR(ref: (Ref.Box | Ref.InterfaceExtension | Ref.InstantiatedInterfaceExtension))), box) =>
            require(hlir.version.hasUniversalGenericsForFusion, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
            assert(!retTy.isStruct, "record get/put fields should use pointers")
            // TODO: support type variable
            val boxType = resolver.refSignature(ref)
            val boxSymType = asClassType(boxType.symType)
            val boxField = ScalaCollections.singleElement(boxSymType.getFields)
            GetField(boxField)(box)
        }

      } else if (name == "is.box") {
        parseArgs {
          case Array(BCNode(HLIR(ref: (Ref.Box | Ref.InterfaceExtension | Ref.InstantiatedInterfaceExtension))), instance) =>
            require(hlir.version.hasUniversalGenericsForFusion, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
            val boxType = resolver.refSignature(ref)
            val iof = InstanceOf(boxType)(instance)
            CondVal(Cmp(IntType, Condition.NE)(iof, IConst(0)))
        }

      } else if (name == "array.alloc.with.contents") {
        val (arrayType, elemType, values) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Array)), values*) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), values)
        }
        val array = arrayNewOp(arrayType, LConst(values.size))

        var constSeqSize = 0
        if (env.enabled(ArrayFillAggregation) && !arrayType.getArrayElemType.isZST && arrayType.getArrayElemType.symKindErased.isIntegral) {
          val constValues = values.iterator.map(IntegralConst.unapply).takeWhile(_.nonEmpty).flatten.toSeq
          if (constValues.size > 1) {
            ArrayFill(arrayType, constValues)(array)
            constSeqSize = constValues.size
          }
        }

        for ((value, i) <- values.iterator.zipWithIndex.drop(constSeqSize)) {
          arrayPut(arrayType, elemType, array, LConst(i), value)
        }
        array

      } else if (name startsWith "array.alloc.with.single.") {
        val (arrayType, elemType, len, value) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Array)), len, value) =>
            (resolver.refSignature(ref), resolver.refSignature(ref, ref.elemType), len, value)
        }
        val array = arrayNewOp(arrayType, len)
        if (arrayType.getArrayElemType.isZST) {
          stats.count(StatsKind.ArrayZeroingElimination, "Unit array zeroing eliminated on parsing", array)
        } else {
          val enrichedElemType = obtainEnrichedElemType(arrayType, elemType)
          AJArrayFill(arrayType, enrichedElemType)(array, value)
        }
        array

      } else if (name startsWith "array.alloc.uninitialized.") {
        val (arrayType, len) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Array)), len) =>
            (resolver.refSignature(ref), len)
        }
        arrayNewOp(arrayType, len)

      } else if (name startsWith "array.alloc.copy.of") {
        val src = parseArgs { case Array(src) => src }
        val length = CangjieArrayLength(src)

        arrayCopyOf(src, IntegralConst(AddrType)(0), length) {
          RTSCall(RTSProc.CJ_ArrayCopyOf)(src, IConst(Domain.CANGJIE.ordinal))
        }

      } else if (name startsWith "array.alloc.with.subrange") {
        val (src, fromIdx, count) = parseArgs { case Array(src, fromIdx, count) => (src, fromIdx, count) }

        arrayCopyOf(src, fromIdx, count) {
          RTSCall(RTSProc.CJ_ArrayCopyOfRange)(src, fromIdx, count, IConst(Domain.CANGJIE.ordinal))
        }

      } else if (name startsWith "array.get.") {
        parseArrayGet(trusted = false)

      } else if (name startsWith "array.put.") {
        parseArrayPut(trusted = false)

      } else if (name startsWith "array.unchecked.get.") {
        require(hlir.version.hasUncheckedArrayOperations, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
        parseArrayGet(trusted = true)

      } else if (name startsWith "array.unchecked.put.") {
        require(hlir.version.hasUncheckedArrayOperations, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
        parseArrayPut(trusted = true)

      } else if (name == "array.size") {
        val array = parseArgs { case Array(array) => array }
        nullCheckWorkaround(array)
        CangjieArrayLength(array)

      } else if (name == "array.copy") {
        val (dst, dstOffset, src, srcOffset, length) = parseArgs {
          case Array(dst, dstOffset, src, srcOffset, length) =>
            (dst, dstOffset, src, srcOffset, length)
        }
        nullCheckWorkaround(dst)
        nullCheckWorkaround(src)
        RTSCall(RTSProc.CJ_UncheckedArrayCopy)(src, srcOffset, dst, dstOffset, length)

      } else if (
        name.startsWith("array.acquire.raw.data")
      ) {
        val array = parseArgs { case Array(array) => array }
        nullCheckWorkaround(array)
        val res = AcquireRawData(array)

        retTy match {
          // TODO: remove this case when #261 is fixed
          case cpointerType: Bitcode.StructType =>
            // TODO: JET-15710 Cangjie rtexports:
            //
            // external record CPointer<T> {
            //   private var ptr: Int64 = 0
            // }

            stackAllocStruct(cpointerType, Seq(
              (Bitcode.Types.i(64), res)))

          case _: Bitcode.PointerType => res
          case t => shouldNotReachHere(s"unexpected CPointer type: $t")
        }

      } else if (
        name.startsWith("array.release.raw.data")
      ) {
        val (array, cpointer) = parseArgs { case Array(array, cpointer) => (array, cpointer) }
        nullCheckWorkaround(array)
        val rawPointer = argTys(1) match {
          // TODO: remove this case when #261 is fixed
          case cpointerType: Bitcode.StructType =>
            // TODO: JET-15710 Cangjie rtexports:
            //
            // external record CPointer<T> {
            //   private var ptr: Int64 = 0
            // }

            val Seq(rawPointer) = loadStructFields(cpointerType, Seq(Bitcode.Types.i(64)), cpointer)
            rawPointer

          case _: Bitcode.PointerType => cpointer
          case t => shouldNotReachHere(s"unexpected CPointer type: $t")
        }

        ReleaseRawData(array, rawPointer)

      } else if (name == "array.slice.create.sub.array") {
        val (arraySliceType, base, start, size) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.ArraySlice)), base, start, size) =>
            (resolver.refSignature(ref), base, start, size)
        }

        createSlice(arraySliceType, base, start, size, LConst(0), CangjieArrayLength(base))

      } else if (name == "array.slice.create.sub.slice") {
        val (arraySliceType, original, start, size) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.ArraySlice)), original, start, size) =>
            (resolver.refSignature(ref), original, start, size)
        }

        val originalBase = arraySliceBase(arraySliceType, original)
        val originalSize = arraySliceSize(arraySliceType, original)
        val originalStart = arraySliceStart(arraySliceType, original)

        createSlice(arraySliceType, originalBase, start, size, originalStart, originalSize)

      } else if (name == "array.slice.underlying.array") {
        val arraySlice = parseArgs { case Array(arraySlice) => arraySlice }
        arraySliceBase(arraySlice) // TODO: specialize by array element type

      } else if (name == "array.slice.start") {
        val arraySlice = parseArgs { case Array(arraySlice) => arraySlice }
        arraySliceStart(arraySlice) // TODO: specialize by array element type

      } else if (name == "array.slice.size") {
        val arraySlice = parseArgs { case Array(arraySlice) => arraySlice }
        arraySliceSize(arraySlice) // TODO: specialize by array element type

      } else if (name startsWith "array.slice.get.") {
        parseArraySliceGet(trusted = false)

      } else if (name startsWith "array.slice.put.") {
        parseArraySlicePut(trusted = false)

      } else if (name startsWith "array.slice.unchecked.get.") {
        require(hlir.version.hasUncheckedArrayOperations, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
        parseArraySliceGet(trusted = true)

      } else if (name startsWith "array.slice.unchecked.put.") {
        require(hlir.version.hasUncheckedArrayOperations, s"intrinsic hlir.$name is not supported in HLIR ${hlir.version.pretty}")
        parseArraySlicePut(trusted = true)

      } else if (name == "java.array.alloc") {
        val (arrayType, len) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.JavaArray)), len) =>
            (resolver.refSignature(ref), len)
        }
        arrayNewOp(arrayType, len)

      } else if (name startsWith "java.array.get.") {
        val (arrayType, elemType, array, idx) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.JavaArray)), array, idx) =>
            (SignatureType.fromSymType(eraseJavaArrayType(resolver.symType(ref).get)), resolver.refSignature(ref.baseType), array, idx)
        }
        javaNullCheck(array)
        arrayIndexCheck(array, idx, arrayType)
        arrayGetOrGetAddr(arrayType, elemType, array, idx)

      } else if (name startsWith "java.array.put.") {
        val (arrayType, elemType, array, idx, value) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.JavaArray)), array, idx, value) =>
            (SignatureType.fromSymType(eraseJavaArrayType(resolver.symType(ref).get)), resolver.refSignature(ref, ref.baseType), array, idx, value)
        }
        javaNullCheck(array)
        arrayIndexCheck(array, idx, arrayType)
        javaArrayStoreCheck(array, value, arrayType)
        arrayPut(arrayType, elemType, array, idx, value)
        noValue()

      } else if (name == "java.array.size") {
        val array = parseArgs { case Array(array) => array }
        javaNullCheck(array)
        JavaArrayLength(array)

      } else if (name == "unsafe.enter") {
        parseArgs { case Array() => }
        assert(retTy == Bitcode.Types.i(64))
        LConst(0)

      } else if (name == "unsafe.exit") {
        val token = parseArgs { case Array(token) => token }
        assert(argTys(0) == Bitcode.Types.i(64))
        assert(retTy == Bitcode.Types.VOID)
        Void()

      } else if (name == "instance.of") {
        // Hack: AtomicReference implementation contains completely redundant type tests for generic parameter.
        //       So we eliminate it immediately to improve performance of atomic operations.
        // TODO: find a better way (JET-13093)
        if (rootMethod.getSourceFullName.toString.contains("std.sync.AtomicReference")) {
          IConst(1)
        } else {
          import HLIRMetadata.Ref
          val (targetType, obj) = parseArgs {
            case Array(BCNode(HLIR(ref: Ref.Type)), obj) =>
            (resolver.refSignature(ref), obj)
          }
          val iof = if (targetType.isDeferred || targetType.hasDeferredSuper) {
            BitcodeDeferred.InstanceOf(targetType)(obj)
          } else {
            InstanceOf(targetType)(obj)
          }
          CondVal(Cmp(IntType, Condition.NE)(iof, IConst(0)))
        }

      } else if ((name == "java.check.cast") || (name == "java.check.cast.nullable")) {
        val (targetType, obj) = parseArgs {
          case Array(BCNode(HLIR(ref: Ref.Type)), obj) =>
            (resolver.refSignature(ref), obj)
        }
        if (targetType.isDeferred) {
          BitcodeDeferred.CheckCast(targetType)(obj)
        } else {
          CheckCast(targetType)(obj)
        }

      } else if (name == "throw") {
        val xobj = parseArgs { case Array(xobj) => xobj }
        Throw(xobj)

      } else if (name == "catch") {
        parseArgs { case Array() => }
        val catchNode = Catch(curBlock.asInstanceOf[XBlock] ensuring (_ != null))
        ConvertDomain(Domain.CANGJIE)(catchNode)

      } else if (name == "string.literal") {
        val xstrValue = parseArgs {
          case Array(BCNode(HLIR(Ref.ConstantString(str)))) => xstr(str)
        }
        val strValue = xstrValue.toString

        // TODO: JET-15710 Cangjie rtexports:
        //
        // external record String {
        //   private let myData: Array < UInt8 >
        //   private var mySize: Int64
        // }

        val sigType = SignatureType.fromSymType(Cangjie.Support.String.symType)
        assert(ty2sig(retTy) == sigType)
        val xstring = currentInlineContext.method.getDeclaringClass.getConstString(xstrValue)

        val sa = StackAlloc.Local(sigType)
        InitStringRecord(sigType, isStatic = false, xstring)(sa)
        sa

      } else if (name == "java.string.literal") {
        val xstrValue = parseArgs {
          case Array(BCNode(HLIR(Ref.ConstantString(str)))) => xstr(str)
        }
        ConstString(currentInlineContext.method.getDeclaringClass.getConstString(xstrValue), typeProvider.getStringType)()

      } else if (name startsWith "divisor.check.") {
        val divisor = parseArgs { case Array(divisor) => divisor }
        DivisorCheck()(divisor)

      } else if (name == "spawn") {
        RTSCall(RTSProc.CJ_Spawn)(args.toSeq: _*)

      } else if ((name startsWith "throwing.") || (name startsWith "saturating.")) {
        val ThrowingSaturatingArithmeticIntrinsic(mode, sign, op) = name

        // See CallSites#isCangjieSafeArithmeticMethod.
        assert(op.length == 3, s"we are assuming that all arithmetic operations are three-letter but we got '$op''")

        val signed = sign == "s"
        val signedChar = if (signed) "" else "U"

        val ty = retTy
        assert(ty.isInteger && ty.getIntegerBitsNum >= 8, s"unsupported type $ty for $name")

        val sig = ty.getIntegerBitsNum match {
          case 8  => if (signed) 'a' else 'h'
          case 16 => if (signed) 's' else 't'
          case 32 => if (signed) 'i' else 'j'
          case 64 => if (signed) 'l' else 'm'
        }

        val unary = op match {
          case "neg" | "inc" | "dec" => true
          case _ => false
        }

        val simpleFuncName = mode + op.asciiCapitalize
        val pkgName = STD_CORE_PACKAGE_NAME
        val prefix = "_ZN8" + pkgName
        val funcName = prefix + simpleFuncName.length + simpleFuncName + "E" + sig + (if (unary) "" else sig)

        val pkg = typeProvider.findClass(xstr(pkgName))
        assert(pkg != null, s"could not find package `$pkgName`")

        def invokeCangjieLibImplThrowing: Node =
          if (op == "pow") {
            RTSCall(RTSProc.CJ_throwingPowI64)(args.toSeq: _*)
          } else {
            val func = pkg.findDeclaredMethodOrNull(xstr(funcName), null)
              .ensuring(_ != null, s"could not find function `$funcName` in package `$pkgName`")
            Invoke(new MethodReference(func, MAK.STATIC))(args.toSeq: _*)
          }

        def invokeCangjieLibImplSaturating: Node = {
          val size = ty.getIntegerBitsNum
          val proc = op.asciiCapitalize match {
            case "Pow" => assert(size == 64); RTSProc.CJ_saturatingPowI64
            case "Add" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingAddI8  else RTSProc.CJ_saturatingAddU8
              case 16 => if (signed) RTSProc.CJ_saturatingAddI16 else RTSProc.CJ_saturatingAddU16
              case 32 => if (signed) RTSProc.CJ_saturatingAddI32 else RTSProc.CJ_saturatingAddU32
              case 64 => if (signed) RTSProc.CJ_saturatingAddI64 else RTSProc.CJ_saturatingAddU64
            }
            case "Sub" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingSubI8  else RTSProc.CJ_saturatingSubU8
              case 16 => if (signed) RTSProc.CJ_saturatingSubI16 else RTSProc.CJ_saturatingSubU16
              case 32 => if (signed) RTSProc.CJ_saturatingSubI32 else RTSProc.CJ_saturatingSubU32
              case 64 => if (signed) RTSProc.CJ_saturatingSubI64 else RTSProc.CJ_saturatingSubU64
            }
            case "Mul" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingMulI8  else RTSProc.CJ_saturatingMulU8
              case 16 => if (signed) RTSProc.CJ_saturatingMulI16 else RTSProc.CJ_saturatingMulU16
              case 32 => if (signed) RTSProc.CJ_saturatingMulI32 else RTSProc.CJ_saturatingMulU32
              case 64 => if (signed) RTSProc.CJ_saturatingMulI64 else RTSProc.CJ_saturatingMulU64
            }
            case "Div" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingDivI8  else RTSProc.CJ_saturatingDivU8
              case 16 => if (signed) RTSProc.CJ_saturatingDivI16 else RTSProc.CJ_saturatingDivU16
              case 32 => if (signed) RTSProc.CJ_saturatingDivI32 else RTSProc.CJ_saturatingDivU32
              case 64 => if (signed) RTSProc.CJ_saturatingDivI64 else RTSProc.CJ_saturatingDivU64
            }
            case "Mod" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingModI8  else RTSProc.CJ_saturatingModU8
              case 16 => if (signed) RTSProc.CJ_saturatingModI16 else RTSProc.CJ_saturatingModU16
              case 32 => if (signed) RTSProc.CJ_saturatingModI32 else RTSProc.CJ_saturatingModU32
              case 64 => if (signed) RTSProc.CJ_saturatingModI64 else RTSProc.CJ_saturatingModU64
            }
            case "Inc" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingIncI8  else RTSProc.CJ_saturatingIncU8
              case 16 => if (signed) RTSProc.CJ_saturatingIncI16 else RTSProc.CJ_saturatingIncU16
              case 32 => if (signed) RTSProc.CJ_saturatingIncI32 else RTSProc.CJ_saturatingIncU32
              case 64 => if (signed) RTSProc.CJ_saturatingIncI64 else RTSProc.CJ_saturatingIncU64
            }
            case "Dec" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingDecI8  else RTSProc.CJ_saturatingDecU8
              case 16 => if (signed) RTSProc.CJ_saturatingDecI16 else RTSProc.CJ_saturatingDecU16
              case 32 => if (signed) RTSProc.CJ_saturatingDecI32 else RTSProc.CJ_saturatingDecU32
              case 64 => if (signed) RTSProc.CJ_saturatingDecI64 else RTSProc.CJ_saturatingDecU64
            }
            case "Neg" => size match {
              case 8  => if (signed) RTSProc.CJ_saturatingNegI8  else RTSProc.CJ_saturatingNegU8
              case 16 => if (signed) RTSProc.CJ_saturatingNegI16 else RTSProc.CJ_saturatingNegU16
              case 32 => if (signed) RTSProc.CJ_saturatingNegI32 else RTSProc.CJ_saturatingNegU32
              case 64 => if (signed) RTSProc.CJ_saturatingNegI64 else RTSProc.CJ_saturatingNegU64
            }
            case f => shouldNotReachHere(s"could not find proc saturating${if (signed) "I" else "U"}$f")
          }
          RTSCall(proc)(args.toSeq: _*)
        }


        if (mode == "throwing") {
          assert(op != "rem", "We don't expect for intrinsic with name \'rem\' to occur, since it's \'mod\'")

          val tpe = ValueType(ty2sig(ty))

          val width = ty2asm(ty).width

          if (op == "mod") {
            val Array(l, r) = args

            val left = CheckedOp.normalizeArg(l.tpe, width, signed, l)
            val right = CheckedOp.normalizeArg(r.tpe, width, signed, r)

            DivisorCheck()(right)
            val result = if (signed) {
              IRem(tpe)(left, right)
            } else {
              URem(tpe)(left, right)
            }

            return result
          }


          val checkedSubStr = CheckedOp.Kind.SUB.toString
          val checkedAddStr = CheckedOp.Kind.ADD.toString

          val (adjustedArgs: Seq[Node], adjustedOp: String) = if (op == "inc" || op == "dec") {
            (args.toSeq ++ Seq(IntegralConst(tpe)(1)), if (op == "inc") checkedAddStr else checkedSubStr)
          } else if (op == "neg") {
            (Seq(IntegralConst(tpe)(0)) ++ args.toSeq, checkedSubStr)
          } else {
            (args.toSeq, op)
          }

          val normalizedArgs = adjustedArgs map { CheckedOp.normalizeArg(tpe, width, signed, _) }

          val node = CheckedOp.Kind.values.find(_.toString == adjustedOp.toUpperCase) map {
            CheckedOp(tpe, width, _, signed, method.isManaged)(normalizedArgs: _*)
          }

          node getOrElse invokeCangjieLibImplThrowing
        } else {
          invokeCangjieLibImplSaturating
        }
      } else {
        shouldNotReachHere(s"unsupported HLIR intrinsic hlir.$name")
      }

    }

    private def callUnhandled(fnTy: Bitcode.FunctionType, target: Node, args: Array[Node], argTys: Array[Bitcode.Type]): Node = {
      target match {
        case BCFunction(fn: Bitcode.Function) =>
          if (hlir.isIntrinsic(fn.name)) {
            intrinsic(fn.name, args, argTys, fn.ty.retTy)
          } else {
            callByRef(hlir.ref(fn.name).get, fnTy, args, argTys)
          }
        case _ => callIndirect(fnTy, target, args, argTys)
      }
    }

    private def callByRef(ref: HLIRMetadata.Ref, fnTy: Bitcode.FunctionType, args: Array[Node], argTys: Array[Bitcode.Type]): Node = {
      import HLIRMetadata.*

      def calculateMethodRef(refType: ClassType, name: String, cjSig: MethodSignature, isStatic: Boolean,
                             isVararg: Boolean, isExternal: Boolean = false, isCFunc: Boolean = false,
                             instantiatedTypeParameters: Seq[SignatureType] = Seq.empty): MethodReference = {

        if (refType.isDeferred) {
          assert(!isExternal, s"unexpected deferred external function call $name${cjSig.toJETSignature}")
          assert(!isVararg, s"unexpected deferred vararg function call $name${cjSig.toJETSignature}")
          val sourceMT = MethodType(cjSig).insertReceiverType(fromSymType(refType), !isStatic)
          // TODO: consider making all record instance methods mut-functions and adapt parameters in runtime by resolved method reference
          val hasMutParameter = BitcodeMethodReference.isCangjieMut(CompiledType(refType), name)
          val receiver = Option.when(!isStatic && !hasMutParameter)(fromSymType(refType))
          val (abiSig, specialParameters) = makeABISignature(cjSig, receiver, hasMutParameter = hasMutParameter, isCFunc = isCFunc)
          val mt = MethodType(abiSig, specialParameters)
          val mak =
            if (hasMutParameter) MAK.MUT
            else if (isStatic) MAK.STATIC
            else MAK.SPECIAL
          new BitcodeMethodReference(mt, sourceMT, mak, CompiledType(refType), xstr(name))

        } else {
          // Single rt$... intrinsic may be referenced with different signatures from different packages.
          // TODO: remove this hack after replacements are reworked (JET-13822)
          //       and proper HLIR metadata is introduced for runtime intrinsics.
          val sig = if (name.startsWith(EXPORTED_SYMBOL_PREFIX)) null else cjSig
          val method = if (instantiatedTypeParameters.nonEmpty) {
            refType.findDeclaredMethodOrNullWithSigEq(xstr(name), sig, MethodSignature.equalInstantiatedLegacy(instantiatedTypeParameters))
          } else {
            refType.findDeclaredMethodOrNull(xstr(name), sig)
          }
          assert(method != null, s"cannot find method '$name' with signature '${cjSig.toJETSignature}' in class '${refType.getName}'")

          val mak =
            if (method.isCangjieMut) {
              MAK.MUT
            } else if (isStatic) {
              MAK.STATIC
            } else {
              MAK.SPECIAL
            }
          val target = new MethodReference(method, mak)
          assert(isVararg == target.method.isVarArgs)
          if (isVararg) {
            assert(isExternal, s"unexpected varargs in regular function $name")
            val varargTypes = calculateVarArgTypes(name, fnTy, argTys)
            target.withMethodType(target.methodType.appendVarArgs(varargTypes))
          } else {
            target
          }
        }
      }

      def callFunction(ref: Ref.HasName with Ref.HasSignature, pkg: ClassType, isExternal: Boolean, isCFunc: Boolean, eraseZSTReturn: Boolean): Node = {
        val name = resolver.symName(ref)
        val sig = resolver.functionSignature(ref, fnTy.vararg, eraseZSTReturn)
        if (isExternal) {
          verifyExternal(name, fnTy.retTy, argTys)
        }
        val target = calculateMethodRef(pkg, name, sig, isStatic = true, fnTy.vararg, isExternal, isCFunc)
        callImpl(target, args.toSeq)
      }

      def callMethod(ref: Ref.MethodDef, isStatic: Boolean): Node = {
        val refType = resolver.symRefType(ref).get
        val name = resolver.symName(ref)
        val sig = resolver.functionSignature(ref, fnTy.vararg)
        ref.refType match {
          case inst: Ref.Instantiated[?] =>
            reporter.require(!refType.isDeferred, s"unexpected deferred universal generic type", ref.md) // FIXME-UG
            val instantiatedTypeParameters = resolver.instantiatedTypeParameterSignatures(inst)
            val target = calculateMethodRef(refType, name, sig, isStatic, fnTy.vararg, instantiatedTypeParameters = instantiatedTypeParameters)
            val genericSig = target.method.getSignature
            val targetWithUGContext = target.toInstantiatedMethodReference(instantiatedTypeParameters, resolver.refSignature(inst.asInstanceOf[Ref.Type]))
            universalGenericCall(targetWithUGContext, args.toSeq, genericSig, sig)
          case _ =>
            val target = calculateMethodRef(refType, name, sig, isStatic, fnTy.vararg)
            val adjustedArgs = if (target.methodType.hasThisTypeInfoParameter) {
              val classType = resolver.refSignature(ref.refType.asInstanceOf[Ref.Type])
              assert(!classType.isZST)
              ensurePrepared(PreparationRequired.forType(classType))
              ScalaCollections.insertAt(args, target.methodType.getThisTypeInfoArgIdx, ThisTypeInfo(classType)).toSeq
            } else {
              args.toSeq
            }
            callImpl(target, adjustedArgs)
        }
      }

      def callInstantiatedMethod(ref: Ref.InstantiatedWithName with Ref.HasSignature, isStatic: Boolean): Node = {
        assert(!fnTy.vararg)

        val generic = ref.generic
        val genericName = resolver.symName(generic)
        val genericSig = resolver.functionSignature(generic.asInstanceOf[Ref.HasSignature], fnTy.vararg)
        val genericRefType = resolver.symRefType(generic).get

        val instantiatedSig = resolver.functionSignature(ref, fnTy.vararg)
        val instantiatedTypeParameters = resolver.instantiatedTypeParameterSignatures(ref)

        val target = calculateMethodRef(genericRefType, genericName, genericSig, isStatic, fnTy.vararg)
        val refType = ref match {
          case ref: Ref.MethodRef => resolver.refSignature(ref.refType.asInstanceOf[Ref.Type])
          case ref: Ref.InstantiatedGlobalFunction => SignatureType.fromSymType(resolver.symType(ref.pkg).get)
        }
        val targetWithUGContext = target.toInstantiatedMethodReference(instantiatedTypeParameters, refType)
        universalGenericCall(targetWithUGContext, args.toSeq, genericSig, instantiatedSig)
      }

      ref match {
        case ref: Ref.ForeignCFunction =>
          ref.name match {
            case StdMathIntrinsic(kind) =>
              MathIntrinsic(kind)(args.toSeq: _*)
            case _ =>
              callFunction(ref, module, isExternal = true, isCFunc = true, eraseZSTReturn = true)
          }

        case ref: Ref.GlobalFunction =>
          val pkg = resolver.symRefType(ref).get
          if (isPackageInit(ref.name)(env)) {
            // Replace all calls to primary global_init with packageInit node,
            // which will invoke global_init at run-time if package is not initialized yet.
            PackageInit(pkg)()
          } else {
            callFunction(ref, pkg, isExternal = false, isCFunc = false, eraseZSTReturn = false)
          }

        case ref: Ref.GlobalCFunction =>
          val pkg = resolver.symRefType(ref).get
          callFunction(ref, pkg, isExternal = false, isCFunc = true, eraseZSTReturn = true)

        case ref: Ref.InstanceMethod =>
          callMethod(ref, isStatic = false)

        case ref: Ref.StaticMethod =>
          callMethod(ref, isStatic = true)

        case ref: Ref.InstantiatedInstanceMethod =>
          callInstantiatedMethod(ref, isStatic = false)

        case ref: Ref.InstantiatedStaticMethod =>
          callInstantiatedMethod(ref, isStatic = true)

        case ref: Ref.InstantiatedGlobalFunction =>
          callInstantiatedMethod(ref, isStatic = true)

        case _ =>
          shouldNotReachHere(s"unexpected call of ${ref.md}")
      }
    }

    private def universalGenericCall(target: InstantiatedMethodReference, args: Seq[Node],
                                     genericSig: MethodSignature, instantiatedSig: MethodSignature) = {

      val (receiver, args0) = if (target.methodType.hasReceiverParameter) {
        (args.headOption, args.tail)
      } else {
        (None, args)
      }

      val ugDescParam = Option.when(target.method.hasUGDescParameter) {
        IntegralConst(AddrType)(0)
      }

      val thisTypeInfoParam = Option.when(target.methodType.hasThisTypeInfoParameter) {
        val classType = target.refType.sigType
        assert(!classType.isZST)
        ensurePrepared(PreparationRequired.forType(classType))
        ThisTypeInfo(classType)
      }

      val convertedArgs = receiver ++ convertUniversalGeneric(args0, instantiatedSig.parameterTypes, genericSig.parameterTypes) ++ ugDescParam ++ thisTypeInfoParam

      val call = universalGenericCallImpl(target, convertedArgs.toSeq, instantiatedSig.returnType)
      UniversalGeneric.convertHolder(from = genericSig.returnType, to = instantiatedSig.returnType)(call)
    }

    private def convertUniversalGeneric(args: Seq[Node], froms: Seq[SignatureType], tos: Seq[SignatureType]): Seq[Node] = {
      assert(args.length == froms.size)
      assert(args.length == tos.size)
      for (arg, (from, to)) <- args zip (froms zip tos)
        yield UniversalGeneric.convertHolder(from, to)(arg)
    }

    private def verifyExternal(rawName: String, retTy: Bitcode.Type, argTys: Array[Bitcode.Type]): Unit = {
      assert(argTys.forall(!_.isZST), s"external function is not expected to have ZST arguments $argTys")
    }

    private def calculateVarArgTypes(rawName: String, fnTy: Bitcode.FunctionType, argTys: Array[Bitcode.Type]): Seq[SignatureType.Primitive] = {
      val varargTys = argTys.toList.drop(fnTy.paramTys.length)
      // Return type and parameter types are already checked during definition.
      CangjieSymLevelMaker.verifyCFunctionSignature(rawName, Bitcode.Types.VOID, varargTys.toArray, isForeign = true)

      varargTys map {
        case ty: Bitcode.FloatingPointType =>
          if (ty.bitsCount < 64) {
            shouldNotReachHere(s"floating point vararg type $ty must be promoted to 64-bit floating point")
          }
          SignatureType.Float64
        case ty: Bitcode.IntegralType =>
          ty.bitsCount match {
            case 64 => SignatureType.Int64
            case 32 => SignatureType.Int32
            case _ => shouldNotReachHere(s"short integral vararg type $ty must be promoted to 32-bit integral")
          }
        case _: Bitcode.PointerType =>
          SignatureType.Address
        case ty =>
          shouldNotReachHere(s"unexpected vararg type $ty")
      }
    }

    private def callImpl(target: MethodReference, args: Seq[Node]) = {
      assert(!target.isInstanceOf[UniversalGenericMethodReference])

      val adjustedArgs = adjustRetByVal(target, adjustMutFunc(target, args)) {
        val abiRetType = target.methodType.returnType
        if (abiRetType.isZST) {
          assert(!target.methodType.hasCFuncRetByValParameter)
          Void()
        } else {
          StackAlloc.Local(abiRetType)
        }
      }

      doCallImpl(target, adjustedArgs) { args =>
        target match {
          case mr: BitcodeMethodReference =>
            val call = BitcodeDeferred.Invoke(mr)(args: _*)
            workaroundJET15803(mr.methodType.returnType, call)
          case mr =>
            assert(!mr.isInstanceOf[UniversalGenericMethodReference])
            if (mr.isInterfCall) {
              val ciao = WeakCast(mr.refClass)(args(target.getReceiverArgIndex), WeakCast.NoCheck())
              InvokeInterface(mr, ciao)(args: _*)
            } else if (mr.methodType.hasThisTypeInfoParameter && mr.accessKind == STATIC_VIRTUAL) {
              InvokeVirtualStatic(mr)(args: _*)
            } else {
              Invoke(mr)(args: _*)
            }
        }
      }
    }

    private def universalGenericCallImpl(target: MethodReference, args: Seq[Node], instantiatedReturnType: SignatureType): Node = {
      assert(target.isInstanceOf[UniversalGenericMethodReference])

      val adjustedArgs = adjustRetByVal(target, adjustMutFunc(target, args)) {
        val abiRetType = target.methodType.returnType
        val sourceRetType = if target.hasMethod then target.method.getSignature.returnType else abiRetType
        if (sourceRetType.isVariableSizeType) {
          if (instantiatedReturnType.isVariableSizeType) {
            // Target returns variable type, so transfer a pointer to off-holder memory stack slot belonging to the current frame.
            // In turn, off-holder memory slot contains a pointer to OHM heap.
            UniversalGeneric.OffHeapMemorySlotPointer(StackAlloc.OffHeapMemory(instantiatedReturnType))
          } else if (instantiatedReturnType.isRecord) {
            // Target returns concrete record type, so transfer a pointer to a pointer to typed stack slot belonging to the current frame.
            // That allows copy.return.vst in target to work correctly.
            val typed = StackAlloc.Local(instantiatedReturnType)
            val untyped = StackAlloc.Local(SignatureType.Address)
            StoreMemory(SignatureType.Address.toAsm, SignatureType.Address, false)(untyped, typed)
            UniversalGeneric.convertHolder(instantiatedReturnType, sourceRetType)(untyped)
          } else {
            // Target doesn't use this parameter, so transfer a zero address.
            UniversalGeneric.HolderConst()
          }
        } else if (abiRetType.isZST) {
          Void()
        } else {
          StackAlloc.Local(abiRetType)
        }
      }

      doCallImpl(target, adjustedArgs) { args =>
        target match {
          case mr: ConstraintCallMethodReference =>
            UniversalGeneric.InvokeConstraintMethod(mr)(args: _*)
          case mr: InstantiatedMethodReference =>
            assert(mr.instantiatedTypeParameters.nonEmpty)
            // TODO: consider to get rid of [[UniversalGeneric.InvokeMethodWithGenericContext]]
            UniversalGeneric.InvokeMethodWithGenericContext(mr)(args: _*)
          case _ => shouldNotReachHere(target)
        }
      }
    }

    private def adjustRetByVal(target: MethodReference, args: Seq[Node])(retByVal: => Node): Seq[Node] = {
      val hasRetByVal = target.methodType.hasRetByValParameter
      val hasCFuncRetByVal = target.methodType.hasCFuncRetByValParameter
      assert(!(hasRetByVal && hasCFuncRetByVal))

      val adjustedArgs = adjustParam(args, hasRetByVal) {
        (target.methodType.getRetByValArgIdx, retByVal)
      }

      adjustParam(adjustedArgs, hasCFuncRetByVal) {
        (target.methodType.getCFuncRetByValArgIdx, retByVal)
      }
    }

    private def adjustParam(args: Seq[Node], pred: Boolean)(indexArg: => (Int, Node)) = {
      if (pred) {
        ScalaCollections.insertAt(args, indexArg._1, indexArg._2).toSeq
      } else {
        args
      }
    }

    @nowarn("msg=match may not be exhaustive")
    private def adjustMutFunc(target: MethodReference, args: Seq[Node]): Seq[Node] = {
      if (target.isCangjieMut) {
        assert(target.methodType.hasMutRecordParameter && target.methodType.hasMutObjectParameter && !target.methodType.hasReceiverParameter)
        val recordType = target match {
          case target: BitcodeMethodReference => target.getMutRecordType
          case target: MethodReference => target.method.getMutRecordType
        }
        val host = MutFunc.Host()
        val offset = MutFunc.Offset(recordType, host, args.head)
        SpecialParamSet(MutRecord, MutObject).elements.toSeq.map { // ensuring the right order
          case MutRecord => offset
          case MutObject => host
        } ++ args.tail
      } else {
        args
      }
    }

    private def doCallImpl(target: MethodReference, args: Seq[Node])(invoke: Seq[Node] => Node): Node = {
      ensurePrepared(PreparationRequired.forInvoke(target))

      // TODO: do we need nullcheck for MUT?
      if (target.hasNonRecordReceiverParameter) {
        val receiverArgIndex = target.getReceiverArgIndex
        if (target.refClass.isJavaReference) {
          javaNullCheck(args(receiverArgIndex))
        } else {
          // TODO: gen nullcheck for invokespecial, when horrible misuse of zeroValue is fixed in stdlib (#707)
          if (target.accessKind != MAK.SPECIAL) {
            nullCheckWorkaround(args(receiverArgIndex))
          }
        }
      }

      val mt = target.methodType
      val abiRetType = mt.returnType
      val res = invoke(args)
      if (abiRetType.isRecord) {
        args(if (mt.hasCFuncRetByValParameter) mt.getCFuncRetByValArgIdx else mt.getRetByValArgIdx)
      } else if (abiRetType.isZST) {
        Void()
      } else {
        res
      }
    }

    private def callIndirect(fnTy: Bitcode.FunctionType, target: Node, args: Array[Node], argTys: Array[Bitcode.Type]) = {
      assert(!fnTy.vararg, s"$fnTy vararg indirect call is not supported yet")
      assert(fnTy.paramTys sameElements argTys, s"$fnTy")
      assert(argTys forall (!_.isZST), s"$fnTy indirect call is not expected to have ZST arguments $argTys")

      val ms = MethodSignature(ty2sig(fnTy.retTy), fnTy.paramTys.map(ty2sig).toSeq)
      val (abiSig, specialParams) = makeABISignature(ms, isCFunc = true)
      val mt = MethodType(abiSig, CCALL, CallKind.CJ_FOREIGN, specialParams, fnTy.vararg)
      val mr = new MethodReference(mt, MAK.STATIC)

      val abiRetType = mt.returnType
      val adjustedArgs = adjustRetByVal(mr, args.toSeq) { 
        if (abiRetType.isZST) {
          assert(!mt.hasCFuncRetByValParameter)
          Void()
        } else {
          StackAlloc.Local(abiRetType)
        }
      }

      val res = Call(mr)(target +: adjustedArgs: _*)

      if (abiRetType.isRecord) {
        adjustedArgs(mt.getCFuncRetByValArgIdx)
      } else if (abiRetType.isZST) {
        Void()
      } else {
        res
      }
    }

    override def call(fnTy: Bitcode.FunctionType, target: Node, args: Array[Node], argTys: Array[Bitcode.Type], handlerBB: Int) = spinal {
      if (handlerBB != Bitcode.InstructionConsumer.NO_HANDLER) {
        val handler = xblockByIdx(handlerBB)
        onCommit.withCallback {
          case n: SpinalNode if n.canThrow => handler addArg n.xpoint
          case _ =>
        } {
          callUnhandled(fnTy, target, args, argTys)
        }
      } else {
        callUnhandled(fnTy, target, args, argTys)
      }
    }

    def cleanupGlobalsAndFunctions(): Unit = {
      import HLIRMetadata.Ref
      allNodes foreach {
        case n: (BCGlobalOrFunction | DeferredGetElementPtr) if n.valueUses.isEmpty => strikeOut(n)

        case n @ BCGlobal(g: Bitcode.Global) =>
          replaceByCode(n) {
            val ref = hlir.ref(g.name).get
            val declaringClass = resolver.symRefType(ref).get
            val name = resolver.symName(ref.asInstanceOf[Ref.HasName])
            val sig = resolver.typeSignature(ref.asInstanceOf[Ref.HasSignature])
            val addr = if (declaringClass.isDeferred) {
              val fieldRef = BitcodeFieldReference(SignatureType.fromSymType(declaringClass), sig, xstr(name), isWrite = false, isStatic = true)
              val fieldOp = BitcodeDeferred.GetField.static(fieldRef)()
              workaroundJET15803(sig, fieldOp)
            } else {
              val f = declaringClass.findDeclaredFieldOrNull(xstr(name), sig)
              fieldAddr(f)
            }
            // The type of record flat field will be record, so we need to cast it to expected AddrType.
            if (sig.isRecord) ReinterpretCast(addr.tpe, AddrType)(addr) else addr
          }

        case n @ BCFunction(f: Bitcode.Function) =>
          replaceByCode(n) {
            val ref = hlir.ref(f.name).get
            val declaringClass = resolver.symRefType(ref).get
            val name = resolver.symName(ref.asInstanceOf[Ref.HasName])
            val eraseZSTReturn = cond(ref) {
              case _: Ref.GlobalCFunction | _: Ref.ForeignCFunction => true
            }
            val sig = resolver.functionSignature(ref.asInstanceOf[Ref.HasSignature], f.ty.vararg, eraseZSTReturn)
            val m = declaringClass.findDeclaredMethod(xstr(name), sig)
            functionAddr(m)
          }

        case _ =>
      }
    }

    object HLIR {
      def unapply(md: Bitcode.MDItem) = hlir.ref(md)
    }
  }

  private object StdMathIntrinsic {
    import Java.Lang.MathIntrinsic.*

    // Note that not all supported by JET intrinsics are supported in Cangjie (e.g. D_ATAN2)
    // and vice-versa (e.g. CAsinh).
    def unapply(name: String) = condOpt(name) {
      case "CJ_MATH_Sin" => D_SIN
      case "CJ_MATH_Cos" => D_COS
      case "CJ_MATH_Tan" => D_TAN
      case "CJ_MATH_Asin" => D_ASIN
      case "CJ_MATH_Acos" => D_ACOS
      case "CJ_MATH_Atan" => D_ATAN
      case "CJ_MATH_Exp" => D_EXP
      case "CJ_MATH_Log" => D_LOG
      case "CJ_MATH_Sqrt" => D_SQRT
      case "CJ_MATH_Sqrtf" => F_SQRT
      case "CJ_MATH_Ceil" => D_CEIL
      case "CJ_MATH_Floor" => D_FLOOR
      case "CJ_MATH_Pow" => D_POW
      case "CJ_MATH_Fabs" => D_ABS
      case "CJ_MATH_Fabsf" => F_ABS
    }
  }

  @nowarn("msg=match may not be exhaustive")
  def ty2asm(ty: Bitcode.Type): AsmType = {
    ty match {
      case Bitcode.Types.REF                   => PTR
      case _: (Bitcode.StructType  | Bitcode.ArrayType)        => PTR
      case _: (Bitcode.PointerType | Bitcode.TypeVariableType) => I64
      case Bitcode.FloatingPointType(_, bits) => bits match {
        case 16 => F16
        case 32 => F32
        case 64 => F64
      }
      case Bitcode.IntegralType(bits) => bits match {
        case 1 |
             8  => I8
        case 16 => I16
        case 32 => I32
        case 64 => I64
      }
    }
  }

  def ty2sig(ty: Bitcode.Type)(implicit resolver: HLIRSymLevelResolver, reporter: HLIRErrorReporter): SignatureType = {
    ty match {
      case _ if ty.isZST =>
        SignatureType.Void
      case ty: Bitcode.ArrayType =>
        SignatureType.VArray(ty2sig(ty.element), ty.length)
      case Bitcode.Types.FLOAT =>
        SignatureType.Float32
      case Bitcode.Types.DOUBLE =>
        SignatureType.Float64
      case Bitcode.Types.HALF =>
        SignatureType.Float16
      case ty: Bitcode.IntegralType => ty.bitsCount match {
        case 1 => SignatureType.Boolean
        case 8 => SignatureType.Int8
        case 16 => SignatureType.Int16
        case 32 => SignatureType.Int32
        case 64 => SignatureType.Int64
      }
      case ty: Bitcode.PointerType =>
        SignatureType.Address
      case Bitcode.Types.ARRAY_SLICE =>
        SignatureType.Record(CangjieSymLevelMaker.ARRAY_SLICE_NAME)
      case ty: Bitcode.StructType =>
        resolver.refSignature(resolver.hlir.ref(ty.name).get.asInstanceOf[HLIRMetadata.Ref.Type])
      case ty: Bitcode.TypeVariableType =>
        resolver.refSignature(resolver.hlir.ref(ty.name).get.asInstanceOf[HLIRMetadata.Ref.Type])
      case Bitcode.Types.REF =>
        SignatureType.fromSymType(typeProvider.getAJObjectType)
      case _: (Bitcode.FloatingPointType | Bitcode.FunctionType | Bitcode.UniqueType) =>
        shouldNotReachHere(s"unable to create proper sig type for $ty")
    }
  }

  private def extendShortIntegral(ty: Bitcode.Type, x: Node, signExtension: Boolean) = {
    assert(!ty.isBoolean, "i1 should not be implicitly extended to another integral type")
    ty match {
      case ty: Bitcode.IntegralType if ty.bitsCount < 32 =>
        BitFieldExtract(IntType, 0, ty.getIntegerBitsNum, signExtension, x)
      case _ =>
        x
    }
  }

  private def signExtendShortIntegral(ty: Bitcode.Type, x: Node) = extendShortIntegral(ty, x, signExtension = true)
  private def zeroExtendShortIntegral(ty: Bitcode.Type, x: Node) = extendShortIntegral(ty, x, signExtension = false)

  private def signExtendToAddr(v: Node) = {
    BitFieldExtract.BFX(AddrType, 0, typeSizeInBits(AddrType) min typeSizeInBits(v.tpe), signExtension = true, v)
  }

  private def methodRefToRTSProc(methodReference: MethodReference): RTSProc = {
    val refClassName = methodReference.refClass.getName
    val refClassSimpleName = refClassName.substring(refClassName.lastIndexOf("/") + 1).replace('$', '_')
    RTSProc.valueOf("CJ_" + refClassSimpleName + "_" + methodReference.method.getName)
  }

  private def inlinedCall(target: MethodReference)(args: Node*) = {
    assert(target.method.isAJInline, s"${target}")
    assert(!target.isInterfCall)
    Invoke(target)(args: _*)
  }

  private def currentMethodSyntheticPos =
    BytecodePosition(currentInlineContext)

  private def workaroundJET15803(sig: SignatureType, x: Node): Node = {
    if (sig.isInterface && !sig.symType.isJavaReference) Deprive(sig.symType)(x) else x
  }
}

object CangjieLLVMIRParser {
  object Regex {
    // match "AtomicInt8_load" etc
    private[CangjieLLVMIRParser] val AtomicOperations: Regex = """Atomic([a-zA-Z0-9]+)_([a-zA-Z]+)""".r

    private[CangjieLLVMIRParser] val ArithmeticWithOverflowLLVMIntrinsic: Regex = """llvm\.[su](?:add|sub|mul)\.with\.overflow\.i(?:8|16|32|64)""".r
    private[CangjieLLVMIRParser] val LLVMJetMetaIntrinsic = "llvm.jet.meta"
    private[CangjieLLVMIRParser] val HLIRMetaIntrinsic = "llvm.hlir.meta"
    private[CangjieLLVMIRParser] val ThrowingSaturatingArithmeticIntrinsic: Regex = """(.*)\.([su])(.*)\..*""".r
  }
}
