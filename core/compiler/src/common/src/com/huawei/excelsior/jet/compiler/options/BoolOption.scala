/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.Env.{isJIT, isStandalone, isWorkMode, languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.options.BoolOption.Alias
import com.huawei.excelsior.jet.compiler.options.NumOption.JProfBodySizeApproximationCoefficient
import com.huawei.excelsior.jet.compiler.options.Option.*
import com.huawei.excelsior.jet.compiler.{Env, Environment, RTConst}

import java.lang.Boolean as JBoolean

/** Boolean compiler option.
  *
  * @author paul
  * @author conwor
  */
enum BoolOption(override val isAlias: Boolean,
                defaultValue: JBoolean,
                defaultLambda: Environment => Boolean,
                override val smartKind: SmartKind = SmartKind.Checked) extends Option[JBoolean] {

  case XCheckNull      extends BoolOption(true)
  case XCheckIndex     extends BoolOption(true)
  case XCheckArrStore  extends BoolOption(true)

  case ONoCode             extends BoolOption(false) // ONoCode compilation mode
  case ONoCodeForCJStdLib  extends BoolOption(false) // Build all CJ stdlib in CBC with ONoCode option

  // TODO: rename to O1?
  case FastBackEnd         extends BoolOption(false) // O1 compilation mode
  case O1ForCJStdLib       extends BoolOption(false) // Enable O1 compilation mode for CJ StdLib
  case SemiO1              extends BoolOption(false) // Debug option - half methods are compiled with O1 compilation mode

  // TODO-FAST-BE: try to disable for FastBackEnd. Now failed in lowerInitializationCheck.
  case ContextTypesInParsing extends BoolOption(true)

  // Might be used to skip generation of Java asserts.
  case XGenAsserts extends BoolOption(true)

  case IgnoreNumbersInPositionsOutput extends BoolOption(false)

  case ForceAddAggressiveClinitAnalysisAssert  extends BoolOption(_ => isWorkMode)
  case VerboseCrashes                          extends BoolOption(true)
  case PreparationAsserts                      extends BoolOption(_ => isWorkMode)

  case SilentCompilation                       extends BoolOption(_ => languagePack.supports(CANGJIE))
  case CleanCompilation                        extends BoolOption(_ => languagePack.supports(CANGJIE))

  case GCSafetyChecks extends BoolOption(_ => isWorkMode)

  // enable strict(er) check of the Java interop HLIR; to be removed after HLIR fully implemented by FE
  case StrictJavaInteropHLIR extends BoolOption(false)

  case UseFramePointer extends BoolOption(false)

  // Ensure that all runtime classes are compiled by opt (except for some platforms). See option NoJetRTGlobalOptim.
  case NoJetRTBackupPath extends BoolOption(_ => targetArch == AMD64) // TODO restore later || targetArch == ARM64)

  /** See also [[NumOption.HugeClinitsLimit]] & [[NumOption.HugeMethodsLimit]]. */
  case NoO1ForLongLongTime extends BoolOption(false)

  case MoveLoadsOutOfLoops           extends BoolOption(false) // Compiler is not ready yet.
  case MoveLoadsOutOfLoopsInPGOHosts extends BoolOption(true)
  case MoveRecordsOutOfLoops         extends BoolOption(false)
  case MoveBuiltinRecordsOutOfLoops  extends BoolOption(true)
  case MoveGetFieldFlat              extends BoolOption(false)
  case MoveGetStaticFlat             extends BoolOption(false)
  case MoveArrayGetFlat              extends BoolOption(false)
  case MoveFlatRecords               extends BoolOption(true)

  case MoveLoadsThroughMonitors extends BoolOption(false) // This option breaks JVM specification, should be enabled only for experiments.

  case TrustStaticFinalFields extends BoolOption(true)

  case TerminateOnNotSuppressedVerboseCrash    extends BoolOption(true)
  case XCheckStack                             extends BoolOption(true)
  case LoopPeeling                             extends BoolOption(true)
  case LoopPredication                         extends BoolOption(true)
  case DecompileStrConcat                      extends BoolOption(true)
  case SpecializeStrConcat                     extends BoolOption(true)
  case SpecializeKeyStrings                    extends BoolOption(true)
  case EliminateEquivalentPhies                extends BoolOption(true)
  case PrintGCMapsLength                       extends BoolOption(false)
  case GCMapsPreferMask                        extends BoolOption(false)
  case EscapeAnalysis                          extends BoolOption(true)
  case GenStackAlloc                           extends BoolOption(true)
  case GenStackAllocJavaCBC                    extends BoolOption(false)
  case RedundantLoadElimination                extends BoolOption(true)
  case SyncCoarsening                          extends BoolOption(true)
  case CrashOnMonitorExitException             extends BoolOption(_ => isWorkMode)
  case BoxingExplosion                         extends BoolOption(_ => targetArch != CBC) // CBC doesn't support BoxedValue
  case ScalaBoxingOptimization                 extends BoolOption(true)
  case SpecializeSimpleGetClassUses            extends BoolOption(_ => targetArch != CBC) // CBC doesn't support GetClass & [X]ClassObject
  case DisableClassNativesIntrinsification     extends BoolOption(false)
  case PreExplosion                            extends BoolOption(true)
  case PhiExplosion                            extends BoolOption(true)
  case ExpressExplosion                        extends BoolOption(true)
  case ReconstructInHotBlocks                  extends BoolOption(false)
  case FoldExplicitNullChecks                  extends BoolOption(true)
  case SimplifyCangjieOptionalRecords          extends BoolOption(false)
  case SimplifyCangjieOptionalSlices           extends BoolOption(true)

  case FailOnOOMInInterProcAnalysis            extends BoolOption(false)

  case GenerateWriteBarriers                   extends BoolOption(_ => RTConst.WriteBarriers.WRITE_BARRIERS_ENABLED.boolValue)
  case OptimizeWriteBarriers                   extends BoolOption(true)
  case PropagateWriteBarrierValue              extends BoolOption(false) // TODO: JET-15499

  case GenDebug                                extends BoolOption(false)
  case GenDebugByLinker                        extends BoolOption(false) // used in jc.tem for linker
  case FailOnUnknownDebugTypeInCBC             extends BoolOption(false)
  case ReuseRtDwarf                            extends BoolOption(false)

  case GenCoverageInCBC            extends BoolOption(false)
  case ZipCbcChunks                extends BoolOption(_ => targetArch == CBC && !isStandalone)
  case LogCbcFileStats             extends BoolOption(false)

  case AICLoopVersioning           extends BoolOption(!_.enabled(GenDebug))
  case AICLoopVersioningInPGOHosts extends BoolOption(false)

  case GenTDBarriers extends BoolOption(false)

  case ContinueCompilationAfterIncorrectGlobalOrder extends BoolOption(false)

  case NonDominatingAICLoopVersioningInPGOHosts extends BoolOption(false)
  case NonDominatingAICLoopVersioningAllArrays  extends BoolOption(false)
  case NonDominatingAICLoopVersioningAllIndices extends BoolOption(false)

  case LoopUnrolling                extends BoolOption(!_.enabled(GenDebug))
  case UnrollCompensationLoop       extends BoolOption(false)
  case UnrollScalarCompensationLoop extends BoolOption(true)
  case FullLoopUnrolling            extends BoolOption(!_.enabled(GenDebug))

  case IteratorAbsorption extends BoolOption(true)
  case SwitchDiamondAbsorption extends BoolOption(true)
  case SwitchAggregation extends BoolOption(true)

  case LoopStreamlining extends BoolOption(true)
  case GradientInvariantLifting extends BoolOption(true)

  case ForInLoopOptimization extends BoolOption(true)
  case ResidualForInLoopOptimization extends BoolOption(true)

  case InductiveVariablesWithInductiveCmp extends BoolOption(true)

  case AOTCPStats extends BoolOption(false)

  // JET-12974: as ExteriorVersioning affects code it should be CLASS_SENSITIVE
  // (as profile classes may have different value of it in comparison to app classes)
  // however CLASS_SENSITIVE options implementation prevents getting the option correctly for a particular class.
  // Make the option context insensitive as workaround.
  case ExteriorVersioning extends BoolOption(false)

  case DisableLambdaClassGeneration extends BoolOption(false)
  case DebugIrLogs extends BoolOption(false)
  case DebugIrLogsAlwaysWithPositions extends BoolOption(false)
  case ZipIrLogs extends BoolOption(false)
  case DetailedIRLogs extends BoolOption(false)
  case DetailedParsingLogs extends BoolOption(env => env.enabled(DetailedIRLogs))
  case DetailedInlineLogs extends BoolOption(env => env.enabled(DetailedIRLogs))
  case DetailedLoweringLogs extends BoolOption(env => env.enabled(DetailedIRLogs))
  case DebugHaltWithPositions extends BoolOption(false)
  case LogReconPlacement extends BoolOption(false)
  case NeverInline extends BoolOption(false)
  case NeverInlineStdlibToCBC extends BoolOption(false)
  case NoTauTests extends BoolOption(false)
  case InlineAllGNew extends BoolOption(false)
  case NoTailRec extends BoolOption(env => env.enabled(GenDebug))
  case InlineNoNew extends BoolOption(env => env.enabled(GenDebug))
  case InlineAllNew extends BoolOption(false)
  case InlineNoIfaceOps extends BoolOption(env => env.enabled(GenDebug))
  case InlineAllIfaceOps extends BoolOption(false)
  case InlineOnlyForced extends BoolOption(env => isWorkMode || isJIT || env.enabled(FastBackEnd) || env.enabled(GenDebug)) // perform inlining of @Inline(forced = false) direct methods only at enduser, see JET-11633
  case GenStackTrace extends BoolOption(_ => isWorkMode || languagePack.supports(CANGJIE))
  case VerboseProgress extends BoolOption(false)
  case VerboseHotnessAnalysis extends BoolOption(false)
  case PGOCodeLayoutOptimization extends BoolOption(true)
  case JProfWarmUpCallSitesOnHotPaths extends BoolOption(false)
  case NoAggressiveClinitAnalysis extends BoolOption(false)
  case NoAggressiveUnsafeNullCheckElimination extends BoolOption(false)
  case NoExplosion extends BoolOption(false)
  case NoFieldsTypeAnalysis extends BoolOption(true)
  case FieldsTypeAnalysisForAllFields extends BoolOption(false)
  case GlobalInitFieldsAnalysis extends BoolOption(false)
  case AlwaysLowerDeprive extends BoolOption(false)
  case NoKeyStrings extends BoolOption(_ => !RTConst.KeyObjects.KEY_OBJECTS_ALLOCATION_ENABLED.boolValue)
  case NoNewArrayCopy extends BoolOption(false)

  // Do not use this option, use [[BuildXKRN]] option instead
  case RegularBuild extends BoolOption(false) // TODO: cleanup O2 shit and replace this option with BuildXKRN

  case BuildXKRN extends BoolOption(!_.enabled(RegularBuild))

  case InstrumentTauBackupPath extends BoolOption(false)
  case InstrumentTauFastPath extends BoolOption(false)
  case NoEnrichInBaseline extends BoolOption(_ => isJIT)
  case PrintDeltaMaps extends BoolOption(false)
  case NoClinitAnalysis extends BoolOption(false)
  case DisableFrameSlotsRecoloring extends BoolOption(_.enabled(GenDebug))
  case DisableSlotsRecoloringToFRegs extends BoolOption(_ => targetArch == CBC) // TODO-NEW-ABI: enable it when volatile regs be available
  case UseAllFRegsForFrameSlotsRecoloringInPGOHosts extends BoolOption(false)
  case BuildIGOnly extends BoolOption(false)

  case DumpCangjieUML extends BoolOption(false)

  // Do not use this option for anything except [[Env.targetArch]] setup. Other code should use [[Env.targetArch]].
  case GenCBC extends BoolOption(false)

  case GenLibrary extends BoolOption(false) //TODO: Investigate if we should add a separate option for compiling the standard library - GenAotStdLib
  case GenCbcStdLib extends BoolOption(false)
  case GenProfileLibrary extends BoolOption(false)

  case GenAOTReflectionInfo extends BoolOption(false)

  case OptimizationTrials extends BoolOption(false)

  case DoCHA          extends BoolOption(false)
  case PGO            extends BoolOption(false)
  case PlainJProfile  extends BoolOption(true) // deprecated, encryption not supported anymore
  case MultipleJProfs extends BoolOption(false)
  case UseHeuristicJProfTrees extends BoolOption(true)
  case DuplicatePositionsInJprof extends BoolOption(false)

  case PGOChains extends BoolOption(false)

  case UseMarkedRegions                 extends BoolOption(true)
  case UseMarkedRegionsInInlinePlanning extends BoolOption(true)
  case MarkedRegionHotnessIsLocal       extends BoolOption(false)
  case AllowLocalMarkedRegionHotnessInInlinePlanning extends BoolOption(false)
  case PlanSubgraphLocalHotEdges        extends BoolOption(true)

  case GenerateWarmPGOAnalysisCFG       extends BoolOption(false)

  case VerboseProfileGraph              extends BoolOption(false)
  case VerboseInlinePlanning            extends BoolOption(false)
  case VerboseInlinePlanningGraphs      extends BoolOption(env => env.enabled(VerboseInlinePlanning))
  case VerboseInlinePlanningOnPG        extends BoolOption(true)
  case VerboseInlinePlanningIterations  extends BoolOption(false)
  case VerboseMarkedRegions             extends BoolOption(false)
  case TerminateAfterInlinePlanning     extends BoolOption(false)
  case PGOStaticAnalysis                extends BoolOption(true)
  case InteractivePGO                   extends BoolOption(false)

  case PGOShouldIgnoreEdgeImaginaryness extends BoolOption(false)
  case PGOIsAfraidOfHeavyLoops          extends BoolOption(false)

  case PGOUseBodySizeApproximation extends BoolOption(BoolOption.aliasOf(_.valueOf(JProfBodySizeApproximationCoefficient) != 0))
  case PGOHotPathBodySize extends BoolOption(true)

  case DiamondDust extends BoolOption(true)

  // Following PGO option is calculated and should not be set directly.
  case GenerateMarkedRegions extends BoolOption(false)
  ////

  case WorkaroundForJET12354 extends BoolOption(false)
  case WorkaroundForJET12487 extends BoolOption(false)
  case WorkaroundForJET13144 extends BoolOption(false)
  case WorkaroundForJET16453 extends BoolOption(_ => languagePack supports CANGJIE)
  case WorkaroundForJET16467 extends BoolOption(true)
  case WorkaroundForJET15957 extends BoolOption(false)

  case UseIsa12 extends BoolOption(_ => !RTConst.CangjieFusion.CANGJIE_FUSION_ENABLED.boolValue)
  case NewGlobalInitMangling extends BoolOption(false)

  case AllowInlineFromBitcode extends BoolOption(true)

  case NotFailOverToOldVerifier extends BoolOption(false)
  case CheckOldMethodTables extends BoolOption(false)
  case CollectFailStats extends BoolOption(false)
  case SmartVerbose extends BoolOption(false)
  case Timing extends BoolOption(false)
  case NoVerify extends BoolOption(false)
  case UnstableSSA extends BoolOption(false)

  case CrossroadsOptimizer extends BoolOption(true)
  case UnleashCrossroadsOptimizer extends BoolOption(false)
  case MultiCmpWorkaroundForCrossroadsOptimizer extends BoolOption(false)
  case DiamondMerge extends BoolOption(true)
  case NegDuplicateIfs extends BoolOption(true)

  case PeelAllOuterLoops extends BoolOption(false)
  case LogDirtyFrameReasons extends BoolOption(false)
  case SeaOfRTFields extends BoolOption(false)
  case OptimizeGetFlatThin extends BoolOption(false)
  case AlwaysGenerateStructuredLockingChecksInBaseline extends BoolOption(false)
  case GenerateFatalErrorOnUnstructuredLockingInOpt extends BoolOption(true)
  case CleanTarget extends BoolOption(false)
  case Arm64CASBackupPath extends BoolOption(false)
  case SoftFP16 extends BoolOption(false)
  case ArrayFillAggregation extends BoolOption(true)

  case ClosedWorld extends BoolOption(false)

  case PrintWorkersOutput extends BoolOption(false)
  case LogWorkersOutputToFile extends BoolOption(true)
  case DebugWorkers extends BoolOption(false)
  case ReuseInlinePlan extends BoolOption(false)
  case DebugInlinePlanSerialization extends BoolOption(false)

  case SplitHugeBlocks extends BoolOption(true)
  case OptimizeAllLiveRanges extends BoolOption(false)

  case HLIRParsingOptimizations extends BoolOption(true)
  case OptimizationLoop extends BoolOption(true)
  case PreInline extends BoolOption(true)
  case PostInline extends BoolOption(true)

  // Pre-lowering stuff
  case ClinitNewAbsorption extends BoolOption(true)
  case LiveRangesOptimization extends BoolOption(true)

  // These options should always be used in pair.
  // 1) If you want to disable middle stage no matter what compiler decide, use "+IgnoreMiddleStageLogic -MiddleStage"
  // 1) If you want to enable middle stage no matter what compiler decide, use "+IgnoreMiddleStageLogic +MiddleStage"
  case IgnoreMiddleStageLogic extends BoolOption(false)
  case MiddleStage extends BoolOption(false)

  case StackAllocSlotsAccessTypeMayDifferFromSlotType extends BoolOption(_ => targetArch != CBC)

  case LogBootstrapPromotion extends BoolOption(false)
  case LogBootstrapPromotionDetailed extends BoolOption(false)

  // Workaround for mod.pdb instability (module checksum may be calculated from *.zip file which is unstable). See JET-14848.
  case IgnoreModuleChecksum extends BoolOption(false)

  case IgnoreNonNullSignatureInfo extends BoolOption(true) // workaround for JET-15125
  case IgnoreHLIRTypeNames extends BoolOption(true)
  case IgnoreGenericHLIRTypeNames extends BoolOption(true)
  case IgnoreHLIRGlobalFuncNames extends BoolOption(true)
  case IgnoreHLIRGlobalVarNames extends BoolOption(true)
  case IgnoreDelayedIntrinsics extends BoolOption(false)

  case HLIRExplicitAccessModifiers extends BoolOption(true)

  case PrefetchForWrite extends BoolOption(_ => targetArch == ARM64)
  case PrefetchIsTemporal extends BoolOption(true)

  case LivenessHintsGeneration extends BoolOption(_ => !isStandalone && (targetArch == CBC || RTConst.ThreadLocalGC.TLGC_ENABLED.boolValue))
  case LivenessHintsAtBlockStart extends BoolOption(false)
  case DebugHintsGeneration extends BoolOption(_ => !isStandalone && ((isWorkMode && targetArch == CBC) || RTConst.ThreadLocalGC.TLGC_ENABLED.boolValue))

  case StackAllocZeroingForValueTypes extends BoolOption(_ => isStandalone || RTConst.ThreadLocalGC.TLGC_ENABLED.boolValue) // TODO: disable for TOP GC as soon as SmartRecordZeroing works for CBC (JET-16939)
  case SmartRecordZeroing extends BoolOption(_ => languagePack.supports(CANGJIE) && targetArch != CBC) // TODO: support for CBC (JET-16939)

  case XScala extends BoolOption(false)

  case LogUnresolvedErrors extends BoolOption(false)
  case IgnoreResolveErrors extends BoolOption(_ => languagePack != LanguagePack.SCALA)

  // For CANGJIE language pack we compile XKRN in special mode - only signature import added to classes. It helps to
  // achieve better compilation time.
  case AddImportFromConstantPool extends BoolOption(env => !env.enabled(BuildXKRN) || (languagePack != LanguagePack.CANGJIE))

  case StrictHLIRLinkageNameChecks extends BoolOption(false)

  case PackageInitFromMain extends BoolOption(true)

  case AllowMappingOfJDKIOToXScala extends BoolOption(false)

  case Evacuation extends BoolOption(_ => !isStandalone && !RTConst.WriteBarriers.WRITE_BARRIERS_ENABLED.boolValue /*TODO: support it JET-16834*/)

  case LambdaCommonSuperclass extends BoolOption(true)

  case StdCoreAnyHierarchyRoot extends BoolOption(true)

  case IdescHigh16BitsCleaning extends BoolOption(_ => RTConst.CompactHeader.COMPACT_HEADER_ENABLED.boolValue)

  case CangjieFusionMode extends BoolOption(_ => isStandalone || RTConst.CangjieFusion.CANGJIE_FUSION_ENABLED.boolValue)

  case PerformMassiveStackZeroingForCBC extends BoolOption(env => !env.enabled(UseIsa12)) // for JET-17840

  // oberon options
  case GenMethodIDs extends BoolOption(true)
  case XDebug extends BoolOption(false)
  case CompressRTData extends BoolOption(false)
  case DoNotCallImportResolver extends BoolOption(false)
  case PackPDB extends BoolOption(false)
  case Package extends BoolOption(false)
  case Prelink extends BoolOption(false)
  case PrelinkExe extends BoolOption(false)
  case OldSpringBoot extends BoolOption(false)
  case GenTomcatScripts extends BoolOption(true)
  case HideConfiguration extends BoolOption(false)
  case SplashGetFromManifest extends BoolOption(true)
  // this option is not supported, defined only for default TRUE value
  case SplashCloseOnAWTWindow extends BoolOption(true)
  case Makefile extends BoolOption(true)
  case HideInjectedFields extends BoolOption(true)
  case GenMegaObj extends BoolOption(false)  // TODO: try to use ProjectLogic

  // Do not use this option, use ProjectLogic.multiapp instead
  case Multiapp extends BoolOption(false)

  def this(defaultValue: Boolean)                 = this(false, defaultValue, null)
  def this(defaultLambda: Environment => Boolean) = this(false, null, defaultLambda)
  def this(alias: Alias)                          = this(true,  null, alias.lambda)

  override def defaultValueOrNull(env: Environment) = if (defaultValue != null) {
    defaultValue
  } else if (defaultLambda != null) {
    defaultLambda(env)
  } else {
    null
  }

  override def parse(value: String): Any = value match {
    case "true" => true
    case "false" => false
    case _ => null
  }

  locally {
    register(this)
  }
}

object BoolOption {
  private[BoolOption] case class Alias(lambda: Environment => Boolean)
  private[BoolOption] def aliasOf(lambda: Environment => Boolean) = Alias(lambda)
}
