/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.compiler.Env.{isWorkMode, languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.RTConst.CPUFeature
import com.huawei.excelsior.jet.compiler.options.BoolOption.{FastBackEnd, SoftFP16}
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind
import com.huawei.excelsior.jet.compiler.{Env, Environment}

import scala.collection.immutable.Range.{Inclusive as Range, inclusive as range}

/** Compiler option with integral value.
  *
  * @author paul
  * @author conwor
  */
enum NumOption(range: Range,
               defaultValue: Integer,
               defaultLambda: Environment => Integer,
               override val smartKind: SmartKind = SmartKind.Checked) extends Option[Integer] {

  case HugeMethodsLimit extends NumOption(2000)
  case HugeClinitsLimit extends NumOption(300)

  case HugeSizeLimitAfterParsingForO1  extends NumOption(Int.MaxValue)
  case SmallSizeLimitAfterParsingForO1 extends NumOption(0)

  case MaxOptimizeIterations extends NumOption(_ => if (isWorkMode) 100 else 1000)

  case ConsistencyCheckLevel extends NumOption(range(0, 3), env => {
    if (isWorkMode) {
      2
    } else if (env.enabled(FastBackEnd)) {
      0
    } else {
      1
    }
  })

  case CodegenLogsLevel extends NumOption(range(0, 3), 0)

  case CompilerCPURequirements extends NumOption(NumOption.getCPURequirements(_))

  case PGIMaxTargets extends NumOption(range(1, Integer.MAX_VALUE), 1)

  case PGIHitsCoverageThreshold extends NumOption(range(1, 100), 100) // de facto ignored if PGIMaxTargets == 1
  case PGIColdBackupPathThreshold extends NumOption(range(0, 100), 10)  // percent = 10^-2      10%

  case DiamondCrossroadBodySizeLimit            extends NumOption(20)
  case SwitchDiamondGlueCodeSizeLimit           extends NumOption(20)
  case InlineNewTinySize                        extends NumOption(56)
  case InlineBodySizeGapOfCDIBytes              extends NumOption(_ => if (targetArch == ARM64) 4 else 0)
  case InlineMaxWeightOfCDITicks                extends NumOption(64)
  case InlineMaxWeightOfRecursiveCDI            extends NumOption(_ => if (targetArch == ARM64) 80 else 68)
  case InlineMaxRecursiveDepth                  extends NumOption(1)
  case InlineMaxRecursiveDepthInPGOHosts        extends NumOption(1)
  case InlineMaxWeightOfCDINestedSync           extends NumOption(_ => if (targetArch == ARM64) 92 else 64)

  case InlineBodyWeightMultiplier               extends NumOption(100)  // percent = 10^-2     100%

  case ScalarInlineMultiplierBase               extends NumOption(9)
  case ScalarInlineMultiplierExpBase            extends NumOption(3)

  case MarkedRegionsLocalThresholdPermille      extends NumOption(200)  // permille = 10^-3     200 * 10^-3 = 0.2 = 20%
  case MarkedRegionsGlobalThresholdPPM          extends NumOption(2500) // ppm = 10^-6          2500 * 10^-6 = 25 * 10^-4 = 0.25%

  case JProfTinyMethodThreshold                 extends NumOption(70)   // in bytes
  case JProfHeavyMethodThreshold                extends NumOption(400)  // in bytes, 400 selected to ensure that `Location.init` in Life-OO-bench is inlined
  case JProfMaxBodyMethodThreshold              extends NumOption(800)  // in bytes
  case JProfHotPathMethodThreshold              extends NumOption(150)  // in bytes

  case JProfIntegrallyHotInlineBudgetForHeavy   extends NumOption(400)  // in bytes
  case JProfIntegrallyHotInlineBudgetForHotPath extends NumOption(150)  // in bytes
  case JProfHeavyLoopThreshold                  extends NumOption(25)

  case JProfHotSubgraphEdgesThreshold           extends NumOption(4)    // permille = 10^-3      4 * 10^-3 = 0.004 = 0.4%
  case JProfHotSubgraphRegionsThreshold         extends NumOption(10)   // permille = 10^-3     10 * 10^-3 = 0.01 = 1%

  case JProfInlineMinProfitPermille             extends NumOption(20)   // permille = 10^-3     20 * 10^-3 = 2%
  case JProfFastPathEdgePercent                 extends NumOption(45)   // percent = 10^-2      45%
  case JProfLongTimeThresholdPPM                extends NumOption(400)  // ppm = 10^-6          400 * 10^-6 = 4 * 10^-4 = 0.04%
  case JProfBodySizeApproximationCoefficient    extends NumOption(0)    // 0 to disable approximation, otherwise `x * 10` for actual coeff. `x`
  case PGOSpectralNorm                          extends NumOption(80)   // percent = 10^-2      80%

  case PGOIterations                            extends NumOption(20)
  case PGOIterationTopRootsLimit                extends NumOption(1)   // 0 to disable limit

  case StackCheckByCallerAdditionalValueForO1Compiled       extends NumOption(1024)

  case TableJumpTargetAlignment                 extends NumOption(0)   // 1 or less to disable alignment
  case LoopBodyAlignment                        extends NumOption(0)   // 1 or less to disable alignment

  case StreamlinedTauPercentThreshold           extends NumOption(90)

  case NewArrayInlinedHitsPrecentThreshold      extends NumOption(range(0, 100), 50)
  case NewArrayCopyInlinedHitsPrecentThreshold  extends NumOption(range(0, 100), 90)

  case UnstableSSAPeriod                        extends NumOption(4)
  case VersionedLoopImpactPercentThreshold      extends NumOption(_ => if (targetArch == ARM64) 25 else 50) // 25 -- for Caffeine.Sieve
  case PeelableLoopImpactPercentThreshold       extends NumOption(50)
  case PeelableLoopMinDepth                     extends NumOption(1)
  case PeelableLoopMaxDepth                     extends NumOption(Integer.MAX_VALUE)
  case LoopUnrollingStep                        extends NumOption(range(2, Integer.MAX_VALUE), 2)
  case FullyUnrollableLoopIterNumLimit          extends NumOption(range(0, Integer.MAX_VALUE), 8)
  case FullyUnrollableLoopIterNumLimitInTrials  extends NumOption(range(0, Integer.MAX_VALUE), 10)
  case FullyUnrollableLoopBodySizeLimit         extends NumOption(400) // 200+ for Caffeine.Float, 400+ for Life-OO-bench
  case FullyUnrollableLoopMinDepth              extends NumOption(range(1, Integer.MAX_VALUE), 2)
  case ArrayLoopUnrollingWordNum                extends NumOption(1)
  case LoopPredicationGlueCodeSizeLimit         extends NumOption(20)
  case SwitchMinTableJumpDensity                extends NumOption(80)
  case SwitchMinTableJumpCases                  extends NumOption(5)
  case SwitchMaxPlainChecks                     extends NumOption(5)
  case MaxPreExplosiveFields                    extends NumOption(2)
  case SplitBlockPartitionSize                  extends NumOption(50)

  case ForInLoopOptimizationExitLimit           extends NumOption(1) // TODO: fix JET-13298

  case FrameSlotsRecoloringMaxSPLimit             extends NumOption(100) // see JET-15252
  case FrameSlotsRecoloringMaxSPLimitForPGOHosts  extends NumOption(600)

  case MinClassAmountForParallelCompilation     extends NumOption(_ => if (languagePack.supports(CANGJIE)) 10 else 90)
  case Worker                                   extends NumOption(0)
  case PortNumber                               extends NumOption(0)

  // NOTE: Do not use this option, use [[ProjectLogic]] module
  case Parallelism                              extends NumOption(1)

  case PrefetchLevel                            extends NumOption(1) // {1, 2, 3} -- cache level

  def this(range: Range, defaultValue: Int)                     = this(range, defaultValue, null)
  def this(range: Range, defaultLambda: Environment => Integer) = this(range, null, defaultLambda)

  def this(defaultValue: Int)                     = this(null, defaultValue)
  def this(defaultLambda: Environment => Integer) = this(null, defaultLambda)

  def rangeCheck(value: Int) = {
    assert(range == null || (range.start <= value && value <= range.end))
    value
  }

  override def defaultValueOrNull(env: Environment) = if (defaultValue != null) {
    defaultValue
  } else if (defaultLambda != null) {
    defaultLambda(env)
  } else {
    null
  }

  override def parse(value: String) = value.toInt

  locally {
    register(this)
  }
}

object NumOption {
  private def getCPURequirements(env: Environment) = {
    var requirements = 0
    if (targetArch == AMD64) {
      if (!env.enabled(SoftFP16)) {
        requirements |= 1 << CPUFeature.F16C.intValue
      }
    }
    requirements
  }
}
