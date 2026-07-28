/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline

import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.{CompilerPhase, IRDeserializationPhase, Phase}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase.{AnalysisResults, CallSiteDesc}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{InlineList, Method}
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.options.BoolOption.PGOHotPathBodySize
import com.huawei.excelsior.jet.compiler.options.NumOption.{JProfBodySizeApproximationCoefficient, JProfHeavyLoopThreshold}
import com.huawei.excelsior.jet.util.ScalaCollections

/**
  * Phase to calculate method's properties needed for PGO.
  *
  * @author ijorch
  */
trait PGOStaticAnalysisPhase extends Phase with IRDeserializationPhase { self: Universe with Scales =>

  private var analysisResults: AnalysisResults = _

  registerVerbose("PGO Static Analysis", Stage.PGOStaticAnalysis) {
    startPhase(CompilerPhase.PGOStaticAnalysis)

    withGCM() {
      analysisResults = AnalysisResults(
        isLongPath = {
          // TODO: consider replacing `cfg` below with `cfgWithoutXEdges` if in night-build there are no examples of the following:
          //       "there are no loops on `cfgWithoutXEdges` but there are some on `cfg` and not all of those are around MonitorExit"
          val loops = cfg.loops

          loops.nonEmpty && {
            val threshold = env.valueOf(JProfHeavyLoopThreshold)

            def hasHeavyLoops = loops.seq exists (nonInvariantLoopWeight(_) > threshold)

            // for now just estimate loop's weight. TODO: counted loops
            (threshold == 0) || (loops.seq exists (_.depth > 1)) || hasHeavyLoops
          }
        },

        notColdCallSites = {
          val exit = Return.unique
          val hotCalls = findHotCalls()

          all[Call].collect{
            case c if c.targetRef.hasMethod && ((exit exists c.dominates) || hotCalls(c))  =>
              CallSiteDesc(InlineList.reversed(c.pos), Method.fromSymlevel(c.targetRef.method))
          }.toSet
        },

        bodySizeApproximation = {
          val bodySizeApproximationCoefficient = env.valueOf(JProfBodySizeApproximationCoefficient)
          if (bodySizeApproximationCoefficient != 0) {
              ScalaCollections.sumBy(allNodes)(nodeWeight).toInt * bodySizeApproximationCoefficient / 10
          } else {
            0
          }
        },

        hotBodySizeApproximation = {
          if (!env.enabled(PGOHotPathBodySize)) {
            Int.MaxValue
          } else {
            val warmAndColdBlocks = findWarmAndColdBlocks()

            if (warmAndColdBlocks.nonEmpty) {
              ScalaCollections.sumBy(all[Node] filterNot (n => warmAndColdBlocks(n.block)))(nodeWeight).toInt
            } else {
              Int.MaxValue
            }
          }
        }
      )
    }
  }

  def runAnalysis(method: Method): AnalysisResults = {
    assert(rootMethod == method.toSymlevel(env), "Can only analyze methods for which our Universe was created.")
    run()
    analysisResults ensuring (_ != null)
  }
}

object PGOStaticAnalysisPhase {

  /** Contains results of analysis of some method. */
  case class AnalysisResults(
                              // Shows whether the analysed method have long path from entrance to exit.
                              isLongPath: Boolean,
                              // Call sites' on some specific execution paths in the analyzed method.
                              notColdCallSites: Set[CallSiteDesc],
                              // Replacement for `bodySize` calculated by node weights
                              // to be used if profiler couldn't measure actual body sizes.
                              bodySizeApproximation: Int,
                              // the total weight of hot code nodes approximates the size of hot code in bytes
                              // if option PGOHotPathBodySize is on, this value is used to select heavy inline roots
                              // Int.MaxValue means hot path approximation shouldn't be used for inline planning.
                              hotBodySizeApproximation: Int
                            )

  object AnalysisResults {
    val empty = AnalysisResults(
      isLongPath = false,
      notColdCallSites = Set.empty,
      bodySizeApproximation = 0,
      hotBodySizeApproximation = Int.MaxValue
    )
  }

  case class CallSiteDesc(il: InlineList, target: Method)
}
