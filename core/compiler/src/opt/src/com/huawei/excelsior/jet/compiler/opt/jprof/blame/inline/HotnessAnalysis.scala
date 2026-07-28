/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline

import com.huawei.excelsior.jet.compiler.options.NumOption._
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.{MarkedRegions, PlanReasoning}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning._
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline.PGOStaticAnalysisPhase.CallSiteDesc
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{CallGraph, Method}
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.Blame.profileGraph
import com.huawei.excelsior.jet.compiler.options.BoolOption._
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.util.Worklist

/**
  * Decide which methods from gathered execution profile are hot.
  *
  * @author ijorch
  */
trait HotnessAnalysis { self: InlinePlanner =>
  import HotnessAnalysis._

  /** Decide whether the given `edge` of `ProfileGraph` is hot enough to inline `edge.target` into `edge.caller`. */
  protected def isHotEnough(edge: Edge): Boolean = {
    reasoning.forEdge(edge) {
      HotEdge(hot(edge)) ||
      HotCallSite(hotCallSite(edge)) ||
      ReachableFromHot(reachableFromHot(edge))
    } || fastOrTiny(edge, reasoning)
  }

  protected def fastOrTiny(edge: Edge, reasoning: PlanReasoning): Boolean = {
    reasoning.forEdge(edge) {
      FastEdge(fast(edge)) ||
      TinyTarget(tiny(edge.target))
    }
  }

  /** For the given `edge` check whether in profile graph exists a path consisting
    * of not-cold edges that leads from some hot method to this `edge`. */
  private def reachableFromHot(edge: Edge) = {
    lazy val hotAbove = {
      // To do it, we start traversing the graph upwards from given `edge`.
      val wl = Worklist(edge)
      wl.track find { e =>                            // For each edge `e` in already formed work list
        val found = oftenCalled(e.caller)             // we check whether it grows from a method which is called often enough.
        if (!found) {                                 // If it is, we don't need to traverse further.
          wl ++= pg.inEdges(e.caller) filter notCold  // Otherwise we need to process up by not-cold incoming edges.
        }
        found
      } map (_.caller)
    }

    Explanation(hotAbove) {
      notCold(edge) && hotAbove.nonEmpty
    }
  }

  private def tiny(m: Method) = Explanation(Some(m.info.bodySize)) {
    m.info.bodySize <= tinyMethodThreshold
  }

  /** Method is considered fast if there are more hits to prologue and epilogue compared to method body. */
  private def fast(edge: Edge) = Explanation(("hh:", edge.info.heuristicHits, "ih:", edge.info.initialHits)) {
    if (edge.info.imaginary) {
      false
    } else {
      edge.info.heuristicHits / edge.info.initialHits.toDouble >= fastPathEdge
    }
  }

  /** The `edge` is definitely hot (i.e. edge.caller calls edge.target many times). */
  private def hot(edge: Edge) = Explanation(("ih:", edge.info.initialHits, "th:", totalHits)) {
    HotnessAnalysis.hot(edge, totalHits)
  }

  protected def aggregatedlyHot(edges: IterableOnce[Edge]) = HotnessAnalysis.aggregatedlyHot(edges, totalHits)

  private def ratio(hits: Int) = HotnessAnalysis.ratio(hits, totalHits)

  /** The `method` is definitely hot (i.e. either it was called many times or has a hot loop inside). */
  protected def hot(method: Method) = ratio(method.info.totalHits) >= minProfit

  /** The `method` received fair share of initial hits. */
  private def oftenCalled(method: Method) = ratio(method.info.initialHits) >= minProfit

  /** To decide whether the `edge` is not definitely cold, we analyse the `edge.caller` body. */
  private def notCold(edge: Edge): Boolean =
    staticAnalysis(edge.caller).notColdCallSites exists { case CallSiteDesc(il, target) =>
      (edge.target sameNameAndSig target) && edge.info.inlineList == il
    }
}

object HotnessAnalysis {
  private def tinyMethodThreshold = env.valueOf(JProfTinyMethodThreshold)
  private def heavyMethodThreshold = env.valueOf(JProfHeavyMethodThreshold)
  private def hotPathSizeThreshold = env.valueOf(JProfHotPathMethodThreshold)
  private def maxBodySizeThreshold = env.valueOf(JProfMaxBodyMethodThreshold)

  private def longTimeThreshold = env.valueOf(JProfLongTimeThresholdPPM) / 1000000.0
  private def fastPathEdge = env.valueOf(JProfFastPathEdgePercent) / 100.0
  private def minProfit = env.valueOf(JProfInlineMinProfitPermille) / 1000.0

  private def hotSubgraphEdgesThreshold = env.valueOf(JProfHotSubgraphEdgesThreshold) / 1000.0
  private def hotSubgraphRegionsThreshold = env.valueOf(JProfHotSubgraphRegionsThreshold) / 1000.0

  private def ratio(hits: Int, totalHits: Int) = hits / totalHits.toDouble

  /** The `edge` is definitely hot (i.e. edge.caller calls edge.target many times). */
  def hot(edge: Edge, totalHits: Int) = ratio(edge.info.initialHits, totalHits) >= minProfit

  def aggregatedlyHot(edges: IterableOnce[Edge], totalHits: Int) =
    ratio(sumBy(edges)(_.info.initialHits), totalHits) >= minProfit

  /** Method is considered to take long time based on the ratio of follow-up hits it got
    * to total hits occurred during profiling, which represent "time" measured in number of hits.
    */
  def longTime(method: Method) = ratio(method.info.followupHits, profileGraph.totalHits) >= longTimeThreshold

  /** Method is considered to be heavy if its size or the size of its hot path is greater than certain threshold. */
  def heavy(staticAnalysis: StaticAnalysis, target: Method) = {
    val bodySize = target.info.bodySize
    lazy val hotSize = staticAnalysis(target).hotBodySizeApproximation

    Explanation(("body:", bodySize, "hot:", hotSize)) {
      if (bodySize < heavyMethodThreshold) {
        false
      } else if (env.enabled(PGOHotPathBodySize)) {
        if (bodySize <= hotSize) {
          // hotSize was calculated with an error, don't use it
          bodySize >= heavyMethodThreshold
        } else {
          bodySize >= maxBodySizeThreshold || hotSize >= hotPathSizeThreshold
        }
      } else {
        true
      }
    }
  }

  /** The call site corresponding to the given `edge` is considered hot with respect to the `subGraph`. */
  def hotCallSite(edge: Edge, subGraph: Option[CallGraph] = None) = {
    env.enabled(UseMarkedRegionsInInlinePlanning) &&
      MarkedRegions.hotness(
        edge.info.inlineList.reverse,
        allowLocal = env.enabled(AllowLocalMarkedRegionHotnessInInlinePlanning),
        subGraph map (_.markedRegionsHits)
      ) == MarkedRegions.Hotness.Hot
  }

  /** The `subGraph` is considered hot inside the `baseGraph` i.e. it received a big enough part of its hits. */
  def hotSubgraph(subGraph: CallGraph, baseGraph: CallGraph) = {
    ratio(subGraph.totalHits, baseGraph.totalHits) >= hotSubgraphEdgesThreshold ||
      ratio(subGraph.markedRegionsHits, baseGraph.markedRegionsHits) >= hotSubgraphRegionsThreshold
  }
}
