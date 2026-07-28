/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.inline

import com.huawei.excelsior.jet.compiler.options.BoolOption._
import com.huawei.excelsior.jet.compiler.options.NumOption._
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning._
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{StronglyConnectedComponent => SCC, _}
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.util.WhileChanged._
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, sumBy}
import com.huawei.excelsior.jet.compiler.util.Sets

import scala.collection.mutable.ArrayBuffer

/**
  * Class used to perform all inline planning.
  *
  * @author ijorch
  * @author cypok
  */
private[blame] class InlinePlanner(val pg: CallGraph,
                                   val staticAnalysis: StaticAnalysis,
                                   verbose: Boolean) extends HotnessAnalysis {

  protected def totalHits = pg.totalHits
  protected val reasoning = PlanReasoning(verbose)

  /** @return `true` iff `target` method should be inline root. */
  private def isTargetPotentialInlineRoot(edge: Edge): Boolean =
    InlinePlanner.isTargetPotentialInlineRoot(edge, staticAnalysis, reasoning)

  /** @return whether given `edge` should be added into inline plan. */
  private def isInlineCandidate(edge: Edge) = {
    !isTargetPotentialInlineRoot(edge) &&
      (Method.isAlwaysInlinedRTProc(edge.target.toSymlevel(env)) || isHotEnough(edge))
  }

  /** Usual check whether the give edge is imaginary, but can be disabled by setting `+PGOShouldIgnoreEdgeImaginaryness`. */
  private def imaginary(e: Edge) = e.info.imaginary && !env.enabled(PGOShouldIgnoreEdgeImaginaryness)

  /** All hot non-imaginary edges which caller and target are in given SCC. */
  private def innerHotEdges(scc: SCC): Iterator[Edge] =
    scc.body.iterator flatMap { m => pg.outEdges(m) filter { e => !imaginary(e) && scc.contains(e.target) && isInlineCandidate(e) } }

  /** Returns subcomponents which are strongly connected via hot edges. */
  private def hotSubSCCs(scc: SCC): Iterable[SCC] = SCC.collect(CallGraph(innerHotEdges(scc)))

  /** Inline planning consists of three steps:
    * 1. Determine what to inline in Strongly Connected Components - using [[SCC]].
    * 2. Determine what to inline in the rest of profile graph - using [[isInlineCandidate]].
    * 3. Determine which methods should be inline and/or optimization roots.
    */
  def plan(): InlinePlan = {
    // 1. Contract all hot SCCs (they are subgraphs of previously calculated SCCs).
    val sccs = SCC.collect(pg)
    val contractedSCCEdges = Sets[Edge].newQSet
    val contractedSCCRoots = Sets[Method].newQSet
    sccs flatMap hotSubSCCs foreach { scc =>
      val entrances = scc.entrances
      // Pick some node from SCC if it has no entrances.
      val roots = if (entrances.nonEmpty) entrances else Iterator.single(scc.body.head)
      contractedSCCRoots ++= roots

      contractedSCCEdges ++= innerHotEdges(scc)
    }
    reasoning.forEdges(contractedSCCEdges)(ContractedSCCEdge)

    // 2. Collect sub-graph from hot edges
    def shouldBeInlined(e: Edge) = contractedSCCEdges(e) || (!contractedSCCRoots(e.target) && isInlineCandidate(e))
    val hotEdges = Sets[Edge].newMSet(pg.edges filter shouldBeInlined)
    val roots = planInlineOfIntegrallyHot(hotEdges)
    val hotSubGraph = CallGraph.sorted(hotEdges)
    val inPlan = hotSubGraph.methodSet

    // 3. Determine inline/optimization roots
    roots ++= contractedSCCRoots; reasoning.forRoots(contractedSCCRoots)(ContractedSCCRoot)
    for {
      m <- inPlan
      if !roots(m)
      if hotSubGraph.inEdges(m).isEmpty && hotSubGraph.outEdges(m).nonEmpty
    } {
      roots += m; reasoning(m) += InlineRoot
    }

    // conflicting hot methods should be turned into roots, because we might inline only one of them
    for {
      m <- inPlan
      (_, edges) <- groupBy(hotSubGraph.outEdges(m))(e => (e.target.name, e.target.sig, e.info.inlineList))
      hotConflicts = edges.iterator.collect{ case e if hot(e.target) || HotnessAnalysis.hotCallSite(e) => e.target }.distinct.toSeq
      if hotConflicts.size > 1
      target <- hotConflicts
    } {
      roots += target; reasoning(target) += ConflictingHotMethod
    }

    // hot methods without inline plan (e.g. hot loop without any calls) should be optimized
    val optimizedHotMethods = pg.methods.filter(m => hot(m) && !inPlan(m)).toSeq
    roots ++= optimizedHotMethods; reasoning.forRoots(optimizedHotMethods)(OptimizedHotMethod)

    // methods which are planned to be inlined only through imaginary edges might not get inlined, so make them roots
    for {
      m <- inPlan
      inEdges = hotSubGraph.inEdges(m)
      if inEdges.nonEmpty && inEdges.forall(_.info.imaginary)
    } {
      roots += m; reasoning(m) += OnlyImaginaryInEdges
    }

    // adjust roots to plan inline into existing methods (e.g. JITted methods are non-existing for AOT compiler)
    val actualRoots = {
      var rs: collection.Set[Method] = roots
      whileChanged { changed =>
        rs = rs flatMap {
          case r if r.toSymlevel(env) == null =>
            changed()
            hotSubGraph.targets(r)
          case r => Iterator(r)
        }
      }
      rs
    }
    reasoning.forRoots(actualRoots diff roots)(ClosestToInaccessibleRoot)

    new InlinePlan(hotSubGraph, actualRoots, totalHits, reasoning)
  }

  /** Method is considered integrally hot if sum of hits on its incoming edges not yet planned to be inlined is big enough.
    * For such methods we additionally plan inline of hottest edges until total size of inlined body exceeds budget.
    * After that, if there are still some not-inlined edges, we mark integrally hot method as root. */
  private def planInlineOfIntegrallyHot(hotEdges: Sets[Edge]#MSet) = {
    val roots = Sets[Method].newQSet
    pg.methods filter hot foreach { m =>
      val notYetPlannedEdges = pg.inEdges(m).filter { e =>
        !imaginary(e) && !hotEdges.contains(e) && !isTargetPotentialInlineRoot(e)
      }.toBuffer.sorted

      if (aggregatedlyHot(notYetPlannedEdges)) {
        val hotSize = staticAnalysis(m).hotBodySizeApproximation

        val numberOfEdgesToInline = if (m.info.bodySize <= hotSize) {
          // hotSize was calculated with an error, don't use it
          env.valueOf(JProfIntegrallyHotInlineBudgetForHeavy) / m.info.bodySize
        } else if (hotSize == 0) {
          // empty method, inline everywhere
          notYetPlannedEdges.size
        } else {
          assert(hotSize > 0)
          env.valueOf(JProfIntegrallyHotInlineBudgetForHotPath) / hotSize
        }

        val (inlined, remaining) = notYetPlannedEdges splitAt numberOfEdgesToInline
        hotEdges ++= inlined; reasoning.forEdges(inlined)(IntegrallyHotInlinedEdge)
        if (remaining.nonEmpty) {
          roots += m
        }
      }
    }
    reasoning.forRoots(roots)(IntegrallyHotRoot)
    roots
  }

  /** Warm spectrum (orange methods) is the smallest set of methods, such that
    * percentage of hits to its methods is close to PGOSpectralNorm. */
  // TODO: find a better place for this outside of InlinePlanner (WarmSpectrum isn't about inline at all)
  def collectWarmSpectrum(): collection.Set[Method] = {
    def percentage(hits: Int) = hits * 100 / totalHits.toDouble
    val warm = Sets[Method].newMSet
    var hits = 0
    for (m <- pg.methods.to(ArrayBuffer).sortInPlaceBy(m => (-m.info.totalHits, m.info.bodySize))) {
      if (percentage(hits) < spectralNorm) {
        warm += m
        hits += m.info.totalHits
      }
    }
    warm
  }
  private val spectralNorm = env.valueOf(PGOSpectralNorm)

  def merge(mainPlan: InlinePlan, borderEdges: IterableOnce[Edge], iterPlan: InlinePlan): InlinePlan = {
    if (mainPlan == null) {
      assert(borderEdges == null)
      return iterPlan
    }
    assert(borderEdges != null)

    val borderReasoning = PlanReasoning(verbose)
    val inlinedBorder = borderEdges.iterator.filter(e => !isTargetPotentialInlineRoot(e) && fastOrTiny(e, borderReasoning)).toSeq

    val graph = CallGraph.sorted(mainPlan.callGraph.edges ++ iterPlan.callGraph.edges ++ inlinedBorder)

    val borderRoots = inlinedBorder.iterator.collect { case Edge(caller, _, _) if graph.isRoot(caller) => caller }.toSeq
    borderReasoning.forRoots(borderRoots)(InlineRoot)

    new InlinePlan(
      graph,
      mainPlan.pgoHostSet ++ iterPlan.pgoHostSet ++ borderRoots,
      mainPlan.totalHits ensuring (_ > iterPlan.totalHits && iterPlan.totalHits == totalHits),
      mainPlan.reasoning ++ iterPlan.reasoning ++ borderReasoning
    )
  }

  def printProfileGraph(name: String, baseGraph: CallGraph): Unit = {
    val potentialInlineRoots = pg.rootSet ++ (pg.edges filter isTargetPotentialInlineRoot map (_.target))
    CallGraphPrinter.printDot(pg, baseGraph, potentialInlineRoots, totalHits, reasoning, isHotEnough, name)
    reasoning.clear()
  }
}

object InlinePlanner {
  def isTargetPotentialInlineRoot(edge: Edge, staticAnalysis: StaticAnalysis, reasoning: PlanReasoning) = {
    // This method should be called using edge because only edge.target guaranteed to have valid bodySize.
    val target = edge.target

    def isInlineRoot: Boolean = reasoning.forRoot(target) {
      HeavyRoot(HotnessAnalysis.heavy(staticAnalysis, target)) ||
      LongTimeRoot(if (pgoIsAfraidOfHeavyLoops) staticAnalysis(target).isLongPath else HotnessAnalysis.longTime(target))
    }

    val targetSym = target.toSymlevel(env)

    Method.isNeverInlined(targetSym) ||
      (isInlineRoot && !Method.isAlwaysInlinedRTProc(targetSym))
  }
  private val pgoIsAfraidOfHeavyLoops = env.enabled(PGOIsAfraidOfHeavyLoops)
}
