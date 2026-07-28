/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.EdgeInfo
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.StronglyConnectedComponent as SCC
import com.huawei.excelsior.jet.util.ScalaCollections.{groupMapReduce, sumBy}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.{BiGraphOnEdges, Dominators, ObjectBiGraph, ObjectGraph}

/** Annotated directional graph of [[Method]]s, with [[Edge]]s going from `caller` to `target`.
  * [[Edge]]s are annotated with [[Edge.info]] and optionally ordered.
  *
  * @author ijorch
  */
private[blame] class CallGraph protected (edges: IterableOnce[Edge],
                                          protected val edgeOrd: Option[Ordering[Edge]])
  extends BiGraphOnEdges[Method, Edge](edges, edgeOrd) with ObjectGraph[Method] { callGraph =>

  def methods = nodes
  def methodSet = nodeSet
  def callers(n: Method) = preds(n)
  def targets(n: Method) = succs(n)

  lazy val totalHits = sumBy(super.edges)(_.info.totalHits)
  lazy val markedRegionsHits = MarkedRegions.hits(this)


  /** @return `(graph, borderEdges)` where
    *         `graph` is formed by subtraction from `this` a subgraph formed by nodes & edges
    *                 dominated by `starts` but not dominated by `stops`
    *         and `borderEdges` are those edges from `this`, that
    *                 connect subtracted subgraph with the resulting one.
    */
  def subtractSubgraph(starts: IterableOnce[Method], stops: collection.Set[Method]): (CallGraph, IterableOnce[Edge]) = {
    val doms = this.withSingleStartAndBlockedNodes(stops).dominators
    val subgraphNodes = Set.from(starts.iterator flatMap doms.dominatedBy)

    def graphEdges = super.edges.filterNot(e => subgraphNodes(e.caller) || subgraphNodes(e.target))
    val graph = CallGraph(graphEdges, edgeOrd)

    val borderEdges = super.edges.filter(e =>
      (subgraphNodes(e.caller) && !subgraphNodes(e.target)) || (!subgraphNodes(e.caller) && subgraphNodes(e.target))
    ).toSeq // no reason to delay traversing

    (graph, borderEdges)
  }

  private def withSingleStartAndBlockedNodes(blocked: collection.Set[Method]) = new ObjectBiGraph[Method] {

    override def start = Method.fakeCaller
    assert(callGraph.inEdges(start).isEmpty)
    assert(callGraph.outEdges(start).isEmpty)

    /** Create iterator over minimal set of nodes unreachable from `callGraph.roots`, but from which all others are reachable. */
    private def sccRoots = SCC.collect(callGraph).iterator filter (_.entrances.isEmpty) map (_.body.head)

    private val roots = Set.from(callGraph.roots ++ sccRoots ++ blocked)

    override def succs(n: Method) = if (n eq start) roots.iterator else callGraph.succs(n) filterNot blocked
    override def preds(n: Method) = if (roots(n)) Iterator.single(start) else callGraph.preds(n)
  }

  def subgraph(methods: Iterable[Method], processCallers: Boolean = false) = {
    CallGraph.subgraphImpl(callGraph, methods, processCallers)
  }

  def croppedSubgraph(roots: Iterable[Method], cropBy: collection.Set[Method]): CallGraph = {
    CallGraph.croppedSubgraphImpl(callGraph, roots, cropBy)
  }

  def croppedSubgraph(root: Method, cropBy: collection.Set[Method]): CallGraph = {
    croppedSubgraph(Iterable.single(root), cropBy)
  }
}

private[blame] object CallGraph {

  def sorted(edges: IterableOnce[Edge]) = {
    CallGraph(edges, Some(Edge.ord))
  }

  def apply(edges: IterableOnce[Edge],
            edgeOrd: Option[Ordering[Edge]] = None) = {
    new CallGraph(edges, edgeOrd)
  }

  case class Edge(caller: Method, info: EdgeInfo, target: Method) extends BiGraphOnEdges.Edge[Method] {
    require(caller == null || info.inlineList.entries.head.method == caller)

    def src = caller
    def dst = target

    def position = (caller, target, info.inlineList)
  }

  object Edge {
    implicit object SetsAndMaps extends Sets.Default[Edge] with Maps.Default[Edge]

    implicit val ord: Ordering[Edge] = Ordering by {
      // primary sorting value is `info.initialHits`, yet others should be present to prevent collisions
      case Edge(c, EdgeInfo(hh, initialHits, fh, il, forced), t) => (-initialHits, t, hh, fh, c, il, forced)
    }

    def deduplicate(edges: IterableOnce[Edge]): IterableOnce[Edge] = {
      def sum(e1: Edge, e2: Edge) = {
        val i1 = e1.info
        val i2 = e2.info
        val edge = Edge(
          e1.caller,
          EdgeInfo(
            i1.heuristicHits + i2.heuristicHits,
            i1.initialHits + i2.initialHits,
            i1.followupHits + i2.followupHits,
            i1.inlineList,
            i1.forced || i2.forced
          ),
          e1.target
        )
        edge.info.kind = i1.kind merge i2.kind
        edge
      }

      groupMapReduce(edges)(_.position)(x => x)(sum).valuesIterator
    }
  }

  private def subgraphImpl(callGraph: CallGraph, methods: Iterable[Method], processCallers: Boolean) = {
    // weirdly enough, if this code is placed directly in CallGraph.subgraph,
    // Edge.SetsAndMaps can't be found by scalac even with explicit import
    val edges =
      Closure(methods.iterator flatMap callGraph.outEdges)(e => callGraph.outEdges(e.target)) ++ {
        if (!processCallers) Sets[Edge].newQSet
        else Closure(methods.iterator flatMap callGraph.inEdges)(e => callGraph.inEdges(e.caller))
      }

    CallGraph(edges, callGraph.edgeOrd)
  }

  private def croppedSubgraphImpl(callGraph: CallGraph, roots: Iterable[Method], cropBy: collection.Set[Method]) = {
    // weirdly enough, if this code is placed directly in CallGraph.subgraph,
    // Edge.SetsAndMaps can't be found by scalac even with explicit import
    CallGraph(
      Closure(roots flatMap callGraph.outEdges) { e =>
        if (cropBy(e.target)) Iterator.empty
        else callGraph.outEdges(e.target)
      }
    )
  }
}

private[blame] class MutableCallGraph(edges: IterableOnce[Edge], edgeOrd: Option[Ordering[Edge]])
  extends CallGraph(edges, edgeOrd) {

  override /*public*/ def addEdge(edge: Edge): Unit = {
    super.addEdge(edge)
  }
}

private[blame] object MutableCallGraph {
  def empty = MutableCallGraph(Iterator.empty)

  def sorted(edges: IterableOnce[Edge]) = {
    MutableCallGraph(edges, Some(Edge.ord))
  }

  def apply(edges: IterableOnce[Edge],
            edgeOrd: Option[Ordering[Edge]] = None) = {
    new MutableCallGraph(edges, edgeOrd)
  }
}