/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.InlineList.Entry
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.EdgeInfo.{Bridge, Profile}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.{EdgeInfo, Hotspot, MethodInfo}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.ProfileForest.CallTreeNode
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DuplicatePositionsInJprof, UseHeuristicJProfTrees}
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, sumBy}


/** Graph representation for blame profiling data.
  *
  * @author ijorch
  */
private[blame] class ProfileGraph (_edges: IterableOnce[Edge], val forest: ProfileForest = ProfileForest.empty)
  extends CallGraph(_edges, Some(Edge.ord)) {

  def +(that: ProfileGraph): ProfileGraph = {
    new ProfileGraph(Edge.deduplicate(this.edges ++ that.edges), forest+that.forest)
  }
}

private[blame] object ProfileGraph {
  def empty = new ProfileGraph(Iterator.empty)

  /** Build graph representation of given `hotspots`. */
  def apply(hotspots: Iterable[Hotspot]) = {
    val edges = (for {
      Hotspot(target, callers) <- hotspots.iterator
      (caller, info) <- callers
    } yield Edge(caller, info, target)).toSeq

    // TODO: There should not be different edges with the same position; see JET-13115
    val edgeGroups = groupBy(edges)(_.position).values

    if (env.enabled(DuplicatePositionsInJprof)) {
      println(s"Edge with the same position:")
      for (group <- edgeGroups) {
        if (group.size > 1) {
          for (edge <- group) {
            println(s"$edge")
          }
        }
      }
    }

    new ProfileGraph(Edge.deduplicate(edges))
  }

  def apply(rawForest: ProfileForest): ProfileGraph = {
    var edges = List.empty[Edge]

    def addEdge(method: Method, child: CallTreeNode): Unit = {
      val edgeInfo = EdgeInfo(child.heuristicHits, child.initialHits, child.followupHits, new InlineList(Entry(method, child.bcInCaller) :: Nil, reversed = true), forced = false)
      if (child.preinlined) {
        edgeInfo.kind = EdgeInfo.Bridge
      } else {
        edgeInfo.kind = EdgeInfo.Profile
      }

      edges = Edge(method, edgeInfo, child.method) :: edges
    }

    def addChildren(node: CallTreeNode): Unit = {
      addEdge(node.parent.method, node)
      node.children foreach addChildren
    }

    rawForest.entries foreach addChildren

    val profileGraph = new ProfileGraph(Edge.deduplicate(wrapPreinlinedEdges(edges)))

    for (m <- profileGraph.methods) {
      val initialHits = sumBy(profileGraph.inEdges(m))(e => e.info.initialHits)
      val followupHits = sumBy(profileGraph.inEdges(m))(e => e.info.followupHits)
      val methodInfo = MethodInfo(m.info.bodySize, initialHits, followupHits, isInlineRoot = true)
      assert(m.info.initialHits == 0)
      m.withInfo(methodInfo, accumulateHits = true)
    }

    // skip heuristic trees for more precise analysis under the option
    val forest = if (env.enabled(UseHeuristicJProfTrees)) rawForest else ProfileForest(rawForest.entries filter (n => !n.hasHeuristicHitsInSubtree))
    new ProfileGraph(profileGraph.edges, forest)
  }

  private def wrapPreinlinedEdges(edges: IterableOnce[Edge]) = {
    val cg = new ProfileGraph(Edge.deduplicate(edges))

    def traverse(edge: Edge) = {
      if (edge.info.kind == Bridge) {
        val il = edge.info.inlineList
        assert(il.entries forall (_.method != edge.target)) // no recursion for preinlined edges
        for (e <- cg.outEdges(edge.target)) yield {
          val updatedInfo = e.info.copy(
            inlineList = new InlineList(il.entries :+ Entry(edge.target, e.info.callSiteBytecodePos), il.reversed),
            forced = false
          )
          updatedInfo.kind = e.info.kind
          Edge(edge.caller, updatedInfo, e.target)
        }
      } else {
        cg.outEdges(edge.target)
      }
    }

    Closure(cg.roots flatMap cg.outEdges)(traverse) filter (_.info.kind == Profile)
  }
}
