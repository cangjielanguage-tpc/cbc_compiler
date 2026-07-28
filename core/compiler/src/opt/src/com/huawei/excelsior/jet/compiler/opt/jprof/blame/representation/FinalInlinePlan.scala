/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.{Blame, PlanReasoning}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.InlineList.{Entry, equalEntries}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.{EdgeInfo, Hotspot}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.EdgeInfo.Bridge
import com.huawei.excelsior.jet.compiler.symlevel.Method as SymMethod
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.jprof.JProfData.Section
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType
import com.huawei.excelsior.jet.jprof.JProfWriter

import scala.annotation.tailrec

class FinalInlinePlan private[blame](callGraph: MutableCallGraph,
                                     _roots: collection.Set[Method],
                                     // following fields should be used only for debug printing
                                     totalHits: Int,
                                     reasoning: PlanReasoning) extends InlinePlan(callGraph, _roots, totalHits, reasoning) with InlinePlanBase {

  /** Complete the inline plan with a "bridge" edge which comes with a not planned statically inlined callsite.
    *
    * Bridge edges can be used only as a part of a path when looking through a call graph.
    * A bridge edge neither can be a starting nor finishing edge of a pgo inline path.
    *
    * A compilation order can effect on the moment when a given bridge edge is added and
    * thus call graph mutation depends on the compilation order.
    *
    * However static inline happens independently of the existing bridge edges,
    * so the resulting inline strategy doesn't depend on the compilation order.
    */
  def markInlined(inlineContext: InlineContext, symTarget: SymMethod, pos: Int): Unit = {
    val symCaller = inlineContext.method
    val caller = Method.fromSymlevel(symCaller)
    val target = Method.fromSymlevel(symTarget)
    if (!callGraph.callers(target).contains(caller)) {
      val info = EdgeInfo(0, 0, 0, new InlineList(Entry(caller, pos) :: Nil, reversed = true), forced = false)
      info.kind = Bridge
      callGraph.addEdge(new Edge(caller, info, target))
    }
  }

  /** @return `true` iff it was planned to inline `target` at the given position. */
  def contains(callSitePos: Position, target: SymMethod): Boolean = {
    val t = Method.fromSymlevel(target)
    traverse(callSitePos) exists (_.target == t)
  }

  /** @return `Iterator` over all `Method`s which are present in plan at given `callSitePos`. */
  def methods(callSitePos: Position): Iterator[(SymMethod, Int)] = {
    val all = traverse(callSitePos) map { e =>
      (e.target.toSymlevel(env, absenceIsFatal = true), e.info.initialHits)
    }

    all
  }

  /** @return iterator over edges suitable for inline at given position. */
  private def traverse(pos: Position): Iterator[Edge] = {
    traverse(
      InlineList.reversed(pos) ensuring (_.nonEmpty),
      pgoHostSet.iterator flatMap callGraph.outEdges filter (_.info.kind.canStart)
    ) filter (_.info.kind.canFinish)
  }

  /** @return iterator over edges suitable for inline in given inline context
    *         if `il` is a path in [[callGraph]] starting from one of the `crown` edges.
    */
  @tailrec
  private def traverse(il: InlineList, crown: Iterator[Edge]): Iterator[Edge] = il match {
    case InlineList(head, rest) =>
      val filteredEdges = crown filter (e => equalEntries(singleElement(e.info.inlineList.entries), head))
      if (rest.nonEmpty) {
        traverse(rest, filteredEdges flatMap (e => callGraph.outEdges(e.target)))
      } else {
        filteredEdges
      }

    case _ => shouldNotReachHere()
  }

  def serialize(jprofWriter: JProfWriter): Unit = {
    val nodes = Sets[Method].newQSet
    nodes ++= callGraph.nodes
    nodes ++= pgoHostSet
    nodes.foreach(_.recalculateInfo(callGraph))
    for (target <- nodes.toArray.sortInPlace()) {
      jprofWriter.entryStart(EntryType.BLAME_HOTSPOT)

      JProf.writeHotspotTarget(jprofWriter, target, pgoHostSet(target))
      for (caller <- callGraph.inEdges(target)) {
        JProf.writeHotspotCaller(jprofWriter, caller)
      }
      jprofWriter.entryEnd()
    }
    nodes.foreach(_.restoreJProfInfo())
  }
}

object FinalInlinePlan {
  def deserialize(section: Section): FinalInlinePlan = {
    // InlinePlan is serialized using the same JProf format as the original ProfileGraph, and hence same
    // parsing methods are used to deserialize it as were used to read original ProfileGraph.
    // ProfileGraph could be already read and thus inline plan should be read in a clean environment.

    Blame.isolatedJProfRead(section) { hotspots =>
      val edges = Sets[Edge].newQSet
      val roots = Sets[Method].newQSet

      for (Hotspot(target, callers) <- hotspots) {
        if (target.info.isInlineRoot) {
          roots += target
        }

        for ((caller, info) <- callers) {
          edges.add(Edge(caller, info, target))
        }
      }

      new FinalInlinePlan(MutableCallGraph.sorted(edges), roots, 0, PlanReasoning.empty)
    }
  }
}
