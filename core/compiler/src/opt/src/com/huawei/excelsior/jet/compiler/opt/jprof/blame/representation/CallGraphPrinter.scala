/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.options.StrOption.OutputName
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.{Blame, PlanReasoning}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning.*
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.StronglyConnectedComponent as SCC
import com.huawei.excelsior.jet.jprof.JProfFormat as JPF
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.MethodInfo
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Closure
import xscala.io.TextOutput

/** Utilities for printing of given [[CallGraph]] in text or DOT format.
  *
  * @author cypok
  * @author ijorch
  * @author haitaka
  */
object CallGraphPrinter {
  private var printed = 0

  /** If `baseGraph` is not `null`, it is printed fully but with only nodes and edges of `graph` highlighted.
    * Note that it this case `graph` must be subgraph of `baseGraph`.
    * Otherwise `graph` is printed on its own.
    */
  def printDot(graph: CallGraph, baseGraph: CallGraph, roots: collection.Set[Method],
               totalHits: Int, reasoning: PlanReasoning, hot: Edge => Boolean,
               name: String
           ): Unit = {
    val (g, coloredSubgraph) = if (baseGraph == null) {
      (graph, null)
    } else {
      assert(roots.diff(baseGraph.methods.toSet).isEmpty)
      assert(graph.methods.toSet.diff(baseGraph.methods.toSet).isEmpty)
      assert(graph.edges.toSet.diff(baseGraph.edges.toSet).isEmpty)
      (baseGraph, graph)
    }

    def longMethodName(m: Method) = {
      val clid = if (m.classLoaderSID.nonEmpty) m.classLoaderSID + "\\n" else ""
      val vers = if (m.versioned) "versioned for " + m.versionedFor + "\\n" else ""
      clid + m.declaringType + "\\n" + m.name + "\\n" + vers + m.sig
    }
    def shortMethodName(m: Method) = {
      val name = if (m.name != JPF.METHOD_NAME_UNKNOWN) "." + m.name else ""
      assert(!m.versioned)
      val lastDot = m.declaringType.lastIndexOf('/')
      val cls = if (lastDot == -1) m.declaringType else m.declaringType.substring(lastDot)
      cls + name
    }

    val labels = (g.methods map { m =>
      def mi(i: MethodInfo, th: Int) = {
        def percentage(hits: Int) = hits * 100 / th.toDouble
        "%s: %.1f%%, %.4f%%".format(i, percentage(i.initialHits), percentage(i.followupHits))
      }
      val info = if (m.info eq m.profileInfo)
        mi(m.info, totalHits)
      else "current: %s\\nprofile: %s".format(
        mi(m.info, totalHits),
        mi(m.profileInfo, Blame.profileGraph.totalHits)
      )
      (m, "\"%s\\n%s%s\"".format(
        longMethodName(m),
        info,
        if (roots(m)) s"\\n${reasoning(m)}" else ""
      ))
    }).toMap

    val outDot = TextOutput.fromFile({
      val output = env.valueOfOrElse(OutputName, "jprof")
      s"$output.$printed.$name.gv"
    })

    try {
      outDot.println("digraph G {")

      outDot.println("\tnode [shape = box, style = filled];")
      outDot.println()

      // Methods' colors:
      val coloredMethod: Method => Boolean = if (coloredSubgraph == null) _ => true else coloredSubgraph.methods.toSet
      val inconsistent = (g.methods filter (_.toSymlevel(env) == null)).toSet
      val neverInlined = (g.methods filter (m => Method.isNeverInlined(m.toSymlevel(env)))).toSet
      val alwaysInlinedRT = (g.methods filter (m => Method.isAlwaysInlinedRTProc(m.toSymlevel(env)))).toSet
      for (m <- g.methods) {
        val attr = s"[fillcolor = ${
          if      (inconsistent(m))         "grey"
          else if (roots(m))                "orange"
          else if (!coloredMethod(m))       "whitesmoke, fontcolor = silver"
          else if (neverInlined(m))         "tomato"
          else if (alwaysInlinedRT(m))      "lawngreen"
          else                              "limegreen"
        }]"
        outDot.println(s"\t${labels(m)} $attr;")
      }
      outDot.println()

      // Probably-fake edges are dotted.
      // Hot edges are bold.
      // Labels show bytecode position of invoke instruction and/or its inline context.
      val coloredEdge: Edge => Boolean = if (coloredSubgraph == null) _ => true else coloredSubgraph.edges.toSet
      for (e <- g.edges) {
        val reason = reasoning(e)
        val style = {
          if      (e.info.imaginary)          "dotted"
          else if (hot(e) || reason(HotEdge)) "bold"
          else                                "solid"
        }
        val label = {
          val topBCPos = e.info.inlineList.entries.head.bcPosInMethod
          val il = e.info.inlineList.entries.tail map { case InlineList.Entry(m, bcPos) => s"\\l -> ${shortMethodName(m)}:$bcPos" } mkString ""
          "\":" + topBCPos + il + "\""
        }
        val color = {
          if      (!coloredEdge(e))                  "whitesmoke, fontcolor = silver"
          else if (reason(FastEdge))                 "purple"
          else if (reason(ReachableFromHot))         "red"
          else if (reason(HotCallSite))              "brown"
          else if (reason(IntegrallyHotInlinedEdge)) "orange"
          else if (reason(SubgraphLocalHotEdge))     "chocolate"
          else if (reason(SubgraphLocalHotCS))       "firebrick"
          else if (reason(ForcedEdge))               "midnightblue"
          else                                       "black"
        }
        outDot.println(s"\t${labels(e.caller)} -> ${labels(e.target)} [style = $style, label = $label, color = $color];")
      }
      outDot.println()

      // Group nodes by SCC.
      for ((scc, idx) <- SCC.collect(g).zipWithIndex) {
        outDot.println(s"\tsubgraph cluster_$idx {")
        outDot.println(s"\t\tcolor = lightgrey;")
        outDot.println(s"\t\tstyle = filled;")
        for (m <- scc.body) {
          outDot.println(s"\t\t${labels(m)};")
        }
        outDot.println(s"\t}")
        outDot.println()
      }

      outDot.println("}")
    } finally {
      outDot.close()
      printed += 1
    }
  }

  def printText(out: TextOutput, graph: CallGraph, roots: Seq[Method], spaces: Boolean = true)(rootInfo: Method => String = _ => "")(edgeInfo: Edge => String = _ => ""): Unit = {

    def fakeEdge(m: Method) = Edge(null, null, m)

    var offset = 0
    var printed = List.empty[Method]
    val set = Sets[Edge].newMSet
    Closure.withPostAction(set, roots.sorted map fakeEdge) { edge =>
      set -= edge // we control recursive traversing via `printed` list
      val method = edge.target
      val next = graph.outEdges(method)

      if (spaces) {
        offset += 2
      }
      out.print(" " * offset)
      if (edge.info == null) {
        out.print(s"(${rootInfo(method)}) $method")
      } else {
        val hits = edge.info.initialHits
        out.print(s"($hits) (${edgeInfo(edge)}) $method")
      }
      val il = if (edge.info == null) InlineList.empty else edge.info.inlineList
      if (il.nonEmpty) {
        val inlinedPart = if (il.length > 1) s" ${il.drop(1).reverse} inlined to" else ""
        out.print(s" called from$inlinedPart bcPos=${il.entries.head.bcPosInMethod}")
      }

      val wasPrinted = printed contains method
      printed = method :: printed
      if (wasPrinted) {
        out.println( " <-- ...")
        Iterator.empty
      } else {
        if (next.nonEmpty) {
          out.println(" <-- Set(")
        } else {
          out.println()
        }
        next
      }

    } { edge =>
      val method = edge.target
      val next = graph.outEdges(method)
      assert(printed.head == method)
      printed = printed.tail
      if (!printed.contains(method) && next.nonEmpty) {
        out.println(" " * offset + "),")
      }
      if (spaces) {
        offset -= 2
      }
    }
  }
}
