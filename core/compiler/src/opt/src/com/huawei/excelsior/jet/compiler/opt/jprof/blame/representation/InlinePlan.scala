/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.opt.jprof.blame.PlanReasoning
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.StronglyConnectedComponent as SCC
import com.huawei.excelsior.jet.compiler.options.BoolOption.SilentCompilation
import com.huawei.excelsior.jet.compiler.options.StrOption.OutputName
import xscala.io.{TextOutput, stdout}

/** Plan of global inline into methods listed in [[pgoHostSet]].
  *
  * @author ijorch
  */
class InlinePlan private[blame] (private[blame] val callGraph: CallGraph,
                                 val pgoHostSet: collection.Set[Method],
                                 // following fields should be used only for debug printing
                                 private[blame] val totalHits: Int,
                                 private[blame] val reasoning: PlanReasoning) {


  /* The subset of PGOHostSet which contains only root nodes based on the callgraph and the recursive hosts.
   * While PGOHostSet may contain other methods as well, for example targets of polymorphic callsites,
   * methods which are planned but not guaranteed to be inlined.
   *
   * All methods from SCC are considered to be true hosts because it is not possible to determine 
   * which of them starts the callsite chains as the inline planning is based on the graph info not call chains.
   */
  private[blame] def truePGOHostSet = {
    val sccs = SCC.collect(callGraph)
    pgoHostSet filter (m => callGraph.isRoot(m) || sccs.exists(_ contains m))
  }

  private[blame] def withAdditionalEdges(additionalEdges: collection.Set[Edge],
                                         rootsReason: PlanReasoning.Reason,
                                         edgesReason: PlanReasoning.Reason): InlinePlan = {
    if (additionalEdges.isEmpty) return this

    val g = CallGraph.sorted(Edge.deduplicate(callGraph.edges ++ additionalEdges))
    val pr = PlanReasoning(reasoning.enabled)

    val additionalRoots = additionalEdges collect { case e if g.isRoot(e.caller) => e.caller }
    pr.forRoots(additionalRoots)(rootsReason)
    pr.forEdges(additionalEdges)(edgesReason)

    new InlinePlan(
      g,
      pgoHostSet ++ additionalRoots,
      totalHits,
      reasoning ++ pr
    )
  }

  /** After each planning iteration inline plan should be limited to subgrapgs of few selected `truePGOHostSet`.
    * The other roots that have been found but not already processed are just added to the root set.
    */
  private[blame] def limitTo(topRoots: collection.Set[Method], otherRoots: collection.Set[Method]): InlinePlan = {
    val sg = callGraph.subgraph(topRoots)
    val limitedRoots = topRoots ++ otherRoots ++ (pgoHostSet intersect sg.methodSet)
    new InlinePlan(sg, limitedRoots, totalHits, reasoning.limitTo(sg, limitedRoots))
  }

  /** Print this inline plan to DOT file. */
  def printAsDOT(name: String, baseGraph: CallGraph): Unit = {
    CallGraphPrinter.printDot(callGraph, baseGraph, pgoHostSet, totalHits, reasoning, _ => false, name)
  }

  /** Print part of this inline plan containing methods matching `names` to DOT file. */
  private[blame] def printSubgraphAsDOT(names: Array[String]): Unit = {
    val methods = (callGraph.methods filter (m => names exists (n => m.toString contains n))).toSeq
    assert(methods.nonEmpty)
    for (n <- names) {
      if (!methods.exists(_.toString contains n)) {
        env.println(s"Method $n is not found in inline plan")
      }
    }

    CallGraphPrinter.printDot(callGraph.subgraph(methods, processCallers = true), null, pgoHostSet, totalHits, reasoning, _ => false, "sub-plan")
  }

  /** Print this inline plan to stdout and file as plain-text tree. */
  private[blame] def printAsPlainText(): Unit = {
    if (!env.enabled(SilentCompilation)) {
      print(stdout)
    }
    val oname = env.valueOfOrElse(OutputName, "inline")
    print(TextOutput.fromFile(s"$oname.plan"))
  }

  /** Represent this inline plan as multi-line string. */
  override def toString = TextOutput.asString { out => print(out) }

  def print(out: TextOutput, printRoots: Seq[Method] = pgoHostSet.toSeq, spaces: Boolean = true): Unit = {
    CallGraphPrinter.printText(out, callGraph, printRoots, spaces) { m =>
      if (pgoHostSet(m)) reasoning(m) mkString "," else ""
    } { e =>
      reasoning(e) mkString ","
    }
  }
}
