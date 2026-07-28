/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.util.graph.Loops

/** Strongly connected component in call graph (i.e. all methods are recursively called from each other).
  *
  * @author cypok
  */
class StronglyConnectedComponent private (g: CallGraph, val body: collection.Set[Method]) {
  def contains(m: Method): Boolean = body contains m
  def notContains(m: Method): Boolean = !contains(m)

  /** Exit has outgoing edge which target is not in this SCC. */
  def hasExit: Boolean =
    body exists { exit => g.succs(exit) exists notContains }

  /** Entrance has incoming edge which caller is not in this SCC. */
  def entrances: Iterator[Method] =
    body.iterator filter { entry => g.preds(entry) exists notContains }
}

object StronglyConnectedComponent {
  import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{StronglyConnectedComponent => SCC}
  def collect(graph: CallGraph): Iterable[SCC] = {
    // Note that body of outer loop contains all inner loops bodies
    graph.loops(graph.nodes).seq collect { case l if l.isOutermost => new SCC(graph, l.body) }
  }
}