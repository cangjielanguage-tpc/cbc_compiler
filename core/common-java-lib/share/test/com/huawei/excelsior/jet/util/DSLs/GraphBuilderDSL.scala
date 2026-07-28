/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import scala.language.implicitConversions

/** Domain Specific Language for building simple graphs.<br/>
 *  Examples:
 *  <ul>
 *    <li>`makeGraph(n0 -> n1 -> n2)` builds straight graph;</li>
 *    <li>`makeGraph(n0 -> (n1 || n2) -> n3)` builds "diamond" graph;</li>
 *    <li>`makeGraph(dw(n0 -> n1 -> n2) -> n3))` builds graph with loop.</li>
 *  </ul>
 */
trait GraphBuilderDSL[N, G] {

  protected def processAttributes(n: N, attrs: Seq[String]): Unit = {}

  /** Constructs graph given it's description.
   */
  final def makeGraph(start: SubGraph): G = {
    assert(start.enterNodes.size == 1)
    createGraph(start.enterNodes.head, start.edges)
  }

  protected def createGraph(start: N, edges: Seq[(N, N)]): G

  protected implicit def node2SubGraph(node: N): SubGraph = new SubGraph(Set(node), Set(node), List.empty)

  class SubGraph(val enterNodes: Set[N], val exitNodes: Set[N], val edges: List[(N, N)]) {
    // TODO: optimize lists concatenation: replace ++ with :::

    /** Creates new subgraph by connecting all exits from this subgraph with all enters from next subgraph.
     */
    def ->(next: SubGraph): SubGraph =
      new SubGraph(this.enterNodes,
                   next.exitNodes,
                   this.edges ++ edgesFromTo(this.exitNodes, next.enterNodes) ++ next.edges)

    /** Creates new subgraph with merged enters and merged exits.
     */
    def ||(other: SubGraph): SubGraph =
      new SubGraph(this.enterNodes | other.enterNodes,
                   this.exitNodes | other.exitNodes,
                   this.edges ++ other.edges)

    /** Creates loop with custom exits (new subgraph with extra back edges from exits to enters and given `exits` nodes as exits).
      */
    def lp(customExits: Set[N]): SubGraph =
      new SubGraph(enterNodes,
                   customExits,
                   edges ++ edgesFromTo(exitNodes, enterNodes))

    /** Creates SubGraph without exits.<br/>
     *  Example: `makeGraph(0 -> (1 || !2) -> 3) builds edges {(0,1), (0,2), (1,3)}`.
     */
    def unary_! : SubGraph = this -> end

    /** Creates new subgraph with enter and exit from this graph but with edges of both graphs.
     */
    def |>|(next: SubGraph): SubGraph =
      new SubGraph(this.enterNodes,
        this.exitNodes,
        this.edges ++ next.edges)


    def @@(attrs: String*): SubGraph = {
      assert(this.enterNodes.size == 1)
      processAttributes(this.enterNodes.head, attrs)
      this
    }


    def @@@(attrs: Seq[String]): SubGraph = @@(attrs*)

    private def edgesFromTo(from: Set[N], to: Set[N]) = for (f <- from; t <- to) yield Tuple2(f, t)

  }

  /** Creates dummy SubGraph without any edges and nodes. Used for node finalization.<br/>
   *  Example: `makeGraph(0 -> ((1 -> end) || 2) -> 3) builds edges {(0,1), (0,2), (2,3)`.
   */
  protected final def end = new SubGraph(Set.empty, Set.empty, List.empty)

  /** Nice helper for using with `loop` method.
    */
  protected final def exits(ns: N*) = Set(ns: _*)

  /** Creates loop with custom exits (new subgraph with extra back edges from exits to enters and given `exits` nodes as exits).
    */
  protected final def lp(subGraph: SubGraph, exits: Set[N]): SubGraph = subGraph.lp(exits)

  /** Creates do-while loop (new subgraph with extra back edges from exits to enters and the same exits).
   */
  protected final def dw(subGraph: SubGraph): SubGraph = subGraph.lp(subGraph.exitNodes)

  /** Creates while-do loop (new subgraph with extra back edges from exits to enters and originals enters as exits).
   */
  protected final def wd(subGraph: SubGraph): SubGraph = subGraph.lp(subGraph.enterNodes)

  /** Helper for building graph by parts (`|>|`).
   */
  protected def seq(s: SubGraph, ss: SubGraph*): SubGraph =
    ss.foldLeft(s)(_ |>| _)

}

