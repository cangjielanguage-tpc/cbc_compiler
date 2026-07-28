/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import com.huawei.excelsior.jet.util.graph.BiGraph

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

trait IntGraphBuilderDSL extends GraphBuilderDSL[Int, BiGraph[Int]] {
  // type N is used here to distinguish Int used as node and Int used as number
  type N = Int

  private var _graph: MutableGraph = _

  protected def graph: MutableGraph = _graph

  class MutableGraph(startNode: N, edges: mutable.Buffer[(N, N)]) extends BiGraph[Int] {
    val start = startNode
    def succs(n: N): Iterator[N] = edges.iterator collect { case (`n`, x) => x }
    def preds(n: N): Iterator[N] = edges.iterator collect { case (x, `n`) => x }
    override def toString = edges sortBy { _._1 } mkString ", "
    def invalidNode = Int.MinValue

    def addEdges(sg: SubGraph): Unit = { edges ++= sg.edges }
    def removeEdges(sg: SubGraph): Unit = { edges --= sg.edges }
  }

  /** Create simple graph that is backed by list of edges between integers.
    */
  protected def createGraph(startNode: N, edges: Seq[(N, N)]): MutableGraph =
    new MutableGraph(startNode, edges.to(mutable.Buffer)) tap (_graph = _)
}
