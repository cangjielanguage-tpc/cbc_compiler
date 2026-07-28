/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph.ordering

import com.huawei.excelsior.jet.util.graph.*

import scala.collection.mutable

object DepthFirstSearch {
  def apply[N](g: Graph[N]) = new DepthFirstSearch(g)
  def apply[N](g: Graph[N], startNodes: Iterator[N]) = new DepthFirstSearch(g, startNodes)
}

class DepthFirstSearch[N] protected (graph: Graph[N], startNodes: Iterator[N])
    extends GeneralGraphOrdering[N](graph, startNodes, preOrder = true, reversed = false) {

  protected def this(graph: Graph[N]) = this(graph, Iterator.single(graph.start))

  // function: node => last node's child
  private var lastChild: mutable.Map[N, N] = _

  /** Called after processing node and all its successors. */
  override protected def afterProcess(node: N, lastProcessedNode: N): Unit = {
    lastChild(node) = lastProcessedNode
  }
  
  override def recalculate(): Unit = {
    lastChild = mutable.HashMap.empty[N, N]
    super.recalculate()
  }

  /** Checks whether one node is ancestor of another in the DFS tree. */
  def isAncestor(ancestor: N, descendant: N) = {
    (ancestor == descendant) || (lteq(ancestor, descendant) && lteq(descendant, lastChild(ancestor)))
  }

}