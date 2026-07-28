/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph.ordering

import com.huawei.excelsior.jet.util.graph.*
import com.huawei.excelsior.jet.util.{Closure, Numbering}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Base for various graph orderings/sorts/traversals.
 *
 *  @param preOrder whether node should be processed before its children
 *  @param reversed whether resulting nodes order should be reversed
 *
 *  @author cypok
 */
private[ordering] class GeneralGraphOrdering[N](graph: Graph[N],
                                                startNodes: Iterator[N],
                                                preOrder: Boolean,
                                                reversed: Boolean) extends Numbering[N] {

  private var nodeOrderNumbering: mutable.Map[N, Int] = _
  private var _order: collection.IndexedSeq[N] = _
  private var numberBias: Int = _

  recalculate()

  // Callbacks that called while graph traversal:

  /** Called after processing node and all its successors. */
  protected def afterProcess(node: N, lastProcessedNode: N): Unit = {}

  def recalculate(): Unit = {
    // function: node => order number
    nodeOrderNumbering = mutable.HashMap.empty[N, Int]

    // function: order number => node
    val nodeOrder = ArrayBuffer.empty[N]

    var nextNum = 0
    val deltaNum = if (reversed) -1 else +1

    /** Process one node independently from its predecessors and successors. */
    def processOne(x: N): Unit = {
      nodeOrderNumbering(x) = nextNum
      nodeOrder += x
      nextNum += deltaNum
    }

    def rotate(it: Iterator[N]): Iterator[N] = if (!reversed) it else {
      // Reversed iterator is used here to get more human order:
      // if node has succs (s1, s2) in reversed ordering s1 will appear
      // earlier than s2.
      it.to(ArrayBuffer).reverseIterator // ArrayBuffer has optimized reverseIterator.
    }

    Closure.withActions(mutable.HashSet.empty[N], rotate(startNodes)) { (x: N) =>
      rotate(graph.succs(x))  // succs
    } { (x: N) =>
      if (preOrder) processOne(x) // pre action
    } { (x: N) =>  // post action
      if (!preOrder) processOne(x)
      afterProcess(x, nodeOrder.last)
    }

    _order = if (reversed) nodeOrder.reverse else nodeOrder
    numberBias = if (reversed) -nextNum-1 else 0
  }

  /** Nodes order. */
  final def order: collection.IndexedSeq[N] = _order

  final def number(x: N): Int = nodeOrderNumbering(x) + numberBias

  final def contains(x: N): Boolean = nodeOrderNumbering.contains(x)
}
