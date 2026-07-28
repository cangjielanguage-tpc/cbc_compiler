/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// TODO-DECAF: Replace with graph.ordering.TopSort.
/** Builds top-sort order (reverse post-order).
  *
  * @author cypok
  */
abstract class TopSort[B] {
  protected def entryBlock: B

  protected def succBlocks(block: B): Iterator[B]

  protected def handlerBlocks(block: B): Iterator[B]

  /** Marks block as visited. Returns `true` if this block is visited for the first time. */
  protected def markVisited(block: B): Boolean

  private val order = ListBuffer.empty[B]
  private val _numbering = mutable.HashMap.empty[B, Int]

  /** Performs DFS on all blocks from entry block. */
  protected def perform(): Unit = {
    assert(_numbering.isEmpty)
    dfs(entryBlock)
  }

  /** Returns blocks that are reachable from entry block (DFS ordering). */
  def topSortedBlocks: collection.Seq[B] = order

  /** Returns top-sort-number of given `block`. */
  def numbering(block: B) = {
    assert(order.nonEmpty)
    if (_numbering.isEmpty) {
      for ((b, i) <- order.zipWithIndex) {
        _numbering += (b -> i)
      }
    }
    _numbering(block)
  }

  private def dfs(block: B): Unit = {
    if (!markVisited(block)) {
      return
    }
    handlerBlocks(block) foreach dfs
    succBlocks(block) foreach dfs
    block +=: order
  }
}
