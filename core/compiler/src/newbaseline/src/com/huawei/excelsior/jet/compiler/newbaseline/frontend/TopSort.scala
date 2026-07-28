/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.NotImplementedFeature.NON_RECURSIVE_TOPSORT
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TopSort private (entryBlock: Block, allBlocksCount: Int, hasHandlers: Boolean) {

  private val visited = new Array[Boolean](allBlocksCount)
  private val visitedAsHandler = if (hasHandlers) new Array[Boolean](allBlocksCount) else null
  private val ordered = new ArrayBuffer[Block](allBlocksCount)

  try {
    dfs(entryBlock)
  } catch {
    case _: StackOverflowError =>
      // Non recursive implementation requires sophisticated structures,
      // see opt's TopSort for sample implementation or reuse it
      notImplemented(NON_RECURSIVE_TOPSORT)
  }
  TopSort.reverseInPlace(ordered)

  private def dfs(b: Block): Unit = {
    // simple recursive implementation is used,
    // may be rewritten without recursion if it is needed
    visited(b.id) = true

    // process handlers first to put them later in resulting order
    for (handler <- b.handlers if !visited(handler.id)) {
      visitedAsHandler(handler.id) = true
      dfs(handler)
    }

    for (succ <- b.end.outputs if !visited(succ.id)) {
      dfs(succ)
    }

    ordered.addOne(b)
  }
}

object TopSort {

  /** Return topsort-like order where every block has at least one forward input edge. */
  def sortAndRemoveUnreachable(allBlocks: mutable.Buffer[Block], entryBlock: Block,
                               exceptionHandlersTreeRoot: SuffixTree[XHInfo[Block]]): collection.Seq[Block] = {

    Block.setIDs(allBlocks) // the following check needs these ids
    if (originalOrderIsSuitable(allBlocks, entryBlock)) {
      // Note that this fast path is not only for performance:
      // compilation of really huge methods crashes with stack overflow on slow path.
      // See JET-9345.
      return allBlocks
    }

    val topSort = new TopSort(entryBlock, allBlocks.size, exceptionHandlersTreeRoot != null)
    val topSortOrder = topSort.ordered

    if (topSortOrder.size < allBlocks.size) {
      val reachable = topSort.visited
      val reachableAsHandler = topSort.visitedAsHandler

      // remove edges from unreachable to reachable blocks:
      for (b <- allBlocks) {
        if (!reachable(b.id)) {
          val reachableSuccs = b.end.outputs filter (succ => reachable(succ.id))
          reachableSuccs foreach { _.inputs -= b.end }
          b.end.outputs --= reachableSuccs
        } else if (b.isHandler && !reachableAsHandler(b.id)) {
          b.isHandler = false
        }
      }

      if (exceptionHandlersTreeRoot != null) {
        // note that if some handler is unreachable then all its children in the tree are unreachable too
        exceptionHandlersTreeRoot.retainAll(xhInfo => reachable(xhInfo.handler.id))
      }
    }

    if (DEBUG_PRINT) {
      println()
      println("Sorted blocks and after unreachable code elimination:")
      for (b <- topSortOrder) {
        println("  " + b.description)
      }
      println()
    }

    Block.setIDs(topSortOrder) // reset proper ids
    topSortOrder
  }

  private def originalOrderIsSuitable(allBlocks: collection.Seq[Block], entryBlock: Block): Boolean = {
    val count = allBlocks.size
    if (count == 1) {
      return true
    }

    assert(allBlocks.head == entryBlock)

    // Note that we do not analyze exceptional inputs of handlers because there is no easy way to do it.
    def hasAnyForwardInput(b: Block) = b.inputs exists (in => in.block.id < b.id)

    // check that all blocks have at least one forward input
    allBlocks.iterator.drop(1) forall hasAnyForwardInput
  }

  private def reverseInPlace[T](xs: mutable.Buffer[T]): Unit = {
    val length = xs.size
    for (i <- 0 until length / 2; j = length - i - 1) {
      val tmp = xs(i)
      xs(i) = xs(j)
      xs(j) = tmp
    }
  }
}
