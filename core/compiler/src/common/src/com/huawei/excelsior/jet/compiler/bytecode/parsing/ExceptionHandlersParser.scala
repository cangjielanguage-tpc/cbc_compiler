/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** [[ExceptionHandlersParser]] parses exception handlers structure by exception table.
  *
  *  - Step 1: collect for each block its possible handlers sequence.
  *    This step maps each block to its ``subtable`` - a subsequence of exception table rows such as
  *    each row's protection region contains bytecode range of the block.
  *    Note: border of every protection region must be a border of some block
  *    (i.e. it must not lay inside of any block).
  *
  *  - Step 2: build SuffixTree from this sequence.
  *    This step merges the subtables from the previous step in the following way:
  *
  *     - if two blocks have subtables with the same rows they will have reference to one subtable
  *       (will share the same subtable)
  *     - if a tail of one subtable contains the same rows of another subtable (is suffix of another subtable),
  *       the second subtable will be shared as subtable of second block and as tail of subtable of the first block.
  *
  *    Thus subtables will be organized in a SuffixTree of subtables,
  *    where there will be no duplicating subtable tails (suffixes).
  *
  *    Note, that two rows of exception table treated as equal if they have the same CatchType and HandlerPC.
  *    It allows to merge different rows of exception table which protection regions were split by javac
  *    while inlining finally clauses.
  *
  * @author cypok
  * @author conwor
  */
abstract class ExceptionHandlersParser[B](codeAttr: MethodCodeAttribute) {
  protected def exceptionHandlerBlocks: Iterator[B]

  protected def blockStartPC(block: B): Int

  protected def blockEndPC(block: B): Int

  private def initializeXHInfos(domain: Domain): Iterable[XHInfo[B]] = {
    val result = ArrayBuffer.empty[XHInfo[B]]
    val xTable = codeAttr.getExceptionTableTraverser
    val xhBlocks = exceptionHandlerBlocks
    while (xTable.hasNext && xhBlocks.hasNext) {
      xTable.queryNext()
      result += new XHInfo[B](xTable.catchTypeIndex, xTable.catchTypeName, xhBlocks.next(), domain)
    }
    assert(xTable.hasNext == xhBlocks.hasNext)
    result
  }

  /** Returns true, if block's bytecode range is contained in given exception range.
    * Throws exception if block's range is not contained in exception's one but intersected with it.
    */
  private def xRangeMatchesBlock(xStart: Int, xEnd: Int, block: B) = {
    val bStart = blockStartPC(block)
    val bEnd = blockEndPC(block)
    if (xStart <= bStart && bEnd <= xEnd) {
      true
    } else if (xEnd <= bStart || bEnd <= xStart) {
      false
    } else {
      shouldNotReachHere(s"Exception with range [$xStart, $xEnd) intersects with block range [$bStart, $bEnd)")
    }
  }

  /** Collect all the pairs (catchType, handlerBlock) which can handle exceptions thrown from the given block. */
  private def collectHandlers(block: B, xhInfos: Iterable[XHInfo[B]]) = {
    val handlers = ArrayBuffer.empty[XHInfo[B]]
    val xTable = codeAttr.getExceptionTableTraverser
    val xhInfosIter = xhInfos.iterator
    while (xTable.hasNext && xhInfosIter.hasNext) {
      xTable.queryNext()
      val xhInfo = xhInfosIter.next()
      if (xRangeMatchesBlock(xTable.startPC, xTable.endPC, block)) {
        handlers += xhInfo
      }
    }
    assert(!xTable.hasNext && !xhInfosIter.hasNext)
    handlers
  }

  /** Makes SuffixTree for all blocks handlers sequences. Returns non-empty map from block to path in built tree. */
  def makeHandlersTree(bytecodeBlocks: Iterable[B], domain: Domain): HandlersTreeMap[B] = {
    val xhInfos = initializeXHInfos(domain)
    val root = SuffixTree.newRoot[XHInfo[B]]()
    val paths =  mutable.LinkedHashMap.empty[B, SuffixTree[XHInfo[B]]]
    for (block <- bytecodeBlocks) {
      val handlers = collectHandlers(block, xhInfos)
      if (handlers.nonEmpty) {
        paths.put(block, root.prepend(handlers))
      }
    }
    new HandlersTreeMap(paths)
  }
}
