/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines

import com.huawei.excelsior.jet.compiler.bytecode.parsing.{HandlersTreeMap, XHInfo}
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Inliner of bytecode subroutines (JSR & RET instructions).
  *
  * @author cypok
  * @author conwor
  */
abstract class SubroutineInliner[B: ClassTag] { si =>

  /** Inline subroutines, update list of all blocks and exception handlers tree.
    *
    * @param subroutines subroutines information from [[SubroutineAnalyzer]]
    */
  final def inlineSubroutines(allBlocks: mutable.Buffer[B],
                              blockHandlersTree: HandlersTreeMap[B],
                              subroutines: Iterable[Subroutine[B]]): Unit = {
    val topSort = new TopSort(subroutines, blockHandlersTree)

    val subroutinesSorted = subroutines.toSeq.sortBy(s => -topSort.numbering(s.entryBlock))

    val reachable = mutable.HashSet.empty[B]
    reachable ++= topSort.topSortedBlocks
    val subgraphCloner = new SubgraphCloner(reachable, allBlocks, blockHandlersTree)
    for (sub <- subroutinesSorted) {
      val body = collectSubroutineBody(reachable, blockHandlersTree, sub)
      doInline(sub, body, subgraphCloner)
    }
  }

  private def doInline(sub: Subroutine[B], body: collection.Set[B], cloner: SubgraphCloner): Unit = {
    val jsrs = sub.jsrs
    val ret = sub.retBlock
    val entry = sub.entryBlock
    assert(!body.contains(entryBlock)) // global entry block should not be inlined

    // inline cloned body to all jsrs but one
    for (jsr <- jsrs.tail) {
      val cloned = cloner.cloneBody(body)
      linkBody(jsr, cloned(entry), cloned(ret))
    }

    // inline original body to remaining jsr
    linkBody(jsrs.head, entry, ret)
  }

  private def linkBody(jsrInfo: JsrInfo[B], entry: B, ret: B): Unit = {
    connectJsrRetBlockToRealTarget(jsrInfo.jsrBlock, entry)
    connectJsrRetBlockToRealTarget(ret, jsrInfo.nextBlock)
  }

  private class TopSort(
    subroutines: Iterable[Subroutine[B]],
    blockHandlersTree: HandlersTreeMap[B],
  ) extends com.huawei.excelsior.jet.compiler.bytecode.parsing.TopSort[B] {

    private val retBlockToJsrNextBlocks = mutable.HashMap.empty[B, collection.Seq[B]]
    private val visited = mutable.HashSet.empty[B]

    for (s <- subroutines) {
      retBlockToJsrNextBlocks += (s.retBlock -> s.jsrs.map(_.nextBlock))
    }

    perform()

    override protected def markVisited(block: B) = visited.add(block)

    override protected def entryBlock = si.entryBlock

    override protected def handlerBlocks(block: B) = {
      blockHandlersTree.get(block) match {
        case None => Iterator.empty
        case Some(suffixTree) => suffixTree.toRoot.map(_.handler)
      }
    }

    override protected def succBlocks(block: B): Iterator[B] = {
      val normalSuccs = si.succBlocks(block)
      retBlockToJsrNextBlocks.get(block) match {
        case Some(jsrNexts) =>
          assert(normalSuccs.isEmpty)
          jsrNexts.iterator

        case None => normalSuccs
      }
    }
  }

  /** Builds set of blocks which belong to all paths from subroutine's `entryBlock` to its `retBlock`
    * This set of blocks is a body of given subroutine.
    */
  private def collectSubroutineBody(reachableBlocks: collection.Set[B],
                                    blockHandlersTree: HandlersTreeMap[B],
                                    sub: Subroutine[B]): collection.Set[B] = {

    /** Collects list of predecessors: normal and exceptional. */
    def buildBlockToPredBlocksMap(): collection.Map[B, collection.Seq[B]] = {
      val result = mutable.HashMap.empty[B, mutable.Buffer[B]]

      for (pred <- reachableBlocks; succ <- succBlocks(pred)) {
        val preds = result.getOrElseUpdate(succ, ArrayBuffer.empty[B])
        preds += pred
      }

      for ((handled, handlers) <- blockHandlersTree.iterator; xhInfo <- handlers.toRoot) {
        val handledBlocks = result.getOrElseUpdate(xhInfo.handler, ArrayBuffer.empty[B])
        handledBlocks += handled
      }

      result
    }

    val predBlocks = buildBlockToPredBlocksMap()
    val visited = mutable.LinkedHashSet.empty[B]

    def collectBlock(block: B): Unit = {
      if ((visited contains block) || !(reachableBlocks contains block)) {
        return
      }

      visited += block

      for (preds <- predBlocks.get(block); pred <- preds) {
        collectBlock(pred)
      }
    }

    // Start from ret-block.
    // Recursively go from block to its predecessors
    // (and to handled blocks in case of handler block).
    // Ignore unreachable blocks.
    // Stop on entry block of subroutine.
    visited += sub.entryBlock
    collectBlock(sub.retBlock)

    visited
  }

  private class SubgraphCloner(
    reachable: mutable.Set[B],
    allBlocks: mutable.Buffer[B],
    blockHandlersTree: HandlersTreeMap[B]
  ) {
    private var bodyToClone: collection.Set[B] = _
    private var clonedBlocks: mutable.Map[B, B] = _

    /** Clones given blocks and returns mapping from original block to cloned one.
      * Properly updates set of all reachable blocks, list of all blocks and mapping from block to its handlers.
      */
    def cloneBody(body: collection.Set[B]) = {
      assert(bodyToClone == null && clonedBlocks == null)

      bodyToClone = body
      clonedBlocks = mutable.HashMap.empty[B, B]

      body foreach cloneBlock

      val result = clonedBlocks
      clonedBlocks = null
      bodyToClone = null
      result
    }

    private def cloneBlock(block: B): B = {
      if (!bodyToClone.contains(block)) {
        return block
      }

      clonedBlocks.get(block) match {
        case Some(alreadyCloned) => return alreadyCloned
        case None =>
      }

      val cloned = si.cloneBlock(block)
      clonedBlocks += (block -> cloned)
      reachable += cloned
      allBlocks += cloned

      assert(succBlocks(cloned).isEmpty)
      connectClonedBlockToClonedTargets(cloned, cloneBlocks(succBlocks(block)))

      for (handlers <- blockHandlersTree.get(block)) {
        blockHandlersTree.put(cloned, cloneHandlers(handlers))
      }

      cloned
    }

    private def cloneBlocks(blocks: Iterator[B]) =
      // Note that it's not very safe to return lazy iterator which clones blocks on demand
      // because in such case we cannot control when cloneBlock is really called.
      blocks.map(cloneBlock).toArray.iterator

    private def cloneHandlers(handlers: SuffixTree[XHInfo[B]]): SuffixTree[XHInfo[B]] = {
      if (handlers.isRoot) {
        return handlers
      }

      val xhInfo = handlers.elem
      val handler = xhInfo.handler
      val clonedHandler = cloneBlock(handler)
      val clonedXHInfo = if (clonedHandler == handler) xhInfo else xhInfo.cloneWithHandler(clonedHandler)
      val clonedParent = cloneHandlers(handlers.parent)
      clonedParent.prepend(clonedXHInfo)
    }
  }

  protected def entryBlock: B

  protected def succBlocks(block: B): Iterator[B]

  protected def cloneBlock(block: B): B

  /** Connects given `block` with given `targetBlocks`.
    * There should be no successors of `block` before call of this method.
    */
  protected def connectClonedBlockToClonedTargets(block: B, targetBlocks: Iterator[B]): Unit

  /** Connects given `block` with given `targetBlock` using direct jump.
    * All previous successors of `block` are removed.
    */
  protected def connectJsrRetBlockToRealTarget(block: B, targetBlock: B): Unit
}
