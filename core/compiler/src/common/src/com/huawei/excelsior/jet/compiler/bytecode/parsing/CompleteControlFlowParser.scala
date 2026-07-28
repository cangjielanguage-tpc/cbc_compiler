/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines.JsrInfo
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Complete parsing of control flow:
  *
  *  - parse control flow from method's bytecode,
  *  - build handlers sequence for every block,
  *  - clone and inline all subroutines.
  *
  * @author cypok
  */
abstract class CompleteControlFlowParser[B >: Null : ClassTag](
  protected val env: Environment,
  protected val method: VerifiableMethod,
  verify: Boolean
) extends ControlFlowParser[B](method.codeAttribute) { self =>

  private var blocks: ArrayBuffer[B] = _
  protected var _blockHandlersTree: HandlersTreeMap[B] = _

  /** Map from bytecode position of jsr instruction to next block (block after jsr). */
  private val jsrNextBlocks = mutable.HashMap.empty[Int, B]

  /** Collection of bytecode positions of ret instructions. */
  private val rets = ArrayBuffer.empty[Int]

  private var _hadSubroutines = false

  override def parse(): Unit = {
    super.parse()

    blocks = ArrayBuffer.empty[B]
    blocks ++= super.allBlocks.iterator

    _blockHandlersTree = if (codeAttr.hasExceptionTable) {
      new ExceptionHandlers().makeHandlersTree(blocks, method.getDomain)
    } else {
      new HandlersTreeMap(mutable.LinkedHashMap.empty)
    }

    afterInitialControlFlowParsing()

    val jsrInfos = collectJsrInfo()
    val rets = collectRetInfo()
    if (jsrInfos.isEmpty || rets.isEmpty) {
      _hadSubroutines = false
    } else {
      val analyzer = new SubroutineAnalyzer(jsrInfos, verify)
      val subroutines = analyzer.analyzeAndCollectSubroutines()
      _hadSubroutines = subroutines.nonEmpty
      if (_hadSubroutines) {
        val inliner = new SubroutineInliner
        inliner.inlineSubroutines(blocks, blockHandlersTree, subroutines)
      }
    }

    afterSubroutinesInlining(hadSubroutines)
  }

  override def allBlocks: collection.Seq[B] = {
    assert(blocks != null)
    blocks
  }

  def blockHandlersTree = _blockHandlersTree

  protected def hadSubroutines = _hadSubroutines

  override protected def allHandlersByExceptionTableIdx: Iterator[B] =
    throw new UnsupportedOperationException("this information becomes invalid after subroutines inlining")

  private def exceptionHandlerBlocksBeforeSubroutinesInlining: Iterator[B] = super.allHandlersByExceptionTableIdx

  override final protected def addJsr(bc: Int, block: B, targetBlock: B, nextBlock: B): Unit = {
    // note that given block may be split in future and may not contain this jsr instruction,
    // so we will get block containing this instruction later in collectJsrInfo
    jsrNextBlocks.put(bc, nextBlock)
    // jsr is interpreted as Goto to target block,
    // note that jsr is just a Goto if there is no corresponding ret
    addJump(bc, block, targetBlock)
  }

  override final protected def addRet(bc: Int, block: B, local: Int): Unit = {
    // note that given block may be splitted in future and may not contain this ret instruction,
    // so we will get block containing this instruction later in collectRetInfo
    rets += bc
    // ret is interpreted as Halt,
    // it is replaced by Goto while subroutine inlining
    addHalt(bc, block)
  }

  private def collectJsrInfo(): collection.Map[Int, JsrInfo[B]] = {
    jsrNextBlocks map { case (jsrPos, nextBlock) =>
      val jsrBlock = blockAt(jsrPos)
      val targetBlock = singleElement(succBlocks(jsrBlock))
      (jsrPos, JsrInfo(jsrBlock, targetBlock, nextBlock))
    }
  }

  private def collectRetInfo(): collection.Map[Int, B] = rets.iterator.map(pos => (pos, blockAt(pos))).toMap

  private class ExceptionHandlers extends ExceptionHandlersParser[B](codeAttr) {
    override protected def exceptionHandlerBlocks = exceptionHandlerBlocksBeforeSubroutinesInlining

    override protected def blockStartPC(block: B) = self.blockStartPC(block)

    override protected def blockEndPC(block: B) = self.blockEndPC(block)
  }

  protected def handlers(block: B) = blockHandlersTree.get(block).orNull

  protected def handlerBlocks(block: B): Iterator[B] = {
    val suffixTree = handlers(block)
    if (suffixTree == null) Iterator.empty else suffixTree.toRoot.map(_.handler)
  }

  private class SubroutineAnalyzer(jsrInfos: collection.Map[Int, JsrInfo[B]], verify: Boolean)
    extends subroutines.SubroutineAnalyzer[B](env, self.method, jsrInfos, verify) {

    override protected def entryBlock              = self.entryBlock
    override protected def succBlocks(block: B)    = self.succBlocks(block)
    override protected def handlerBlocks(block: B) = self.handlerBlocks(block)
    override protected def blockStartPC(block: B)  = self.blockStartPC(block)
    override protected def blockEndPC(block: B)    = self.blockEndPC(block)
  }

  final private class SubroutineInliner extends subroutines.SubroutineInliner[B] {
    override def entryBlock = self.entryBlock

    override def succBlocks(block: B) = self.succBlocks(block)

    override protected def cloneBlock(block: B) = self.cloneBlock(block)

    override protected def connectClonedBlockToClonedTargets(block: B, targetBlocks: Iterator[B]): Unit =
      self.connectClonedBlockToClonedTargets(block, targetBlocks)

    override protected def connectJsrRetBlockToRealTarget(block: B, targetBlock: B): Unit =
      self.connectJsrRetBlockToRealTarget(block, targetBlock)
  }

  /** Callback which is called after control flow parsing and collecting handlers sequences. */
  protected def afterInitialControlFlowParsing(): Unit = {}

  /** Callback which is called after subroutines inlining. */
  protected def afterSubroutinesInlining(hasSubroutines: Boolean): Unit = {}

  protected def blockStartPC(block: B): Int

  protected def blockEndPC(block: B): Int

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
