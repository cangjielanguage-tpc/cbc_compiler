/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.parsing.ControlFlowParser.NO_BYTECODE_POSITION
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalysisResult
import com.huawei.excelsior.jet.compiler.bytecode.parsing.{CompleteControlFlowParserAndStructuredLockingAnalyzer, HandlersTreeMap, XHInfo}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.ControlFlow.ANY_BC_POS
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.SuffixTree

/** Parse bytecode, build CFG.
  *
  * @author cypok
  */
final class ControlFlow(env: Environment, rootMethod: Method)
  extends CompleteControlFlowParserAndStructuredLockingAnalyzer[Block](env, env.asVerifiableMethod(rootMethod)) {

  parse()

  if (!blockHandlersTree.isEmpty) {
    markHandlerBlocks(blockHandlersTree.root.getChildren)
    for (block <- allBlocks) {
      block.handlerInfoSequence = handlers(block)
    }
  }

  afterCompleteParsing()

  def structuredLockingState = structuredLockingInfo.state

  override def blockHasNormalExit(block: Block) = block.end.kind == Block.End.Kind.RETURN

  private def markHandlerBlocks(handlersTreeElems: Iterable[SuffixTree[XHInfo[Block]]]): Unit = {
    // this process may mark some handlers more than once, we ignore this inefficiency
    for (handlersTreeElem <- handlersTreeElems) {
      handlersTreeElem.elem.handler.isHandler = true
      markHandlerBlocks(handlersTreeElem.getChildren)
    }
  }

  override def setBlockBCRange(block: Block, start: Int, end: Int): Unit = {
    assert(block.startBC == start)
    block.endBC = end
  }

  override def blockStartPC(block: Block) = block.startBC

  override def blockEndPC(block: Block) = block.endBC

  override def succBlocks(block: Block) = block.end.outputs.iterator

  override def createBlock(bc: Int) = {
    val nonNegativeBC = if (bc != NO_BYTECODE_POSITION) bc else codeAttr.bytecodeLength
    new Block(nonNegativeBC)
  }

  private def createBlock(startBC: Int, endBC: Int) = new Block(startBC, endBC)

  override def cloneBlock(block: Block) = {
    val copy = createBlock(block.startBC, block.endBC)
    addBlockEnd(copy, block.end.kind)
    assert(!block.isHandler)
    assert(!block.hasHandler)
    copy
  }

  private def connectBlocks(src: Block, dst: Block): Unit = {
    assert(src.end != null)
    src.connectTo(dst)
  }

  override def connectClonedBlockToClonedTargets(block: Block, targetBlocks: Iterator[Block]): Unit = {
    assert(block.end != null && block.end.outputs.isEmpty)
    for (targetBlock <- targetBlocks) {
      connectBlocks(block, targetBlock)
    }
  }

  override def connectJsrRetBlockToRealTarget(block: Block, targetBlock: Block): Unit = {
    block.destroyEndAndOutputConnections()
    addJump(block, targetBlock)
  }

  override def splitBlock(bc: Int, block: Block) = {
    val newBlock = createBlock(bc)
    if (block.end != null) {
      newBlock.end = block.end
    }
    addJump(block, newBlock)
    newBlock
  }

  private def addBlockEnd(block: Block, kind: Block.End.Kind): Unit = {
    assert(block.end == null)
    block.end = new Block.End(kind)
  }

  override def addReturn(bc: Int, block: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.RETURN)
  }

  override def addThrow(bc: Int, block: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.THROW)
  }

  override def addHalt(bc: Int, block: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.HALT)
  }

  override def addJump(bc: Int, block: Block, targetBlock: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.GOTO)
    connectBlocks(block, targetBlock)
  }

  private def addJump(block: Block, targetBlock: Block): Unit = {
    addJump(ANY_BC_POS, block, targetBlock)
  }

  override def addIf(bc: Int, block: Block, falseTarget: Block, trueTarget: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.IF)
    connectBlocks(block, falseTarget)
    connectBlocks(block, trueTarget)
  }

  override def addTableSwitch(bc: Int, block: Block, lowMatch: Int, highMatch: Int, targetBlocks: Array[Block], defaultBlock: Block): Unit = {
    addSwitch(block, targetBlocks, defaultBlock)
  }

  override def addLookupSwitch(bc: Int, block: Block, matches: Array[Int], targetBlocks: Array[Block], defaultBlock: Block): Unit = {
    addSwitch(block, targetBlocks, defaultBlock)
  }

  private def addSwitch(block: Block, targetBlocks: Array[Block], defaultBlock: Block): Unit = {
    addBlockEnd(block, Block.End.Kind.SWITCH)
    connectBlocks(block, defaultBlock)
    for (target <- targetBlocks) {
      connectBlocks(block, target)
    }
  }

  override def afterInitialControlFlowParsing(): Unit = {
    if (DEBUG_PRINT) {
      printBlocks("after initial parsing of", blockDescriptionWithHandlersInfo)
    }
  }

  override def afterSubroutinesInlining(hasSubroutines: Boolean): Unit = {
    if (DEBUG_PRINT && hasSubroutines) {
      printBlocks("after subroutines inlining in", blockDescriptionWithHandlersInfo)
    }
  }

  private def afterCompleteParsing(): Unit = {
    if (DEBUG_PRINT) {
      printBlocks("after complete control flow parsing of", _.description)
    }
  }

  private def printBlocks(message: String, blockDescription: Block => String): Unit = {
    ControlFlow.printBlocks(method.getFullName, allBlocks, message, blockDescription)
  }

  private def blockDescriptionWithHandlersInfo(block: Block) = {
    assert(!block.hasHandler)
    val handlersInfo = handlers(block)
    s"${block.description}${if (handlersInfo != null) " handlers: " + handlersInfo.toRootToString else ""}"
  }
}

object ControlFlow {
  case class Result(entryBlock: Block,
                    allBlocks: collection.Seq[Block],
                    handlersTree: HandlersTreeMap[Block],
                    maxLocals: Int,
                    maxStack: Int,
                    structuredLockingState: StructuredLockingAnalysisResult)

  def parse(env: Environment, method: Method) = {
    val cf = new ControlFlow(env, method)
    val attr = method.codeAttribute
    Result(cf.entryBlock, cf.allBlocks, cf.blockHandlersTree, attr.maxLocals, attr.maxStack, cf.structuredLockingState)
  }

  // We ignore positions of block and its end, only range of bytecode is interesting.
  private val ANY_BC_POS = -1

  private[frontend] def printBlocks(methodFullName: String, blocks: collection.Seq[Block], message: String, blockDescription: Block => String): Unit = {
    println()
    println(s"Blocks $message $methodFullName:")
    for (block <- blocks) {
      println(s"  ${blockDescription(block)}")
    }
    println()
  }
}
