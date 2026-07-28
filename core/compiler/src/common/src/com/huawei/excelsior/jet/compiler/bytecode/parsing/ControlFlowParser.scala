/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.compiler.bytecode.parsing.ControlFlowParser.*
import com.huawei.excelsior.jet.compiler.bytecode.{Bytecode, BytecodeIterator, MethodCodeAttribute, OpKind}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Java bytecode control flow analysis, CFG builder and exception handlers parser.
  *
  * @author cypok
  * @author paul
  */
object ControlFlowParser {
  /** This is a position of instructions that are not present in bytecode explicitly. */
  val NO_BYTECODE_POSITION = Integer.MIN_VALUE
}

abstract class ControlFlowParser[B >: Null : ClassTag](protected val codeAttr: MethodCodeAttribute) {
  private var curBC = 0
  private var blocks: Array[B] = _
  private val handlers = ArrayBuffer.empty[B]
  private val resultingBlocks = ArrayBuffer.empty[B]

  /** Parse control instructions and exception table. */
  protected def parse(): Unit = {
    parseInstructions()
    parseExceptionTable()
    prepareResultingBlocks()
  }

  def entryBlock: B = resultingBlocks(0)

  /** Returns all blocks sorted by bc offset */
  def allBlocks: collection.Seq[B] = resultingBlocks

  /** Returns all exception handler blocks ordered by their index in the exception table */
  protected def allHandlersByExceptionTableIdx: Iterator[B] = handlers.iterator

  private def parseInstructions(): Unit = {
    import OpKind.*
    import Bytecode.*

    assert(blocks == null)
    blocks = new Array[B](codeAttr.bytecodeLength + 1)

    val bc = new BytecodeIterator(codeAttr)

    curBC = 0
    blocks(curBC) = createBlock(curBC)

    while (bc.hasNext) {
      curBC = bc.offset
      val op = bc.next()
      val nextBC = bc.offset
      var wasFallThrough = false

      op.kind match {
        case XRETURN => xreturn()
        case UNARY_IF | BINARY_IF => branch(nextBC, bc.param)

        case CONTROL => (op: @unchecked) match {
          case GOTO | GOTO_W => jump(bc.param)
          case JSR | JSR_W => jsr(nextBC, bc.param)
          case RET => ret(bc.param)
          case ATHROW => athrow()
          case TABLESWITCH => tableSwitch(bc.param(0), bc.param(1), bc.param(2), bc.getSwitchTargets)
          case LOOKUPSWITCH => lookupSwitch(bc.param(0), bc.getSwitchMatches, bc.getSwitchTargets)
        }

        case _ =>
          if (op == MONITORENTER || op == MONITOREXIT) {
            monitorOp(op)
            // StructuredLockingAnalyzer expects that each block with monitor* operation ends with this operation
            markBlock(nextBC)
          }
          fallThrough(nextBC)
          wasFallThrough = true
      }

      if (bc.hasNext) {
        if (!wasFallThrough) {
          // next instruction will be in the new block
          markBlock(nextBC)
        }
      } else {
        if (wasFallThrough) {
          // last instruction falls through,
          // current block must be unreachable in verifiable bytecode
          addHalt(NO_BYTECODE_POSITION, curBlock)
        }
      }
    }
  }

  private def parseExceptionTable(): Unit = {
    assert(handlers.isEmpty)
    val xTable = codeAttr.getExceptionTableTraverser
    while (xTable.hasNext) {
      xTable.queryNext()
      markBlock(xTable.startPC)

      val endPC = xTable.endPC
      if (endPC != codeAttr.bytecodeLength) {
        markBlock(endPC)
      }

      handlers += markBlock(xTable.handlerPC)
    }
  }

  private def prepareResultingBlocks(): Unit = {
    assert(resultingBlocks.isEmpty)

    var prevBC = -1
    var prevBlock: B = null
    for ((b, bc) <- blocks.iterator.zipWithIndex if b != null) {
      resultingBlocks += b
      if (prevBlock != null) {
        setBlockBCRange(prevBlock, prevBC, bc)
      }
      prevBC = bc
      prevBlock = b
    }
    assert(prevBlock != null) // because blocks is not empty
    setBlockBCRange(prevBlock, prevBC, codeAttr.bytecodeLength)
  }

  protected def blockAt(bc: Int): B = {
    val startPos = blocks.lastIndexWhere(_ != null, bc) ensuring (_ >= 0)
    blocks(startPos)
  }

  private def curBlock: B = blockAt(curBC)

  private def markBlock(bc: Int): B = {
    val block = blocks(bc)
    if (block != null) {
      return block
    }

    val bytecodeLength = codeAttr.bytecodeLength
    val newBlock = if (bc == bytecodeLength) {
      // Block after last bytecode instruction may be created if last instruction is e.g.
      // conditional branch. This situation is allowed by old verification, but this block
      // should be obviously unreachable, so we add Halt block end.
      val b = createBlock(NO_BYTECODE_POSITION)
      addHalt(NO_BYTECODE_POSITION, b)
      b
    } else {
      assert(bc < bytecodeLength)
      if (bc > curBC) {
        createBlock(bc)
      } else {
        splitBlock(bc, blockAt(bc))
      }
    }
    blocks(bc) = newBlock
    newBlock
  }

  private def fallThrough(nextBC: Int): Unit = {
    val nextBlock = blocks(nextBC)
    if (nextBlock != null) {
      addJump(NO_BYTECODE_POSITION, curBlock, nextBlock)
    }
  }

  private def xreturn(): Unit = addReturn(curBC, curBlock)

  private def athrow(): Unit = addThrow(curBC, curBlock)

  private def jump(targetBC: Int): Unit = {
    val target = markBlock(targetBC)
    addJump(curBC, curBlock, target)
  }

  private def branch(nextBC: Int, targetBC: Int): Unit = {
    val falseTarget = markBlock(nextBC)
    val trueTarget = markBlock(targetBC)
    addIf(curBC, curBlock, falseTarget, trueTarget)
  }

  private def tableSwitch(defaultBC: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit = {
    val targetBlocks = getSwitchCaseBlocks(bcTargets)
    val defaultBlock = markBlock(defaultBC)
    addTableSwitch(curBC, curBlock, lowMatch, highMatch, targetBlocks, defaultBlock)
  }

  private def lookupSwitch(defaultBC: Int, matches: Array[Int], bcTargets: Array[Int]): Unit = {
    val targetBlocks = getSwitchCaseBlocks(bcTargets)
    val defaultBlock = markBlock(defaultBC)
    addLookupSwitch(curBC, curBlock, matches, targetBlocks, defaultBlock)
  }

  private def getSwitchCaseBlocks(bcTargets: Array[Int]) =
    Array.tabulate(bcTargets.length){ i => markBlock(bcTargets(i)) }

  private def jsr(nextBC: Int, targetBC: Int): Unit = {
    val target = markBlock(targetBC)
    val nextBlock = markBlock(nextBC)
    addJsr(curBC, curBlock, target, nextBlock)
  }

  private def ret(local: Int): Unit = addRet(curBC, curBlock, local)

  private def monitorOp(op: Bytecode): Unit = {
    assert(op == Bytecode.MONITORENTER || op == Bytecode.MONITOREXIT)
    addMonitorOp(curBC, curBlock, op == Bytecode.MONITORENTER)
  }

  protected def createBlock(bc: Int): B

  protected def splitBlock(bc: Int, block: B): B

  protected def setBlockBCRange(block: B, start: Int, end: Int): Unit

  protected def addReturn(bc: Int, block: B): Unit

  protected def addThrow(bc: Int, block: B): Unit

  protected def addHalt(bc: Int, block: B): Unit

  protected def addJump(bc: Int, block: B, targetBlock: B): Unit

  protected def addIf(bc: Int, block: B, falseTarget: B, trueTarget: B): Unit

  protected def addTableSwitch(bc: Int, block: B, lowMatch: Int, highMatch: Int, targetBlocks: Array[B], defaultBlock: B): Unit

  protected def addLookupSwitch(bc: Int, block: B, matches: Array[Int], targetBlocks: Array[B], defaultBlock: B): Unit

  protected def addJsr(bc: Int, block: B, targetBlock: B, nextBlock: B): Unit

  protected def addRet(bc: Int, block: B, local: Int): Unit

  protected def addMonitorOp(bc: Int, block: B, isEnter: Boolean): Unit
}
