/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodeTypeKind, Slots}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.frontend.GlobalLiveness.*
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.SuffixTree
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged

import scala.collection.mutable

/** @param livenessAtHandlersInfo Liveness of slots on exception edge from normal block to its handler
  *                               (stack slot #0 is assumed to be not alive on this edge).
  */
final class GlobalLiveness private(livenessBlocksInfo: Array[LivenessBlockInfo],
                                   livenessAtHandlersInfo: mutable.Map[SuffixTree[XHInfo[Block]], Array[Boolean]]) {

  private def updateLivenessAtBlock(block: Block, readWriteBlocksInfo: Array[ReadWriteBlockInfo], slots: Slots) = {
    var changed = false

    val livenessInfo = livenessBlocksInfo(block.id) match {
      case null =>
        val livenessInfo = new LivenessBlockInfo(slots.totalCount)
        livenessBlocksInfo(block.id) = livenessInfo
        livenessInfo

      case v => v
    }

    val livenessAtHandlerInfo: Array[Boolean] = if (block.hasHandler) {
      livenessAtHandlersInfo.getOrElseUpdate(block.handlerInfoSequence, new Array[Boolean](slots.totalCount))
    } else {
      null
    }

    val readWriteInfo = readWriteBlocksInfo(block.id)

    for (s <- 0 until slots.totalCount) {
      val wasLiveAtEnd = livenessInfo.slotsEnd(s)
      val liveAtEnd = wasLiveAtEnd || block.end.outputs.exists { output =>
        val outputInfo = livenessBlocksInfo(output.id)
        outputInfo != null && outputInfo.slotsStart(s)
      }

      if (liveAtEnd && !wasLiveAtEnd) {
        livenessInfo.slotsEnd(s) = true
        changed = true
      }

      val liveAtHandler = (livenessAtHandlerInfo != null) && livenessAtHandlerInfo(s)

      val wasLiveAtStart = livenessInfo.slotsStart(s)
      val liveAtStart = wasLiveAtStart || readWriteInfo.slotsReadBeforeWrite(s) ||
        (liveAtEnd && !readWriteInfo.slotsWrite(s)) ||
        liveAtHandler

      if (liveAtStart && !wasLiveAtStart) {
        livenessInfo.slotsStart(s) = true
        changed = true
      }
    }

    changed
  }

  private def updateLivenessAtHandlersSeq(handlersSeq: SuffixTree[XHInfo[Block]], liveness: Array[Boolean], slots: Slots) = {
    var changed = false

    for (s <- 0 until slots.totalCount if !liveness(s)) {
      val liveAtSomeHandler = handlersSeq.toRoot exists { xhInfo =>
        val handler = xhInfo.handler
        livenessBlocksInfo(handler.id).slotsStart(s) &&
          s != slots.stackIdx(handler.exceptionObjStackIdx)
      }

      if (liveAtSomeHandler) {
        liveness(s) = true
        changed = true
      }
    }

    changed
  }

  private def debugPrint(slots: Slots, blocks: collection.Seq[Block]): Unit = {
    if (DEBUG_PRINT) {
      println()
      println("Liveness:")
      for (b <- blocks) {
        println(s"  block $b")
        print("    start, locals: {")
        for (i <- 0 until slots.localsCount) {
          print(if (isSlotAliveAtBlockStart(slots.localIdx(i), b)) "+" else "-")
        }
        print("}, stack: {")
        for (i <- 0 until slots.stackCount) {
          print(if (isSlotAliveAtBlockStart(slots.stackIdx(i), b)) "+" else "-")
        }
        println("}")
        print("      end, locals: {")
        for (i <- 0 until slots.localsCount) {
          print(if (isSlotAliveAtBlockEnd(slots.localIdx(i), b)) "+" else "-")
        }
        print("}, stack: {")
        for (i <- 0 until slots.stackCount) {
          print(if (isSlotAliveAtBlockEnd(slots.stackIdx(i), b)) "+" else "-")
        }
        println("}")

        if (b.hasHandler) {
          print("  handler, locals: {")
          for (i <- 0 until slots.localsCount) {
            print(if (isSlotAliveAtHandler(slots.localIdx(i), b)) "+" else "-")
          }
          print("}, stack: {")
          for (i <- 0 until slots.stackCount) {
            print(if (isSlotAliveAtHandler(slots.stackIdx(i), b)) "+" else "-")
          }
          println("}")
        }

        println()
      }
    }
  }

  def isSlotAliveAtBlockStart(slotIdx: Int, block: Block) = {
    livenessBlocksInfo(block.id).slotsStart(slotIdx)
  }

  def isSlotAliveAtBlockEnd(slotIdx: Int, block: Block) = {
    livenessBlocksInfo(block.id).slotsEnd(slotIdx)
  }

  def isSlotAliveAtHandler(slotIdx: Int, block: Block) = {
    block.hasHandler && livenessAtHandlersInfo(block.handlerInfoSequence)(slotIdx)
  }
}

object GlobalLiveness {

  def analyze(method: Method, slots: Slots,
              blocks: collection.Seq[Block], entryBlock: Block, hasExceptionHandlers: Boolean) = {

    if (blocks.size == 1 && blocks.head.inputs.isEmpty) {
      entryBlock.stackHeightAtStart = 0

      val livenessBlocksInfo = livenessForSingleBlock(method, slots, entryBlock)

      assert(!hasExceptionHandlers)
      val analyzer = new GlobalLiveness(livenessBlocksInfo, null)

      analyzer.debugPrint(slots, blocks)
      analyzer

    } else {
      val readWriteBlocksInfo = new Array[ReadWriteBlockInfo](blocks.size)

      for (block <- blocks) {
        block.stackHeightAtStart = calculateStartStackHeightFor(block, entryBlock, readWriteBlocksInfo)

        val info = new ReadWriteBlockInfo(slots.totalCount)
        val processor = new ReadWriteProcessor(method, slots, block, info)
        processor.iterateBytecode()

        val stackHeightEnd = processor.curStackHeight
        info.stackHeightEnd = stackHeightEnd
        readWriteBlocksInfo(block.id) = info
      }

      val livenessBlocksInfo = new Array[LivenessBlockInfo](blocks.size)
      val livenessAtHandlersInfo = if (hasExceptionHandlers) {
        new mutable.HashMap[SuffixTree[XHInfo[Block]], Array[Boolean]]
      } else {
        null
      }

      val analyzer = new GlobalLiveness(livenessBlocksInfo, livenessAtHandlersInfo)

      // Right now it is implemented as a backward data flow analysis
      // which is not linear in time in case of loops.
      // TODO: optimize it to cache readBeforeWrite & write information for the whole loop body.
      whileChanged { changed =>
        for (b <- blocks.reverseIterator) {
          if (analyzer.updateLivenessAtBlock(b, readWriteBlocksInfo, slots)) {
            changed()
          }
        }

        if (livenessAtHandlersInfo != null) {
          for ((key, value) <- livenessAtHandlersInfo) {
            if (analyzer.updateLivenessAtHandlersSeq(key, value, slots)) {
              changed()
            }
          }
        }
      }

      analyzer.debugPrint(slots, blocks)
      analyzer
    }
  }

  private class ReadWriteBlockInfo(slotsCount: Int) {
    // TODO: this may be replaced by the single array or somehow

    val slotsReadBeforeWrite = new Array[Boolean](slotsCount)
    val slotsWrite = new Array[Boolean](slotsCount)
    var stackHeightEnd = -1
  }

  private class LivenessBlockInfo(slotsCount: Int) {
    // TODO: this may be replaced by the single array or somehow

    val slotsStart = new Array[Boolean](slotsCount)
    val slotsEnd = new Array[Boolean](slotsCount)
  }

  /** It is easy to calculate liveness for method with the only block without back edge:
    * all parameters are alive at the start and everything is dead at the end.
    */
  private def livenessForSingleBlock(method: Method, slots: Slots, entryBlock: Block) = {
    val livenessInfo = new LivenessBlockInfo(slots.totalCount)

    // Actually, some parameters may be unused in the method.
    // But it is ok, BlockLivenessAnalyzer will figure this out and mark them as "dead".
    val methodType = method.getRealMethodType(null)
    var localIdx = 0
    for (p <- 0 until methodType.parameterCount) {
      val paramTypeKind = methodType.parameterType(p).jbcKindErased
      livenessInfo.slotsStart(slots.localIdx(localIdx)) = true
      localIdx += paramTypeKind.nslots
    }

    assert(entryBlock.id == 0)
    Array[LivenessBlockInfo](livenessInfo)
  }

  /** Return stack height at the start of the block. */
  private def calculateStartStackHeightFor(block: Block, entryBlock: Block, readWriteBlocksInfo: Array[ReadWriteBlockInfo]) = {
    val stackHeight = if (block == entryBlock) {
      0
    } else if (block.isHandler) {
      1 // exception object is on stack at start of handler
    } else {
      predecessorsStackHeight(block, readWriteBlocksInfo)
    }

    assert(stackHeight >= 0)
    assert(block.inputs forall { in =>
      val info = readWriteBlocksInfo(in.block.id)
      info == null || info.stackHeightEnd == stackHeight
    })

    stackHeight
  }

  private def predecessorsStackHeight(block: Block, readWriteBlocksInfo: Array[ReadWriteBlockInfo]): Int = {
    val infoOpt = block.inputs.iterator.map(in => readWriteBlocksInfo(in.block.id)).find(_ != null)
    infoOpt match {
      case Some(info) => info.stackHeightEnd
      case None => shouldNotReachHere("block must have already analyzed predecessors")
    }
  }

  private type UnusedValue = Unit

  private class ReadWriteProcessor(method: Method,
                                   slots: Slots,
                                   block: Block,
                                   info: ReadWriteBlockInfo)
    extends SimpleDataFlowParser[UnusedValue](method, block, slots) {

    override def writeSlot(slotIdx: Int, value: UnusedValue): Unit = {
      info.slotsWrite(slotIdx) = true
    }

    override def readSlot(slotIdx: Int): UnusedValue = {
      if (!info.slotsWrite(slotIdx)) {
        info.slotsReadBeforeWrite(slotIdx) = true
      }
    }

    override protected def newValue(`type`: BytecodeTypeKind): UnusedValue = {}

    override def longHalfOf(value: UnusedValue): UnusedValue = {}

    override protected def useValue(value: UnusedValue): Unit = {}
  }
}
