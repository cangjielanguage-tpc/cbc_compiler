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
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodeTypeKind, Slots}
import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT
import com.huawei.excelsior.jet.compiler.newbaseline.backend.GlobalInfo
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Node, NodeType}
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.collection.mutable.ArrayBuffer

class BlockLivenessAnalyzer(method: Method, slots: Slots, globalInfo: GlobalInfo, block: Block) {
  private val _nodes = new ArrayBuffer[Node]

  {
    val processor = new ValueLivenessProcessor
    processor.iterateBytecode()

    processor.setUsesAtBlockEnd()

    if (DEBUG_PRINT) {
      println()
      println(s"Liveness in block $block:")
      for (n <- _nodes) {
        println(s"  $n")
      }
      println()
    }
  }

  def nodes: collection.Seq[Node] = _nodes

  private def addNode(n: Node) = {
    _nodes += n
    n
  }

  final private class ValueLivenessProcessor extends SimpleDataFlowParser[Node](method, block, slots) { processor =>

    private var curBC = 0
    private val slotNodes = Array.tabulate(processor.slots.totalCount) { s =>
      if (globalInfo.isSlotAliveAtBlockStart(s, block)) {
        val `type` = globalInfo.typeAtBlockStart(block, s)
        assert(`type` != null)
        addNode(Node.forInputSlot(`type`, s))
      } else {
        null
      }
    }

    def setUsesAtBlockEnd(): Unit = {
      for {
        s <- slotNodes.indices
        aliveAtHandler = globalInfo.isSlotAliveAtBlockEnd(s, block) ||
          // Every bytecode instruction may throw exception.
          // So every node has to be alive until the last instruction
          // if it is stored in the slot which is alive at handler.
          // @see BlockLivenessAnalyzer.ValueLivenessProcessor#writeSlot
          //
          // Note that the last instruction (e.g. store) may write the new value
          // to the slot and it is not accessible in handler.
          // We ignore such a situation and assume that this value is alive
          // until the end of the block.
          globalInfo.isSlotAliveAtHandler(s, block)

        if aliveAtHandler
      } {
        val value = slotNodes(s)
        assert(value != null, s"trying to access alive uninitialized slot ${processor.slots.slotToString(s)}")
        value.usedAtTheEnd()
      }
    }

    override def newValue(`type`: BytecodeTypeKind) = {
      addNode(Node.forBCOffset(NodeType.by(`type`), curBC))
    }

    override def longHalfOf(value: Node) = value.`type` match {
      case NodeType.LONG | NodeType.DOUBLE => Node.LONG_HALF
      case _ => shouldNotReachHere()
    }

    override def useValue(value: Node): Unit = {
      value.usedAtBCOffset(curBC)
    }

    override def writeSlot(slotIdx: Int, value: Node): Unit = {
      if (globalInfo.isSlotAliveAtHandler(slotIdx, block)) {
        // Every bytecode instruction may throw exception.
        // So every node has to be alive all the time
        // while it is stored in the slot which is alive at handler.
        // Last "use" of such node is the moment when new node is written in this slot.
        // @see MethodBytecodeGenerator.BlockGenerator#writeSlot
        // @see BlockLivenessAnalyzer.ValueLivenessProcessor#setUsesAtBlockEnd
        val oldValue = slotNodes(slotIdx)
        useValue(oldValue)
      }
      slotNodes(slotIdx) = value
    }

    override def readSlot(slotIdx: Int) = {
      val value = slotNodes(slotIdx)
      assert(value != null, s"trying to access uninitialized slot ${processor.slots.slotToString(slotIdx)}")
      value
    }

    override def startInstruction(offset: Int, nextOffset: Int): Unit = {
      super.startInstruction(offset, nextOffset)
      curBC = offset
    }
  }
}
