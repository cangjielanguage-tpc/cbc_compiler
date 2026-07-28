/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.simple

import com.huawei.excelsior.jet.compiler.bytecode.parsing.simple.SimpleBlock.UNKNOWN_END_BC

import scala.collection.mutable.ArrayBuffer

object SimpleBlock {
  val UNKNOWN_END_BC = -1
}

/** The simplest implementation of basic block required for
  * [[com.huawei.excelsior.jet.compiler.bytecode.parsing.ControlFlowParser]].
  */
abstract class SimpleBlock[B <: SimpleBlock[B]](
  /** First bytecode instruction of this block. */
  val startBC: Int,
  protected var _endBC: Int,
) {
  assert(startBC >= 0)
  assert(_endBC == UNKNOWN_END_BC || startBC <= _endBC)

  // TODO-DECAF: Remove when there are no Java subclasses.
  def this(startBC: Int) = this(startBC, UNKNOWN_END_BC)

  private var _outputs = ArrayBuffer.empty[B]

  /** Next after the last bytecode instruction of this block (exclusive range). */
  def endBC: Int = _endBC ensuring (_ != UNKNOWN_END_BC)
  def endBC_=(value: Int): Unit = _endBC = value.ensuring(v => v >= startBC && v >= 0)

  def setBlockBCRange(start: Int, end: Int): Unit = {
    assert(startBC == start)
    endBC = end
  }

  def outputs: Iterator[B] = _outputs.iterator

  def hasNoOutputs = _outputs.isEmpty

  def connectTo(that: B): Unit = _outputs += that

  def connectTo(x: B, y: B): Unit = {
    connectTo(x)
    connectTo(y)
  }

  def connectTo(x: B, ys: Array[B]): Unit = {
    connectTo(x)
    ys foreach connectTo
  }

  def split(bc: Int): B = {
    val tail = newTailOfSplit(bc)
    if (_endBC != SimpleBlock.UNKNOWN_END_BC) {
      tail.endBC = _endBC
    }
    endBC = bc

    val emptyOutputs = tail._outputs ensuring (_.isEmpty)
    tail._outputs = _outputs
    _outputs = emptyOutputs

    connectTo(tail)
    tail
  }

  def newTailOfSplit(splitBC: Int): B

  def connectClonedToClonedTargets(targetBlocks: Iterator[B]): Unit = {
    assert(hasNoOutputs)
    for (block <- targetBlocks) {
      connectTo(block)
    }
  }

  def connectJsrRetToRealTarget(targetBlock: B): Unit = {
    _outputs.clear()
    connectTo(targetBlock)
  }

  override def toString = s"#$hashCode [$startBC,${_endBC})"
}
