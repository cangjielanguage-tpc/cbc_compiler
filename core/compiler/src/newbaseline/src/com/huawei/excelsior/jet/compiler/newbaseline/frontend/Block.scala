/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.util.SuffixTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Block represents linear sequence of bytecode instructions
  * in the range from [[startBC]] until [[endBC]].
  *
  * @param startBC First bytecode instruction of this block.
  */
final class Block(val startBC: Int) {

  assert(startBC >= 0)

  /** Number of block in topsort order. */
  private var _id = -1

  private var _endBC = -1

  private val _inputs = new ArrayBuffer[Block.End]

  private var _end: Block.End = _

  private var _stackHeightAtStart = -1

  /** Marker for block which is a handler of some try block.
    * It means that control may come to this block implicitly (not from [[inputs]]).
    */
  var isHandler = false

  /** Sequence of [[XHInfo]] elements describing handlers of this block.
    * Equals to `null` if block does not have any handlers.
    */
  var handlerInfoSequence: SuffixTree[XHInfo[Block]] = _

  def this(startBC: Int, endBC: Int) = {
    this(startBC)
    this.endBC = endBC
  }

  def id = _id ensuring (_ >= 0)

  /** Next after the last bytecode instruction of this block (exclusive range). */
  def endBC = _endBC ensuring (_ >= 0)

  def endBC_=(endBC: Int): Unit = {
    assert(startBC <= endBC)
    this._endBC = endBC
  }

  def inputs: mutable.Buffer[Block.End] = _inputs

  def end = _end

  def end_=(end: Block.End): Unit = {
    if (this._end != null) {
      this._end._block = null
    }
    if (end != null) {
      if (end.block != null) {
        assert(end.block._end == end)
        end.block._end = null
      }
      end._block = this
    }
    this._end = end
  }

  def destroyEndAndOutputConnections(): Unit = {
    for (succ <- end.outputs) {
      succ.inputs -= end
    }
    end = null
  }

  def connectTo(that: Block): Unit = {
    this.end.outputs += that
    that.inputs += this.end
  }

  def stackHeightAtStart = _stackHeightAtStart ensuring (_ != -1)

  def stackHeightAtStart_=(stackHeightAtStart: Int): Unit = {
    assert(this._stackHeightAtStart == -1)
    assert(stackHeightAtStart >= 0)
    this._stackHeightAtStart = stackHeightAtStart
  }

  /** Returns stack index of exception object which is the only available stack slot for this block. */
  def exceptionObjStackIdx = {
    assert(isHandler)
    0
  }

  def hasHandler = handlerInfoSequence != null

  def handlers = if (!hasHandler) {
    Iterator.empty
  } else {
    handlerInfoSequence.toRoot.map(_.handler)
  }

  override def toString = s"$idDescription$rangeDescription"

  def description = {
    val ins = inputs.map(_.block).mkString("in: {", ", ", "}")
    val outs = end.outputs.mkString(s"out: ${end.kind} -> {", ", ", "}")
    val handlerInfo = if (hasHandler) s" handlers: ${handlerInfoSequence.toRootToString}" else ""

    s"Block$idDescription $rangeDescription $ins $outs$handlerInfo"
  }

  private def idDescription = if (_id >= 0) s"#${_id}" else ""

  private def rangeDescription = s"[$startBC,$endBC)"
}

object Block {
  class End(val kind: Block.End.Kind) {

    val outputs: mutable.Buffer[Block] = new ArrayBuffer[Block]

    private[Block] var _block: Block = _

    def block = _block
  }

  object End {
    enum Kind:
      case RETURN, THROW, HALT, GOTO, IF, SWITCH
  }

  /** Set id of given blocks using their index number. */
  def setIDs(order: collection.Seq[Block]): Unit = {
    for ((b, i) <- order.zipWithIndex) {
      b._id = i
    }
  }
}
