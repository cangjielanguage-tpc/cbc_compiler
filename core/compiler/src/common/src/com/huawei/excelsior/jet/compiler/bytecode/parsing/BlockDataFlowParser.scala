/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.compiler.bytecode.{Bytecode, BytecodeIterator, BytecodeProcessor, BytecodeTypeKind, MethodCodeAttribute, Slots}
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerificationUnit}

import scala.annotation.nowarn

object BlockDataFlowParser {
  val RET_ADDR_KIND = BytecodeTypeKind.CLASS
}

abstract class BlockDataFlowParser[V](
  protected val slots: Slots,
  stackHeightAtStart: Int,
  verify: Boolean,
  verificationContext: VerifiableMethod
) extends VerificationUnit(verify, verificationContext) with BytecodeProcessor {

  private var _curStackHeight = stackHeightAtStart

  assert(0 <= stackHeightAtStart && stackHeightAtStart <= slots.stackCount)

  protected def writeSlot(slotIdx: Int, value: V): Unit

  protected def readSlot(slotIdx: Int): V

  protected def longHalfOf(value: V): V

  def iterateBytecode(code: MethodCodeAttribute, startBC: Int, endBC: Int): Unit = {
    val bc = new BytecodeIterator(code, false, null)
    bc.iterate(this, startBC, endBC)
  }

  def curStackHeight = _curStackHeight

  protected def clearStack(): Unit = { _curStackHeight = 0 }

  final protected def push(tpe: BytecodeTypeKind, value: V): Unit = {
    if (tpe.is2Slots) {
      pushRaw(longHalfOf(value))
    }
    pushRaw(value)
  }

  final protected def pop(tpe: BytecodeTypeKind) = {
    val value = popRaw()
    if (tpe.is2Slots) {
      popRaw()
    }
    value
  }

  private def pushRaw(value: V): Unit = {
    // Note that during verification we have no inlining so baseStackHeight == 0 && slots.stackCount == method.maxStack.
    verifyThat(curStackHeight < slots.stackCount, "Stack overflow")
    writeSlot(slots.stackIdx(curStackHeight), value)
    _curStackHeight += 1
  }

  private def popRaw() = {
    verifyThat(curStackHeight > 0, "Pop from empty stack")
    _curStackHeight -= 1
    readSlot(slots.stackIdx(curStackHeight))
  }

  /** @param offsetFromStackTop 0 for top stack slot, 1 for the next below it and so on. */
  final protected def peekRaw(offsetFromStackTop: Int) = {
    val idx = curStackHeight - 1 - offsetFromStackTop
    assert(idx >= 0)
    readSlot(slots.stackIdx(idx))
  }

  final protected def write(tpe: BytecodeTypeKind, localIdx: Int, value: V): Unit = {
    if (tpe.is2Slots) {
      writeRaw(localIdx + 1, longHalfOf(value))
    }
    writeRaw(localIdx, value)
  }

  final protected def read(tpe: BytecodeTypeKind, localIdx: Int) = readRaw(localIdx)

  private def writeRaw(localIdx: Int, value: V): Unit = writeSlot(slots.localIdx(localIdx), value)

  private def readRaw(localIdx: Int) = readSlot(slots.localIdx(localIdx))

  override final def pushLocal(tpe: BytecodeTypeKind, index: Int): Unit = {
    val x = read(tpe, index)
    push(tpe, x)
  }

  override final def storeLocal(tpe: BytecodeTypeKind, index: Int): Unit = {
    val x = pop(tpe)
    write(tpe, index, x)
  }

  override final def stackOp(op: Bytecode): Unit = (op: @unchecked) match {
    case Bytecode.POP =>
      // ..., x => ...
      popRaw()

    case Bytecode.POP2 =>
      // ..., y, x => ...
      popRaw()
      popRaw()

    case Bytecode.DUP =>
      // ..., x => ..., x, x
      val x = popRaw()
      pushRaw(x)
      pushRaw(x)

    case Bytecode.DUP_X1 =>
      // ..., y, x => ..., x, y, x
      val x = popRaw()
      val y = popRaw()
      pushRaw(x)
      pushRaw(y)
      pushRaw(x)

    case Bytecode.DUP_X2 =>
      // ..., z, y, x => ..., x, z, y, x
      val x = popRaw()
      val y = popRaw()
      val z = popRaw()
      pushRaw(x)
      pushRaw(z)
      pushRaw(y)
      pushRaw(x)

    case Bytecode.DUP2 =>
      // ..., y, x => ..., y, x, y, x
      val x = popRaw()
      val y = popRaw()
      pushRaw(y)
      pushRaw(x)
      pushRaw(y)
      pushRaw(x)

    case Bytecode.DUP2_X1 =>
      // ..., z, y, x => ..., y, x, z, y, x
      val x = popRaw()
      val y = popRaw()
      val z = popRaw()
      pushRaw(y)
      pushRaw(x)
      pushRaw(z)
      pushRaw(y)
      pushRaw(x)

    case Bytecode.DUP2_X2 =>
      // ..., w, z, y, x => ..., y, x, w, z, y, x
      val x = popRaw()
      val y = popRaw()
      val z = popRaw()
      val w = popRaw()
      pushRaw(y)
      pushRaw(x)
      pushRaw(w)
      pushRaw(z)
      pushRaw(y)
      pushRaw(x)

    case Bytecode.SWAP =>
      // ..., y, x => ..., x, y
      val x = popRaw()
      val y = popRaw()
      pushRaw(x)
      pushRaw(y)
  }
}
