/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.cbc.Local.{Loc8, LocX}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{CBC_ID16, CBC_ID32}
import com.huawei.excelsior.jet.assembler.{AsmType, Fixup, Segment, Symbol, Width}
import xscala.util.MathUtils.{isNBits, isNBitsSigned}

object Bits {
  def isImm4 (imm: Int): Boolean = isNBitsSigned(imm, 4)
  def isImm8 (imm: Int): Boolean = isNBitsSigned(imm, 8)
  def isImm16(imm: Int): Boolean = isNBitsSigned(imm, 16)

  def isImm4 (imm: Long): Boolean = isNBitsSigned(imm, 4)
  def isImm8 (imm: Long): Boolean = isNBitsSigned(imm, 8)
  def isImm16(imm: Long): Boolean = isNBitsSigned(imm, 16)
  def isImm32(imm: Long): Boolean = isNBitsSigned(imm, 32)

  def asImm4 (imm: Int) = { assert(isImm4(imm), imm); imm & 0xf }
  def asImm8 (imm: Int) = { assert(isImm8(imm), imm); imm & 0xff }
  def asImm16(imm: Int) = { assert(isImm16(imm), imm); imm & 0xffff }

  def jump(seg: Segment, pos: Int, width: Width, offset: Int): Unit = {
    seg.setByte(pos, 0x38 + jmpOp(width))
    seg.setSigned(width, pos + 1, offset)
  }

  def jmpOp(width: Width): Int = (width: @unchecked) match {
    case W8  => 0
    case W16 => 1
    case W32 => 2
  }

  def convertOp(fromType: AsmType, toType: AsmType): Int = {
    assert(fromType != toType)
    (fromType, toType) match {
      case (I32, F64) => 0x00
      case (U32, F64) => 0x01
      case (I64, F64) => 0x02
      case (U64, F64) => 0x03
      case (F64, I32) => 0x04
      case (F64, I64) => 0x05
      case (F64, U32) => 0x06
      case (F64, U64) => 0x07
      case (I32, I64) => 0x08
      case (I32, I16) |
           (I32, F16) => 0x09 // Because F16 is implemented with I16
      case (I32, U16) => 0x0A
      case (I32,  I8) => 0x0B
      case (I32,  U8) => 0x0C
      case (I64, I32) => 0x0D
      case (U32, I64) => 0x0E
      case (U32, I16) => 0x0F
      case (U32, U16) => 0x10
      case (U32,  I8) => 0x11
      case (U32,  U8) => 0x12
      case (U64, I32) => 0x13
      case (U64, U32) => 0x14
      case (I32, F32) => 0x15
      case (U32, F32) => 0x16
      case (I64, F32) => 0x17
      case (U64, F32) => 0x18
      case (F32, I32) => 0x19
      case (F32, I64) => 0x1A
      case (F32, U32) => 0x1B
      case (F32, U64) => 0x1C
      case (F32, F64) => 0x1D
      case (F64, F32) => 0x1E
      case (F16, F32) => 0x1F
      case (F32, F16) => 0x20
      case _ => -1
    }
  }

  def ldarrOp(`type`: AsmType): Int = `type` match {
    case I8 => 0
    case U8 => 1
    case I16 | F16 => 2
    case U16 => 3
    case I32 | U32 => 4
    case I64 | U64 => 5
    case F32 => 6
    case F64 => 7
    case _ => -1
  }

  def starrOp(`type`: AsmType): Int = `type`.width match {
    case W8 => 0
    case W16 => 1
    case W32 => if (`type`.isFloatingPoint) 4 else 2
    case W64 => if (`type`.isFloatingPoint) 5 else 3
    case _ => -1
  }

  def u4_u4(low: Long, high: Long): Int = {
    assert(low == low.toInt, s"${low.toHexString}")
    assert(high == high.toInt, s"${high.toHexString}")
    u4_u4(low.toInt, high.toInt)
  }

  def u4_u4(low: Int, high: Int): Int = {
    assert(isNBits(low, 4) && isNBits(high, 4))
    low | high << 4
  }

  /** Sequentially encodes two 16-bits integrals `first` and `second` to be read in the same order.
    *
    * Such implementation is related to little-endianess of .cbc.
    */
  def u16_u16(first: Int, second: Int): Int = {
    assert(isNBits(first, 16) && isNBits(second, 16))
    first | second << 16
  }
}

private[cbc] abstract class Bits {
  def seg: Segment
  def addFixup(fixup: Fixup): Unit

  def op(opcode: Int): Bits = op0(null, opcode)
  def op(prefix: OpcodePrefix, opcode: Int): Bits = op0(prefix, opcode)
  def op(width: Width, opcode32: Int, opcode64: Int): Bits = op0(null, width, opcode32, opcode64)
  def op(prefix: OpcodePrefix, width: Width, opcode32: Int, opcode64: Int): Bits = op0(prefix, width, opcode32, opcode64)

  private def op0(prefix: OpcodePrefix, width: Width, opcode32: Int, opcode64: Int): Bits = {
    op0(prefix, if (w32(width)) opcode32 else opcode64)
  }

  private def op0(prefix: OpcodePrefix, opcode: Int): Bits = {
    if (prefix != null) {
      seg.putW8(prefix.encoding)
    }

    seg.putW8(opcode & 0xff)
    this
  }

  private def u4_u4(low: Int, high: Int): Bits = {
    seg.putW8(Bits.u4_u4(low, high))
    this
  }

  def r4(r: Register): Bits = {
    u4_u4(r.idx, 0)
  }

  def gap4_r4(r: Register): Bits = {
    u4_u4(0, r.idx)
  }

  def r4_gap4(r: Register) : Bits = {
    u4_u4(r.idx, 0)
  }

  def r4_r4(rx: Register, ry: Register): Bits = {
    u4_u4(rx.idx, ry.idx)
  }

  def r4_imm4(r: Register, imm: Int): Bits = {
    u4_u4(r.idx, Bits.asImm4(imm))
  }

  def r4_u4(r: Register, imm: Int): Bits = {
    u4_u4(r.idx, imm)
  }

  def r4_u4(r: Register, imm: Long): Bits = {
    assert(imm == imm.toInt, s"${imm.toHexString.toUpperCase}")
    r4_u4(r, imm.toInt)
  }

  def v8(v: LocX): Bits = {
    assert(v.isInstanceOf[Loc8])
    seg.putW8(v.encoding)
    this
  }

  def us(us: StackSlot.Untyped): Bits = {
    seg.putW16(us.slot)
    this
  }

  def ts(ts: StackSlot.Typed): Bits = {
    seg.putW16(ts.idx)
    this
  }

  def reg_offs(r: IR, offs: Int): Bits = {
    seg.putW16(r.idx | (offs << 4))
    this
  }

  def tk(tk: CbcTypeKind): Bits = {
    seg.putW16(tk.ordinal)
    this
  }

  def ohms(ohms: StackSlot.OffHeapMemory): Bits = {
    seg.putW16(ohms.idx)
    this
  }

  def bytes(byteArray: Array[Byte]): Bits = {
    seg.putBytes(byteArray)
    this
  }

  def byte(imm: Int): Bits = {
    seg.putW8(imm)
    this
  }

  def imm8(imm: Int): Bits = {
    seg.putW8(Bits.asImm8(imm))
    this
  }

  def imm16(imm: Int): Bits = {
    seg.putW16(Bits.asImm16(imm))
    this
  }

  def uimm16(uimm: Int): Bits = {
    assert(isNBits(uimm, 16))
    seg.putW16(uimm)
    this
  }

  def imm32(imm: Int): Bits = {
    seg.putW32(imm)
    this
  }

  def imm64(imm: Long): Bits = {
    seg.putW64(imm)
    this
  }

  def id32(id32: Symbol): Bits = {
    addFixup(new Relocation(CBC_ID32, id32))
    this
  }

  def id16(id16: Symbol): Bits = {
    addFixup(new Relocation(CBC_ID16, id16))
    this
  }

  ///////////////////////////////////////////////////////////////////////////

  private def w32(w: Width): Boolean = (w: @unchecked) match {
    case W32 => true
    case W64 => false
    case WPTR => false // TODO: get rid of WPTR
  }
}