/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BytecodeReferenceSymbol, FieldReference, MethodReference, RawData, Signature, StringLiteral}
import com.huawei.excelsior.jet.assembler.cbc.{Register, StackSlot}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.CC
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.{Opcode, Ordinal}
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{CBC_ID16, CBC_ID32}
import com.huawei.excelsior.jet.assembler.{AsmType, Segment, Symbol}
import xscala.io.LEB128Encoder
import xscala.util.MathUtils

import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

trait ByteStream {
  def write8(x: Int): ByteStream

  /** Writes a bit stream, which must be byte rounded. */
  def bits(f: BitStream => BitStream): ByteStream = {
    var stream: ByteStream = this
    for (b <- f(new BitStream()).send()) {
      stream = stream.write8(b)
    }
    stream
  }

  /** Writes a bit stream, rounding it up to byte boundary. */
  def bitsRounded(f: BitStream => BitStream): ByteStream = {
    var stream: ByteStream = this
    for (b <- f(new BitStream()).sendRounded()) {
      stream = stream.write8(b)
    }
    stream
  }

  final def write16(x: Int): ByteStream =
    write8(x & 0xff).write8((x >>> 8) & 0xff)

  final def write32(x: Int): ByteStream =
    write16(x & 0xffff).write16(x >>> 16)

  final def write64(x: Long): ByteStream =
    write32(x.toInt).write32((x >>> 32).toInt)

  final def sleb(x: Long): ByteStream = {
    LEB128Encoder.encodeSLEB128(x, write8)
    this
  }

  final def uleb(x: Long): ByteStream = {
    LEB128Encoder.encodeULEB128(x, write8)
    this
  }

  final def ts16(ts: StackSlot.Typed): ByteStream = write16(ts.idx)
  final def us16(us: StackSlot.Untyped): ByteStream = write16(us.slot)
  final def opc8(x: Opcode): ByteStream = write8(x.ordinal)

  def sym16(x: FieldReference | MethodReference | Signature)(implicit asm: ForkedAssembler): ByteStream = {
    asm.fixup(Fixups.Reference(BytecodeReferenceSymbol(x)))
    this
  }

  def sym32(x: StringLiteral | RawData)(implicit asm: ForkedAssembler): ByteStream = {
    asm.fixup(Fixups.Reference(BytecodeReferenceSymbol(x)))
    this
  }
}

/**
  * Decorates [[Segment]] with [[ByteStream]]
  */
class SegmentByteStream(seg: Segment) extends ByteStream {
  override def write8(x: Int): ByteStream = {
    seg.putW8(x)
    this
  }
}

/**
  * Writes inside the segment without extending it.
  */
class InteriorByteStream(seg: Segment, private var _pos: Int) extends ByteStream {
  override def write8(x: Int): ByteStream = {
    seg.setW8(_pos, x)
    _pos += 1
    this
  }

  def pos: Int = _pos
}

/**
  * Bit stream of arbitrary length. Bits are packed from most significant one,
  * so the stream is read back in the order it was written.
  */
class BitStream {
  private var bitCount: Int = 0
  private var data: Long = 0L
  private val bytes: ArrayBuffer[Int] = ArrayBuffer.empty

  /** Writes low `bits` bits of `x`. */
  def write(x: Long, bits: Int): BitStream = {
    assert(0 <= bits && bits <= 64)
    assert(MathUtils.isNBits(x, bits), s"$x doesn't fit into $bits bits")
    var shift = bits
    if (bitCount > 0) {
      // complete the partially filled byte first
      val taken = math.min(8 - bitCount, shift)
      data = (data << taken) | ((x >>> (shift - taken)) & MathUtils.rightNBits64(taken))
      shift -= taken
      bitCount += taken
      if (bitCount == 8) {
        bytes += (data & 0xff).toInt
        data = 0L
        bitCount = 0
      }
    }
    while (shift >= 8) {
      shift -= 8
      bytes += ((x >>> shift) & 0xff).toInt
    }
    if (shift > 0) {
      data = (data << shift) | (x & MathUtils.rightNBits64(shift))
      bitCount += shift
    }
    this
  }

  def w1(x: Boolean): BitStream = w1(if (x) 1 else 0)
  def w1(x: Int): BitStream = write(x, 1)
  def w2(x: Int): BitStream = write(x, 2)
  def w4(x: Int): BitStream = write(x, 4)
  def w8(x: Int): BitStream = write(x, 8)
  def w16(x: Int): BitStream = write(x, 16)
  def w36(x: Long): BitStream = write(x, 36)

  /** An amount of bits written so far. */
  def size: Int = bytes.length * 8 + bitCount

  /** Returns bytes of the stream, asserting that it is byte rounded. */
  def send(): Seq[Int] = {
    assert(bitCount == 0, s"bit stream is not byte rounded: $bitCount trailing bits")
    bytes.toSeq
  }

  /** Returns bytes of the stream, padding the last byte with zero bits. */
  def sendRounded(): Seq[Int] = {
    flush()
    bytes.toSeq
  }

  private def flush(): Unit = {
    if (bitCount > 0) {
      bytes += ((data << (8 - bitCount)) & 0xff).toInt
      bitCount = 0
      data = 0L
    }
  }

  def w4(x: Register | AsmType | CC | Ordinal | LoadAccessKind | StoreAccessKind): BitStream = w4(x match {
    case x: Register => x.idx
    case x: AsmType => toInt(x)
    case x: CC => x.ordinal
    case x: Ordinal => x.ordinal
    case x: LoadAccessKind => x.ordinal
    case x: StoreAccessKind => x.ordinal
  })

  @nowarn("msg=match may not be exhaustive")
  def toInt(tpe: AsmType): Int = {
    import com.huawei.excelsior.jet.assembler.AsmType.*
    tpe match {
      case I8 =>  0x0
      case U8 =>  0x1
      case I16 => 0x2
      case U16 => 0x3
      case I32 => 0x4
      case U32 => 0x5
      case I64 => 0x6
      case U64 => 0x7
      case F16 => 0x8
      case F32 => 0x9
      case F64 => 0xa
    }
  }
}