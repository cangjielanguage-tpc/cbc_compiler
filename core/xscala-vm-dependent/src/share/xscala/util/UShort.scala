/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

final class UShort (private val self: Short) extends AnyVal {

  @inline def toByte = self.toByte
  @inline def toShort = self
  @inline def toChar = self.toChar
  @inline def toInt = self & 0xffff

  @inline def toUByte = UByte(this.toInt)
  // def toUShort = ???
  @inline def toUInt = UInt(this.toInt)
  @inline def toSet32 = Set32(this.toInt)

  override def toString = toInt.toString

  //def unary_~ = ???

  @inline def +(that: UShort): UShort = (this.self + that.self)
  @inline def -(that: UShort): UShort = (this.self - that.self)
  @inline def *(that: UShort): UShort = (this.self * that.self)
  @inline def /(that: UShort): UShort = (this.toInt / that.toInt)
  @inline def %(that: UShort): UShort = (this.toInt % that.toInt)

  @inline def << (bits: UInt): UShort = (self << bits.toInt)
  @inline def >> (bits: UInt): UShort = (this >>> bits.toInt)
  @inline def >>>(bits: UInt): UShort = (this.toInt >>> bits.toInt)

  @inline def ==(that: UShort): Boolean = (this.self == that.self)
  @inline def !=(that: UShort): Boolean = (this.self != that.self)
  @inline def >=(that: UShort): Boolean = (this.toInt >= that.toInt)
  @inline def <=(that: UShort): Boolean = (this.toInt <= that.toInt)
  @inline def > (that: UShort): Boolean = (this.toInt >  that.toInt)
  @inline def < (that: UShort): Boolean = (this.toInt <  that.toInt)

  @inline def |(that: UShort): UShort = (this.self | that.self)
  @inline def &(that: UShort): UShort = (this.self & that.self)
  @inline def ^(that: UShort): UShort = (this.self ^ that.self)

  def to(end: UShort) = new UShortRange(this.toInt, end.toInt, 1)
}

object UShort {
  import language.implicitConversions

  @inline def apply(self: Int) = int2ushort(self)
  @inline implicit def int2ushort(self: Int): UShort = new UShort(self.toShort)
  @inline implicit def ushort2uint(ushort: UShort): UInt = ushort.toUInt

  @inline def apply(self: Char) = new UShort(self.toShort)

  final val MinValue = 0
  final val MaxValue = 0xFFFF
}

class UShortRange(start: Int, end: Int, step: Int) extends IndexedSeq[UShort] {
  private val range = new Range.Inclusive(start, end, step)

  def length = range.length
  def apply(idx: Int): UShort = range(idx)

  def by(step: Int) = new UShortRange(start, end, step)
}

