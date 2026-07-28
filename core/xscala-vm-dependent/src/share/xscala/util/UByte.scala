/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

final class UByte (private val self: Byte) extends AnyVal {

  @inline def toByte = self
  @inline def toShort = (self & 0xff).toShort
  @inline def toInt = self & 0xff
  @inline def toLong = (self & 0xff).toLong

  // def toUByte = ???
  @inline def toUShort = UShort(this.toInt)
  @inline def toUInt = UInt(this.toInt)
  @inline def toULong = ULong(this.toLong)
  @inline def toSet32 = Set32(this.toInt)

  override def toString = toInt.toString

  //def unary_~ = ???

  @inline def +(that: UByte): UByte = (this.self + that.self)
  @inline def -(that: UByte): UByte = (this.self - that.self)
  @inline def *(that: UByte): UByte = (this.self * that.self)
  @inline def /(that: UByte): UByte = (this.toInt / that.toInt)
  @inline def %(that: UByte): UByte = (this.toInt % that.toInt)

  @inline def << (bits: UInt): UByte = (self << bits.toInt)
  @inline def >> (bits: UInt): UByte = (this >>> bits.toInt)
  @inline def >>>(bits: UInt): UByte = (this.toInt >>> bits.toInt)

  @inline def ==(that: UByte): Boolean = (this.self == that.self)
  @inline def !=(that: UByte): Boolean = (this.self != that.self)
  @inline def >=(that: UByte): Boolean = (this.toInt >= that.toInt)
  @inline def <=(that: UByte): Boolean = (this.toInt <= that.toInt)
  @inline def > (that: UByte): Boolean = (this.toInt >  that.toInt)
  @inline def < (that: UByte): Boolean = (this.toInt <  that.toInt)

  @inline def |(that: UByte): UByte = (this.self | that.self)
  @inline def &(that: UByte): UByte = (this.self & that.self)
  @inline def ^(that: UByte): UByte = (this.self ^ that.self)

  def to(end: UByte) = new UByteRange(this.toInt, end.toInt, 1)
}

object UByte {
  import language.implicitConversions

  @inline def apply(self: Int) = int2ubyte(self)

  @inline implicit def int2ubyte(self: Int): UByte = new UByte(self.toByte)

  @inline implicit def ubyte2ushort(ubyte: UByte): UShort = ubyte.toUShort
  @inline implicit def ubyte2uint(ubyte: UByte): UInt = ubyte.toUInt
  @inline implicit def ubyte2ulong(ubyte: UByte): ULong = ubyte.toULong

  final val MinValue = 0
  final val MaxValue = 0xFF

  implicit val ord: Ordering[UByte] = Ordering by { _.self }
}

class UByteRange(start: Int, end: Int, step: Int) extends IndexedSeq[UByte] {
  private val range = new Range.Inclusive(start, end, step)

  def length = range.length
  def apply(idx: Int): UByte = range(idx)

  def by(step: Int) = new UByteRange(start, end, step)
}
