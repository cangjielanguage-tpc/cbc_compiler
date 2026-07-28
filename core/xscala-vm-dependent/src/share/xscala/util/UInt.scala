/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

final class UInt (private val self: Int) extends AnyVal {

  @inline def toByte = self.toByte
  @inline def toShort = self.toShort
  @inline def toInt = self
  @inline def toLong = self & 0xffffffffL

  @inline def toUByte = UByte(self)
  @inline def toUShort = UShort(self)
  @inline def toULong = ULong(self)

  @inline def toSet32 = Set32(self)
  @inline def toSet64 = Set64(self)

  override def toString = toLong.toString

  //def unary_~ = ???

  @inline def +(that: UInt): UInt = (this.self + that.self)
  @inline def -(that: UInt): UInt = (this.self - that.self)
  @inline def *(that: UInt): UInt = (this.self * that.self)
  @inline def /(that: UInt): UInt = (this.toLong / that.toLong).toInt
  @inline def %(that: UInt): UInt = (this.toLong % that.toLong).toInt

  @inline def << (bits: UInt): UInt = (self << bits.toInt)
  @inline def >> (bits: UInt): UInt = (self >>> bits.toInt)
  @inline def >>>(bits: UInt): UInt = (self >>> bits.toInt)

  @inline def ==(that: UInt): Boolean = (this.self == that.self)
  @inline def !=(that: UInt): Boolean = (this.self != that.self)
  @inline def >=(that: UInt): Boolean = (this.self + Int.MinValue >= that.self + Int.MinValue)
  @inline def <=(that: UInt): Boolean = (this.self + Int.MinValue <= that.self + Int.MinValue)
  @inline def > (that: UInt): Boolean = (this.self + Int.MinValue >  that.self + Int.MinValue)
  @inline def < (that: UInt): Boolean = (this.self + Int.MinValue <  that.self + Int.MinValue)

  @inline def |(that: UInt): UInt = (this.self | that.self)
  @inline def &(that: UInt): UInt = (this.self & that.self)
  @inline def ^(that: UInt): UInt = (this.self ^ that.self)

  // def to(end: UInt) = ???
}

object UInt {
  import language.implicitConversions

  @inline def apply(self: Int) = int2uint(self)
  @inline implicit def int2uint(self: Int): UInt = new UInt(self)

  final val MinValue = 0
  final val MaxValue = UInt(0xFFFFFFFF)
}
