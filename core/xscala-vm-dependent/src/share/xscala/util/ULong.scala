/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

import xscala.util.ULong.{isSimple, simpleDivision, largeDivision}

final class ULong (private val self: Long) extends AnyVal {

  @inline def toByte = self.toByte
  @inline def toShort = self.toShort
  @inline def toInt = self.toInt
  @inline def toLong = self

  @inline def toUByte = UByte(self.toInt)
  @inline def toUShort = UShort(self.toInt)
  @inline def toUInt = UInt(self.toInt)

  @inline def toSet64 = Set64(self)

  override def toString = toLong.toString

  //def unary_~ = ???

  @inline def +(that: ULong): ULong = (this.self + that.self)
  @inline def -(that: ULong): ULong = (this.self - that.self)
  @inline def *(that: ULong): ULong = (this.self * that.self)

  @inline def /(that: ULong): ULong = {
    if (that == ULong(0)) {
      throw ArithmeticException("/ by zero")
    }

    if (isSimple(this, that)) {
      simpleDivision(this, that)
    } else {
      largeDivision(this, that)
    }
  }

  @inline def %(that: ULong): ULong = {
    if (that == ULong(0)) {
      throw ArithmeticException("% by zero")
    }

    this - (this / that) * that
  }

  @inline def << (bits: ULong): ULong = (self << bits.toInt)
  @inline def >> (bits: ULong): ULong = (self >>> bits.toInt)
  @inline def >>>(bits: ULong): ULong = (self >>> bits.toInt)

  @inline def ==(that: ULong): Boolean = (this.self == that.self)
  @inline def !=(that: ULong): Boolean = (this.self != that.self)
  @inline def >=(that: ULong): Boolean = (this.self + Long.MinValue >= that.self + Long.MinValue)
  @inline def <=(that: ULong): Boolean = (this.self + Long.MinValue <= that.self + Long.MinValue)
  @inline def > (that: ULong): Boolean = (this.self + Long.MinValue >  that.self + Long.MinValue)
  @inline def < (that: ULong): Boolean = (this.self + Long.MinValue <  that.self + Long.MinValue)

  @inline def |(that: ULong): ULong = (this.self | that.self)
  @inline def &(that: ULong): ULong = (this.self & that.self)
  @inline def ^(that: ULong): ULong = (this.self ^ that.self)

  // def to(end: ULong) = ???
}

object ULong {
  import language.implicitConversions

  @inline def apply(self: Long) = long2ULong(self)
  @inline implicit def long2ULong(self: Long): ULong = new ULong(self)

  @inline private def isSimple(num: ULong, div: ULong): Boolean = (num <= Long.MaxValue && div <= Long.MaxValue) || (num < div)

  /**
    * Simple case of 64-bit unsigned division.
    * (arguments satisfy [[ isSimple ]])
    *
    * @param num  Dividend (numerator).
    * @param div  Divisor.
    * @return `num / div`
    */
  @inline private def simpleDivision(num: ULong, div: ULong): ULong = if (num < div) 0 else num.toLong / div.toLong

  /**
    * Complex case of 64-bit unsigned division.
    * In such case the result can be calculated as: `n = (Long.MaxValue / b)`; `m = ((n + 1) * b)`.
    * If `m <= a` then result is `(a - m) / b + (n + 1)`,
    * otherwise `n`.
    *
    * @param num  Dividend (numerator).
    * @param div  Divisor.
    * @return `num / div`
    */
  @inline private def largeDivision(num: ULong, div: ULong): ULong  = {
    val n = simpleDivision(Long.MaxValue, div)
    val m = (n + 1) * div

    if (m <= num) {
      simpleDivision(num - m, div) + n + 1
    } else {
      n
    }
  }

  final val MinValue = 0
  final val MaxValue = ULong(0xFFFFFFFFFFFFFFFFL)
}
