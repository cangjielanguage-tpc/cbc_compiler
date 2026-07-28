/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

final class Set64(private val self: Long) extends AnyVal {
  import Set64.*

  @inline def contains(elem: ULong): Boolean = (maskLong(elem) & self) != 0

  @inline def << (bits: ULong) = Set64(self << bits.toLong)
  @inline def >> (bits: ULong) = Set64(self >>> bits.toLong)

  @inline def | (that: Set64) = Set64(this.self | that.self)
  @inline def & (that: Set64) = Set64(this.self & that.self)
  @inline def ^ (that: Set64) = Set64(this.self ^ that.self)
  @inline def &~ (that: Set64) = Set64(this.self & ~that.self)

  @inline def + (elem: ULong) = Set64(this.self | maskLong(elem))
  @inline def - (elem: ULong) = Set64(this.self & ~maskLong(elem))

  @inline private def fullSetMask(length: Long) = if (length == 64) 0xFFFFFFFFFFFFFFFFL else (1L << length) - 1L

  /** complement of Set64, set length is specified */
  @inline def complement(length: Long) = Set64(self ^ fullSetMask(length))

  @inline def toByte = self.toByte
  @inline def toShort = self.toShort
  @inline def toLong = self

  @inline def toULong = ULong(self)
  @inline def toSet64 = this
}

object Set64 {
  final val maxElem = 63

  @inline private def mask(elem: UByte): Long = (1L << elem.toLong) ensuring (elem <= maxElem)
  
  @inline private def maskLong(elem: ULong): Long = (1L << elem.toLong) ensuring (elem <= maxElem)

  @inline def apply(self: Long) = new Set64(self)

  @inline def empty = Set64(0)

  @inline def of(x: UByte): Set64 = {
    Set64(mask(x))
  }

  @inline def of(x1: UByte, x2: UByte): Set64 = {
    Set64(mask(x1) | mask(x2))
  }

  @inline def of(x1: UByte, x2: UByte, x3: UByte): Set64 = {
    Set64(mask(x1) | mask(x2) | mask(x3))
  }

  @inline def of(x1: UByte, x2: UByte, x3: UByte, x4: UByte): Set64 = {
    Set64(mask(x1) | mask(x2) | mask(x3) | mask(x4))
  }

  def of(elements: Any*): Set64 = {
    var set = empty
    for (elem <- elements) {
      elem match {
        case v: Byte        => set += v.toLong
        case v: Short       => set += v.toLong
        case v: Int         => set += v.toLong
        case v: UByte       => set += v
        case v: UShort      => set += v.toLong
        case v: UInt        => set += v.toLong
        case v: ULong       => set += v
        case r: Range       => r foreach (set += _.toLong)
        case r: UByteRange  => r foreach (set += _)
        case r: UShortRange => r foreach (set += _.toLong)
      }
    }
    set
  }
}
