/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

final class Set32(private val self: Int) extends AnyVal {
  import Set32.*

  @inline def contains(elem: UInt): Boolean = (mask(elem) & self) != 0

  @inline def << (bits: UInt) = Set32(self << bits.toInt)
  @inline def >> (bits: UInt) = Set32(self >>> bits.toInt)

  @inline def | (that: Set32) = Set32(this.self | that.self)
  @inline def & (that: Set32) = Set32(this.self & that.self)
  @inline def ^ (that: Set32) = Set32(this.self ^ that.self)
  @inline def &~ (that: Set32) = Set32(this.self & ~that.self)

  @inline def + (elem: UInt) = Set32(this.self | mask(elem))
  @inline def - (elem: UInt) = Set32(this.self & ~mask(elem))

  @inline private def fullSetMask(length: Int) = if (length == 32) 0xFFFFFFFF else (1 << length) - 1

  /** complement of Set32, set length is specified */
  @inline def complement(length: Int) = Set32(self ^ fullSetMask(length))

  @inline def toByte = self.toByte
  @inline def toShort = self.toShort
  @inline def toInt = self

  @inline def toUByte = UByte(self)
  @inline def toUShort = UShort(self)
  @inline def toUInt = UInt(self)
  @inline def toSet32 = this
}

object Set32 {
  final val maxElem = 31

  @inline private def mask(elem: UInt): Int = (1 << elem.toInt) ensuring (elem <= maxElem)

  @inline def apply(self: Int) = new Set32(self)

  @inline def empty = Set32(0)

  @inline def of(x: UByte): Set32 = {
    Set32(mask(x))
  }

  @inline def of(x1: UByte, x2: UByte): Set32 = {
    Set32(mask(x1) | mask(x2))
  }

  @inline def of(x1: UByte, x2: UByte, x3: UByte): Set32 = {
    Set32(mask(x1) | mask(x2) | mask(x3))
  }

  @inline def of(x1: UByte, x2: UByte, x3: UByte, x4: UByte): Set32 = {
    Set32(mask(x1) | mask(x2) | mask(x3) | mask(x4))
  }

  def of(elements: Any*): Set32 = {
    var set = empty
    for (elem <- elements) {
      elem match {
        case v: Byte        => set += v.toInt
        case v: Short       => set += v.toInt
        case v: Int         => set += v
        case v: UByte       => set += v
        case v: UShort      => set += v
        case v: UInt        => set += v
        case r: Range       => r foreach (set += _)
        case r: UByteRange  => r foreach (set += _)
        case r: UShortRange => r foreach (set += _)
      }
    }
    set
  }
}
