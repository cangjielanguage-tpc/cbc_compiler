/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.util

import com.huawei.excelsior.jet.assembler.AsmType
import xscala.util.MathUtils.{nthBit64, rightNBits64, ulss}

/** Overflow predicates namespace. Source: Hacker's Delight, chapter 2, paragraph 13 "Overflow Detection".
  */
object Overflows {

  private def isWidthAllowed(width: Int) = width == 8 || width == 16 || width == 32 || width == 64

  def add(x: Long, y: Long, asmType: AsmType): Boolean = {
    assert(asmType.isIntegral && asmType != AsmType.NONE && asmType != AsmType.PTR)

    val width = asmType.width.nbits
    if (asmType.signed) sadd(x, y, width) else uadd(x, y, width)
  }

  def sub(x: Long, y: Long, asmType: AsmType): Boolean = {
    assert(asmType.isIntegral && asmType != AsmType.NONE && asmType != AsmType.PTR)

    val width = asmType.width.nbits
    if (asmType.signed) ssub(x, y, width) else usub(x, y, width)
  }

  def mul(x: Long, y: Long, asmType: AsmType): Boolean = {
    assert(asmType.isIntegral && asmType != AsmType.NONE && asmType != AsmType.PTR)

    val width = asmType.width.nbits
    if (asmType.signed) smul(x, y, width) else umul(x, y, width)
  }

  /** Returns true iff `x` + `y` overflows. */
  def sadd(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    val mask = rightNBits64(width)
    val xm = x & mask
    val ym = y & mask
    val res = (xm + ym) & mask
    (((res ^ xm) & (res ^ ym)) >>> (width - 1)) != 0
  }

  /** Returns true iff unsigned `x` + `y` overflows. */
  def uadd(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    val max = rightNBits64(width)
    ulss(max ^ (x & max), y & max)
  }

  /** Returns true iff `x` - `y` overflows. */
  def ssub(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    val mask = rightNBits64(width)
    val xm = x & mask
    val ym = y & mask
    val res = (xm - ym) & mask
    (((res ^ xm) & ((res ^ ym) ^ mask)) >>> (width - 1)) != 0
  }

  /** Returns true iff unsigned `x` - `y` overflows. */
  def usub(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    val mask = rightNBits64(width)
    ulss(x & mask, y & mask)
  }

  /** Returns true if `x` * `y` overflows. */
  def smul(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    val res = x * y
    if (width == 64) {
      return ((x < 0L) && (y == 0x8000_0000_0000_0000L)) || ((x != 0L) && (res / x != y))
    }

    val mask = rightNBits64(width)
    val low = res & mask
    val min = nthBit64(width - 1)
    val high = (res >>> width) & mask
    val extended = if ((low & min) == min) mask else 0L
    high != extended
  }

  /** Returns true if unsigned `x` * `y` overflows. */
  def umul(x: Long, y: Long, width: Int): Boolean = {
    assert(isWidthAllowed(width))
    if (width == 64) {
      val high = umulh(x, y, width)
      return 0L != high
    }

    val mask = rightNBits64(width)
    val res = (x & mask) * (y & mask)
    ((res >>> width) & mask) != 0
  }

  /** Returns high part of result of unsigned `a` * `b` multiplication. */
  private def umulh(a: Long, b: Long, width: Int): Long = {
    val mask = (-1L) >>> (64 - width)
    val halfWidth = width / 2
    val halfMask = mask >>> halfWidth

    val al = a & halfMask
    val ah = (a >>> halfWidth) & halfMask
    val bl = b & halfMask
    val bh = (b >>> halfWidth) & halfMask

    val lowImpact = al * bl
    val mid0 = al * bh
    val mid1 = ah * bl
    val highImpact = ah * bh

    val withCarry = (lowImpact >>> halfWidth) + (mid0 & halfMask) + (mid1 & halfMask)
    val carry = withCarry >>> halfWidth

    highImpact + (mid0 >>> halfWidth) + (mid1 >>> halfWidth) + carry
  }
}
