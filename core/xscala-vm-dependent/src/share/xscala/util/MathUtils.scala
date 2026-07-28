/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

/** Common math utils
  *
  * @author conwor
  * @author cypok
  */
object MathUtils {

  /** Returns rounded down log2 of the given positive `x`. */
  def log2(x: Int) = {
    assert(x > 0)
    31 - Integer.numberOfLeadingZeros(x)
  }

  /** Returns true iff `x` is a power of 2. */
  def isPowerOf2(x: Int) = {
    assert(x > 0)
    (x & (x - 1)) == 0
  }

  /** Returns true iff `x` is a power of 2. */
  def isPowerOf2(x: Long) = {
    assert(x > 0)
    (x & (x - 1)) == 0
  }

  /** Returns `min 2^k : 2^k >= x`. Fails if `x <= 0` or if overflow is occurred. */
  def nextPowerOf2(x: Int) = {
    val result = if (x <= 1) 1 else Integer.highestOneBit(x - 1) << 1
    assert(x > 0 && result >= x)
    result
  }

  /** Returns true iff `value` is aligned to `alignment` */
  def isAligned(value: Int, alignment: Int) = {
    assert(alignment > 0)
    (value % alignment) == 0
  }

  /** Returns true iff `value` is aligned to `n` bits. */
  def isAlignedToNBits(value: Int, n: Int) = {
    n == 0 || ((value & rangeMask32(0, n - 1)) == 0)
  }

  /** Aligns `value` up by `alignment`. */
  def alignUp(value: Int, alignment: Int) = {
    assert(alignment > 0)
    alignDown(value + alignment - 1, alignment)
  }

  /** Aligns `value` up by `alignment`. */
  def alignUp(value: Long, alignment: Int) = {
    assert(alignment > 0)
    alignDown(value + alignment - 1, alignment)
  }

  /** Aligns `value` down by `alignment`. */
  def alignDown(value: Int, alignment: Int) = {
    assert(alignment > 0)
    value / alignment * alignment
  }

  /** Aligns `value` down by `alignment`. */
  def alignDown(value: Long, alignment: Int) = {
    assert(alignment > 0)
    value / alignment * alignment
  }

  /** Returns true, iff `x` is correct bit number in 32-bit value. */
  def isBitNumber32(x: Int) = (0 <= x) && (x <= 31)

  /** Returns true, iff `x` is correct bit number in 64-bit value. */
  def isBitNumber64(x: Int) = (0 <= x) && (x <= 63)

  /** Returns integer value with only `n`-th bit set. */
  def nthBit32(n: Int): Int = 1 << (n ensuring isBitNumber32 _)

  /** Returns long value with only `n`-th bit set. */
  def nthBit64(n: Int): Long = 1L << (n ensuring isBitNumber64 _)

  /** Returns true, iff `value` has only first `n` bits set. */
  def isNBits(value: Int, n: Int): Boolean = n == 32 || (value >> (n ensuring isBitNumber32 _)) == 0

  /** Returns true, iff `value` has only first `n` bits set. */
  def isNBits(value: Long, n: Int): Boolean = n == 64 || (value >> (n ensuring isBitNumber64 _)) == 0L

  /** Returns true, iff `value` is signed value could be encoded in `bitsNum` bits including sign bit. */
  def isNBitsSigned(value: Int, bitsNum: Int) = {
    if (bitsNum == 32) {
      true
    } else {
      assert(isBitNumber32(bitsNum) && (bitsNum != 0))
      val extension = value >> (bitsNum - 1)
      (extension == 0) || (extension == -1)
    }
  }

  /** Returns true, iff `value` is signed value could be encoded in `bitsNum` bits including sign bit. */
  def isNBitsSigned(value: Long, bitsNum: Int) = {
    if (bitsNum == 64) {
      true
    } else {
      assert(isBitNumber64(bitsNum) && (bitsNum != 0))
      val extension = value >> (bitsNum - 1)
      (extension == 0L) || (extension == -1L)
    }
  }

  def isNBits(signed: Boolean, value: Long, bitsNum: Int): Boolean = {
    if (signed) isNBitsSigned(value, bitsNum) else isNBits(value, bitsNum)
  }

  def isNBits(signed: Boolean, value: Int, bitsNum: Int): Boolean = {
    if (signed) isNBitsSigned(value, bitsNum) else isNBits(value, bitsNum)
  }

  /** Returns `x` value sign-extended.
    *
    * @param bitsNum an amount of bits in value including sign bit
    */
  def signExtend(x: Int, bitsNum: Int): Int = {
    if (bitsNum == 32) {
      x
    } else {
      assert(isBitNumber32(bitsNum) && (bitsNum != 0))
      assert(bits(x, bitsNum, 31) == 0)
      val shiftAmount = 32 - bitsNum
      (x << shiftAmount) >> shiftAmount
    }
  }

  /** Returns `x` value sign-extended.
    *
    * @param bitsNum an amount of bits in value including sign bit
    */
  def signExtend(x: Long, bitsNum: Int): Long = {
    if (bitsNum == 64) {
      x
    } else {
      assert(isBitNumber64(bitsNum) && (bitsNum != 0))
      assert(bits(x, bitsNum, 63) == 0)
      val shiftAmount = 64 - bitsNum
      (x << shiftAmount) >> shiftAmount
    }
  }

  /** Returns `x` value zero-extended to long. */
  def zeroExtend(x: Int): Long = x.toLong & 0xFFFFFFFFL

  /** Returns integer value with right `n` bits set. */
  def rightNBits32(n: Int): Int = {
    if (n == 32) {
      -1
    } else {
      assert(isBitNumber32(n))
      nthBit32(n) - 1
    }
  }

  /** Returns long value with right `n` bits set. */
  def rightNBits64(n: Int): Long = {
    if (n == 64) {
      -1L
    } else {
      assert(isBitNumber64(n))
      nthBit64(n) - 1
    }
  }

  /** Returns 32-bit mask with bits set in range `[from, to]`. */
  def rangeMask32(from: Int, to: Int): Int = {
    assert(isBitNumber32(from) && isBitNumber32(to) && (from <= to))
    rightNBits32((to - from) + 1) << from
  }

  /** Returns 64-bit mask with bits set in range `[from, to]`. */
  def rangeMask64(from: Int, to: Int): Long = {
    assert(isBitNumber64(from) && isBitNumber64(to) && (from <= to))
    rightNBits64((to - from) + 1) << from
  }

  /** Returns bits value from `x` value in given range.
    * For example: `bits(0b11010, 0, 2)` returns `0b010`.
    */
  def bits(x: Int, from: Int, to: Int): Int = {
    assert(isBitNumber32(from) && isBitNumber32(to) && (from <= to))
    (x >> from) & rightNBits32((to - from) + 1)
  }

  /** Returns bits value from `x` value in the given range.
    * For example: `bits(0b11010, 0, 2)` returns `0b010`.
    */
  def bits(x: Long, from: Int, to: Int): Long = {
    assert(isBitNumber64(from) && isBitNumber64(to) && (from <= to))
    (x >> from) & rightNBits64((to - from) + 1)
  }

  /** Returns sign-extended bits value from `x` value in the given range. */
  def bitsSigned(x: Int, from: Int, to: Int): Int = signExtend(bits(x, from, to), to - from + 1)

  /** Returns sign-extended bits value from `x` value in the given range. */
  def bitsSigned(x: Long, from: Int, to: Int): Long = signExtend(bits(x, from, to), to - from + 1)

  /** Returns `number`'s bit from `x`. */
  def bit(x: Int, number: Int) = (x >> (number ensuring isBitNumber32 _)) & 0x1

  /** Returns `number`'s bit from `x`. */
  def bit(x: Long, number: Int) = ((x >> (number ensuring isBitNumber64 _)) & 0x1).toInt

  /** Returns `value` with `n`-th bit set. */
  def setBit(value: Int, n: Int) = value | nthBit32(n)

  /** Returns `value` with `n`-th bit set. */
  def setBit(value: Long, n: Int) = value | nthBit64(n)
  
  /** Returns `value` with `n`-th bit unset. */
  def clearBit(value: Int, n: Int) = value & ~nthBit32(n)

  /** Returns `value` with `n`-th bit unset. */
  def clearBit(value: Long, n: Int) = value & ~nthBit64(n)

  /** Returns true, iff `n`-th bit in `value` is set. */
  def isBitSet(value: Int, n: Int) = (value & nthBit32(n)) != 0
  
  def isBitSubset(superset: Long, subset: Long) = (superset & subset) == subset

  /** Returns number of first set bit in `value`.
    * Fails, if `value` equals zero.
    */
  def maxBitNumber(value: Int) = {
    assert(value != 0)
    31 - Integer.numberOfLeadingZeros(value)
  }

  /** Returns bit `b` replicated `n` times. */
  def replicate(b: Int, n: Int): Int = {
    assert(isNBits(b, 1))
    if (b == 0) 0 else rightNBits32(n)
  }

  /** Returns bit `b` replicated `n` times. */
  def replicate(b: Long, n: Int): Long = {
    assert(isNBits(b, 1))
    if (b == 0L) 0L else rightNBits64(n)
  }

  /** Creates 64-bit value by the given low and high parts. */
  def makeLong(lo: Int, hi: Int): Long = (lo.toLong & 0xffffffffL) + (hi.toLong << 32)

  /** Returns low bits value from `x` value. */
  def low32Bits(x: Long) = bits(x, 0, 31).toInt

  /** Returns high bits value from `x` value. */
  def high32Bits(x: Long) = bits(x, 32, 63).toInt

  /** Returns low bits value from `x` value. */
  def low16Bits(x: Int) = bits(x, 0, 15)

  /** Returns high bits value from `x` value. */
  def high16Bits(x: Int) = bits(x, 16, 31)

  /** Returns 32-bit value with swapped low & high halves. */
  def swapW16(x: Int) = {
    val lo = x & 0xffff
    val hi = x >>> 16
    (lo << 16) | hi
  }

  /** Returns count of one-bits in `x`. */
  def bitCount(x: Int) = Integer.bitCount(x)

  /** Returns count of one-bits in `x`. */
  def bitCount(x: Long) = java.lang.Long.bitCount(x)

  /** Returns count of zero-bits in `x`. */
  def zeroesCount(x: Int) = 32 - bitCount(x)

  /** Returns count of zero-bits in `x`. */
  def zeroesCount(x: Long) = 64 - bitCount(x)

  val UINT_MAX = -1
  val UINT_MIN = 0

  val ULONG_MAX = -1L
  val ULONG_MIN = 0L

  def ulss(x: Long, y: Long) = x + Long.MinValue <  y + Long.MinValue
  def uleq(x: Long, y: Long) = x + Long.MinValue <= y + Long.MinValue
  def ugtr(x: Long, y: Long) = x + Long.MinValue >  y + Long.MinValue
  def ugeq(x: Long, y: Long) = x + Long.MinValue >= y + Long.MinValue

  private val UINT_MASK = 0xffffffffL

  def udiv(x: Int, y: Int) = ((x & UINT_MASK) / (y & UINT_MASK)).toInt
  def urem(x: Int, y: Int) = ((x & UINT_MASK) % (y & UINT_MASK)).toInt

  def udiv(x: Long, y: Long) = (ULong(x) / ULong(y)).toLong
  def urem(x: Long, y: Long) = x - udiv(x, y) * y

  /**
    * Computing high part of 64-bit multiplication with the following scheme:
    * {{{
    *       hv1   lv1
    * v1 = b...b|b...b = (hv1 << 32) + lv1
    *
    *
    *       hv2   lv2
    * v2 = b...b|b...b = (hv2 << 32) + lv2
    *
    * w = v1 * v2 = (hv1 * hv2) << 64 + (hv1 * lv2 + hv2 * lv2) << 32 + lv1 * lv2
    *
    * hw = (hv1 * hv2) + ((hv1 * lv2 + hv2 * lv2) + (lv1 * lv2 >> 32)) >> 32
    * }}}
    * You can imagine it like this:
    * {{{
    *             | lv1 * lv2 |
    *       | hv2 * lv2 |
    *       | hv1 * lv2 |
    * | hv1 * hv2 |
    * --------------------------
    * |    hw     |     lw    |
    * }}}
    *
    * The algorithm is described in following paper:
    * ''"Hacker's Delight" by Henry S. Warren, Jr. (2002);
    * Chapter 8 "Multiplication";
    * Section 8-2''.
    *
    */
  def mulh(v1: Long, v2: Long): Long = {
    val lowBitMask: Long = 0xFFFFFFFF

    val lv1: ULong = v1 & lowBitMask
    val hv1: Long = v1 >> 32

    val lv2: ULong = v2 & lowBitMask
    val hv2: Long = v2 >> 32

    val w0 = lv1 * lv2

    val t: Long = (lv1 * hv2 + (w0 >>> 32)).toLong
    var w1: Long = t & lowBitMask
    val w2: Long = t >> 32

    w1 = (lv2 * hv1 + w1).toLong

    hv1 * hv2 + w2 + (w1 >> 32)
  }

  /**
    * Computing high part of 64-bit unsigned multiplication.
    *
    * It is implemented through `mulh`.
    *
    * The algorithm is described in following paper:
    * ''"Hacker's Delight" by Henry S. Warren, Jr. (2002);
    * Chapter 8 "Multiplication";
    * Section 8-3''.
    */
  def umulh(v1: Long, v2: Long): Long = {
    val p = mulh(v1, v2)

    val t1 = (v1 >> 63) & v2
    val t2 = (v2 >> 63) & v1

    p + t1 + t2
  }

  /** Returns minimal representable signed value with given width sign-extended to 64 bits. */
  def minExtended(width: Int) = (-1L) << ((width - 1) ensuring isBitNumber64 _)
}
