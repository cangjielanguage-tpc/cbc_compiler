/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.util.MathUtils.*
import org.scalatest.funsuite.AnyFunSuite

/** Tests for MathUtils class.
  *
  * @author conwor
  */
class MathUtilsSuite extends AnyFunSuite {

  test("IsPowerOf2") {
    assert(isPowerOf2(1))
    assert(isPowerOf2(2))
    assert(isPowerOf2(4))
    assert(isPowerOf2(8))
    assert(isPowerOf2(16))
    assert(isPowerOf2(1024))
    assert(isPowerOf2(4096))
    assert(isPowerOf2(0x40000000))

    assert(!isPowerOf2(3))
    assert(!isPowerOf2(5))
    assert(!isPowerOf2(9))
    assert(!isPowerOf2(17))
    assert(!isPowerOf2(1025))
    assert(!isPowerOf2(4097))
  }

  test("IsBitNumber32") {
    assert(!isBitNumber32(-1))
    assert(isBitNumber32(0))
    assert(isBitNumber32(4))
    assert(isBitNumber32(8))
    assert(isBitNumber32(16))
    assert(isBitNumber32(24))
    assert(isBitNumber32(31))
    assert(!isBitNumber32(32))
    assert(!isBitNumber32(64))
  }

  test("IsBitNumber64") {
    assert(!isBitNumber64(-1))
    assert(isBitNumber64(0))
    assert(isBitNumber64(4))
    assert(isBitNumber64(8))
    assert(isBitNumber64(16))
    assert(isBitNumber64(24))
    assert(isBitNumber64(31))
    assert(isBitNumber64(32))
    assert(isBitNumber64(48))
    assert(isBitNumber64(63))
    assert(!isBitNumber64(64))
  }

  test("IsNBits32") {
    assert(isNBits(0x0, 0))
    assert(isNBits(0x0, 1))
    assert(isNBits(0x0, 4))
    assert(isNBits(0x0, 8))
    assert(isNBits(0x0, 16))
    assert(isNBits(0x0, 32))

    assert(!isNBits(0xF, 0))
    assert(!isNBits(0xF, 1))
    assert(isNBits(0xF, 4))
    assert(isNBits(0xF, 8))
    assert(isNBits(0xF, 16))
    assert(isNBits(0xF, 32))

    assert(!isNBits(0xFFF, 0))
    assert(!isNBits(0xFFF, 1))
    assert(!isNBits(0xFFF, 4))
    assert(!isNBits(0xFFF, 8))
    assert(isNBits(0xFFF, 16))
    assert(isNBits(0xFFF, 32))

    assert(!isNBits(0xFFFFFFFF, 0))
    assert(!isNBits(0xFFFFFFFF, 1))
    assert(!isNBits(0xFFFFFFFF, 4))
    assert(!isNBits(0xFFFFFFFF, 8))
    assert(!isNBits(0xFFFFFFFF, 16))
    assert(isNBits(0xFFFFFFFF, 32))
  }

  test("IsNBits64") {
    assert(isNBits(0x0L, 0))
    assert(isNBits(0x0L, 1))
    assert(isNBits(0x0L, 4))
    assert(isNBits(0x0L, 8))
    assert(isNBits(0x0L, 16))
    assert(isNBits(0x0L, 32))
    assert(isNBits(0x0L, 48))
    assert(isNBits(0x0L, 64))

    assert(!isNBits(0xFL, 0))
    assert(!isNBits(0xFL, 1))
    assert(isNBits(0xFL, 4))
    assert(isNBits(0xFL, 8))
    assert(isNBits(0xFL, 16))
    assert(isNBits(0xFL, 32))
    assert(isNBits(0xFL, 48))
    assert(isNBits(0xFL, 64))

    assert(!isNBits(0xFFFL, 0))
    assert(!isNBits(0xFFFL, 1))
    assert(!isNBits(0xFFFL, 4))
    assert(!isNBits(0xFFFL, 8))
    assert(isNBits(0xFFFL, 16))
    assert(isNBits(0xFFFL, 32))
    assert(isNBits(0xFFFL, 48))
    assert(isNBits(0xFFFL, 64))

    assert(!isNBits(0xFFFFFFFFL, 0))
    assert(!isNBits(0xFFFFFFFFL, 1))
    assert(!isNBits(0xFFFFFFFFL, 4))
    assert(!isNBits(0xFFFFFFFFL, 8))
    assert(!isNBits(0xFFFFFFFFL, 16))
    assert(isNBits(0xFFFFFFFFL, 32))
    assert(isNBits(0xFFFFFFFFL, 48))
    assert(isNBits(0xFFFFFFFFL, 64))

    assert(!isNBits(0xFFFFFFFFFFFFL, 0))
    assert(!isNBits(0xFFFFFFFFFFFFL, 1))
    assert(!isNBits(0xFFFFFFFFFFFFL, 4))
    assert(!isNBits(0xFFFFFFFFFFFFL, 8))
    assert(!isNBits(0xFFFFFFFFFFFFL, 16))
    assert(!isNBits(0xFFFFFFFFFFFFL, 32))
    assert(isNBits(0xFFFFFFFFFFFFL, 48))
    assert(isNBits(0xFFFFFFFFFFFFL, 64))

    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 0))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 1))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 4))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 8))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 16))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 32))
    assert(!isNBits(0xFFFFFFFFFFFFFFFFL, 48))
    assert(isNBits(0xFFFFFFFFFFFFFFFFL, 64))
  }

  test("IsNBitsSigned") {
    assert(!isNBitsSigned(0xF, 2))
    assert(!isNBitsSigned(0xF, 4))
    assert(isNBitsSigned(0xF, 5))

    assert(!isNBitsSigned(0xFF, 4))
    assert(!isNBitsSigned(0xFF, 8))
    assert(isNBitsSigned(0xFF, 9))

    assert(isNBitsSigned(0xFFFFFFFF, 2))
    assert(isNBitsSigned(0xFFFFFFFF, 8))
    assert(isNBitsSigned(0xFFFFFFFF, 32))

    assert(!isNBitsSigned(0xFFFFFF37, 8))
    assert(isNBitsSigned(0xFFFFFF37, 9))
  }

  test("NthBit32") {
    assertResult(0x1)(nthBit32(0))
    assertResult(0x10)(nthBit32(4))
    assertResult(0x40)(nthBit32(6))
    assertResult(0x100)(nthBit32(8))
    assertResult(0x80000)(nthBit32(19))
    assertResult(0x80000000)(nthBit32(31))
  }

  test("NthBit64") {
    assertResult(0x1L)(nthBit64(0))
    assertResult(0x10L)(nthBit64(4))
    assertResult(0x40L)(nthBit64(6))
    assertResult(0x100L)(nthBit64(8))
    assertResult(0x80000L)(nthBit64(19))
    assertResult(0x80000000L)(nthBit64(31))
    assertResult(0x800000000000L)(nthBit64(47))
    assertResult(0x8000000000000000L)(nthBit64(63))
  }

  test("RightNBits32") {
    assertResult(0x0)(rightNBits32(0))
    assertResult(0x7)(rightNBits32(3))
    assertResult(0xFF)(rightNBits32(8))
    assertResult(0xFFF)(rightNBits32(12))
    assertResult(0xFFFFFFFF)(rightNBits32(32))
  }

  test("RightNBits64") {
    assertResult(0x0L)(rightNBits64(0))
    assertResult(0x7L)(rightNBits64(3))
    assertResult(0xFFL)(rightNBits64(8))
    assertResult(0xFFFL)(rightNBits64(12))
    assertResult(0xFFFFFFFFL)(rightNBits64(32))
    assertResult(0xFFFFFFFFFFFFL)(rightNBits64(48))
    assertResult(0xFFFFFFFFFFFFFFFFL)(rightNBits64(64))
  }

  test("Bits32") {
    assertResult(0x5)(bits(0x3EF, 3, 5))
    assertResult(0x5)(bits(0x2F0, 7, 9))

    assertResult(0x4)(bits(0x3FC, 0, 2))
    assertResult(0x7)(bits(0xFFFFFFFF, 29, 31))

    assertResult(0x0)(bits(0x3BC, 6, 6))
    assertResult(0x1)(bits(0x3BC, 5, 5))
  }

  test("Bits64") {
    assertResult(0x5L)(bits(0x3EFL, 3, 5))
    assertResult(0x5L)(bits(0x2F0L, 7, 9))

    assertResult(0x4L)(bits(0x3FCL, 0, 2))
    assertResult(0x7L)(bits(0xFFFFFFFFL, 29, 31))

    assertResult(0x0L)(bits(0x3BCL, 6, 6))
    assertResult(0x1L)(bits(0x3BCL, 5, 5))

    assertResult(0x1BCL)(bits(0xABCDE3BA29AFCCB7L, 43, 52))
  }

  test("Bit32") {
    assertResult(0x1)(bit(0x15, 0))
    assertResult(0x0)(bit(0x15, 1))
    assertResult(0x1)(bit(0x15, 2))
    assertResult(0x0)(bit(0x15, 3))
    assertResult(0x1)(bit(0x15, 4))
  }

  test("Bit64") {
    assertResult(0x1)(bit(0x15L, 0))
    assertResult(0x0)(bit(0x15L, 1))
    assertResult(0x1)(bit(0x15L, 2))
    assertResult(0x0)(bit(0x15L, 3))
    assertResult(0x1)(bit(0x15L, 4))

    assertResult(0x0)(bit(0xABCDE3BA29AFCCB7L, 52))
    assertResult(0x1)(bit(0xABCDE3BA29AFCCB7L, 51))
  }

  test("IsBitSet") {
    assert(isBitSet(0x15, 0))
    assert(!isBitSet(0x15, 1))
    assert(isBitSet(0x15, 2))
    assert(!isBitSet(0x15, 3))
    assert(isBitSet(0x15, 4))
  }

  test("SignExtend32") {
    assertResult(0x0)(signExtend(0x0, 1))
    assertResult(0x0)(signExtend(0x0, 2))
    assertResult(0x0)(signExtend(0x0, 28))
    assertResult(0x0)(signExtend(0x0, 32))
    assertResult(0xFFFF_FFFF)(signExtend(0xFFFF_FFFF, 32))

    assertResult(-1)(signExtend(0x1, 1))
    assertResult(1)(signExtend(0x1, 2))

    assertResult(0x9)(signExtend(0x9, 14))

    assertResult(0xFFFFFFF9)(signExtend(0x9, 4))
  }

  test("SignExtend64") {
    assertResult(0x0)(signExtend(0x0L, 1))
    assertResult(0x0)(signExtend(0x0L, 2))
    assertResult(0x0)(signExtend(0x0L, 63))
    assertResult(0x0)(signExtend(0x0L, 64))
    assertResult(0xFFFF_FFFF_FFFF_FFFFL)(signExtend(0xFFFF_FFFF_FFFF_FFFFL, 64))

    assertResult(-1)(signExtend(0x1L, 1))
    assertResult(1)(signExtend(0x1L, 2))

    assertResult(0x9)(signExtend(0x9L, 14))

    assertResult(0xFFFFFFFFFFFFFFF9L)(signExtend(0x9L, 4))
  }

  test("ZeroExtend") {
    assertResult(0L)(zeroExtend(0))
    assertResult(1L)(zeroExtend(1))
    assertResult(0xFFFFFFFFL)(zeroExtend(-1))
  }

  test("BitsSigned32") {
    assertResult(0)(bitsSigned(0, 0, 10))
    assertResult(1)(bitsSigned(1, 0, 1))
    assertResult(-1)(bitsSigned(1, 0, 0))
    assertResult(-1)(bitsSigned(0xF, 0, 2))
    assertResult(-1)(bitsSigned(-1, 0, 31))
    assertResult(Int.MaxValue)(bitsSigned(Int.MaxValue, 0, 31))
    assertResult(3)(bitsSigned(0xB, 0, 2))
    assertResult(73)(bitsSigned(73, 0, 7))
    assertResult(-55)(bitsSigned(73, 0, 6))
  }

  test("BitsSigned64") {
    assertResult(0L)(bitsSigned(0L, 0, 10))
    assertResult(1L)(bitsSigned(1L, 0, 1))
    assertResult(-1L)(bitsSigned(1L, 0, 0))
    assertResult(-1L)(bitsSigned(0xFL, 0, 2))
    assertResult(-1L)(bitsSigned(-1L, 0, 31))
    assertResult(Long.MaxValue)(bitsSigned(Long.MaxValue, 0, 63))
    assertResult(3L)(bitsSigned(0xBL, 0, 2))
    assertResult(73L)(bitsSigned(73L, 0, 7))
    assertResult(-55L)(bitsSigned(73L, 0, 6))
  }

  test("MaxBitNumber") {
    assertResult(0)(maxBitNumber(1))
    assertResult(7)(maxBitNumber(0xFF))
    assertResult(8)(maxBitNumber(0x1FF))
    assertResult(10)(maxBitNumber(0x7FF))
    assertResult(31)(maxBitNumber(-74))
  }

  test("MaxBitNumberFail") {
    assertThrows[AssertionError] {
      maxBitNumber(0)
    }
  }

  test("Replicate32") {
    assertResult(0x0)(replicate(0x0, 3))
    assertResult(0x0)(replicate(0x0, 5))
    assertResult(0x0)(replicate(0x0, 17))
    assertResult(0x0)(replicate(0x0, 31))
    assertResult(0x0)(replicate(0x0, 32))

    assertResult(0x7)(replicate(0x1, 3))
    assertResult(0x1F)(replicate(0x1, 5))
    assertResult(0x1FFFF)(replicate(0x1, 17))
    assertResult(0x7FFFFFFF)(replicate(0x1, 31))
    assertResult(0xFFFFFFFF)(replicate(0x1, 32))
  }

  test("Replicate64") {
    assertResult(0x0L)(replicate(0x0L, 3))
    assertResult(0x0L)(replicate(0x0L, 5))
    assertResult(0x0L)(replicate(0x0L, 17))
    assertResult(0x0L)(replicate(0x0L, 31))
    assertResult(0x0L)(replicate(0x0L, 32))
    assertResult(0x0L)(replicate(0x0L, 48))
    assertResult(0x0L)(replicate(0x0L, 63))
    assertResult(0x0L)(replicate(0x0L, 64))

    assertResult(0x7L)(replicate(0x1L, 3))
    assertResult(0x1FL)(replicate(0x1L, 5))
    assertResult(0x1FFFFL)(replicate(0x1L, 17))
    assertResult(0x7FFFFFFFL)(replicate(0x1L, 31))
    assertResult(0xFFFFFFFFL)(replicate(0x1L, 32))
    assertResult(0xFFFFFFFFFFFFL)(replicate(0x1L, 48))
    assertResult(0x7FFFFFFFFFFFFFFFL)(replicate(0x1L, 63))
    assertResult(0xFFFFFFFFFFFFFFFFL)(replicate(0x1L, 64))
  }

  test("RangeMask32") {
    assertResult(0x1)(rangeMask32(0, 0))
    assertResult(0x2)(rangeMask32(1, 1))
    assertResult(0x4)(rangeMask32(2, 2))

    assertResult(0x6)(rangeMask32(1, 2))
    assertResult(0x7)(rangeMask32(0, 2))

    assertResult(-1)(rangeMask32(0, 31))
  }

  test("RangeMask64") {
    assertResult(0x1L)(rangeMask64(0, 0))
    assertResult(0x2L)(rangeMask64(1, 1))
    assertResult(0x4L)(rangeMask64(2, 2))

    assertResult(0x6L)(rangeMask64(1, 2))
    assertResult(0x7L)(rangeMask64(0, 2))

    assertResult(-1L)(rangeMask64(0, 63))
  }

  test("IsAlignedToNBits") {
    assert(isAlignedToNBits(0x0, 0))
    assert(isAlignedToNBits(0x1, 0))

    assert(isAlignedToNBits(0x0, 1))
    assert(isAlignedToNBits(0x2, 1))
    assert(!isAlignedToNBits(0x1, 1))

    assert(isAlignedToNBits(0x0, 2))
    assert(!isAlignedToNBits(0x1, 2))
    assert(!isAlignedToNBits(0x2, 2))
    assert(!isAlignedToNBits(0x3, 2))
    assert(isAlignedToNBits(0x4, 2))
  }

  test("mulh") {
    assert(mulh(3, 2) == 0)
    assert(mulh(Long.MaxValue, Long.MaxValue) == 0x3fffffffffffffffL)
    assert(mulh(Long.MinValue, 123) == -62)

    assert(mulh(Long.MinValue, 0) == 0)
    assert(mulh(0, Long.MaxValue) == 0)

    assert(mulh(-6900413673144823051L, 7807569112197259409L) == -2920594357494930018L)
    assert(mulh(7303644825733277849L, 4063591318380238505L) == 1608903315825869098L)
  }
  
  test("umulh") {
    assert(umulh(3, 2) == 0)
    assert(umulh(1L << 63, 1L << 63) == 4611686018427387904L)

    assert(umulh(Long.MinValue, 0) == 0)
    assert(umulh(0, Long.MaxValue) == 0)

    assert(umulh(-6900413673144823051L, 7807569112197259409L) == 4886974754702329391L)
    assert(umulh(7303644825733277849L, 4063591318380238505L) == 1608903315825869098L)
  }
}
