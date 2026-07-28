/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.util

import xscala.util.MathUtils.minExtended
import org.scalatest.funsuite.AnyFunSuite

class OverflowsSuite extends AnyFunSuite {

  test("SignedAddOverflows") {
    assert(Overflows.sadd(0x7F, 0x01, 8))
    assert(Overflows.sadd(0x7F, 0x02, 8))
    assert(Overflows.sadd(0x7F, 0x40, 8))
    assert(Overflows.sadd(0x4F, 0x40, 8))
    assert(!Overflows.sadd(0x40, 0x00, 8))
    assert(!Overflows.sadd(0x30, 0x01, 8))
    assert(!Overflows.sadd(0x40, -0x40, 8))

    assert(Overflows.sadd(0x7FFF, 0x0001, 16))
    assert(Overflows.sadd(0x7FFF, 0x7FFF, 16))
    assert(Overflows.sadd(0x4000, 0x4000, 16))
    assert(!Overflows.sadd(0x7FFF, 0xFFFF, 16))
    assert(!Overflows.sadd(0x7FFF, 0xFFFF, 16))
    assert(!Overflows.sadd(0x7FF0, 0x000F, 16))

    assert(Overflows.sadd(0x8000_0000_0000_0000L, 0xFFFF_FFFF_FFFF_FFFFL, 64))
    assert(Overflows.sadd(0x8000_0000_0000_0000L, 0x8000_0000_0000_0000L, 64))
    assert(Overflows.sadd(0x7FFF_FFFF_FFFF_FFFFL, 0x0000_0000_0000_0001L, 64))
    assert(!Overflows.sadd(0x8000_0000_0000_0000L, 0x0000_0000_0000_0001L, 64))
    assert(!Overflows.sadd(0x7FFF_FFFF_FFFF_FFFFL, 0xFFFF_FFFF_FFFF_FFFFL, 64))
  }

  test("SignedMulOverflows") {
    assert(Overflows.smul(0xFF, 0x80, 8))
    assert(Overflows.smul(0x02, 0x80, 8))
    assert(Overflows.smul(0x02, 0x7F, 8))
    assert(!Overflows.smul(0x02, 0x3F, 8))
    assert(!Overflows.smul(0x01, 0x01, 8))
    assert(!Overflows.smul(0x08, 0x0F, 8))

    assert(Overflows.smul(0xFFFF, 0x8000, 16))
    assert(Overflows.smul(0x0002, 0x8000, 16))
    assert(Overflows.smul(0x0002, 0x7FFF, 16))
    assert(!Overflows.smul(0x0002, 0x3F00, 16))
    assert(!Overflows.smul(0x0001, 0x0001, 16))
    assert(!Overflows.smul(0x0008, 0x0F00, 16))

    assert(Overflows.smul(0xFFFF_FFFF, 0x8000_0000, 32))
    assert(Overflows.smul(0x0000_0002, 0x8000_0000, 32))
    assert(Overflows.smul(0x0000_0002, 0x7FFF_FFFF, 32))
    assert(!Overflows.smul(0x0000_0002, 0x3F00_0000, 32))
    assert(!Overflows.smul(0x0000_0001, 0x0000_0001, 32))
    assert(!Overflows.smul(0x0000_0008, 0x0F00_0000, 32))

    assert(Overflows.smul(0xFFFF_FFFF_FFFF_FFFFL, 0x8000_0000_0000_0000L, 64))
    assert(Overflows.smul(0x0000_0000_0000_0002L, 0x8000_0000_0000_0000L, 64))
    assert(Overflows.smul(0x0000_0000_0000_0002L, 0x7FFF_FFFF_FFFF_FFFFL, 64))
    assert(!Overflows.smul(0x0000_0000_0000_0002L, 0x3F00_0000_0000_0000L, 64))
    assert(!Overflows.smul(0x0000_0000_0000_0001L, 0x0000_0000_0000_0001L, 64))
    assert(!Overflows.smul(0x0000_0000_0000_0008L, 0x0F00_0000_0000_0000L, 64))
  }

  test("UnsignedMulOverflows") {
    assert(Overflows.umul(0xFF, 0x80, 8))
    assert(Overflows.umul(0x02, 0x80, 8))
    assert(!Overflows.umul(0x02, 0x7F, 8))
    assert(!Overflows.umul(0x02, 0x3F, 8))
    assert(!Overflows.umul(0x01, 0x01, 8))
    assert(!Overflows.umul(0x08, 0x0F, 8))

    assert(Overflows.umul(0xFFFF, 0x8000, 16))
    assert(Overflows.umul(0x0002, 0x8000, 16))
    assert(!Overflows.umul(0x0002, 0x7FFF, 16))
    assert(!Overflows.umul(0x0002, 0x3F00, 16))
    assert(!Overflows.umul(0x0001, 0x0001, 16))
    assert(!Overflows.umul(0x0008, 0x0F00, 16))

    assert(Overflows.umul(0xFFFF_FFFF, 0x8000_0000, 32))
    assert(Overflows.umul(0x0000_0002, 0x8000_0000, 32))
    assert(!Overflows.umul(0x0000_0002, 0x7FFF_FFFF, 32))
    assert(!Overflows.umul(0x0000_0002, 0x3F00_0000, 32))
    assert(!Overflows.umul(0x0000_0001, 0x0000_0001, 32))
    assert(!Overflows.umul(0x0000_0008, 0x0F00_0000, 32))

    assert(Overflows.umul(0xFFFF_FFFF_FFFF_FFFFL, 0x8000_0000_0000_0000L, 64))
    assert(Overflows.umul(0x0000_0000_0000_0002L, 0x8000_0000_0000_0000L, 64))
    assert(!Overflows.umul(0x0000_0000_0000_0002L, 0x7FFF_FFFF_FFFF_FFFFL, 64))
    assert(!Overflows.umul(0x0000_0000_0000_0002L, 0x3F00_0000_0000_0000L, 64))
    assert(!Overflows.umul(0x0000_0000_0000_0001L, 0x0000_0000_0000_0001L, 64))
    assert(!Overflows.umul(0x0000_0000_0000_0008L, 0x0F00_0000_0000_0000L, 64))
  }

  test("UnsignedAddOverflows") {
    assert(Overflows.uadd(0xFFL, 0x01L, 8))
    assert(Overflows.uadd(0xFEL, 0x02L, 8))
    assert(Overflows.uadd(0xF7L, 0x0AL, 8))
    assert(Overflows.uadd(0xF0L, 0xF0L, 8))
    assert(Overflows.uadd(0x80L, 0x80L, 8))

    assert(Overflows.uadd(0xFFFFL, 0x0001L, 16))
    assert(Overflows.uadd(0xFFFEL, 0x0002L, 16))
    assert(Overflows.uadd(0xFFF7L, 0x000AL, 16))
    assert(Overflows.uadd(0xF000L, 0xF000L, 16))
    assert(Overflows.uadd(0x8000L, 0x8000L, 16))

    assert(Overflows.uadd(0xFFFF_FFFFL, 0x0000_0001L, 32))
    assert(Overflows.uadd(0xFFFF_FFFEL, 0x0000_0002L, 32))
    assert(Overflows.uadd(0xFFFF_FFF7L, 0x0000_000AL, 32))
    assert(Overflows.uadd(0xF000_0000L, 0xF000_0000L, 32))
    assert(Overflows.uadd(0x8000_0000L, 0x8000_0000L, 32))

    assert(Overflows.uadd(0xFFFF_FFFF_FFFF_FFFFL, 0x0000_0000_0000_0001L, 64))
    assert(Overflows.uadd(0xFFFF_FFFF_FFFF_FFFEL, 0x0000_0000_0000_0002L, 64))
    assert(Overflows.uadd(0xFFFF_FFFF_FFFF_FFF7L, 0x0000_0000_0000_000AL, 64))
    assert(Overflows.uadd(0xF000_0000_0000_0000L, 0xF000_0000_0000_0000L, 64))
    assert(Overflows.uadd(0x8000_0000_0000_0000L, 0x8000_0000_0000_0000L, 64))
  }

  test("SignedSubOverflows") {
    assert(!Overflows.ssub(0x7F, 0x01, 8))
    assert(!Overflows.ssub(0x7F, 0x02, 8))
    assert(!Overflows.ssub(0x7F, 0x40, 8))
    assert(!Overflows.ssub(0x4F, 0x40, 8))
    assert(Overflows.ssub(0x80, 0x01, 8))
    assert(Overflows.ssub(0x81, 0x02, 8))
    assert(Overflows.ssub(0x40, -0x40, 8))

    assert(!Overflows.ssub(0x7FFF, 0x0001, 16))
    assert(!Overflows.ssub(0x7FFF, 0x7FFF, 16))
    assert(!Overflows.ssub(0x4000, 0x4000, 16))
    assert(Overflows.ssub(0x7FFF, 0xFFFF, 16))
    assert(Overflows.ssub(0x7FFF, 0xFFFF, 16))
    assert(Overflows.ssub(0x7FF0, 0xFFF0, 16))

    assert(!Overflows.ssub(0x8000_0000_0000_0000L, 0xFFFF_FFFF_FFFF_FFFFL, 64))
    assert(!Overflows.ssub(0x8000_0000_0000_0000L, 0x8000_0000_0000_0000L, 64))
    assert(!Overflows.ssub(0x7FFF_FFFF_FFFF_FFFFL, 0x0000_0000_0000_0001L, 64))
    assert(Overflows.ssub(0x8000_0000_0000_0000L, 0x0000_0000_0000_0001L, 64))
    assert(Overflows.ssub(0x7FFF_FFFF_FFFF_FFFFL, 0xFFFF_FFFF_FFFF_FFFFL, 64))
  }

  test("UnsignedSubOverflows") {
    assert(Overflows.usub(0x00L, 0x01L, 8))
    assert(Overflows.usub(0x01L, 0x02L, 8))
    assert(Overflows.usub(0x08L, 0x0AL, 8))
    assert(Overflows.usub(0xE0L, 0xF0L, 8))
    assert(Overflows.usub(0x70L, 0x80L, 8))

    assert(Overflows.usub(0x0000L, 0x0001L, 16))
    assert(Overflows.usub(0x0001L, 0x0002L, 16))
    assert(Overflows.usub(0x0008L, 0x000AL, 16))
    assert(Overflows.usub(0xE000L, 0xF000L, 16))
    assert(Overflows.usub(0x7000L, 0x8000L, 16))

    assert(Overflows.usub(0x0000_0000L, 0x0000_0001L, 32))
    assert(Overflows.usub(0x0000_0001L, 0x0000_0002L, 32))
    assert(Overflows.usub(0x0000_0003L, 0x0000_000AL, 32))
    assert(Overflows.usub(0xA000_0000L, 0xF000_0000L, 32))
    assert(Overflows.usub(0x3000_0000L, 0x8000_0000L, 32))

    assert(Overflows.usub(0x0000_0000_0000_0000L, 0x0000_0000_0000_0001L, 64))
    assert(Overflows.usub(0x0000_0000_0000_0001L, 0x0000_0000_0000_0002L, 64))
    assert(Overflows.usub(0x0000_0000_0000_0009L, 0x0000_0000_0000_000AL, 64))
    assert(Overflows.usub(0xD000_0000_0000_0000L, 0xF000_0000_0000_0000L, 64))
    assert(Overflows.usub(0x1000_0000_0000_0000L, 0x8000_0000_0000_0000L, 64))
  }

  test("MinExtended") {
    assertResult(minExtended(2))(0xFFFF_FFFF_FFFF_FFFEL)
    assertResult(minExtended(4))(0xFFFF_FFFF_FFFF_FFF8L)
    assertResult(minExtended(8))(0xFFFF_FFFF_FFFF_FF80L)
    assertResult(minExtended(16))(0xFFFF_FFFF_FFFF_8000L)
    assertResult(minExtended(32))(0xFFFF_FFFF_8000_0000L)
    assertResult(minExtended(64))(0x8000_0000_0000_0000L)
  }
}
