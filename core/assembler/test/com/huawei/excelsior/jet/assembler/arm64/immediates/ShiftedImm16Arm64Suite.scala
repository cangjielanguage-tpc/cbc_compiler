/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.arm64.immediates.ShiftedImm16.{canEncode, encode}
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Width}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[ShiftedImm16]] class.
  *
  * @author conwor
  */
class ShiftedImm16Arm64Suite extends AnyFunSuite with AssemblerToolbox[Null] {
  private def assertEncoding(imm: Long, hw: Int, imm16: Int, width: Width): Unit = {
    assert(canEncode(imm, width))
    val encoding1 = encode(imm, width)
    val encoding2 = encode(imm16, hw * 16, width)
    assertResult(hw)(encoding1.hw)
    assertResult(hw)(encoding2.hw)
    assertResult(imm16)(encoding1.imm16)
    assertResult(imm16)(encoding2.imm16)
    assertResult(imm)(encoding1.decode(width))
    assertResult(imm)(encoding2.decode(width))
  }

  private def assertEncoding(imm: Int, hw: Int, imm16: Int): Unit = {
    assertEncoding(imm, hw, imm16, W32)
  }

  private def assertEncoding(imm: Long, hw: Int, imm16: Int): Unit = {
    assertEncoding(imm, hw, imm16, W64)
  }

  private def assertNotEncoded(imm: Long, width: Width): Unit = {
    assert(!canEncode(imm, width))
  }

  private def assertNotEncoded(imm: Int): Unit = {
    assertNotEncoded(imm, W32)
  }

  private def assertNotEncoded(imm: Long): Unit = {
    assertNotEncoded(imm, W64)
  }

  test("32") {
    assertEncoding(42,         0, 42)
    assertEncoding(42 << 16,   1, 42)

    assertEncoding(0,          0, 0)
    assertEncoding(1,          0, 1)
    assertEncoding(0x8000,     0, 0x8000)
    assertEncoding(0xffff,     0, 0xffff)
    assertEncoding(0x7fff,     0, 0x7fff)
    assertEncoding(0x80000000, 1, 0x8000)
    assertEncoding(0xffff0000, 1, 0xffff)
    assertEncoding(0x7fff0000, 1, 0x7fff)

    assertNotEncoded((42 << 16) | 37)
  }

  test("64") {
    assertEncoding(42L,                0, 42)
    assertEncoding(42L << 16,          1, 42)
    assertEncoding(42L << 32,          2, 42)
    assertEncoding(42L << 48,          3, 42)

    assertEncoding(0,                   0, 0)
    assertEncoding(1,                   0, 1)
    assertEncoding(0x8000000000000000L, 3, 0x8000)
    assertEncoding(0xffff000000000000L, 3, 0xffff)
    assertEncoding(0x7fff000000000000L, 3, 0x7fff)

    assertNotEncoded((42L << 16) | 37L)
    assertNotEncoded((42L << 32) | 37L)
    assertNotEncoded((42L << 48) | 37L)
    assertNotEncoded(~42L)
  }
}
