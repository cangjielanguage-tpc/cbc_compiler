/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.arm64.immediates.BitMaskImm.{canEncode, encode}
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Width}
import org.scalatest.funsuite.AnyFunSuite

class BitMaskImmArm64Suite extends AnyFunSuite with AssemblerToolbox[Null] {
  def assertEncoding(imm: Long, N: Int, immr: Int, imms: Int, width: Width): Unit = {
    assert(canEncode(imm, width))
    val encoding = encode(imm, width)
    assertResult(N)(encoding.N)
    assertResult(immr)(encoding.immr)
    assertResult(imms)(encoding.imms)
    assertResult(imm)(encoding.decode(width))
  }

  def assertEncoding(imm: Int, N: Int, immr: Int, imms: Int): Unit = {
    assertEncoding(imm, N, immr, imms, W32)
  }

  def assertEncoding(imm: Long, N: Int, immr: Int, imms: Int): Unit = {
    assertEncoding(imm, N, immr, imms, W64)
  }

  def assertNotEncoded(imm: Long, width: Width): Unit = {
    assert(!canEncode(imm, width))
  }

  def assertNotEncoded(imm: Int): Unit = {
    assertNotEncoded(imm, W32)
  }

  def assertNotEncoded(imm: Long): Unit = {
    assertNotEncoded(imm, W64)
  }

  test("32") {
    assertEncoding(0xaaaaaaaa, 0, bini("000001"), bini("111100"))
    assertEncoding(0x66666666, 0, bini("000011"), bini("111001"))
    assertEncoding(0x1e1e1e1e, 0, bini("000111"), bini("110011"))
    assertEncoding(0xe000e000, 0, bini("000011"), bini("100010"))
    assertEncoding(0x00f80000, 0, bini("001101"), bini("000100"))
    assertEncoding(0xfffdfffd, 0, bini("001110"), bini("101110"))

    assertNotEncoded(0)
  }

  test("64") {
    assertEncoding(0xfffdfffdfffdfffdL, 0, bini("001110"), bini("101110"))
    assertEncoding(0x8000000380000003L, 0, bini("000001"), bini("000010"))
    assertEncoding(0x0000780000007800L, 0, bini("010101"), bini("000011"))
    assertEncoding(0xffe0000000000001L, 1, bini("001011"), bini("001011"))
    assertEncoding(0xc0000001ffffffffL, 1, bini("000010"), bini("100010"))
    assertEncoding(0x03f8000000000000L, 1, bini("001101"), bini("000110"))
    assertEncoding(0x0000000e00000000L, 1, bini("011111"), bini("000010"))

    assertNotEncoded(0L)
    assertNotEncoded(binl("11001100"))
  }

  test("AllValues") {
    for (log2size <- 1 until 7) {
      for (immr <- 0 until 1 << log2size; x <- 0 until (1 << log2size) - 1) {
        val N = if (log2size == 6) 1 else 0
        val imms = bini("111111") & (bini("111110") << log2size) | x
        val imm = new BitMaskImm(N, immr, imms)
        if (log2size != 6) {
          assertEncoding(imm.decode(W32).toInt, N, immr, imms)
        }
        assertEncoding(imm.decode(W64), N, immr, imms)
      }
    }
  }
}
