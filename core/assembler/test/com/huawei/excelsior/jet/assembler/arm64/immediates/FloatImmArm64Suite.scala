/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.arm64.immediates.FloatImm.{canEncode, encode}
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Width}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.Double.doubleToLongBits

/** Tests for [[FloatImm]] class.
  *
  * @author orangebyte256
  */
class FloatImmArm64Suite extends AnyFunSuite with AssemblerToolbox[Null] {
  private def assertEncoding(imm: Double, imm8: Int, width: Width): Unit = {
    val encodingPos = encode(imm, width)
    val encodingNeg = encode(-imm, width)
    assertResult(imm8)(encodingPos.imm8)
    assertResult(doubleToLongBits(imm))(doubleToLongBits(encodingPos.decode(width)))
    assertResult(imm8 | 1 << 7)(encodingNeg.imm8)
    assertResult(doubleToLongBits(-imm))(doubleToLongBits(encodingNeg.decode(width)))
  }

  private def assertEncoding(imm: Double, imm8: Int): Unit = {
    assertEncoding(imm, imm8, W64)
  }

  private def assertEncoding(imm: Float, imm8: Int): Unit = {
    assertEncoding(imm, imm8, W32)
  }

  private def assertNotEncoded(imm: Double, width: Width): Unit = {
    assert(!canEncode(imm, width))
  }

  private def assertNotEncoded(imm: Double): Unit = {
    assertNotEncoded(imm, W64)
  }

  test("32") {
    assertEncoding(1.0f, bini("01110000"))
    assertEncoding(2.0f, 0)
    assertEncoding(16.0f, bini("00110000"))
    assertEncoding(0.125f, bini("01000000"))
    assertEncoding(31.0f, bini("00111111"))

    assertNotEncoded(32.0f)
    assertNotEncoded(0.0625f)
    assertNotEncoded(0.0f)
    assertNotEncoded(-0.0f)
    assertNotEncoded(java.lang.Float.NaN)
    assertNotEncoded(java.lang.Float.NEGATIVE_INFINITY)
    assertNotEncoded(java.lang.Float.POSITIVE_INFINITY)
  }

  test("64") {
    assertEncoding(1.0d, bini("01110000"))
    assertEncoding(2.0d, 0)
    assertEncoding(16.0d, bini("00110000"))
    assertEncoding(0.125d, bini("01000000"))
    assertEncoding(31.0d, bini("00111111"))

    assertNotEncoded(32.0d)
    assertNotEncoded(0.0625d)
    assertNotEncoded(0.0d)
    assertNotEncoded(-0.0d)
    assertNotEncoded(java.lang.Double.NaN)
    assertNotEncoded(java.lang.Double.NEGATIVE_INFINITY)
    assertNotEncoded(java.lang.Double.POSITIVE_INFINITY)
  }
}
