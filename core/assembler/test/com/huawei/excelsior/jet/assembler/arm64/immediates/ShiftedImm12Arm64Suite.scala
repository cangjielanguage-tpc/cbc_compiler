/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import com.huawei.excelsior.jet.assembler.AssemblerToolbox
import com.huawei.excelsior.jet.assembler.arm64.immediates.ShiftedImm12.{canEncode, encode}
import org.scalatest.funsuite.AnyFunSuite

class ShiftedImm12Arm64Suite extends AnyFunSuite with AssemblerToolbox[Null] {
  private def assertEncoding(imm: Int, imm12: Int, shift: Int): Unit = {
    assert(canEncode(imm))
    val encoding = encode(imm)
    assertResult(imm12)(encoding.imm12)
    assertResult(shift)(encoding.shift)
    assertResult(imm)(encoding.decode)
  }

  private def assertNotEncoded(imm: Int): Unit = {
    assert(!canEncode(imm))
  }

  test("test") {
    assertEncoding(bini("0"),                  0, 0)
    assertEncoding(bini("1"),                  1, 0)
    assertEncoding(bini("111111111111"),       bini("111111111111"), 0)
    assertEncoding(bini("111111111111") << 12, bini("111111111111"), 1)

    assertNotEncoded(bini("1111111111111"))
    assertNotEncoded(-1)
  }
}
