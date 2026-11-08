/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.isa12.Float16
import org.scalatest.funsuite.AnyFunSuite

class Float16Suite extends AnyFunSuite {
  test("double rounds directly to float16, ties rounds to even") {
    val cases = Seq(
      (0.0, 0x0000), (-0.0, 0x8000), (1.0, 0x3c00),
      (65504.0, 0x7bff), (65520.0, 0x7c00),
      (Math.scalb(1.0, -14), 0x0400), (Math.scalb(1.0, -24), 1),
      (Math.scalb(1.0, -25), 0), (Math.nextUp(Math.scalb(1.0, -25)), 1),
      (1.00048828125, 0x3c00), (1.0004882812500002, 0x3c01),
      (1.00146484375, 0x3c02), (1.0014648437499998, 0x3c01),
      (Double.PositiveInfinity, 0x7c00), (Double.NegativeInfinity, 0xfc00)
    )
    for ((value, expected) <- cases) {
      assert(Float16.bits(value) == expected, s"$value")
    }
    assert((Float16.bits(Double.NaN) & 0x7fff) > 0x7c00)
  }

  test("serialized half opcodes and raw immediate bytes agree with the engine") {
    import com.huawei.excelsior.jet.assembler.Symbol
    import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.BytecodeReference
    import com.huawei.excelsior.jet.assembler.cbc.Register.FR
    import com.huawei.excelsior.jet.assembler.Width
    import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{Assembler, SymbolAdapter}
    val opcodes = Seq(Assembler.Opcode.Float16, Assembler.Opcode.FMov16,
      Assembler.Opcode.FMov16i, Assembler.Opcode.Bcc16, Assembler.Opcode.Scc16,
      Assembler.Opcode.FMathUn16)
    assert(opcodes.map(_.ordinal) == (174 to 179))
    val asm = new Assembler with SymbolAdapter {
      def adapt(symbol: Symbol): BytecodeReference = throw new UnsupportedOperationException
    }
    val segment = asm.withNewSegment {
      asm.fmovi16Bits(FR.FR2, 0xfd55)
      asm.fret(FR.FR2, Width.W16)
    }
    assert(segment.toByteArray.map(_ & 255).toSeq == Seq(176, 2, 0x55, 0xfd, 71, 0x92))
  }
}
