/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AssemblerToolbox
import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.amd64.Feature.SHORTJUMPS
import org.scalatest.funsuite.AnyFunSuite

/** Tests for short jumps transformation.
  *
  * @author conwor
  */
class ShortJumpsAmd64Suite extends AnyFunSuite with AssemblerToolbox[Assembler] {

  var asm: Assembler = _

  override def createEmitter() = {
    asm = new Assembler(SHORTJUMPS)
    asm.setUp()
    asm
  }
  override val resultParseFormat = ResultParseFormat.INTEL

  test("ShortJump") {
    val label = asm.newLabel
    asm.jmp(label)
    emitZeroes(3)
    asm.bind(label)

    checkFinal(0xeb, 0x03, zeroes(3))
  }

  test("LongJump") {
    val label = asm.newLabel
    asm.jmp(label)
    emitZeroes(300)
    asm.bind(label)

    checkFinal(0xe9, 0x2c, 0x01, 0x00, 0x00, zeroes(300))
  }

  test("2ShortJumps") {
    val label1 = asm.newLabel
    val label2 = asm.newLabel

    asm.bind(label2)
    asm.jmp(label1)
    emitZeroes(100)
    asm.bind(label1)
    asm.jmp(label2)

    checkFinal(
      0xeb, 0x64,
      zeroes(100),
      0xeb, 0x98)
  }

  test("2LongJumps") {
    val label1 = asm.newLabel
    val label2 = asm.newLabel

    asm.bind(label2)
    asm.jmp(label1)
    emitZeroes(200)
    asm.bind(label1)
    asm.jmp(label2)

    checkFinal(
      0xe9, 0xc8, 0x00, 0x00, 0x00,
      zeroes(200),
      0xe9, 0x2e, 0xff, 0xff, 0xff)
  }

  test("SerialShortJumps") {
    val label1 = asm.newLabel
    val label2 = asm.newLabel
    val label3 = asm.newLabel
    val label4 = asm.newLabel

    asm.jmp(label1)
    emitZeroes(125)

    asm.jmp(label2)
    asm.bind(label1)
    emitZeroes(125)

    asm.jmp(label3)
    asm.bind(label2)
    emitZeroes(125)

    asm.jmp(label4)
    asm.bind(label3)
    emitZeroes(127)

    asm.bind(label4)

    checkFinal(
      0xeb, 0x7f,
      zeroes(125),
      0xeb, 0x7f,
      zeroes(125),
      0xeb, 0x7f,
      zeroes(125),
      0xeb, 0x7f,
      zeroes(127))
  }

  test("SerialLongJumps") {
    val label1 = asm.newLabel
    val label2 = asm.newLabel
    val label3 = asm.newLabel
    val label4 = asm.newLabel

    asm.jmp(label1)
    emitZeroes(125)

    asm.jmp(label2)
    asm.bind(label1)
    emitZeroes(125)

    asm.jmp(label3)
    asm.bind(label2)
    emitZeroes(125)

    asm.jmp(label4)
    asm.bind(label3)
    emitZeroes(128)

    asm.bind(label4)

    checkFinal(
      0xe9, 0x82, 0x00, 0x00, 0x00,
      zeroes(125),
      0xe9, 0x82, 0x00, 0x00, 0x00,
      zeroes(125),
      0xe9, 0x82, 0x00, 0x00, 0x00,
      zeroes(125),
      0xe9, 0x80, 0x00, 0x00, 0x00,
      zeroes(128))
  }
}
