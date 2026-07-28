/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.arm64.CC.{EQ, GE, LT, NE}
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.W15
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{X0, X10, X15, X20}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{ARM64_ADD_IMM_LO12, ARM64_ADRP_IMM, ARM64_B_BL_IMM}
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Label}
import org.scalatest.funsuite.AnyFunSuite

class FixupArm64Suite extends AnyFunSuite with AssemblerToolbox[Assembler] {
  var asm: Assembler = _

  val testSymbol = newSymbol

  override def createEmitter() = {
    asm = new Assembler()
    asm.setUp()
    asm
  }

  override val resultParseFormat = ResultParseFormat.ARM64

  test("Jump") {
    val l0 = new Label
    val l1 = new Label
    val l2 = new Label
    val l3 = new Label
    asm.bind(l0)
    emitZeroes(16)
    asm.b(l0)
    asm.bind(l1)
    asm.b(l1)
    asm.b(l2)
    asm.bind(l2)
    asm.b(l3)
    emitZeroes(16)
    asm.bind(l3)
    asm.b(testSymbol)

    checkFinal(
      zeroes(16),
      "fcffff17", "00000014", "01000014", "05000014",
      zeroes(16),
      relocation(ARM64_B_BL_IMM, testSymbol), "00000014")
  }

  test("Branch") {
    val l0 = new Label
    val l1 = new Label
    val l2 = new Label
    val l3 = new Label
    asm.bind(l0)
    emitZeroes(16)
    asm.b(NE, l0)
    asm.bind(l1)
    asm.b(EQ, l1)
    asm.b(LT, l2)
    asm.bind(l2)
    asm.b(GE, l3)
    emitZeroes(16)
    asm.bind(l3)

    checkFinal(
      zeroes(16),
      "81ffff54", "00000054", "2b000054", "aa000054",
      zeroes(16))
  }

  test("OffsFixup") {
    val l0 = new Label
    val l1 = new Label
    val l2 = new Label
    asm.bind(l0)
    emitZeroes(30)
    asm.movOffs32InMethod(X20, l0)
    asm.bind(l1)
    emitZeroes(30)
    asm.movOffs32InMethod(X10, l1)
    asm.movOffs32InMethod(W15, l2)
    emitZeroes(1000000)
    asm.bind(l2)

    checkFinal(
      zeroes(30),
      "140080d2", "1400a0f2", zeroes(30),
      "ca0480d2", "0a00a0f2", "8f528852", "ef01a072",
      zeroes(1000000))
  }

  test("AddrFixup") {
    val l0 = new Label
    val l1 = new Label
    asm.bind(l0)
    emitZeroes(30)
    asm.adr(X20, l0)
    asm.adr(X10, l1)
    emitZeroes(200)
    asm.bind(l1)
    asm.adr(X15, testSymbol)

    checkFinal(
      zeroes(30),
      "14ffff50", "6a060010", zeroes(200),
      relocation(ARM64_ADRP_IMM, testSymbol), "0f000090",
      relocation(ARM64_ADD_IMM_LO12, testSymbol), "ef010091")
  }

  test("CompareBranch") {
    val l0 = new Label
    val l1 = new Label
    asm.bind(l0)
    emitZeroes(16)
    asm.cbz(X0, l0)
    asm.cbnz(X0, l0)
    asm.cbnz(X0, l1)
    asm.cbz(X0, l1)
    emitZeroes(16)
    asm.bind(l1)

    checkFinal(
      zeroes(16),
      "80ffffb4", "60ffffb5", "c00000b5", "a00000b4",
      zeroes(16))
  }

  test("TestBranch") {
    val l0 = new Label
    val l1 = new Label
    asm.bind(l0)
    emitZeroes(16)
    asm.tbz(X0, 32, l0)
    asm.tbnz(X0, 2, l0)
    asm.tbnz(X0, 32, l1)
    asm.tbz(X0, 2, l1)
    emitZeroes(262144)
    asm.bind(l1)

    checkFinal(
      zeroes(16),
      "80ff07b6", "60ff1737", "1f0060f2", "61002054", "1f007ef2", "20002054",
      zeroes(262144))
  }
}
