/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.arm64.Arg.*
import com.huawei.excelsior.jet.assembler.arm64.CC.*
import com.huawei.excelsior.jet.assembler.arm64.DBOption.*
import com.huawei.excelsior.jet.assembler.arm64.ExtendMode.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode.*
import com.huawei.excelsior.jet.assembler.arm64.MemAtomicOp.*
import com.huawei.excelsior.jet.assembler.arm64.PrfOp.*
import com.huawei.excelsior.jet.assembler.arm64.ShiftMode.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.H.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.S.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.V.*
import com.huawei.excelsior.jet.assembler.{AsmError, AssemblerToolbox, Width}
import org.scalatest.funsuite.AnyFunSuite

class AssemblerArm64Suite extends AnyFunSuite with AssemblerToolbox[Assembler] {
  val CODE_ALIGNMENT = 16
  var asm: Assembler = _

  override def createEmitter() = {
    asm = new Assembler()
    asm.setUp()
    asm.alignStart(CODE_ALIGNMENT)
    asm
  }
  override val resultParseFormat = ResultParseFormat.ARM64

  ///////////////////////////////////////////////////////////////////////////
  // Instructions related to arithmetic and conversions

  test("ArithmeticWithExtended") {
    def generateArithmeticWithExtended(dst: IRegister, src1: IRegister, src2: Arg.ExtendedReg): Unit = {
      asm.add(dst, src1, src2)
      asm.sub(dst, src1, src2)
      asm.subs(dst, src1, src2)
      asm.cmp(src1, src2)
    }

    generateArithmeticWithExtended(X2, X7, R(X8, UXTX, 4))
    generateArithmeticWithExtended(X10, X3, R(W5, SXTW, 3))
    generateArithmeticWithExtended(W1, W12, R(W18, SXTB))
    generateArithmeticWithExtended(W1, W12, R(W18, SXTX, 2))
    checkFinal(
      "e270288b", "e27028cb", "e27028eb", "ff7028eb",
      "6acc258b", "6acc25cb", "6acc25eb", "7fcc25eb",
      "8181320b", "8181324b", "8181326b", "9f81326b",
      "81e9320b", "81e9324b", "81e9326b", "9fe9326b")

    assertThrows[AsmError] {
      generateArithmeticWithExtended(W1, W12, R(X18, SXTX))
    }
  }

  test("ArithmeticWithImm") {
    def generateArithmeticWithImm(dst: IRegister, src1: IRegister, src2: Int, shift: Int): Unit = {
      asm.add(dst, src1, src2 << shift)
      asm.sub(dst, src1, src2 << shift)
      asm.cmp(dst, src2 << shift)
    }

    generateArithmeticWithImm(W0, W16, 0, 0)
    generateArithmeticWithImm(X2, X7, 214, 12)
    generateArithmeticWithImm(X10, SP, 1024, 0)
    generateArithmeticWithImm(W1, W12, 4047, 12)
    checkFinal(
      "00020011", "00020051", "1f000071",
      "e2584391", "e25843d1", "5f5843f1",
      "ea031091", "ea0310d1", "5f0110f1",
      "813d7f11", "813d7f51", "3f3c7f71")
  }


  test("ArithmeticWithShifted") {
    def generateArithmeticWithShifted(dst: IRegister, src1: IRegister, src2: Arg.ShiftedReg): Unit = {
      val skipAddSub = src2.mode == ROR
      if (!skipAddSub) {
        asm.add(dst, src1, src2)
        asm.sub(dst, src1, src2)
      }
      asm.and(dst, src1, src2)
      asm.ands(dst, src1, src2)
      asm.eor(dst, src1, src2)
      asm.orr(dst, src1, src2)
      asm.orn(dst, src1, src2)
      asm.tst(dst, src2)
      if (!skipAddSub) {
        asm.cmp(dst, src2)
        asm.neg(dst, src2)
      }
    }

    generateArithmeticWithShifted(X6, X21, R(X6, LSL, 4))
    generateArithmeticWithShifted(X21, X2, R(X10, ASR, 60))
    generateArithmeticWithShifted(W5, W10, R(W12, LSR, 30))
    generateArithmeticWithShifted(W6, W22, R(W10, LSL, 0))
    generateArithmeticWithShifted(X3, X3, R(X11, ASR, 55))
    generateArithmeticWithShifted(W8, W7, R(W20, LSR, 20))
    generateArithmeticWithShifted(X0, X1, R(X2, ROR, 32))
    checkFinal(
      "a612068b", "a61206cb", "a612068a", "a61206ea",
      "a61206ca", "a61206aa", "a61226aa", "df1006ea",
      "df1006eb", "e61306cb", "55f08a8b", "55f08acb",
      "55f08a8a", "55f08aea", "55f08aca", "55f08aaa",
      "55f0aaaa", "bff28aea", "bff28aeb", "f5f38acb",
      "45794c0b", "45794c4b", "45794c0a", "45794c6a",
      "45794c4a", "45794c2a", "45796c2a", "bf784c6a",
      "bf784c6b", "e57b4c4b", "c6020a0b", "c6020a4b",
      "c6020a0a", "c6020a6a", "c6020a4a", "c6020a2a",
      "c6022a2a", "df000a6a", "df000a6b", "e6030a4b",
      "63dc8b8b", "63dc8bcb", "63dc8b8a", "63dc8bea",
      "63dc8bca", "63dc8baa", "63dcabaa", "7fdc8bea",
      "7fdc8beb", "e3df8bcb", "e850540b", "e850544b",
      "e850540a", "e850546a", "e850544a", "e850542a",
      "e850742a", "1f51546a", "1f51546b", "e853544b",
      "2080c28a", "2080c2ea", "2080c2ca", "2080c2aa",
      "2080e2aa", "1f80c2ea")
  }

  test("AddSubWithROR") {
    assertThrows[AsmError] {
      asm.add(X0, X1, R(X2, ROR, 1))
    }
  }

  test("BadShiftedAmount1") {
    assertThrows[AsmError] {
      asm.add(X0, X1, R(X2, LSL, 100))
    }
  }

  test("BadShiftedAmount2") {
    assertThrows[AsmError] {
      asm.eor(W0, W1, R(W2, LSL, 33))
    }
  }

  test("ArithmeticWithImm13") {
    def generateArithmeticWithMasked(dst: IRegister, src1: IRegister, src2: Long): Unit = {
      asm.orr(dst, src1, src2)
      asm.eor(dst, src1, src2)
      asm.and(dst, src1, src2)
      asm.tst(dst, src2)
      asm.mov(dst, src2)
    }

    generateArithmeticWithMasked(X1, X3, binl("1001100110011001100110011001100110011001100110011001100110011001"))
    generateArithmeticWithMasked(W2, W5, binl("0000010000000100000001000000010000000100000001000000010000000100"))
    checkFinal(
      "61e401b2", "61e401d2", "61e40192", "3fe401f2",
      "e1e701b2", "a2c00632", "a2c00652", "a2c00612",
      "5fc00672", "e2c30632")
  }

  test("Adds") {
    asm.adds(X1, X2, X3)
    asm.adds(X15, X14, X3)
    asm.adds(W1, W8, W21)
    asm.adds(W23, W0, W1)
    checkFinal("410003ab", "cf0103ab", "0101152b", "1700012b")
  }


  test("ArithmeticWithBothRegister") {
    def generateArithmeticWithBothRegister(dst: IRegister, src1: IRegister, src2: IRegister): Unit = {
      asm.mul(dst, src1, src2)
      asm.lsl(dst, src1, src2)
      asm.asr(dst, src1, src2)
      asm.lsr(dst, src1, src2)
      asm.sdiv(dst, src1, src2)
    }

    generateArithmeticWithBothRegister(X1, X11, X7)
    generateArithmeticWithBothRegister(X14, X5, X1)
    generateArithmeticWithBothRegister(W14, W21, W9)
    generateArithmeticWithBothRegister(W3, W21, W4)
    checkFinal(
      "617d079b", "6121c79a", "6129c79a", "6125c79a",
      "610dc79a", "ae7c019b", "ae20c19a", "ae28c19a",
      "ae24c19a", "ae0cc19a", "ae7e091b", "ae22c91a",
      "ae2ac91a", "ae26c91a", "ae0ec91a", "a37e041b",
      "a322c41a", "a32ac41a", "a326c41a", "a30ec41a")
  }

  test("ShiftsImmediate") {
    // lsl has non trivial imms/immr calculation, we test it harder
    asm.lsl(W3, W20, 0)
    asm.lsl(W3, W20, 17)
    asm.lsl(W3, W20, 31)

    asm.lsl(X8, X10, 49)

    asm.lsr(X5, X25, 32)

    asm.asr(W2, W17, 2)

    checkFinal(
      "837e0053",
      "833a0f53",
      "83020153",

      "48394fd3",

      "25ff60d3",

      "227e0213")
  }

  test("MulHL") {
    asm.smulh(X2, X10, X25)
    asm.umulh(X6, X20, X10)

    asm.smaddl(X3, W10, W20, X15)
    asm.smull(X20, W1, W15)

    asm.umaddl(X13, W0, W10, X25)
    asm.umull(X2, W10, W5)

    checkFinal(
      "427d599b", "867eca9b",
      "433d349b", "347c2f9b",
      "0d64aa9b", "427da59b")
  }

  test("Casts") {
    // these two must be the same:
    asm.uxth(X3, W11)
    asm.uxth(W3, W11)

    asm.uxtb(W3, W11)

    asm.sxtw(X3, W11)

    asm.sxth(X3, W11)
    asm.sxth(W3, W11)

    asm.sxtb(X3, W11)
    asm.sxtb(W3, W11)

    checkFinal(
      "633d0053",
      "633d0053",

      "631d0053",

      "637d4093",

      "633d4093",
      "633d0013",

      "631d4093",
      "631d0013")
  }

  test("Bfm") {
    asm.ubfm(X3, X11, 15, 31)
    asm.sbfm(W3, W11, 15, 7)
    checkFinal(
      "637d4fd3",
      "631d0f13")
  }

  test("CondCompares") {
    def generateCondCompares(dst: IRegister, src1: IRegister, imm: Int, cc: CC): Unit = {
      asm.ccmp(dst, src1, imm, cc)
    }

    generateCondCompares(X2, X21, 0, EQ)
    generateCondCompares(X3, X6, 15, NE)
    generateCondCompares(X8, X4, 3, GT)
    generateCondCompares(X22, X7, 10, LE)
    generateCondCompares(W5, W10, 0, MI)
    generateCondCompares(W15, W1, 5, PL)
    generateCondCompares(W21, W17, 10, HS)
    generateCondCompares(W1, W5, 15, LO)
    checkFinal(
      "400055fa",
      "6f1046fa",
      "03c144fa",
      "cad247fa",
      "a0404a7a",
      "e551417a",
      "aa22517a",
      "2f30457a")
  }

  test("CondSelect") {
    def generateCondSelect(dst: IRegister, src1: IRegister, src2: IRegister, cc: CC): Unit = {
      asm.csinc(dst, src1, src2, cc)
      asm.csinv(dst, src1, src2, cc)
      asm.csneg(dst, src1, src2, cc)

      asm.cinc(dst, src1, cc)
      asm.cinv(dst, src1, cc)
      asm.cneg(dst, src1, cc)

      asm.cset(dst, cc)
      asm.csetm(dst, cc)
      asm.csel(dst, src1, src2, cc)
    }

    generateCondSelect(X2, X21, X7, EQ)
    generateCondSelect(X3, X6, X10, NE)
    generateCondSelect(X8, X4, X3, GT)
    generateCondSelect(X22, X7, X0, LE)
    generateCondSelect(W5, W10, W20, MI)
    generateCondSelect(W15, W1, W0, PL)
    generateCondSelect(W21, W17, W11, HS)
    generateCondSelect(W1, W5, W9, LO)
    checkFinal(
      "a206879a", "a20287da", "a20687da",
      "a216959a", "a21295da", "a21695da",
      "e2179f9a", "e2139fda", "a202879a",

      "c3148a9a", "c3108ada", "c3148ada",
      "c304869a", "c30086da", "c30486da",
      "e3079f9a", "e3039fda", "c3108a9a",

      "88c4839a", "88c083da", "88c483da",
      "88d4849a", "88d084da", "88d484da",
      "e8d79f9a", "e8d39fda", "88c0839a",

      "f6d4809a", "f6d080da", "f6d480da",
      "f6c4879a", "f6c087da", "f6c487da",
      "f6c79f9a", "f6c39fda", "f6d0809a",

      "4545941a", "4541945a", "4545945a",
      "45558a1a", "45518a5a", "45558a5a",
      "e5579f1a", "e5539f5a", "4541941a",

      "2f54801a", "2f50805a", "2f54805a",
      "2f44811a", "2f40815a", "2f44815a",
      "ef479f1a", "ef439f5a", "2f50801a",

      "35268b1a", "35228b5a", "35268b5a",
      "3536911a", "3532915a", "3536915a",
      "f5379f1a", "f5339f5a", "35228b1a",

      "a134891a", "a130895a", "a134895a",
      "a124851a", "a120855a", "a124855a",
      "e1279f1a", "e1239f5a", "a130891a")
  }

  test("ArithmeticWithShiftedOffset") {
    def generateArithmeticWithShiftedOffset(dst: IRegister, imm: Int, shift: Int): Unit = {
      asm.movn(dst, imm, shift * 16)
      asm.movk(dst, imm, shift * 16)
      asm.movz(dst, imm, shift * 16)
    }

    generateArithmeticWithShiftedOffset(W2, 2000, 0)
    generateArithmeticWithShiftedOffset(W6, 4000, 1)
    generateArithmeticWithShiftedOffset(X6, 0, 2)
    generateArithmeticWithShiftedOffset(X15, 65535, 3)
    checkFinal(
      "02fa8012", "02fa8072", "02fa8052", "06f4a112",
      "06f4a172", "06f4a152", "0600c092", "0600c0f2",
      "0600c0d2", "efffff92", "effffff2", "efffffd2")
  }

  test("MovImm") {
    asm.orr(IP0, XZR, 1)
    asm.movz(IP0, 1, 0)
    asm.movk(IP0, 1, 0)
    asm.movn(IP0, 0, 0)
    asm.mov(IP0, 1)
    asm.mov(IP0, -1)
    asm.mov(IP0, 0x8000000380000003L)
    checkFinal(
      "f00340b2", "300080d2", "300080f2",
      "10008092", "300080d2", "10008092", "f00b01b2")
  }

  ///////////////////////////////////////////////////////////////////////////
  // Instructions related to memory

  def generateRegMemoryTest(reg: Register, m: Arg.Mem): Unit = {
    asm.ldr(reg, m)
    asm.str(reg, m)

    reg match {
      case reg: IRegister.W =>
        asm.ldrb(reg, m)
        asm.ldrh(reg, m)
        asm.strb(reg, m)
        asm.strh(reg, m)
      case _ =>
    }

    reg match {
      case reg: IRegister =>
        asm.ldrsb(reg, m)
        asm.ldrsh(reg, m)
      case _ =>
    }

    reg match {
      case x: IRegister.X =>
        asm.ldrsw(x, m)
      case _ =>
    }
  }

  def generateMemoryWithOffsetTest(reg: Register, base: IRegister.X, offset: Int): Unit = {
    generateRegMemoryTest(reg, M(UNSCALED, base, offset))

    if (offset >= 0) {
      generateRegMemoryTest(reg, M(base, offset))
    }

    asm.ldr(reg, M(PRE_IDX, base, offset))
    asm.ldr(reg, M(POST_IDX, base, offset))
    asm.str(reg, M(PRE_IDX, base, offset))
    asm.str(reg, M(POST_IDX, base, offset))
  }

  def generateIndexedMemoryTest(reg: Register, m: Arg.MemRR): Unit = {
    generateRegMemoryTest(reg, m)
  }

  def generatePairMemoryTest(reg1: Register, reg2: Register, base: IRegister.X, offset: Int): Unit = {
    asm.stp(reg1, reg2, M(base, offset))
    asm.stp(reg1, reg2, M(PRE_IDX, base, offset))
    asm.stp(reg1, reg2, M(POST_IDX, base, offset))
    asm.ldp(reg1, reg2, M(base, offset))
    asm.ldp(reg1, reg2, M(PRE_IDX, base, offset))
    asm.ldp(reg1, reg2, M(POST_IDX, base, offset))
  }


  test("MemoryWithOffsetTest") {
    generateMemoryWithOffsetTest(X6, X10, 24)
    generateMemoryWithOffsetTest(W10, X1, -84)
    generateMemoryWithOffsetTest(W5, X3, 160)
    generateMemoryWithOffsetTest(S11, X8, 56)
    generateMemoryWithOffsetTest(D11, X12, -30)
    generateMemoryWithOffsetTest(V7, X5, -20)
    generateMemoryWithOffsetTest(V7, X5, 64)
    checkFinal(
      "468141f8", "468101f8", "46818138", "46818178",
      "468181b8", "460d40f9", "460d00f9", "46618039",
      "46318079", "461980b9",
      "468d41f8", "468541f8", "468d01f8", "468501f8",
      "2ac05ab8", "2ac01ab8", "2ac05a38", "2ac05a78",
      "2ac01a38", "2ac01a78", "2ac0da38", "2ac0da78",
      "2acc5ab8", "2ac45ab8", "2acc1ab8", "2ac41ab8",
      "65004ab8", "65000ab8", "65004a38", "65004a78",
      "65000a38", "65000a78", "6500ca38", "6500ca78",
      "65a040b9", "65a000b9", "65804239", "65404179",
      "65800239", "65400179", "6580c239", "6540c179",
      "650c4ab8", "65044ab8", "650c0ab8", "65040ab8",
      "0b8143bc", "0b8103bc", "0b3940bd", "0b3900bd",
      "0b8d43bc", "0b8543bc", "0b8d03bc", "0b8503bc",
      "8b215efc", "8b211efc", "8b2d5efc", "8b255efc",
      "8b2d1efc", "8b251efc",
      "a7c0de3c", "a7c09e3c", "a7ccde3c", "a7c4de3c",
      "a7cc9e3c", "a7c49e3c",
      "a700c43c", "a700843c", "a710c03d", "a710803d",
      "a70cc43c", "a704c43c", "a70c843c", "a704843c")
  }

  test("IndexedMemoryTest") {
    generateIndexedMemoryTest(X6, M(X11, X13))
    generateIndexedMemoryTest(X11, M(X1, scaled(sxtw(W9))))
    generateIndexedMemoryTest(D7, M(IP0, scaled(X2)))
    generateIndexedMemoryTest(S12, M(X1, X10))
    generateIndexedMemoryTest(W0, M(X2, X3))
    checkFinal(
      "66696df8", "66692df8", "6669ad38", "6669ad78", "6669adb8",
      "2bd869f8", "2bd829f8", "2bd8a938", "2bd8a978", "2bd8a9b8",
      "077a62fc", "077a22fc", "2c686abc", "2c682abc",
      "406863b8", "406823b8", "40686338", "40686378",
      "40682338", "40682378", "4068e338", "4068e378")
  }

  test("PairMemoryTest") {
    generatePairMemoryTest(X6, X2, X12, 24)
    generatePairMemoryTest(W10, W3, X5, -24)
    generatePairMemoryTest(D7, D2, X7, 48)
    generatePairMemoryTest(S2, S8, X11, -48)
    generatePairMemoryTest(V2, V8, X11, 48)
    checkFinal(
      "868901a9", "868981a9", "868981a8", "868941a9",
      "8689c1a9", "8689c1a8", "aa0c3d29", "aa0cbd29",
      "aa0cbd28", "aa0c7d29", "aa0cfd29", "aa0cfd28",
      "e708036d", "e708836d", "e708836c", "e708436d",
      "e708c36d", "e708c36c", "62213a2d", "6221ba2d",
      "6221ba2c", "62217a2d", "6221fa2d", "6221fa2c",
      "62a101ad", "62a181ad", "62a181ac", "62a141ad",
      "62a1c1ad", "62a1c1ac")
  }

  test("PrefetchMemoryTest") {
    asm.prfm(PST | L1 | KEEP, M(X12, 24))
    asm.prfm(PST | L1 | KEEP, M(SP, 256))
    asm.prfm(PLD | L1 | KEEP, M(X12, 24))
    asm.prfm(PLD | L1 | KEEP, M(SP, 256))
    asm.prfm(PST | L1 | KEEP, M(X12, scaled(X13)))
    asm.prfm(PST | L1 | KEEP, M(SP, IP0))
    asm.prfm(PLD | L1 | KEEP, M(X12, scaled(X13)))
    asm.prfm(PLD | L1 | KEEP, M(SP, IP0))
    checkFinal(
      "900d80f9", "f08380f9", "800d80f9", "e08380f9",
      "9079adf8", "f06bb0f8", "8079adf8", "e06bb0f8")
  }

  test("LdrLiteral") {
    asm.ldrLiteral(X0, 0x1234567890ABCDEFL)
    asm.ldrLiteral(W3, 0x13579ACE)
    asm.ldrLiteral(D1, 0xFEDCBA0987654321L)
    asm.ldrLiteral(S0, 0x24680BDF)
    asm.ldrswLiteral(X28, 0x11223344)
    checkFinal(
      // Code
      "c0000058", "e3000018", "0101005c", "2001001c",
      "3c010098",
      // Data
      "00000000", // alignment
      "efcdab90", "78563412",
      "ce9a5713",
      "00000000", // alignment
      "21436587", "09badcfe",
      "df0b6824",
      "44332211")
  }

  ///////////////////////////////////////////////////////////////////////////
  // Tests for FP instructions

  def generateConvertTest(dst: Register, src: Register): Unit = {
    (dst, src) match {
      case (dst: VFPRegister, src: VFPRegister) =>
        if (dst.width == src.width) {
          asm.fmov(dst, src)
        } else {
          asm.fcvt(dst, src)
        }

      case (dst: IRegister, src: VFPRegister) =>
        if (dst.width == src.width) {
          asm.fmov(dst, src)
        }
        asm.fcvtzs(dst, src)

      case (dst: VFPRegister, src: IRegister) =>
        if (dst.width == src.width) {
          asm.fmov(dst, src)
        }
        asm.scvtf(dst, src)
        asm.ucvtf(dst, src)

      case _ =>
    }
  }

  def generateFPTest(dst: VFPRegister, src1: VFPRegister, src2: VFPRegister): Unit = {
    asm.fadd(dst, src1, src2)
    asm.fsub(dst, src1, src2)
    asm.fmul(dst, src1, src2)
    asm.fdiv(dst, src1, src2)
    asm.fcmp(src1, src2)
    asm.frintz(src1, src2)
    asm.fneg(src1, src2)
  }

  test("FPArithmeticTest") {
    generateFPTest(D1, D5, D9)
    generateFPTest(D11, D0, D20)
    generateFPTest(S5, S11, S21)
    generateFPTest(S3, S13, S22)
    checkFinal(
      "a128691e", "a138691e", "a108691e", "a118691e",
      "a020691e", "25c1651e", "2541611e", "0b28741e",
      "0b38741e", "0b08741e", "0b18741e", "0020741e",
      "80c2651e", "8042611e", "6529351e", "6539351e",
      "6509351e", "6519351e", "6021351e", "abc2251e",
      "ab42211e", "a329361e", "a339361e", "a309361e",
      "a319361e", "a021361e", "cdc2251e", "cd42211e")
  }

  test("Converts") {
    generateConvertTest(D1, D5)
    generateConvertTest(S1, D7)
    generateConvertTest(D1, S9)
    generateConvertTest(S3, S5)
    generateConvertTest(X1, D5)
    generateConvertTest(D2, X15)
    generateConvertTest(S1, W5)
    generateConvertTest(D9, X1)
    checkFinal(
      "a140601e", "e140621e", "21c1221e", "a340201e",
      "a100669e", "a100789e", "e201679e", "e201629e", "e201639e", 
      "a100271e", "a100221e", "a100231e", "2900679e", "2900629e", "2900639e")
  }

  test("Half") {
    generateConvertTest(H0, S1)
    generateConvertTest(H2, D3)
    generateConvertTest(S4, H5)
    generateConvertTest(D6, H7)
    checkFinal("20c0231e", "62c0631e", "a440e21e", "e6c0e21e")
  }

  test("FMovImm") {
    asm.fmov(D8, 2.0)
    asm.fmov(S2, -2.0)
    asm.fmov(D2, 1.0)
    asm.fmov(S2, 0.25)
    asm.fmov(D21, 0.125)
    asm.fmov(D21, -0.125)
    checkFinal("0810601e", "0210301e", "02106e1e", "02102a1e", "1510681e", "1510781e")
  }

  ///////////////////////////////////////////////////////////////////////////
  // Tests for not classified cases

  test("NotClassifiedCases") {
    asm.msub(X0, X5, X15, X20)
    asm.msub(W3, W9, W17, W25)

    asm.madd(X0, X5, X15, X20)
    asm.madd(W3, W9, W17, W25)

    asm.fcmp(D5, 0.0)
    asm.fcmp(S15, 0.0)

    asm.ret(X1)

    asm.dmb(SY)
    asm.dmb(ST)

    asm.add(X0, X1, 100)
    asm.sub(X0, X1, 100)
    asm.cmp(X0, 100)

    asm.br(X20)
    asm.blr(X10)

    asm.clz(X7, X20)
    asm.rbit(W20, W7)

    asm.udiv(X7, X2, X20)

    checkFinal(
      "a0d00f9b", "23e5111b", "a0500f9b", "2365111b",
      "a820601e", "e821201e", "20005fd6", "bf3f03d5",
      "bf3e03d5", "20900191", "209001d1", "1f9001f1",
      "80021fd6", "40013fd6", "8712c0da", "f400c05a",
      "4708d49a")
  }

  ///////////////////////////////////////////////////////////////////////////
  // Tests for atomic instruction

  def generateAtomicTest(w: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, mo: MemoryOrdering): Unit = {
    asm.cas(w, rs, rt, rn, mo)
    asm.swp(w, rs, rt, rn, mo)
    asm.ldOP(ADD, w, rs, rt, rn, mo)
    asm.ldOP(BIC, w, rs, rt, rn, mo)
    asm.ldOP(EOR, w, rs, rt, rn, mo)
    asm.ldOP(ORR, w, rs, rt, rn, mo)
    asm.ldOP(SMIN, w, rs, rt, rn, mo)
    asm.ldOP(UMIN, w, rs, rt, rn, mo)
    asm.ldOP(SMAX, w, rs, rt, rn, mo)
    asm.ldOP(UMAX, w, rs, rt, rn, mo)
    if (mo.a == 0) {
      asm.stOP(ADD, w, rs, rn, mo)
      asm.stOP(BIC, w, rs, rn, mo)
      asm.stOP(EOR, w, rs, rn, mo)
      asm.stOP(ORR, w, rs, rn, mo)
    }
  }

  test("Atomic") {
    generateAtomicTest(Width.W32, W1, W2, X3, MemoryOrdering.NONE)
    generateAtomicTest(Width.W8, W1, W2, X3, MemoryOrdering.RELEASE)
    generateAtomicTest(Width.W64, X1, X2, X3, MemoryOrdering.ACQUIRE_RELEASE)
    checkFinal(
      "627ca188", "628021b8",
      "620021b8", "621021b8", "622021b8", "623021b8",
      "625021b8", "627021b8", "624021b8", "626021b8",
      "7f0021b8", "7f1021b8", "7f2021b8", "7f3021b8",

      "62fca108", "62806138",
      "62006138", "62106138", "62206138", "62306138",
      "62506138", "62706138", "62406138", "62606138",
      "7f006138", "7f106138", "7f206138", "7f306138",

      "62fce1c8", "6280e1f8",
      "6200e1f8", "6210e1f8", "6220e1f8", "6230e1f8",
      "6250e1f8", "6270e1f8", "6240e1f8", "6260e1f8")
  }

  def generateMemSpecialTest(w: Width, rs: IRegister.W, rt: IRegister, rn: IRegister.X): Unit = {
    asm.ldar(w, rt, rn)
    asm.ldxr(w, rt, rn)
    asm.ldaxr(w, rt, rn)
    asm.stlr(w, rt, rn)
    asm.stxr(w, rs, rt, rn)
    asm.stlxr(w, rs, rt, rn)
  }

  test("MemSpecial") {
    generateMemSpecialTest(Width.W32, W1, W2, X3)
    generateMemSpecialTest(Width.W8, W1, W2, X3)
    generateMemSpecialTest(Width.W64, W1, X2, X3)
    checkFinal(
      "62fcdf88", "627c5f88", "62fc5f88",
      "62fc9f88", "627c0188", "62fc0188",

      "62fcdf08", "627c5f08", "62fc5f08",
      "62fc9f08", "627c0108", "62fc0108",

      "62fcdfc8", "627c5fc8", "62fc5fc8",
      "62fc9fc8", "627c01c8", "62fc01c8")
  }

  test("AddSubSPvsXZR") {
    asm.add(XZR, XZR, IP0)
    asm.add(XZR, XZR, R(IP0, LSL, 0))

    asm.add(SP, SP, IP0)
    asm.add(SP, SP, R(IP0, LSL, 0))
    asm.add(SP, SP, R(IP0, UXTX))

    asm.add(SP, SP, R(IP0, UXTX, 3))
    asm.add(SP, SP, R(IP0, LSL, 3))

    asm.cmp(SP, X0)
    asm.cmp(SP, R(X0, LSL, 0))

    asm.cmp(SP, R(X10, UXTX, 4))
    asm.cmp(SP, R(X10, LSL, 4))

    checkFinal(
      "ff03108b", "ff03108b",
      "ff63308b", "ff63308b", "ff63308b",
      "ff6f308b", "ff6f308b",
      "ff6320eb", "ff6320eb",
      "ff732aeb", "ff732aeb")
  }

  test("AddSPShiftedError") {
    assertThrows[AsmError] {
      asm.add(SP, SP, R(IP0, LSL, 5))
    }
  }

  test("AddXZRExtended") {
    assertThrows[AsmError] {
      asm.add(XZR, XZR, R(IP0, UXTX, 1))
    }
  }

  test("MovToSIMD") {
    asm.mov(V16, 0, X0)
    asm.mov(V17, 1, X9)
    asm.mov(V16, 0, W0)
    asm.mov(V17, 3, W6)

    asm.mov(W0, V16, 0)
    asm.mov(W7, V18, 2)
    asm.mov(X0, V16, 1)
    asm.mov(X8, V18, 0)

    checkFinal(
      "101c084e", "311d184e", "101c044e", "d11c1c4e",
      "003e040e", "473e140e", "003e184e", "483e084e")
  }

  test("Addv") {
    asm.addv(V16, Width.W8, V16, 8)
    asm.addv(V16, Width.W8, V17, 8)
    asm.addv(V17, Width.W8, V16, 8)

    asm.addv(V16, Width.W8, V16, 16)
    asm.addv(V16, Width.W16, V17, 4)
    asm.addv(V17, Width.W32, V16, 4)

    checkFinal(
      "10ba310e", "30ba310e", "11ba310e",
      "10ba314e", "30ba710e", "11bab14e")
  }

  test("Cnt") {
    asm.cnt(V16, V16, 8)
    asm.cnt(V16, V17, 16)
    asm.cnt(V17, V16, 8)

    checkFinal("105a200e", "305a204e", "115a200e")
  }
}
