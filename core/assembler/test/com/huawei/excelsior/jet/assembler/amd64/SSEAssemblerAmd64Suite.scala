/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.{M, scaled}
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.{DWORD, OWORD, QWORD, WORD}
import com.huawei.excelsior.jet.assembler.amd64.Register16.AX
import com.huawei.excelsior.jet.assembler.amd64.Register32.*
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.assembler.{AsmError, AssemblerToolbox}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for SSE/AVX instructions in [[Assembler]] and [[Bits]].
  *
  * @author cypok
  */
class SSEAssemblerAmd64Suite extends AnyFunSuite with AssemblerToolbox[Assembler] {

  var asm: Assembler = _
  var sse: SSE = _
  var avx: AVX = _

  override def createEmitter() = {
    asm = new Assembler()
    asm.setUp()
    sse = asm.sse
    avx = asm.avx
    asm
  }

  override val resultParseFormat = ResultParseFormat.INTEL

  test("MovSSToXMM0FromMem") {
    sse.movss(XMM0, M(RBX))
    checkFinal(0xF3, 0x0F, 0x10, 0x03)
  }

  test("MovSSToXMM8FromMem") {
    sse.movss(XMM8, M(RBX))
    checkFinal(0xF3, 0x44, 0x0F, 0x10, 0x03)
  }

  test("MovSSFromQWORD") {
    assertThrows[AsmError] {
      sse.movss(XMM0, M(QWORD, RBX))
    }
  }

  test("MovSDToXMM0FromMem") {
    sse.movsd(XMM0, M(RBX))
    checkFinal(0xF2, 0x0F, 0x10, 0x03)
  }

  test("MovSDToXMM8FromMem") {
    sse.movsd(XMM8, M(RBX))
    checkFinal(0xF2, 0x44, 0x0F, 0x10, 0x03)
  }

  test("MovSDFromDWORD") {
    assertThrows[AsmError] {
      sse.movsd(XMM0, M(DWORD, RBX))
    }
  }


  test("MovSSToMemFromXMM0") {
    sse.movss(M(RBX), XMM0)
    checkFinal(0xF3, 0x0F, 0x11, 0x03)
  }

  test("MovSSToMemFromXMM8") {
    sse.movss(M(RBX), XMM8)
    checkFinal(0xF3, 0x44, 0x0F, 0x11, 0x03)
  }

  test("MovSSToQWORD") {
    assertThrows[AsmError] {
      sse.movss(M(QWORD, RBX), XMM0)
    }
  }

  test("MovSDToMemFromXMM0") {
    sse.movsd(M(RBX), XMM0)
    checkFinal(0xF2, 0x0F, 0x11, 0x03)
  }

  test("MovSDToMemFromXMM8") {
    sse.movsd(M(RBX), XMM8)
    checkFinal(0xF2, 0x44, 0x0F, 0x11, 0x03)
  }

  test("MovSDToDWORD") {
    assertThrows[AsmError] {
      sse.movsd(M(DWORD, RBX), XMM0)
    }
  }

  test("MovSSwithRegs") {
    sse.movss(XMM3, XMM2)
    checkFinal(0xF3, 0x0F, 0x10, 0xDA)
  }

  test("MovSDwithRegs") {
    sse.movsd(XMM0, XMM10)
    checkFinal(0xF2, 0x41, 0x0F, 0x10, 0xC2)
  }

  test("MovSDwithRegs2") {
    sse.movsd(XMM15, XMM8)
    checkFinal(0xF2, 0x45, 0x0F, 0x10, 0xF8)
  }

  test("MovDwithRegXmm") {
    sse.movd(R8D, XMM14)
    checkFinal(0x66, 0x45, 0x0F, 0x7E, 0xF0)
  }

  test("MovDwithMemXmm") {
    sse.movd(M(RCX, 7), XMM14)
    checkFinal(0x66, 0x44, 0x0F, 0x7E, 0x71, 0x07)
  }

  test("MovDwithXmmMem") {
    sse.movd(XMM0, M(DWORD, RCX))
    checkFinal(0x66, 0x0F, 0x6E, 0x01)
  }

  test("MovDwithXmmMem_incorrect") {
    assertThrows[AsmError] {
      sse.movd(XMM0, M(QWORD, RCX))
    }
  }

  test("MovQwithXmmReg") {
    sse.movq(XMM0, RAX)
    checkFinal(0x66, 0x48, 0x0F, 0x6E, 0xC0)
  }

  test("MovQwithRegXmm") {
    sse.movq(R8, XMM3)
    checkFinal(0x66, 0x49, 0x0F, 0x7E, 0xD8)
  }

  test("MovQwithXmmMem") {
    sse.movq(XMM0, M(RCX))
    checkFinal(0xF3, 0x0F, 0x7E, 0x01)
  }

  test("MovQwithXmmMem2") {
    sse.movq(XMM10, M(QWORD, R15))
    checkFinal(0xF3, 0x45, 0x0F, 0x7E, 0x17)
  }

  test("MovQwithMemXmm") {
    sse.movq(M(RAX), XMM5)
    checkFinal(0x66, 0x0F, 0xD6, 0x28)
  }

  test("MovQwithXmmMem_incorrect") {
    assertThrows[AsmError] {
      sse.movq(XMM0, M(DWORD, RCX))
    }
  }

  test("MovAPSXmmMem") {
    sse.movaps(XMM0, M(RCX))
    checkFinal(0x0F, 0x28, 0x01)
  }

  test("MovAPSXmmXmm") {
    sse.movaps(XMM8, XMM9)
    checkFinal(0x45, 0x0F, 0x28, 0xC1)
  }

  test("MovAPSMemXmm") {
    sse.movaps(M(OWORD, R8, 42), XMM3)
    checkFinal(0x41, 0x0F, 0x29, 0x58, 0x2A)
  }

  test("MovAPDXmmMem") {
    sse.movapd(XMM10, M(OWORD, R9, 33))
    checkFinal(0x66, 0x45, 0x0F, 0x28, 0x51, 0x21)
  }

  test("MovAPDXmmXmm") {
    sse.movapd(XMM3, XMM4)
    checkFinal(0x66, 0x0F, 0x28, 0xDC)
  }

  test("MovAPDXmmMem_incorrect") {
    assertThrows[AsmError] {
      sse.movapd(XMM3, M(QWORD, RAX))
    }
  }

  test("ComiSSOfXMM0AndMem") {
    sse.comiss(XMM0, M(RBX))
    checkFinal(0x0F, 0x2F, 0x03)
  }

  test("XorPDwithRegs1") {
    sse.xorpd(XMM0, XMM0)
    checkFinal(0x66, 0x0F, 0x57, 0xC0)
  }

  test("XorPDwithRegs2") {
    sse.xorpd(XMM8, XMM7)
    checkFinal(0x66, 0x44, 0x0F, 0x57, 0xC7)
  }

  test("XorPSOfXMM0AndXMM0") {
    sse.xorps(XMM0, XMM0)
    checkFinal(0x0F, 0x57, 0xC0)
  }

  test("PXorOfXMM0AndXMM1") {
    sse.pxor(XMM0, XMM1)
    checkFinal(0x66, 0x0f, 0xef, 0xc1)
  }

  test("CvtTSD2SI_64") {
    sse.cvttsd2si(RAX, M(QWORD, RBX))
    checkFinal(0xF2, 0x48, 0x0F, 0x2C, 0x03)
  }

  test("CvtTSD2SI_32") {
    sse.cvttsd2si(EAX, M(QWORD, RBX))
    checkFinal(0xF2, 0x0F, 0x2C, 0x03)
  }

  test("CvtTSD2SI_64_withNoWidth") {
    sse.cvttsd2si(RAX, M(RBX))
    checkFinal(0xF2, 0x48, 0x0F, 0x2C, 0x03)
  }

  test("CvtTSD2SI_64_withBadMemWidth") {
    assertThrows[AsmError] {
      sse.cvttsd2si(RAX, M(WORD, RBX))
    }
  }

  test("CvtTSD2SI_16") {
    assertThrows[AsmError] {
      sse.cvttsd2si(AX, M(QWORD, RBX))
    }
  }

  test("CvtTSD2SIwithRegXmm_64") {
    sse.cvttsd2si(RAX, XMM9)
    checkFinal(0xF2, 0x49, 0x0F, 0x2C, 0xC1)
  }

  test("CvtTSD2SIwithRegXmm_32") {
    sse.cvttsd2si(R10D, XMM3)
    checkFinal(0xF2, 0x44, 0x0F, 0x2C, 0xD3)
  }

  test("CvtTSS2SIwithRegXmm_32") {
    sse.cvttss2si(EAX, XMM0)
    checkFinal(0xF3, 0x0F, 0x2C, 0xC0)
  }

  test("CvtSI2SS_64") {
    sse.cvtsi2ss(XMM0, M(QWORD, RBX))
    checkFinal(0xF3, 0x48, 0x0F, 0x2A, 0x03)
  }

  test("CvtSI2SS_32") {
    sse.cvtsi2ss(XMM0, M(DWORD, RBX))
    checkFinal(0xF3, 0x0F, 0x2A, 0x03)
  }

  test("CvtSI2SS_NoWidth") {
    assertThrows[AsmError] {
      sse.cvtsi2ss(XMM0, M(RBX))
    }
  }

  test("CvtSI2SS_16") {
    assertThrows[AsmError] {
      sse.cvttsd2si(RAX, M(WORD, RBX))
    }
  }

  test("CvtSI2SSwithXmmReg_64") {
    sse.cvtsi2ss(XMM0, RBX)
    checkFinal(0xF3, 0x48, 0x0F, 0x2A, 0xC3)
  }

  test("CvtSI2SDwithXmmReg_32") {
    sse.cvtsi2sd(XMM10, R11D)
    checkFinal(0xF2, 0x45, 0x0F, 0x2A, 0xD3)
  }

  test("AddSSwithRegs") {
    sse.addss(XMM3, XMM3)
    checkFinal(0xF3, 0x0F, 0x58, 0xDB)
  }

  test("AddSSwithMem") {
    sse.addss(XMM10, M(DWORD, RBX, 42))
    checkFinal(0xF3, 0x44, 0x0F, 0x58, 0x53, 0x2A)
  }

  test("AddSDwithRegs") {
    sse.addsd(XMM11, XMM1)
    checkFinal(0xF2, 0x44, 0x0F, 0x58, 0xD9)
  }

  test("AddSDwithMem") {
    sse.addsd(XMM0, M(QWORD, R12, -16))
    checkFinal(0xF2, 0x41, 0x0F, 0x58, 0x44, 0x24, 0xF0)
  }

  test("VCVTPS2PH") {
    avx.vcvtps2ph(XMM1, XMM2)
    checkFinal(0xC4, 0xE3, 0x79, 0x1D, 0xD1, 0x00)
  }

  test("VCVTPS2PHWithMem") {
    avx.vcvtps2ph(M(QWORD, RAX, 0x12345), XMM1)
    checkFinal(0xC4, 0xE3, 0x79, 0x1D, 0x88, 0x45, 0x23, 0x01, 0x00, 0x00)
  }

  test("VCVTPH2PS") {
    avx.vcvtph2ps(XMM11, XMM12)
    checkFinal(0xC4, 0x42, 0x79, 0x13, 0xDC)
  }

  test("VCVTPH2PSWithMem") {
    avx.vcvtph2ps(XMM0, M(QWORD, RAX, 0x12345))
    checkFinal(0xC4, 0xE2, 0x79, 0x13, 0x80, 0x45, 0x23, 0x01, 0x00)
  }

  test("BMI2") {
    avx.shlx(RAX, RCX, R15)
    avx.shlx(EAX, ECX, R15D)
    avx.shlx(R8D, M(RCX, 8), EDX)

    avx.shrx(RAX, RCX, R15)
    avx.shrx(EBP, EDI, ESI)
    avx.sarx(EBP, EDI, ESI)

    avx.rorx(EBP, EDI, 40)
    avx.rorx(EBP, M(RDI), 40)

    avx.mulx(RAX, RAX, RCX)
    avx.mulx(R8D, R9D, M(RCX, scaled(DWORD, RSI), 24))

    checkFinal(
      0xC4, 0xE2, 0x81, 0xF7, 0xC1,
      0xC4, 0xE2, 0x01, 0xF7, 0xC1,
      0xC4, 0x62, 0x69, 0xF7, 0x41, 0x08,

      0xC4, 0xE2, 0x83, 0xF7, 0xC1,
      0xC4, 0xE2, 0x4B, 0xF7, 0xEF,
      0xC4, 0xE2, 0x4A, 0xF7, 0xEF,

      0xC4, 0xE3, 0x7B, 0xF0, 0xEF, 0x28,
      0xC4, 0xE3, 0x7B, 0xF0, 0x2F, 0x28,

      0xC4, 0xE2, 0xFB, 0xF6, 0xC1,
      0xC4, 0x62, 0x33, 0xF6, 0x44, 0xB1, 0x18)
  }
}
