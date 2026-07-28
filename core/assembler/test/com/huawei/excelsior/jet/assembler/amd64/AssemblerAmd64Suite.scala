/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.*
import com.huawei.excelsior.jet.assembler.amd64.FPURegister.*
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.*
import com.huawei.excelsior.jet.assembler.amd64.Register16.*
import com.huawei.excelsior.jet.assembler.amd64.Register32.*
import com.huawei.excelsior.jet.assembler.amd64.Register8.*
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import com.huawei.excelsior.jet.assembler.{AsmError, AssemblerToolbox, Segment}
import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[Assembler]] and [[Bits]].
  *
  * @author cypok
  */
class AssemblerAmd64Suite extends AnyFunSuite with AssemblerToolbox[Assembler] {

  var asm: Assembler = _
  var x87: X87 = _

  override def createEmitter() = {
    asm = new Assembler()
    asm.setUp()
    x87 = asm.x87
    asm
  }
  override val resultParseFormat = ResultParseFormat.INTEL

  test("Labels") {
    asm.newBoundLabel
    asm.nop()
    checkFinal(0x90)
  }

  test("UnusedLabels") {
    asm.newLabel
    asm.nop()
    checkFinal(0x90)
  }

  test("UnboundLabels") {
    assertThrows[AsmError] {
      val label1 = asm.newLabel
      asm.jmp(label1)
      checkCrashOnFinish()
    }
  }

  test("MultipleTimesBoundLabels") {
    assertThrows[AsmError] {
      val label1 = asm.newLabel
      asm.bind(label1)
      asm.bind(label1)
      checkCrashOnFinish()
    }
  }

  test("Nop") {
    asm.nop()
    checkFinal(0x90)
  }

  test("MovA_R8") {
    asm.mov(M(RBX), AL)
    checkFinal(0x88, 0x03)
  }

  test("MovA_AH") {
    asm.mov(M(RBX), AH)
    checkFinal(0x88, 0x23)
  }

  test("MovA_SPL") {
    asm.mov(M(RBX), SPL)
    checkFinal(0x40, 0x88, 0x23)
  }

  test("MovA_R16") {
    asm.mov(M(RBX), AX)
    checkFinal(0x66, 0x89, 0x03)
  }

  test("MovA_R32") {
    asm.mov(M(RBX), EAX)
    checkFinal(0x89, 0x03)
  }

  test("MovA_R64") {
    asm.mov(M(RBX), RAX)
    checkFinal(0x48, 0x89, 0x03)
  }

  test("MovR8_A") {
    assertThrows[AsmError] {
      asm.mov(AH, M(R13))
    }
  }

  test("MovR_A") {
    asm.mov(RAX, M(RBX))
    checkFinal(0x48, 0x8B, 0x03)
  }

  test("MovR_R") {
    asm.mov(RAX, RBX)
    checkFinal(0x48, 0x89, 0xd8)
  }

  test("MovR32_R32") {
    asm.mov(R15D, EDX)
    checkFinal(0x41, 0x89, 0xd7)
  }

  test("MovR16_R16") {
    asm.mov(R15W, DX)
    checkFinal(0x66, 0x41, 0x89, 0xd7)
  }

  test("ArithRM_RM") {
    asm.add(M(RCX), EDX)
    asm.add(ECX, EDX)
    asm.add(EDX, M(RCX))

    asm.or(M(RCX), EDX)
    asm.or(ECX, EDX)
    asm.or(EDX, M(RCX))

    asm.adc(M(RCX), EDX)
    asm.adc(ECX, EDX)
    asm.adc(EDX, M(RCX))

    asm.sbb(M(RCX), EDX)
    asm.sbb(ECX, EDX)
    asm.sbb(EDX, M(RCX))

    asm.and(M(RCX), EDX)
    asm.and(ECX, EDX)
    asm.and(EDX, M(RCX))

    asm.sub(M(RCX), EDX)
    asm.sub(ECX, EDX)
    asm.sub(EDX, M(RCX))

    asm.xor(M(RCX), EDX)
    asm.xor(ECX, EDX)
    asm.xor(EDX, M(RCX))

    asm.cmp(M(RCX), EDX)
    asm.cmp(ECX, EDX)
    asm.cmp(EDX, M(RCX))

    asm.test(M(RCX), EDX)
    asm.test(ECX, EDX)
    asm.test(EDX, M(RCX))

    checkFinal(
      0x01, 0x11,
      0x01, 0xd1,
      0x03, 0x11,

      0x09, 0x11,
      0x09, 0xd1,
      0x0b, 0x11,

      0x11, 0x11,
      0x11, 0xd1,
      0x13, 0x11,

      0x19, 0x11,
      0x19, 0xd1,
      0x1b, 0x11,

      0x21, 0x11,
      0x21, 0xd1,
      0x23, 0x11,

      0x29, 0x11,
      0x29, 0xd1,
      0x2b, 0x11,

      0x31, 0x11,
      0x31, 0xd1,
      0x33, 0x11,

      0x39, 0x11,
      0x39, 0xd1,
      0x3b, 0x11,

      0x85, 0x11,
      0x85, 0xd1,
      0x85, 0x11)
  }

  test("ArithRM_Imm") {
    asm.add(M(BYTE, RAX), 5) // op m8, i8
    asm.add(M(WORD, RAX), 5) // op m16, i8
    asm.add(M(DWORD, RAX), 5) // op m32, i8
    asm.add(M(QWORD, RAX), 5) // op m64, i8

    asm.add(M(WORD, RAX), 0x1122) // op m16, i16
    asm.add(M(DWORD, RAX), 0xAABBCCDD) // op m32, i32
    asm.add(M(QWORD, RAX), 0xAABBCCDD) // op m64, i32

    asm.add(ECX, 5) // op r32, i8
    asm.add(ECX, 0xAABBCCDD) // op r32, i32

    asm.add(AL, 5) // op acc8, i8
    asm.add(AX, 0x1122) // op acc16, i16
    asm.add(EAX, 0xAABBCCDD) // op acc32, i32
    asm.add(RAX, 0xAABBCCDD) // op acc64, i32

    asm.add(AX, 15) // op acc16, i8
    asm.add(EAX, 15) // op acc32, i8
    asm.add(RAX, 15) // op acc64, i8

    checkFinal(
      0x80, 0x00, 0x05,
      0x66, 0x83, 0x00, 0x05,
      0x83, 0x00, 0x05,
      0x48, 0x83, 0x00, 0x05,

      0x66, 0x81, 0x00, 0x22, 0x11,
      0x81, 0x00, 0xDD, 0xCC, 0xBB, 0xAA,
      0x48, 0x81, 0x00, 0xDD, 0xCC, 0xBB, 0xAA,

      0x83, 0xc1, 0x05,
      0x81, 0xc1, 0xDD, 0xCC, 0xBB, 0xAA,

      0x04, 0x05,
      0x66, 0x05, 0x22, 0x11,
      0x05, 0xDD, 0xCC, 0xBB, 0xAA,
      0x48, 0x05, 0xDD, 0xCC, 0xBB, 0xAA,

      0x66, 0x83, 0xc0, 0x0f,
      0x83, 0xc0, 0x0f,
      0x48, 0x83, 0xc0, 0x0f)
  }

  test("TestA_I") {
    asm.test(AL, 0x01)        // op acc8, i8
    asm.test(AX, 0x01)        // op acc16, i8
    asm.test(EAX, 0x01)       // op acc32, i8
    asm.test(RAX, 0x01)       // op acc64, i8
    asm.test(RAX, 0xAABBCCDD) // op acc64, i32

    asm.test(R14B, 0x01)      // op r8, i8
    asm.test(R14W, 0x01)      // op r16, i8
    asm.test(R14D, 0x01)      // op r32, i8
    asm.test(R14, 0x01)       // op r64, i8
    asm.test(R14, 0xAABBCCDD) // op r64, i32
    asm.test(M(QWORD, R14), 0xAABBCCDD) // op m64, i32

    checkFinal(
      0xa8, 0x01,
      0x66, 0xa9, 0x01, 0x00,
      0xa9, 0x01, 0x00, 0x00, 0x00,
      0x48, 0xa9, 0x01, 0x00, 0x00, 0x00,
      0x48, 0xa9, 0xDD, 0xCC, 0xBB, 0xAA,

      0x41, 0xf6, 0xc6, 0x01,
      0x66, 0x41, 0xf7, 0xc6, 0x01, 0x00,
      0x41, 0xf7, 0xc6, 0x01, 0x00, 0x00, 0x00,
      0x49, 0xf7, 0xc6, 0x01, 0x00, 0x00, 0x00,
      0x49, 0xf7, 0xc6, 0xDD, 0xCC, 0xBB, 0xAA,
      0x49, 0xf7, 0x06, 0xDD, 0xCC, 0xBB, 0xAA)
  }

  test("NotA") {
    asm.not(M(QWORD, RSP, 4))
    checkFinal(0x48, 0xf7, 0x54, 0x24, 0x04)
  }

  test("NegR") {
    asm.neg(R13B)
    checkFinal(0x41, 0xf6, 0xDD)
  }

  test("IMulR_R") {
    asm.imul(R12W, CX)
    checkFinal(0x66, 0x44, 0x0f, 0xaf, 0xe1)
  }

  test("IMulR8_R8") {
    assertThrows[AsmError] {
      asm.imul(R11B, R12B)
    }
  }

  test("IMulR_A_I") {
    asm.imul(RSI, M(R10, 4), 7)
    asm.imul(ESI, M(R10, 4), 0xaabbccdd)
    checkFinal(
      0x49, 0x6b, 0x72, 0x04, 0x07,
      0x41, 0x69, 0x72, 0x04, 0xdd, 0xcc, 0xbb, 0xaa)
  }

  test("BitInstructions") {
    asm.bsf(EAX, M(RCX, 8))
    asm.bsf(RAX, RCX)
    asm.bsf(AX, CX)
    asm.bsr(R10, R11)

    asm.bt(M(QWORD, R10, 4), 15)
    asm.btr(M(QWORD, R10, 4), 15)
    asm.bts(M(QWORD, R10, 4), 15)
    asm.btc(M(QWORD, R10, 4), 15)

    asm.bt(M(RDX), ESI)
    asm.btr(EDX, ESI)
    asm.bts(EDX, ESI)
    asm.btc(EDX, ESI)

    asm.btc(EAX, 31)

    checkFinal(
      0x0f, 0xbc, 0x41, 0x08,
      0x48, 0x0f, 0xbc, 0xc1,
      0x66, 0x0f, 0xbc, 0xc1,
      0x4d, 0x0f, 0xbd, 0xd3,

      0x49, 0x0f, 0xba, 0x62, 0x04, 0x0f,
      0x49, 0x0f, 0xba, 0x72, 0x04, 0x0f,
      0x49, 0x0f, 0xba, 0x6a, 0x04, 0x0f,
      0x49, 0x0f, 0xba, 0x7a, 0x04, 0x0f,

      0x0f, 0xa3, 0x32,
      0x0f, 0xb3, 0xf2,
      0x0f, 0xab, 0xf2,
      0x0f, 0xbb, 0xf2,

      0x0f, 0xba, 0xf8, 0x1f)
  }

  test("CMov") {
    asm.cmov(CC.B, AX, CX)
    asm.cmov(CC.E, EAX, ECX)
    asm.cmov(CC.A, RAX, RCX)
    asm.cmov(CC.G, RAX, M(RCX))

    checkFinal(
      0x66, 0x0f, 0x42, 0xc1,
      0x0f, 0x44, 0xc1,
      0x48, 0x0f, 0x47, 0xc1,
      0x48, 0x0f, 0x4f, 0x01)
  }


  test("SignExtends") {
    asm.cbw()
    asm.cwde()
    asm.cdqe()
    asm.cwd()
    asm.cdq()
    asm.cqo()

    checkFinal(0x66, 0x98,
      0x98,
      0x48, 0x98,
      0x66, 0x99,
      0x99,
      0x48, 0x99)
  }

  test("MovXx") {
    asm.movsx(R9D, R13B)
    asm.movsx(R9D, R13W)
    asm.movsx(R9, R13B)
    asm.movsx(R9, R13W)
    asm.movsxd(R9, R13D)

    asm.movzx(R9D, R13B)
    asm.movzx(R9D, R13W)
    asm.movzx(R9, R13B)
    asm.movzx(R9, R13W)
    asm.mov(R9D, R13D)

    checkFinal(
      0x45, 0x0f, 0xbe, 0xcd,
      0x45, 0x0f, 0xbf, 0xcd,
      0x4d, 0x0f, 0xbe, 0xcd,
      0x4d, 0x0f, 0xbf, 0xcd,
      0x4d, 0x63, 0xcd,

      0x45, 0x0f, 0xb6, 0xcd,
      0x45, 0x0f, 0xb7, 0xcd,
      0x4d, 0x0f, 0xb6, 0xcd,
      0x4d, 0x0f, 0xb7, 0xcd,
      0x45, 0x89, 0xe9)
  }

  test("MovSx_R32_R8_high_with_rex") {
    assertThrows[AsmError] {
      asm.movsx(R9D, AH)
    }
  }

  test("MovSxD_R16_R32") {
    assertThrows[AsmError] {
      asm.movsxd(R9W, R13D)
    }
  }

  test("MovRM_I") {
    asm.mov(RDX, 7)
    asm.mov(M(QWORD, RDX), 7)
    asm.mov(R9D, 12345)
    asm.mov(R9, 0x123456789ABCDEF0L) // r64 <- i64
    asm.mov(RDX, 0x1122334455667788L) // r64 <- i64
    asm.mov(RDX, 0x11223344) // r64 <- i32
    asm.mov(EDX, 0x11223344) // r32 <- i32
    asm.mov(M(WORD, RDX), 0x1122) // m16 <- i16
    asm.mov(M(QWORD, RDX), 0x11223344) // m64 <- i32

    checkFinal(
      0x48, 0xc7, 0xc2, 0x07, 0x00, 0x00, 0x00,
      0x48, 0xc7, 0x02, 0x07, 0x00, 0x00, 0x00,
      0x41, 0xb9, 0x39, 0x30, 0x00, 0x00,
      0x49, 0xb9, 0xf0, 0xde, 0xbc, 0x9a, 0x78, 0x56, 0x34, 0x12,
      0x48, 0xba, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11,
      0x48, 0xc7, 0xc2, 0x44, 0x33, 0x22, 0x11,
      0xba, 0x44, 0x33, 0x22, 0x11,
      0x66, 0xc7, 0x02, 0x22, 0x11,
      0x48, 0xc7, 0x02, 0x44, 0x33, 0x22, 0x11)
  }

  test("MovM_I64") {
    assertThrows[AsmError] {
      asm.mov(M(RDX), 0x1122334455667788L)
    }
  }

  test("MovR_stringRef32") {
    val s = newSymbol("sym")
    asm.mov(RDX, Immediate.stringRef32(s))
    checkFinal(0x48, 0xc7, 0xc2, relocation(BYTE_STR_32, s), 0, 0, 0, 0)
  }

  test("MovR_addr32") {
    val s = newSymbol("sym")
    asm.mov(RAX, Immediate.addr32(s))
    checkFinal(0x48, 0xc7, 0xc0, relocation(ADDR32, s), 0, 0, 0, 0)
  }

  test("MovR_addr64") {
    val s = newSymbol("sym")
    asm.mov(RDI, Immediate.addr64(s))
    checkFinal(0x48, 0xbf, relocation(ADDR64, s), 0, 0, 0, 0, 0, 0, 0, 0)
  }

  test("ShlR_CL") {
    asm.shl(RDI, CL)
    checkFinal(0x48, 0xd3, 0xe7)
  }

  test("Lea") {
    asm.lea(EAX, M(RBX, scaled(DWORD, RDI), 5))
    asm.lea(RAX, M(RBX, scaled(DWORD, RDI), 5))
    checkFinal(
      0x8d, 0x44, 0xbb, 0x05,
      0x48, 0x8d, 0x44, 0xbb, 0x05)
  }

  test("PushPop") {
    asm.push(RAX)
    asm.push(R15)
    asm.pop(RAX)
    asm.pop(R8)

    asm.push(M(R8))
    asm.push(M(RSP, 32))
    asm.pop(M(RSP, 32))

    asm.push(1)
    asm.push(0x11223344)

    checkFinal(
      0x50,
      0x41, 0x57,
      0x58,
      0x41, 0x58,

      0x41, 0xff, 0x30,
      0xff, 0x74, 0x24, 0x20,
      0x8f, 0x44, 0x24, 0x20,

      0x6a, 0x01,
      0x68, 0x44, 0x33, 0x22, 0x11)
  }

  test("ShldImm") {
    asm.shld(M(RSP, 4), RDI, 8)
    checkFinal(0x48, 0x0f, 0xa4, 0x7c, 0x24, 0x04, 0x08)
  }

  test("ShrdCL") {
    asm.shrd(M(RSP, 4), RDI, CL)
    checkFinal(0x48, 0x0f, 0xad, 0x7c, 0x24, 0x04)
  }

  test("FAdd0_3") {
    x87.fadd(ST0, ST3)
    checkFinal(0xd8, 0xc3)
  }

  test("FAdd3_0") {
    x87.fadd(ST3, ST0)
    checkFinal(0xdc, 0xc3)
  }

  test("FAdd0_0") {
    x87.fadd(ST0, ST0)
    checkFinal(0xd8, 0xc0)
  }

  test("FIAddQ") {
    assertThrows[AsmError] {
      x87.fiadd(M(QWORD, RSP))
    }
  }

  test("FIAddD") {
    x87.fiadd(M(DWORD, RSP))
    checkFinal(0xda, 0x04, 0x24)
  }

  test("FIAddW") {
    x87.fiadd(M(WORD, RSP))
    checkFinal(0xde, 0x04, 0x24)
  }

  test("FLDCW") {
    x87.fldcw(M(RAX))
    checkFinal(0xd9, 0x28)
  }

  test("NotOfTWord") {
    assertThrows[AsmError] {
      asm.not(M(TWORD, RSP))
    }
  }

  test("XCHG") {
    asm.xchg(EAX, EAX)
    asm.xchg(EAX, ECX)
    asm.xchg(ECX, EAX)
    asm.xchg(R13D, EAX)

    asm.xchg(AX, CX)
    asm.xchg(AX, AX)

    asm.xchg(RCX, RAX)
    asm.xchg(RAX, RAX)
    asm.xchg(RBX, RCX)

    asm.xchg(AL, BL)
    asm.xchg(CL, CH)
    asm.xchg(CL, R8B)

    checkFinal(
      0x87, 0xc0,
      0x91,
      0x91,
      0x41, 0x95,

      0x66, 0x91,
      0x66, 0x90,

      0x48, 0x91,
      0x48, 0x90,
      0x48, 0x87, 0xd9,

      0x86, 0xc3,
      0x86, 0xcd,
      0x41, 0x86, 0xc8)
  }

  test("BSWAP") {
    asm.bswap(ECX)
    asm.bswap(RCX)
    asm.bswap(R10D)
    checkFinal(
      0x0f, 0xc9,
      0x48, 0x0f, 0xc9,
      0x41, 0x0f, 0xca)
  }

  test("BSWAP_CX") {
    assertThrows[AsmError] {
      asm.bswap(CX)
    }
  }

  test("BSWAP_CH") {
    assertThrows[AsmError] {
      asm.bswap(CH)
    }
  }

  test("SETcc") {
    asm.set(CC.A, CL)
    asm.set(CC.A, CH)
    asm.set(CC.A, BPL)
    asm.set(CC.A, R15B)
    asm.set(CC.A, M(R15))

    checkFinal(
      0x0f, 0x97, 0xC1,
      0x0f, 0x97, 0xC5,
      0x40, 0x0f, 0x97, 0xC5,
      0x41, 0x0f, 0x97, 0xC7,
      0x41, 0x0f, 0x97, 0x07)
  }

  test("FCMOVNU") {
    x87.fcmov(FPUCC.NU, ST0, ST1)
    checkFinal(0xdb, 0xd9)
  }

  test("FCMOVE") {
    x87.fcmov(FPUCC.E, ST0, ST1)
    checkFinal(0xda, 0xc9)
  }

  test("FCMOVNB") {
    x87.fcmov(FPUCC.NB, ST0, ST1)
    checkFinal(0xdb, 0xc1)
  }

  test("FCMOVBE") {
    x87.fcmov(FPUCC.BE, ST0, ST1)
    checkFinal(0xda, 0xd1)
  }

  test("JMP_A") {
    asm.jmp(M(R8))
    checkFinal(0x41, 0xff, 0x20)
  }

  test("MODRM_REG") {
    asm.add(EBX, EAX)
    checkFinal(0x01, 0xc3)
  }

  test("MODRM_BASE_DISP") {
    asm.add(M(RBX), EAX)
    asm.add(M(RSP), EAX)
    asm.add(M(RBP), EAX)
    asm.add(M(RBX, 4), EAX)
    asm.add(M(RSP, 4), EAX)
    asm.add(M(RBP, 4), EAX)
    asm.add(M(R11), EAX)
    asm.add(M(R12), EAX)
    asm.add(M(R13), EAX)
    asm.add(M(R11, 4), EAX)
    asm.add(M(R12, 4), EAX)
    asm.add(M(R13, 4), EAX)
    checkFinal(
      0x01, 0x03,
      0x01, 0x04, 0x24,
      0x01, 0x45, 0x00,

      0x01, 0x43, 0x04,
      0x01, 0x44, 0x24, 0x04,
      0x01, 0x45, 0x04,

      0x41, 0x01, 0x03,
      0x41, 0x01, 0x04, 0x24,
      0x41, 0x01, 0x45, 0x00,

      0x41, 0x01, 0x43, 0x04,
      0x41, 0x01, 0x44, 0x24, 0x04,
      0x41, 0x01, 0x45, 0x04)
  }

  test("MODRM_BASE_INDEX_DISP") {
    asm.add(M(RBX, scaled(WORD, RAX)), EAX)
    asm.add(M(RSP, scaled(WORD, RAX)), EAX)
    asm.add(M(RBP, scaled(WORD, RAX)), EAX)
    asm.add(M(RBX, scaled(WORD, RAX), 4), EAX)
    asm.add(M(RSP, scaled(WORD, RAX), 4), EAX)
    asm.add(M(RBP, scaled(WORD, RAX), 4), EAX)
    asm.add(M(R11, scaled(WORD, RAX)), EAX)
    asm.add(M(R12, scaled(WORD, RAX)), EAX)
    asm.add(M(R13, scaled(WORD, RAX)), EAX)
    asm.add(M(R11, scaled(WORD, RAX), 4), EAX)
    asm.add(M(R12, scaled(WORD, RAX), 4), EAX)
    asm.add(M(R13, scaled(WORD, RAX), 4), EAX)
    checkFinal(
      0x01, 0x04, 0x43,
      0x01, 0x04, 0x44,
      0x01, 0x44, 0x45, 0x00,

      0x01, 0x44, 0x43, 0x04,
      0x01, 0x44, 0x44, 0x04,
      0x01, 0x44, 0x45, 0x04,

      0x41, 0x01, 0x04, 0x43,
      0x41, 0x01, 0x04, 0x44,
      0x41, 0x01, 0x44, 0x45, 0x00,

      0x41, 0x01, 0x44, 0x43, 0x04,
      0x41, 0x01, 0x44, 0x44, 0x04,
      0x41, 0x01, 0x44, 0x45, 0x04)
  }

  test("MODRM_INDEX_DISP") {
    asm.add(M(scaled(QWORD, RAX)), EAX)
    asm.add(M(scaled(QWORD, RAX), 4), EAX)
    checkFinal(
      0x01, 0x04, 0xc5, 0x00, 0x00, 0x00, 0x00,
      0x01, 0x04, 0xc5, 0x04, 0x00, 0x00, 0x00)
  }

  test("MODRM_FIXUP") {
    val s = newSymbol
    asm.add(M(s), EAX)
    asm.add(M(s, 0x0100), EAX)
    checkFinal(
      0x01, 0x05, relocation(OFFS32, s), 0xfc, 0xff, 0xff, 0xff,
      0x01, 0x05, relocation(OFFS32, s), 0xfc, 0x00, 0x00, 0x00)
  }

  test("MODRM_FIXUP_RESOLVED") {
    val back = asm.newBoundLabel
    asm.nop()
    asm.add(M(back, 0x100), EAX)

    asm.add(M(DWORD, back, 0x100), 0x11)
    asm.add(M(DWORD, back, 0x100), 0x11223344)

    checkFinal(
      0x90,
      0x01, 0x05, 0xf9, 0x00, 0x00, 0x00,
      0x83, 0x05, 0xf2, 0x00, 0x00, 0x00, 0x11,
      0x81, 0x05, 0xe8, 0x00, 0x00, 0x00, 0x44, 0x33, 0x22, 0x11)
  }

  test("RET_Zero") {
    asm.ret(0)
    checkFinal(0xC3)
  }

  test("RET_Short") {
    asm.ret(0x1122)
    checkFinal(0xC2, 0x22, 0x11)
  }

  test("RET_DWORD") {
    assertThrows[AsmError] {
      asm.ret(0x11223344)
    }
  }

  test("MovFS0") {
    asm.mov(RAX, FS(0))
    checkFinal(0x64, 0x48, 0x8b, 0x04, 0x25, 0x00, 0x00, 0x00, 0x00)
  }

  test("MovRelScaled") {
    assertThrows[AsmError] {
      val s = newSymbol
      asm.mov(EAX, M(s, scaled(DWORD, RBX)))
    }
  }

  test("CmpAX") {
    asm.cmp(AL, 4)
    asm.cmp(AL, 42)
    asm.cmp(AX, 8)
    asm.cmp(AX, 42)
    asm.cmp(AX, (1 << 15) - 1)
    asm.cmp(EAX, 64)
    asm.cmp(RAX, 100)
    checkFinal(
      0x3c, 0x04,
      0x3c, 0x2a,
      0x66, 0x83, 0xf8, 0x08,
      0x66, 0x83, 0xf8, 0x2a,
      0x66, 0x3d, 0xff, 0x7f,
      0x83, 0xf8, 0x40,
      0x48, 0x83, 0xf8, 0x64)
  }

  test("CmpRAX_I64") {
    assertThrows[AsmError] {
      asm.cmp(RAX, Immediate.asImm(0xAABBCCDDAABBCCDDL))
    }
  }

  test("Align") {
    asm.nop()
    asm.alignCode(QWORD.nbytes)
    asm.alignStart(8)
    val seg = freezeAndTearDown()
    assertResult(8)(seg.length)
  }

  test("StringOps") {
    asm.movsb()
    asm.movsw()
    asm.movsd()
    asm.movsq()
    asm.stosb()
    asm.stosw()
    asm.stosd()
    asm.stosq()
    checkFinal(
      0xA4,
      0x66, 0xA5,
      0xA5,
      0x48, 0xA5,
      0xAA,
      0x66, 0xAB,
      0xAB,
      0x48, 0xAB)
  }

  test("LockCMPXCHG") {
    asm.lock()
    asm.cmpxchg(M(RCX), R8)
    asm.lock()
    asm.cmpxchg16b(M(RSI))
    asm.lock()
    asm.cmpxchg16b(M(R14))
    checkFinal(
      0xF0, 0x4C, 0x0F, 0xB1, 0x01,
      0xf0, 0x48, 0x0f, 0xc7, 0x0e,
      0xf0, 0x49, 0x0f, 0xc7, 0x0e)
  }

  test("PrefetchNTA") {
    asm.sse.prefetchnta(M(RAX, 42))
    checkFinal(0x0F, 0x18, 0x40, 0x2a)
  }

  test("PrefetchW") {
    asm.prefetchw(M(RAX, 42))
    checkFinal(0x0F, 0x0D, 0x48, 0x2a)
  }

  test("ConstAddrMode") {
    asm.mov(EAX, absolute(37))
    checkFinal(0x8B, 0x04, 0x25, 0x25, 0x00, 0x00, 0x00)
  }

  test("ModRMWithExternalLabel") {
    val s = newSymbol
    val otherSeg = new Segment(s)

    otherSeg.putZeroes(42)
    val l = otherSeg.newBoundLabel

    asm.mov(RAX, M(l, 37))

    val seg = freezeAndTearDown()

    checkIntermediate(seg,
      0x48, 0x8b, 0x05,
      new Relocation(OFFS32, l, 37 - 4),
      0x00, 0x00, 0x00, 0x00)

    checkFinal(seg,
      0x48, 0x8b, 0x05,
      relocation(OFFS32, s),
      42 + (37 - 4), 0x00, 0x00, 0x00)
  }

  test("LockXCHG1") {
    asm.lockxchg(M(RAX, 42), RBX)
    checkFinal(0x48, 0x87, 0x58, 0x2a)
  }

  test("LockXCHG2") {
    asm.lockxchg(M(RBX, 37), RSI)
    checkFinal(0x48, 0x87, 0x73, 0x25)
  }

  test("LockXCHG3") {
    asm.lockxchg(M(DWORD, RAX, scaled(WORD, RBX), 66), EDI)
    checkFinal(0x87, 0x7c, 0x58, 0x42)
  }

  test("XADD1") {
    asm.xadd(M(RBX, 37), RSI)
    checkFinal(0x48, 0x0f, 0xc1, 0x73, 0x25)
  }

  test("XADD2") {
    asm.xadd(M(DWORD, RAX, scaled(WORD, RBX), 66), EDI)
    checkFinal(0x0f, 0xc1, 0x7c, 0x58, 0x42)
  }

  test("POPCNT1") {
    assertThrows[AsmError] {
      asm.popcnt(BL, DL)
    }
  }

  test("POPCNT2") {
    asm.popcnt(EAX, ECX)
    asm.popcnt(EAX, R9D)
    asm.popcnt(RAX, R10)
    asm.popcnt(BX, DX)
    asm.popcnt(AX, R10W)

    checkFinal(
      0xf3, 0x0f, 0xb8, 0xc1,
      0xf3, 0x41, 0x0f, 0xb8, 0xc1,
      0xf3, 0x49, 0x0f, 0xb8, 0xc2,
      0x66, 0xf3, 0x0f, 0xb8, 0xda,
      0x66, 0xf3, 0x41, 0x0f, 0xb8, 0xc2)
  }
}
