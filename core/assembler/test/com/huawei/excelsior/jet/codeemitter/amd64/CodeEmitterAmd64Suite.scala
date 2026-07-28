/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter.amd64

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.Width.{W16, W32, W64, WPTR}
import com.huawei.excelsior.jet.assembler.amd64.*
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.M
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.Register16.AX
import com.huawei.excelsior.jet.assembler.amd64.Register32.{EAX, EBX, ECX, EDX}
import com.huawei.excelsior.jet.assembler.amd64.Register8.CL
import com.huawei.excelsior.jet.assembler.amd64.XMM.{XMM0, XMM1, XMM2, XMM3}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.*
import com.huawei.excelsior.jet.codeemitter.BranchOp.*
import com.huawei.excelsior.jet.codeemitter.{CodeEmitterToolbox, ScratchPool}

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

class CodeEmitterAmd64Suite extends CodeEmitterToolbox[CodeEmitterAmd64] {
  // Registers allocation:
  //   1) RAX, RCX, RDX, RBX, RBP, RSI, RDI - used for tests variables allocation
  //   2) R8,  R9,  R10, R11, R12, R13, R14, R15 - used as scratches
  val tmp1 = R8
  val tmp2 = R9
  val tmp1_32 = tmp1.asReg32
  val tmp2_32 = tmp2.asReg32

  var asm: Assembler = _
  var sse: SSE = _
  override def scratchProvider = ScratchPool.empty().appendScratches(Array(tmp1, tmp2))
  override def createEmitter() = {
    asm = new Assembler(Feature.SHORTJUMPS)
    sse = asm.sse
    val emitter = new CodeEmitterAmd64(asm, scratchProvider, new FakeSymbolInfo)
    emitter.setUp()
    emitter.alignStart(8)
    emitter
  }

  val smallImm = 42
  val bigImm = java.lang.Long.MAX_VALUE
  val offset = 64

  override val scratchesNumber = 1


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers without link

  testSame("jump direct symbol") {
    emit.jump(directSymbol)
  } {
    asm.jmp(directSymbol)
  }

  testSame("jump far symbol") {
    emit.jump(farSymbol)
  } {
    emit.lea(tmp1, farSymbol)
    emit.jump(tmp1)
  }

  testSame("jump mem indirect") {
    emit.jumpIndirect(mem(PTR, RAX, offset))
  } {
    asm.jmp(M(WPTR, RAX, offset))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers with link

  testSame("call direct symbol") {
    emit.call(directSymbol)
  } {
    asm.call(directSymbol)
  }

  testSame("call far symbol") {
    emit.call(farSymbol)
  } {
    asm.call(M(asm.literal(farSymbol)))
  }

  testSame("call mem indirect") {
    emit.callIndirect(mem(PTR, RAX, offset))
  } {
    asm.call(M(WPTR, RAX, offset))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Conditional (register with immediate) control transfers

  testBranchSame("branch if reg imm 0") { l => emit.branchIf(EQ, RAX, 0, W32, l) } { l => asm.test(EAX, EAX); asm.jcc(CC.Z, l)  }
  testBranchSame("branch if reg imm 1") { l => emit.branchIf(NE, RBX, 0, W64, l) } { l => asm.test(RBX, RBX); asm.jcc(CC.NZ, l) }
  testBranchSame("branch if reg imm 2") { l => emit.branchIf(LT, RCX, 0, W32, l) } { l => asm.test(ECX, ECX); asm.jcc(CC.L, l)  }
  testBranchSame("branch if reg imm 3") { l => emit.branchIf(GE, RDX, 0, W64, l) } { l => asm.test(RDX, RDX); asm.jcc(CC.GE, l) }

  testBranchSame("branch if reg imm 4") { l => emit.branchIf(TESTZ, RAX, smallImm, W32, l) } { l => asm.test(EAX, smallImm); asm.jcc(CC.Z, l) }
  testBranchSame("branch if reg imm 5") { l => emit.branchIf(TESTNZ, RAX, -1,      W32, l) } { l => asm.test(EAX, -1);       asm.jcc(CC.NZ, l) }

  testBranchSame("branch if reg imm 6") { l => emit.branchIf(UGT, RBX, smallImm, W64, l)   } { l => asm.cmp(RBX, smallImm);  asm.jcc(CC.A, l) }

  testBranchSame("branch if reg imm 7") { l => emit.branchIf(EQ, RCX, bigImm, W64, l) } { l => emit.mov64(tmp1, bigImm); emit.branchIf(EQ, RCX, tmp1, W64, l) }


  ///////////////////////////////////////////////////////////////////////////
  // Conditional (memory with immediate) control transfers

  testBranchSame("branch if mem imm 0") { l =>
    emit.branchIf(mem(I32, RAX, offset), TESTZ, smallImm, l)
  } { l =>
    asm.test(M(W32, RAX, offset), smallImm)
    asm.jcc(CC.E, l)
  }

  testBranchSame("branch if mem imm 1") { l =>
    emit.branchIf(mem(I64, RBX, offset), ULT, bigImm, l)
  } { l =>
    emit.mov64(tmp1, bigImm)
    emit.branchIf(tmp1, UGT, mem(I64, RBX, offset), l)
  }

  testBranchSame("branch if mem imm 2") { l =>
    emit.branchIf(mem(I16, RAX, offset), EQ, smallImm, l)
  } { l =>
    asm.cmp(M(W16, RAX, offset), smallImm)
    asm.jcc(CC.E, l)
  }

  test("branch if mem imm 3") { assertThrows[ArithmeticException] {
    emit.branchIf(mem(I16, RAX, offset), EQ, bigImm, emit.newLabel) }}

  testBranchSame("branch if static direct mem imm") { l =>
    emit.branchIf(mem(I32, directSymbol, offset), EQ, smallImm, l)
  } { l =>
    asm.cmp(M(W32, directSymbol, offset), smallImm)
    asm.jcc(CC.E, l)
  }

  testBranchSame("branch if static far mem imm") { l =>
    emit.branchIf(mem(I32, farSymbol, offset), EQ, smallImm, l)
  } { l =>
    asm.mov(tmp1, Immediate.addr64(farSymbol, offset))
    asm.cmp(M(W32, tmp1), smallImm)
    asm.jcc(CC.E, l)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Conditional (register with memory) control transfers

  testBranchSame("branch if reg mem I64") { l =>
    emit.branchIf(RAX, EQ, mem(I64, RBX, offset), l)
  } { l =>
    asm.cmp(RAX, M(W64, RBX, offset))
    asm.jcc(CC.E, l)
  }

  testBranchSame("branch if reg mem U64") { l =>
    emit.branchIf(RAX, EQ, mem(U64, RBX, offset), l)
  } { l =>
    asm.cmp(RAX, M(W64, RBX, offset))
    asm.jcc(CC.E, l)
  }

  testBranchSame("branch if reg mem I32") { l =>
    emit.branchIf(RAX, EQ, mem(I32, RBX, offset), l)
  } { l =>
    asm.cmp(EAX, M(W32, RBX, offset))
    asm.jcc(CC.E, l)
  }

  testBranchSame("branch if reg mem U32") { l =>
    emit.branchIf(RAX, EQ, mem(U32, RBX, offset), l)
  } { l =>
    asm.cmp(EAX, M(W32, RBX, offset))
    asm.jcc(CC.E, l)
  }

  testBranchThrows("branch if reg mem I16 not supported yet") { l =>
    emit.branchIf(RAX, EQ, mem(I16, RBX, offset), l)
  } { l =>
    asm.cmp(AX, M(W16, RBX, offset))
    asm.jcc(CC.E, l)
  }

  testBranchThrows("branch if reg mem U16 not supported yet") { l =>
    emit.branchIf(RAX, EQ, mem(U16, RBX, offset), l)
  } { l =>
    asm.cmp(AX, M(W16, RBX, offset))
    asm.jcc(CC.E, l)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Move immediate to register

  testSame("fmov 0") {
    emit.fmov32(XMM0, intBitsToFloat(0))
  } {
    sse.xorps(XMM0, XMM0)
  }

  testSame("fmov 1") {
    emit.fmov64(XMM1, longBitsToDouble(0))
  } {
    sse.xorpd(XMM1, XMM1)
  }

  testSame("fmov 2") {
    emit.fmov32(XMM2, 3.14F)
  } {
    asm.mov(tmp1_32, floatToRawIntBits(3.14F))
    sse.movd(XMM2, tmp1_32)
  }

  testSame("fmov 3") {
    emit.fmov64(XMM3, 3.14)
  } {
    asm.mov(tmp1, doubleToRawLongBits(3.14))
    sse.movq(XMM3, tmp1)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Load effective address

  testSame("lea") {
    emit.lea(RAX, mem(I16, RBX, Location.scaled(RCX, W16), 42))
  } {
    asm.lea(RAX, M(WPTR, RBX, AddrMode.scaled(W16, RCX), 42))
  }

  testSame("lea direct symbol") {
    emit.lea(RSI, mem(PTR, directSymbol, offset))
  } {
    asm.lea(RSI, M(WPTR, directSymbol, offset))
  }

  testSame("lea far symbol") {
    emit.lea(RDI, mem(PTR, farSymbol, offset))
  } {
    asm.mov(RDI, Immediate.addr64(farSymbol, offset))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Load/Store (memory <-> register)

  testSame("load 0") {
    emit.load(RAX, mem(I16, RBX, offset))
  } {
    asm.movsx(EAX, M(W16, RBX, offset))
  }

  testSame("load 1") {
    emit.load(RAX, mem(U16, RBX, offset))
  } {
    asm.movzx(EAX, M(W16, RBX, offset))
  }

  testSame("fstore 0") {
    emit.store(mem(F32, RAX, 0), XMM0)
  } {
    sse.movss(M(W32, RAX, 0), XMM0)
  }

  testSame("fstore 1") {
    emit.store(mem(F64, RAX, 0), XMM0)
  } {
    sse.movsd(M(W64, RAX, 0), XMM0)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Store immediate to memory

  testSame("store imm 0") {
    emit.store(mem(U16, RAX, 0), 0xFFFF)
  } {
    asm.mov(M(W16, RAX, 0), 0xFFFFFFFF)
  }

  testSame("store imm 1") {
    emit.store(mem(I32, RAX, 0), -2)
  } {
    asm.mov(M(W32, RAX, 0), -2)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic with registers

  testSame("add reg reg 0") {
    emit.add32(RAX, RAX, RBX)
  } {
    asm.add(EAX, EBX)
  }

  testSame("add reg reg 1") {
    emit.add64(RAX, RBX, RAX)
  } {
    asm.add(RAX, RBX)
  }

  testSame("add reg reg 2") {
    emit.add64(RAX, RBX, RCX)
  } {
    asm.lea(RAX, M(RBX, RCX))
  }

  testSame("add reg reg 3") {
    emit.add32(RAX, RBX, RCX)
  } {
    asm.lea(EAX, M(RBX, RCX))
  }

  testSame("sub reg reg 0") {
    emit.sub(RAX, RBX, RAX, W64)
  } {
    asm.neg(RAX)
    asm.add(RAX, RBX)
  }

  testSame("sub reg reg 1") {
    emit.sub(RAX, RAX, RBX, W64)
  } {
    asm.sub(RAX, RBX)
  }

  testSame("sub reg reg 2") {
    emit.sub(RAX, RBX, RCX, W32)
  } {
    asm.mov(EAX, EBX)
    asm.sub(EAX, ECX)
  }

  testSame("sub reg reg 3") {
    emit.sub(RAX, RAX, RAX, W64)
  } {
    asm.sub(EAX, EAX)
  }

  testSame("mul reg reg 0") {
    emit.mul(RAX, RBX, RAX, W64)
  } {
    asm.imul(RAX, RBX)
  }

  testSame("mul reg reg 1") {
    emit.mul(RAX, RAX, RBX, W32)
  } {
    asm.imul(EAX, EBX)
  }

  testSame("mul reg reg 2") {
    emit.mul(RAX, RBX, RCX, W32)
  } {
    asm.mov(EAX, EBX)
    asm.imul(EAX, ECX)
  }

  testSame("and reg reg 0") {
    emit.and(RAX, RBX, RAX, W64)
  } {
    asm.and(RAX, RBX)
  }

  testSame("and reg reg 1") {
    emit.and(RAX, RAX, RBX, W64)
  } {
    asm.and(RAX, RBX)
  }

  testSame("and reg reg 2") {
    emit.and(RAX, RBX, RCX, W32)
  } {
    asm.mov(EAX, EBX)
    asm.and(EAX, ECX)
  }

  testSame("or reg reg 0") {
    emit.or(RAX, RBX, RAX, W64)
  } {
    asm.or(RAX, RBX)
  }

  testSame("or reg reg 1") {
    emit.or(RAX, RAX, RBX, W64)
  } {
    asm.or(RAX, RBX)
  }

  testSame("or reg reg 2") {
    emit.or(RAX, RBX, RCX, W32)
  } {
    asm.mov(EAX, EBX)
    asm.or(EAX, ECX)
  }

  testSame("xor reg reg 0") {
    emit.xor(RAX, RBX, RAX, W64)
  } {
    asm.xor(RAX, RBX)
  }

  testSame("xor reg reg 1") {
    emit.xor(RAX, RAX, RBX, W64)
  } {
    asm.xor(RAX, RBX)
  }

  testSame("xor reg reg 2") {
    emit.xor(RAX, RBX, RCX, W32)
  } {
    asm.mov(EAX, EBX)
    asm.xor(EAX, ECX)
  }

  testSame("shr reg reg reg") {
    emit.lsr(RAX, RAX, RCX, W64)
  } {
    asm.shr(RAX, CL)
  }

  testSame("sar reg reg reg") {
    emit.asr(RAX, RAX, RCX, W64)
  } {
    asm.sar(RAX, CL)
  }

  testSame("shl reg reg reg") {
    emit.lsl(RAX, RAX, RCX, W64)
  } {
    asm.shl(RAX, CL)
  }

  testSame("shr reg reg 3") {
    emit.lsri(RAX, RBX, 3, W64)
  } {
    asm.mov(RAX, RBX)
    asm.shr(RAX, 3)
  }

  testSame("sar reg reg 3") {
    emit.asri(RAX, RBX, 3, W64)
  } {
    asm.mov(RAX, RBX)
    asm.sar(RAX, 3)
  }

  testSame("shl reg reg 3") {
    emit.lsli(RAX, RBX, 3, W64)
  } {
    asm.mov(RAX, RBX)
    asm.shl(RAX, 3)
  }

  testSame("shl d l r cl (1)") {
    emit.lsl(RAX, RBX, RDX, W64)
  } {
    asm.mov(RAX, RBX)
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RDX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl (d, cl) l r (2)") {
    emit.lsl(RCX, RBX, RDX, W64)
  } {
    asm.mov(tmp1, RBX)
    asm.mov(RCX, RDX)
    asm.shl(RBX, CL)
    asm.mov(RCX, RBX)
    asm.mov(RBX, tmp1)
  }

  testSame("shl d (cl, l) r (3)") {
    emit.lsl(RAX, RCX, RBX, W64)
  } {
    asm.mov(RAX, RCX)
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RBX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl d l (cl, r) (4)") {
    emit.lsl(RAX, RBX, RCX, W64)
  } {
    asm.mov(RAX, RBX)
    asm.shl(RAX, CL)
  }

  testSame("shl (d, l) r cl (5)") {
    emit.lsl(RAX, RAX, RBX, W64)
  } {
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RBX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl d (r, l) cl (6)") {
    emit.lsl(RAX, RBX, RBX, W64)
  } {
    asm.mov(RAX, RBX)
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RBX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl (d, r) l cl (7)") {
    emit.lsl(RAX, RBX, RAX, W64)
  } {
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RAX)
    asm.mov(RAX, RBX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl (d, l, r) cl (8)") {
    emit.lsl(RAX, RAX, RAX, W64)
  } {
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RAX)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl (d, l, cl) r (9)") {
    emit.lsl(RCX, RCX, RAX, W64)
  } {
    asm.mov(tmp1, RAX)
    asm.mov(RAX, RCX)
    asm.mov(RCX, tmp1)
    asm.shl(RAX, CL)
    asm.mov(RCX, RAX)
    asm.mov(RAX, tmp1)
  }

  testSame("shl (d, r, cl) l (10)") {
    emit.lsl(RCX, RAX, RCX, W64)
  } {
    asm.mov(tmp1, RAX)
    asm.shl(RAX, CL)
    asm.mov(RCX, RAX)
    asm.mov(RAX, tmp1)
  }

  testSame("shl d (l, r, cl) (11)") {
    emit.lsl(RAX, RCX, RCX, W64)
  } {
    asm.mov(RAX, RCX)
    asm.shl(RAX, CL)
  }

  testSame("shl (d, cl) (l, r) (12)") {
    emit.lsl(RCX, RAX, RAX, W64)
  } {
    asm.mov(RCX, RAX)
    asm.shl(RCX, CL)
  }

  testSame("shl (d, r) (cl, l) (13)") {
    emit.lsl(RAX, RCX, RAX, W64)
  } {
    asm.mov(tmp1, RCX)
    asm.mov(RCX, RAX)
    asm.mov(RAX, tmp1)
    asm.shl(RAX, CL)
    asm.mov(RCX, tmp1)
  }

  testSame("shl (d, l) (cl, r) (14)") {
    emit.lsl(RAX, RAX, RCX, W64)
  } {
    asm.shl(RAX, CL)
  }

  testSame("shl (d, l, r, cl) (15)") {
    emit.lsl(RCX, RCX, RCX, W64)
  } {
    asm.shl(RCX, CL)
  }

  testSame("div RDX dividend divisor") {
    emit.div(RDX, RBX, RBP, W32)
  } {
    emit.mov(tmp1, RAX)
    emit.mov(RAX, RBX, W32)
    asm.cdq()
    asm.idiv(RBP.asReg32)
    emit.mov(RDX, RAX, W32)
    emit.mov(RAX, tmp1)
  }

  testSame("udiv RDX dividend divisor") {
    emit.udiv(RDX, RBX, RBP, W32)
  } {
    emit.mov(tmp1, RAX)
    emit.mov(RAX, RBX, W32)
    asm.xor(EDX, EDX)
    asm.div(RBP.asReg32)
    emit.mov(RDX, RAX, W32)
    emit.mov(RAX, tmp1)
  }

  testSame("rem RDX dividend divisor") {
    emit.rem(RDX, RBX, RBP, W32)
  } {
    emit.mov(tmp1, RAX)
    emit.mov(RAX, RBX, W32)
    asm.cdq()
    asm.idiv(RBP.asReg32)
    emit.mov(RAX, tmp1)
  }

  testSame("urem RDX dividend divisor") {
    emit.urem(RDX, RBX, RBP, W32)
  } {
    emit.mov(tmp1, RAX)
    emit.mov(RAX, RBX, W32)
    asm.xor(EDX, EDX)
    asm.div(RBP.asReg32)
    emit.mov(RAX, tmp1)
  }

  testSame("div destination dividend RDX") {
    emit.div(RCX, RBX, RDX, W32)
  } {
    emit.mov(RCX, RBX, W32)
    emit.swap(RCX, RAX)
    emit.mov(R8, RDX, W64)
    asm.cdq()
    asm.idiv(R8.as(W32))
    emit.swap(RCX, RAX)
    emit.mov(RDX, R8, W64)
  }

  testSame("div destination dividend NOT_RAX") {
    emit.div(RCX, RBX, RBP, W32)
  } {
    emit.mov(RCX, RBX, W32)
    emit.swap(RCX, RDX)
    emit.mov(tmp1, RAX, W64)
    emit.mov(RAX, RDX, W32)
    asm.cdq()
    asm.idiv(RBP.as(W32))
    emit.swap(RCX, RDX)
    emit.mov(RCX, RAX, W32)
    emit.mov(RAX, tmp1, W64)
  }

  testSame("div destination dividend RAX") {
    emit.div(RCX, RBX, RAX, W32)
  } {
    emit.mov(RCX, RBX, W32)
    emit.swap(RCX, RDX)
    emit.mov(tmp1, RAX)
    emit.mov(RAX, RDX, W32)
    asm.cdq()
    asm.idiv(tmp1.asReg32)
    emit.swap(RCX, RDX)
    emit.mov(RCX, RAX, W32)
    emit.mov(RAX, tmp1)
  }

  testSame("div destination dividend destination") {
    emit.div(RCX, RBX, RCX, W32)
  } {
    emit.withoutScratch(tmp1) {
      emit.mov(tmp1, RCX, W32)
      emit.div(RCX, RBX, tmp1, W32)
    }
  }

  testSame("udiv destination dividend destination") {
    emit.udiv(RCX, RBX, RCX, W32)
  } {
    emit.withoutScratch(tmp1) {
      emit.mov(tmp1, RCX, W32)
      emit.udiv(RCX, RBX, tmp1, W32)
    }
  }

  testSame("rem destination dividend destination") {
    emit.rem(RCX, RBX, RCX, W32)
  } {
    emit.withoutScratch(tmp1) {
      emit.mov(tmp1, RCX, W32)
      emit.rem(RCX, RBX, tmp1, W32)
    }
  }

  testSame("urem destination dividend destination") {
    emit.urem(RCX, RBX, RCX, W32)
  } {
    emit.withoutScratch(tmp1) {
      emit.mov(tmp1, RCX, W32)
      emit.urem(RCX, RBX, tmp1, W32)
    }
  }

  testSame("fadd reg reg 0") {
    emit.fadd(XMM0, XMM0, XMM1, W32)
  } {
    sse.addss(XMM0, XMM1)
  }

  testSame("fadd reg reg 1") {
    emit.fadd(XMM0, XMM1, XMM0, W64)
  } {
    sse.addsd(XMM0, XMM1)
  }

  testSame("fadd reg reg 2") {
    emit.fadd(XMM0, XMM1, XMM2, W32)
  } {
    sse.movaps(XMM0, XMM1)
    sse.addss(XMM0, XMM2)
  }

  testSame("fsub reg reg 0") {
    emit.fsub(XMM0, XMM1, XMM0, W32)
  } {
    emit.mov(tmp1, XMM1, W32)
    sse.subss(XMM1, XMM0)
    emit.fmov32(XMM0, XMM1)
    emit.mov(XMM1, tmp1, W32)
  }

  testSame("fsub reg reg 1") {
    emit.fsub(XMM0, XMM1, XMM0, W64)
  } {
    emit.mov(tmp1, XMM1, W64)
    sse.subsd(XMM1, XMM0)
    emit.fmov64(XMM0, XMM1)
    emit.mov(XMM1, tmp1, W64)
  }

  testSame("fsub reg reg 2") {
    emit.fsub(XMM0, XMM1, XMM2, W32)
  } {
    emit.fmov32(XMM0, XMM1)
    sse.subss(XMM0, XMM2)
  }

  testSame("fsub reg reg 3") {
    emit.fsub(XMM0, XMM0, XMM1, W64)
  } {
    sse.subsd(XMM0, XMM1)
  }

  testSame("fsub reg reg 4") {
    emit.fsub(XMM1, XMM1, XMM1, W32)
  } {
    sse.subss(XMM1, XMM1)
  }

  testSame("fmul reg reg 0") {
    emit.fmul(XMM0, XMM1, XMM0, W32)
  } {
    sse.mulss(XMM0, XMM1)
  }

  testSame("fmul reg reg 1") {
    emit.fmul(XMM0, XMM1, XMM2, W64)
  } {
    emit.fmov(XMM0, XMM1)
    sse.mulsd(XMM0, XMM2)
  }

  testSame("fmul reg reg 2") {
    emit.fmul(XMM0, XMM0, XMM1, W32)
  } {
    sse.mulss(XMM0, XMM1)
  }

  testSame("fdiv reg reg 0") {
    emit.fdiv(XMM1, XMM2, XMM3, W64)
  } {
    emit.fmov(XMM1, XMM2)
    sse.divsd(XMM1, XMM3)
  }

  testSame("fdiv reg reg 1") {
    emit.fdiv(XMM1, XMM2, XMM3, W32)
  } {
    emit.fmov32(XMM1, XMM2)
    sse.divss(XMM1, XMM3)
  }

  testSame("fdiv reg reg 2") {
    emit.fdiv(XMM1, XMM1, XMM2, W64)
  } {
    asm.sse.divsd(XMM1, XMM2)
  }

  testSame("fdiv reg reg 3") {
    emit.fdiv(XMM1, XMM1, XMM2, W32)
  } {
    asm.sse.divss(XMM1, XMM2)
  }

  testSame("fdiv reg reg 4") {
    emit.fdiv(XMM1, XMM2, XMM1, W64)
  } {
    emit.mov(tmp1, XMM2, W64)
    sse.divsd(XMM2, XMM1)
    emit.fmov(XMM1, XMM2)
    emit.mov(XMM2, tmp1, W64)
  }

  testSame("fdiv reg reg 5") {
    emit.fdiv(XMM1, XMM2, XMM1, W32)
  } {
    emit.mov(tmp1, XMM2, W32)
    sse.divss(XMM2, XMM1)
    emit.fmov(XMM1, XMM2, W32)
    emit.mov(XMM2, tmp1, W32)
  }

  testSame("fdiv reg reg 6") {
    emit.fdiv(XMM1, XMM1, XMM1, W64)
  } {
    sse.divsd(XMM1, XMM1)
  }

  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic register with immediate

  testSame("add reg imm 0") {
    emit.add32(RBP, RBP, 0)
  } {}

  testSame("add reg imm 1") {
    emit.add64(RAX, RBP, 0)
  } {
    asm.mov(RAX, RBP)
  }

  testSame("add reg imm 2") {
    emit.add32(RAX, RAX, smallImm)
  } {
    asm.add(EAX, smallImm)
  }

  testSame("add reg imm 3") {
    emit.add32(RAX, RBX, smallImm)
  } {
    asm.lea(EAX, M(RBX, smallImm))
  }

  testSame("add reg imm 4") {
    emit.add64(RCX, RDX, bigImm)
  } {
    emit.mov64(RCX, bigImm)
    asm.add(RCX, RDX)
  }

  testSame("add reg imm 5") {
    emit.add64(RCX, RCX, bigImm)
  } {
    emit.mov64(tmp1, bigImm)
    asm.add(RCX, tmp1)
  }

  testSame("mul reg imm 0") {
    emit.mul(RAX, RBX, smallImm, W32)
  } {
    asm.imul(EAX, EBX, smallImm)
  }

  testSame("mul reg imm 1") {
    emit.mul(RAX, RBX, smallImm, W64)
  } {
    asm.imul(RAX, RBX, smallImm)
  }

  testSame("mul reg imm 2") {
    emit.mul(RAX, RBX, bigImm, W64)
  } {
    emit.mov64(tmp1, bigImm)
    emit.mov64(RAX, RBX)
    asm.imul(RAX, tmp1)
  }

  testSame("and reg imm 0") {
    emit.and(RAX, RBX, smallImm, W32)
  } {
    emit.mov32(RAX, RBX)
    asm.and(EAX, smallImm)
  }

  testSame("and reg imm 1") {
    emit.and(RAX, RBX, smallImm, W64)
  } {
    emit.mov64(RAX, RBX)
    asm.and(RAX, smallImm)
  }

  testSame("and reg imm 2") {
    emit.and(RAX, RBX, bigImm, W64)
  } {
    emit.mov64(tmp1, bigImm)
    emit.mov64(RAX, RBX)
    asm.and(RAX, tmp1)
  }

  testSame("or reg imm 0") {
    emit.or(RAX, RBX, smallImm, W32)
  } {
    emit.mov32(RAX, RBX)
    asm.or(EAX, smallImm)
  }

  testSame("or reg imm 1") {
    emit.or(RAX, RBX, smallImm, W64)
  } {
    emit.mov64(RAX, RBX)
    asm.or(RAX, smallImm)
  }

  testSame("or reg imm 2") {
    emit.or(RAX, RBX, bigImm, W64)
  } {
    emit.mov64(tmp1, bigImm)
    emit.mov64(RAX, RBX)
    asm.or(RAX, tmp1)
  }

  testSame("xor reg imm 0") {
    emit.xor(RAX, RBX, smallImm, W32)
  } {
    emit.mov32(RAX, RBX)
    asm.xor(EAX, smallImm)
  }

  testSame("xor reg imm 1") {
    emit.xor(RAX, RBX, smallImm, W64)
  } {
    emit.mov64(RAX, RBX)
    asm.xor(RAX, smallImm)
  }

  testSame("xor reg imm 2") {
    emit.xor(RAX, RBX, bigImm, W64)
  } {
    emit.mov64(tmp1, bigImm)
    emit.mov64(RAX, RBX)
    asm.xor(RAX, tmp1)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Mem barrier

  testSame("MemBarrierLL") { emit.memBarrier(LOAD_LOAD)   } {                                     }
  testSame("MemBarrierLS") { emit.memBarrier(LOAD_STORE)  } {                                     }
  testSame("MemBarrierSL") { emit.memBarrier(STORE_LOAD)  } { asm.lock(); asm.add(M(W32, RSP), 0) }
  testSame("MemBarrierSS") { emit.memBarrier(STORE_STORE) } {                                     }
  testSame("MemBarrierSM") { emit.memBarrier(STRICT_MEM)  } {                                     }

  testSame("MemBarrierLL_LS") { emit.memBarrier(LOAD_LOAD,   LOAD_STORE)  } {                                     }
  testSame("MemBarrierLS_SL") { emit.memBarrier(LOAD_STORE,  STORE_LOAD)  } { asm.lock(); asm.add(M(W32, RSP), 0) }
  testSame("MemBarrierSL_SS") { emit.memBarrier(STORE_LOAD,  STORE_STORE) } { asm.lock(); asm.add(M(W32, RSP), 0) }
  testSame("MemBarrierSS_SM") { emit.memBarrier(STORE_STORE, STRICT_MEM)  } {                                     }
  testSame("MemBarrierSM_LL") { emit.memBarrier(STRICT_MEM,  LOAD_LOAD)   } {                                     }


  ///////////////////////////////////////////////////////////////////////////
  // Copy memory

  testSame("copyMem 0") { emit.copyMem(mem(RAX), mem(RBX), 0) } {}

  testSame("copyMem 4") {
    emit.copyMem(mem(RAX), mem(RBX), 4)
  } {
    emit.copyAny(mem(RAX).field(U32, 0), mem(RBX).field(U32, 0))
  }

  testSame("copyMem 5") {
    emit.copyMem(mem(RAX), mem(RBX), 5)
  } {
    emit.copyAny(mem(RAX).field(U32, 0), mem(RBX).field(U32, 0))
    emit.copyAny(mem(RAX).field(U8,  4), mem(RBX).field(U8,  4))
  }

  testSame("copyMem 8") {
    emit.copyMem(mem(RAX), mem(RBX), 8)
  } {
    emit.copyAny(mem(RAX).field(PTR, 0), mem(RBX).field(PTR, 0))
  }

  testSame("copyMem 10") {
    emit.copyMem(mem(RAX), mem(RBX), 10)
  } {
    emit.copyAny(mem(RAX).field(PTR, 0), mem(RBX).field(PTR, 0))
    emit.copyAny(mem(RAX).field(U16, 8), mem(RBX).field(U16, 8))
  }

  testSame("copyMem 16") {
    emit.copyMem(mem(RAX), mem(RBX), 16)
  } {
    emit.copyAny(mem(RAX).field(PTR, 0), mem(RBX).field(PTR, 0))
    emit.copyAny(mem(RAX).field(PTR, 8), mem(RBX).field(PTR, 8))
  }

  testSame("copyMem 23") {
    emit.copyMem(mem(RAX), mem(RBX), 23)
  } {
    emit.copyAny(mem(RAX).field(PTR, 0),  mem(RBX).field(PTR, 0))
    emit.copyAny(mem(RAX).field(PTR, 8),  mem(RBX).field(PTR, 8))
    emit.copyAny(mem(RAX).field(U32, 16), mem(RBX).field(U32, 16))
    emit.copyAny(mem(RAX).field(U16, 20), mem(RBX).field(U16, 20))
    emit.copyAny(mem(RAX).field(U8,  22), mem(RBX).field(U8,  22))
  }
}
