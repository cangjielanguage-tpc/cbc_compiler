/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter.arm64

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.{mem, scaled}
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.arm64.Arg.{M, R}
import com.huawei.excelsior.jet.assembler.arm64.DBOption.{LD, ST, SY}
import com.huawei.excelsior.jet.assembler.arm64.ExtendMode.UXTX
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.{W0, W1, W2, WZR}
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode.UNSCALED
import com.huawei.excelsior.jet.assembler.arm64.ShiftMode.LSL
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D.{D0, D1, D2}
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.S.{S0, S1, S2}
import com.huawei.excelsior.jet.assembler.arm64.immediates.ShiftedImm12
import com.huawei.excelsior.jet.assembler.arm64.{Arg, Assembler, CC}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.*
import com.huawei.excelsior.jet.codeemitter.BranchOp.{EQ, NE, TESTNZ, TESTZ}
import com.huawei.excelsior.jet.codeemitter.{CodeEmitterToolbox, ScratchPool}
import xscala.util.MathUtils.bits

class CodeEmitterArm64Suite extends CodeEmitterToolbox[CodeEmitterArm64] {

  val CODE_ALIGNMENT = 16

  private val tmp1 = IP0
  private val tmp2 = IP1
  private val scratches = Array(tmp1, tmp2)

  private var asm: Assembler = _
  override val scratchProvider = ScratchPool.empty().appendScratches(scratches)
  override def createEmitter() = {
    asm = new Assembler
    val emitter = new CodeEmitterArm64(asm, scratchProvider, new FakeSymbolInfo, false)
    emitter.setUp()
    emitter.alignStart(CODE_ALIGNMENT)
    emitter
  }

  private val goodOffset = 64

  override val scratchesNumber = 2


  ///////////////////////////////////////////////////////////////////////////
  // Move immediate to register

  testSame("Mov0") {
    emit.mov32(X0, bini("00000000000000000000000000000011"))
  } {
    asm.movz(W0, bini("0000000000000011"), 0)
  }

  testSame("Mov1") {
    emit.mov32(X0, bini("00011000000000000000000000001100"))
  } {
    asm.movz(W0, bini("0000000000001100"), 0)
    asm.movk(W0, bini("0001100000000000"), 16)
  }

  testSame("Mov2") {
    emit.mov32(X0, bini("11111111111111111111110011100111"))
  } {
    asm.movn(W0, bits(~bini("1111110011100111"), 0, 15), 0)
  }

  testSame("Mov3") {
    emit.mov32(X0, bini("00000000000000111111111110000000"))
  } {
    asm.mov(W0, bini("00000000000000111111111110000000"))
  }

  testSame("Mov4") {
    emit.mov64(X0, binl("0101010101010101010101010101010101010101010101010101010101010000"))
  } {
    asm.movz(X0, 0x5550, 0)
    asm.movk(X0, 0x5555, 16)
    asm.movk(X0, 0x5555, 32)
    asm.movk(X0, 0x5555, 48)
  }

  testSame("Mov5") {
    emit.mov64(X0, binl("0000000000001110101000000000000000000110100000000000000000000000"))
  } {
    asm.movz(X0, 0x680, 16)
    asm.movk(X0, 0xa000, 32)
    asm.movk(X0, 0xe, 48)
  }

  testSame("Mov6") {
    emit.mov64(X0, binl("1111111111111111111111101010110101011010101111111111111111111111"))
  } {
    asm.movn(X0, bits(~bini("0101101010111111"), 0, 15), 16)
    asm.movk(X0, bini("1111111010101101"), 32)
  }

  testSame("Mov7") {
    emit.mov64(X0, 0)
  } {
    asm.movz(X0, 0, 0)
  }

  testSame("Mov8") {
    emit.mov32(X0, 0)
  } {
    asm.movz(W0, 0, 0)
  }

  testSame("Mov9") {
    emit.mov64(X0, -1)
  } {
    asm.movn(X0, 0, 0)
  }

  testSame("Mov10") {
    emit.mov32(X0, -1)
  } {
    asm.movn(W0, 0, 0)
  }

  testSame("Mov11") {
    emit.mov64(X0, binl("0000000000000000000000000001011110111101010111111111111111111111"))
  } {
    asm.movz(X0, 0xffff, 0)
    asm.movk(X0, 0xbd5f, 16)
    asm.movk(X0, 0x17, 32)
  }

  testSame("Mov12") {
    emit.mov32(X0, -42)
  } {
    asm.mov(W0, -42)
  }

  testSame("Mov13") {
    emit.mov64(X0, -42)
  } {
    asm.mov(X0, -42)
  }

  testSame("Mov14") {
    emit.mov64(X0, binl("1010101010101010101010101010101010101010101010101010101010101010"))
  } {
    asm.mov(X0, binl("1010101010101010101010101010101010101010101010101010101010101010"))
  }

  testSame("Mov15") {
    emit.mov64(X0, 0x0000000e00000000L)
  } {
    asm.mov(X0, 0x0000000e00000000L)
  }

  testSame("Mov16") {
    emit.mov64(X0, 0x000f7f7f7f7f7f7fL)
  } {
    asm.movz(X0, 0x7f7f, 0)
    asm.movk(X0, 0x7f7f, 16)
    asm.movk(X0, 0x7f7f, 32)
    asm.movk(X0, 0xf, 48)
  }

  testSame("Mov17") {
    emit.mov32(X0, 0xff000000)
  } {
    asm.movz(W0, 0xff00, 16)
  }

  testSame("Mov18") {
    emit.mov32(X0, bini("01010101010101010101010101010101"))
  } {
    asm.mov(W0, bini("01010101010101010101010101010101"))
  }

  testSame("Mov19") {
    emit.mov64(X0, 0xffffabcdef012345L)
  } {
    asm.movn(X0, 0xdcba, 0)
    asm.movk(X0, 0xef01, 16)
    asm.movk(X0, 0xabcd, 32)
  }

  testSame("Mov20") {
    emit.mov64(X0, 0x8000000000000000L)
  } {
    asm.movz(X0, 0x8000, 48)
  }

  testSame("Mov21") {
    emit.mov64(X0, 0x7fffffffffffffffL)
  } {
    asm.movn(X0, 0x8000, 48)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic register with immediate

  testSame("Add0") {
    emit.add32(X0, X0, 0)
  } {}

  testSame("Add1") {
    emit.add32(X0, X0, bini("11111111111111111111111010101000"))
  } {
    asm.sub(W0, W0, 0x158)
  }

  testSame("Add2") {
    emit.add32(X0, X0, bini("00000000000000000000000001001110"))
  } {
    asm.add(W0, W0, 0x4e)
  }

  testSame("Add3") {
    emit.add32(X1, X0, bini("11111111111010101111111010101000"))
  } {
    asm.sub(W1, W0, 0x158)
    asm.sub(W1, W1, 0x150000)
  }

  testSame("Add4") {
    emit.add32(X1, X0, bini("00000000000010101010110101011100"))
  } {
    asm.add(W1, W0, 0xd5c)
    asm.add(W1, W1, 0xaa000)
  }

  testSame("Add5") {
    emit.add64(X1, X0, binl("1111111111111111111111111111111111111111111111111111111010101000"))
  } {
    asm.sub(X1, X0, 0x158)
  }

  testSame("Add6") {
    emit.add64(X1, X0, binl("0000000000000000000000000000000000000000000000000000000001001110"))
  } {
    asm.add(X1, X0, 0x4e)
  }

  testSame("Add7") {
    emit.add64(X1, X0, binl("1111111111111111111111111111111111111111111010101111111010101000"))
  } {
    asm.sub(X1, X0, 0x158)
    asm.sub(X1, X1, 0x150000)
  }

  testSame("Add8") {
    emit.add64(X1, X0, binl("0000000000000000000000000000000000000000000010101010110101011100"))
  } {
    asm.add(X1, X0, 0xd5c)
    asm.add(X1, X1, 0xaa000)
  }

  testSame("Add15") {
    emit.add64(SP, X0, binl("1111111111111111111111111100000000000000000000000000000000000000"))
  } {
    asm.mov(tmp1, binl("1111111111111111111111111100000000000000000000000000000000000000"))
    asm.add(SP, X0, R(tmp1, UXTX, 0))
  }

  testSame("Add16") {
    emit.add64(SP, X0, binl("1111111111111111111011010100000000000000000000000000000000000000"))
  } {
    asm.movz(tmp1, 0x12c0, 32)
    asm.sub(SP, X0, R(tmp1, UXTX, 0))
  }

  testSame("Add17") {
    emit.add32(X1, X0, 0xf0123456)
  } {
    emit.mov32(X1, 0xf0123456)
    asm.add(W1, W0, W1)
  }

  testSame("Add18") {
    emit.add32(X1, X1, 0xf0123456)
  } {
    emit.mov32(tmp1, 0xf0123456)
    asm.add(W1, W1, tmp1.asW)
  }

  testSame("Mul0") {
    emit.mul(X0, X0, 0xf0123456, W64)
  } {
    emit.mov64(tmp1, 0xf0123456)
    asm.mul(X0, X0, tmp1)
  }

  testSame("Mul1") {
    emit.mul(X0, X1, 0xf0123456, W64)
  } {
    emit.mov64(X0, 0xf0123456)
    asm.mul(X0, X1, X0)
  }

  testSame("Mul2") {
    emit.mul(X0, X1, 0xf0123456, W32)
  } {
    emit.mov32(X0, 0xf0123456)
    asm.mul(W0, W1, W0)
  }

  testSame("And0") {
    emit.and(X0, X0, 0x4444444444444444L, W64)
  } {
    asm.and(X0, X0, 0x4444444444444444L)
  }

  testSame("And1") {
    emit.and(X0, X0, 0x44444444, W32)
  } {
    asm.and(W0, W0, 0x44444444)
  }

  testSame("And2") {
    emit.and(X0, X0, 0xf0123456, W64)
  } {
    emit.mov64(tmp1, 0xf0123456)
    asm.and(X0, X0, tmp1)
  }

  testSame("And3") {
    emit.and(X0, X1, 0xf0123456, W64)
  } {
    emit.mov64(X0, 0xf0123456)
    asm.and(X0, X1, X0)
  }

  testSame("And4") {
    emit.and(X0, X1, 0xf0123456, W32)
  } {
    emit.mov32(X0, 0xf0123456)
    asm.and(W0, W1, W0)
  }

  testSame("Or0") {
    emit.or(X0, X0, 0x4444444444444444L, W64)
  } {
    asm.orr(X0, X0, 0x4444444444444444L)
  }

  testSame("Or1") {
    emit.or(X0, X0, 0x44444444, W32)
  } {
    asm.orr(W0, W0, 0x44444444)
  }

  testSame("Or2") {
    emit.or(X0, X0, 0xf0123456, W64)
  } {
    emit.mov64(tmp1, 0xf0123456)
    asm.orr(X0, X0, tmp1)
  }

  testSame("Or3") {
    emit.or(X0, X1, 0xf0123456, W64)
  } {
    emit.mov64(X0, 0xf0123456)
    asm.orr(X0, X1, X0)
  }

  testSame("Or4") {
    emit.or(X0, X1, 0xf0123456, W32)
  } {
    emit.mov32(X0, 0xf0123456)
    asm.orr(W0, W1, W0)
  }

  testSame("Xor0") {
    emit.xor(X0, X0, 0x4444444444444444L, W64)
  } {
    asm.eor(X0, X0, 0x4444444444444444L)
  }

  testSame("Xor1") {
    emit.xor(X0, X0, 0x44444444, W32)
  } {
    asm.eor(W0, W0, 0x44444444)
  }

  testSame("Xor2") {
    emit.xor(X0, X0, 0xf0123456, W64)
  } {
    emit.mov64(tmp1, 0xf0123456)
    asm.eor(X0, X0, tmp1)
  }

  testSame("Xor3") {
    emit.xor(X0, X1, 0xf0123456, W64)
  } {
    emit.mov64(X0, 0xf0123456)
    asm.eor(X0, X1, X0)
  }

  testSame("Xor4") {
    emit.xor(X0, X1, 0xf0123456, W32)
  } {
    emit.mov32(X0, 0xf0123456)
    asm.eor(W0, W1, W0)
  }

  testSame("shr reg reg reg") {
    emit.lsr(X0, X1, X2, W64)
  } {
    asm.lsr(X0, X1, X2)
  }

  testSame("sar reg reg reg") {
    emit.asr(X0, X1, X2, W64)
  } {
    asm.asr(X0, X1, X2)
  }

  testSame("shl reg reg reg") {
    emit.lsl(X0, X1, X2, W64)
  } {
    asm.lsl(X0, X1, X2)
  }

  testSame("shr reg reg imm") {
    emit.lsri(X0, X1, 63 + 64, W64)
  } {
    asm.lsr(X0, X1, 63)
  }

  testSame("sar reg reg imm") {
    emit.asri(X0, X1, 63 + 64, W64)
  } {
    asm.asr(X0, X1, 63)
  }

  testSame("shl reg reg imm") {
    emit.lsli(X0, X1, 63 + 64, W64)
  } {
    asm.lsl(X0, X1, 63)
  }

  testSame("div reg reg reg") {
    emit.div(X0, X1, X2, W64)
  } {
    asm.sdiv(X0, X1, X2)
  }

  testSame("udiv reg reg reg") {
    emit.udiv(X0, X1, X2, W64)
  } {
    asm.udiv(X0, X1, X2)
  }

  testSame("rem reg0 reg1 reg2") {
    emit.rem(X0, X1, X2, W64)
  } {
    asm.sdiv(X0, X1, X2)
    asm.msub(X0, X0, X2, X1)
  }

  testSame("rem reg0 reg0 reg1") {
    emit.rem(X0, X0, X1, W64)
  } {
    asm.sdiv(tmp1, X0, X1)
    asm.msub(X0, tmp1, X1, X0)
  }

  testSame("rem reg0 reg1 reg0") {
    emit.rem(X0, X1, X0, W64)
  } {
    asm.sdiv(tmp1, X1, X0)
    asm.msub(X0, tmp1, X0, X1)
  }

  testSame("urem reg0 reg1 reg2") {
    emit.urem(X0, X1, X2, W64)
  } {
    asm.udiv(X0, X1, X2)
    asm.msub(X0, X0, X2, X1)
  }

  testSame("urem reg0 reg0 reg1") {
    emit.urem(X0, X0, X1, W64)
  } {
    asm.udiv(tmp1, X0, X1)
    asm.msub(X0, tmp1, X1, X0)
  }

  testSame("urem reg0 reg1 reg0") {
    emit.urem(X0, X1, X0, W64)
  } {
    asm.udiv(tmp1, X1, X0)
    asm.msub(X0, tmp1, X0, X1)
  }

  ///////////////////////////////////////////////////////////////////////////
  // Load/Store (memory <-> register)

  testSame("LoadBasedI8")  { emit.load(X0, mem(I8,  X1, goodOffset)) } { asm.ldrsb (W0, M(X1, goodOffset)) }
  testSame("LoadBasedU8")  { emit.load(X0, mem(U8,  X1, goodOffset)) } { asm.ldrb  (W0, M(X1, goodOffset)) }
  testSame("LoadBasedI16") { emit.load(X0, mem(I16, X1, goodOffset)) } { asm.ldrsh (W0, M(X1, goodOffset)) }
  testSame("LoadBasedU16") { emit.load(X0, mem(U16, X1, goodOffset)) } { asm.ldrh  (W0, M(X1, goodOffset)) }
  testSame("LoadBasedI32") { emit.load(X0, mem(I32, X1, goodOffset)) } { asm.ldr   (W0, M(X1, goodOffset)) }
  testSame("LoadBasedI64") { emit.load(X0, mem(I64, X1, goodOffset)) } { asm.ldr   (X0, M(X1, goodOffset)) }
  testSame("LoadBasedF32") { emit.load(D0, mem(F32, X1, goodOffset)) } { asm.ldr   (S0, M(X1, goodOffset)) }
  testSame("LoadBasedF64") { emit.load(D0, mem(F64, X1, goodOffset)) } { asm.ldr   (D0, M(X1, goodOffset)) }

  testSame("StoreBasedI8")  { emit.store(mem(I8,  X1, goodOffset), X0) } { asm.strb (W0, M(X1, goodOffset)) }
  testSame("StoreBasedI16") { emit.store(mem(I16, X1, goodOffset), X0) } { asm.strh (W0, M(X1, goodOffset)) }
  testSame("StoreBasedI32") { emit.store(mem(I32, X1, goodOffset), X0) } { asm.str  (W0, M(X1, goodOffset)) }
  testSame("StoreBasedI64") { emit.store(mem(I64, X1, goodOffset), X0) } { asm.str  (X0, M(X1, goodOffset)) }
  testSame("StoreBasedF32") { emit.store(mem(F32, X1, goodOffset), D0) } { asm.str  (S0, M(X1, goodOffset)) }
  testSame("StoreBasedF64") { emit.store(mem(F64, X1, goodOffset), D0) } { asm.str  (D0, M(X1, goodOffset)) }

  testSame("LoadIndexBaseI8")  { emit.load(X0, mem(I8,  X1, scaled(X2, W8))) } { asm.ldrsb (W0, M(X1, X2)) }
  testSame("LoadIndexBaseU8")  { emit.load(X0, mem(U8,  X1, scaled(X2, W8))) } { asm.ldrb  (W0, M(X1, X2)) }
  testSame("LoadIndexBaseI16") { emit.load(X0, mem(I16, X1, scaled(X2, W8))) } { asm.ldrsh (W0, M(X1, X2)) }
  testSame("LoadIndexBaseU16") { emit.load(X0, mem(U16, X1, scaled(X2, W8))) } { asm.ldrh  (W0, M(X1, X2)) }
  testSame("LoadIndexBaseI32") { emit.load(X0, mem(I32, X1, scaled(X2, W8))) } { asm.ldr   (W0, M(X1, X2)) }
  testSame("LoadIndexBaseI64") { emit.load(X0, mem(I64, X1, scaled(X2, W8))) } { asm.ldr   (X0, M(X1, X2)) }
  testSame("LoadIndexBaseF32") { emit.load(D0, mem(F32, X1, scaled(X2, W8))) } { asm.ldr   (S0, M(X1, X2)) }
  testSame("LoadIndexBaseF64") { emit.load(D0, mem(F64, X1, scaled(X2, W8))) } { asm.ldr   (D0, M(X1, X2)) }

  testSame("StoreIndexBaseI8")  { emit.store(mem(I8,  X1, scaled(X2, W8)), X0) } { asm.strb (W0, M(X1, X2)) }
  testSame("StoreIndexBaseI16") { emit.store(mem(I16, X1, scaled(X2, W8)), X0) } { asm.strh (W0, M(X1, X2)) }
  testSame("StoreIndexBaseI32") { emit.store(mem(I32, X1, scaled(X2, W8)), X0) } { asm.str  (W0, M(X1, X2)) }
  testSame("StoreIndexBaseI64") { emit.store(mem(I64, X1, scaled(X2, W8)), X0) } { asm.str  (X0, M(X1, X2)) }
  testSame("StoreIndexBaseF32") { emit.store(mem(F32, X1, scaled(X2, W8)), D0) } { asm.str  (S0, M(X1, X2)) }
  testSame("StoreIndexBaseF64") { emit.store(mem(F64, X1, scaled(X2, W8)), D0) } { asm.str  (D0, M(X1, X2)) }

  testSame("LoadIndexBaseWithScaleI8")  { emit.load(X0, mem(I8,  X1, scaled(X2, W8)))  } { asm.ldrsb (W0, M(X1, X2)) }
  testSame("LoadIndexBaseWithScaleU8")  { emit.load(X0, mem(U8,  X1, scaled(X2, W8)))  } { asm.ldrb  (W0, M(X1, X2)) }
  testSame("LoadIndexBaseWithScaleI16") { emit.load(X0, mem(I16, X1, scaled(X2, W16))) } { asm.ldrsh (W0, M(X1, Arg.scaled(X2))) }
  testSame("LoadIndexBaseWithScaleU16") { emit.load(X0, mem(U16, X1, scaled(X2, W16))) } { asm.ldrh  (W0, M(X1, Arg.scaled(X2))) }
  testSame("LoadIndexBaseWithScaleI32") { emit.load(X0, mem(I32, X1, scaled(X2, W32))) } { asm.ldr   (W0, M(X1, Arg.scaled(X2))) }
  testSame("LoadIndexBaseWithScaleI64") { emit.load(X0, mem(I64, X1, scaled(X2, W64))) } { asm.ldr   (X0, M(X1, Arg.scaled(X2))) }
  testSame("LoadIndexBaseWithScaleF32") { emit.load(D0, mem(F32, X1, scaled(X2, W32))) } { asm.ldr   (S0, M(X1, Arg.scaled(X2))) }
  testSame("LoadIndexBaseWithScaleF64") { emit.load(D0, mem(F64, X1, scaled(X2, W64))) } { asm.ldr   (D0, M(X1, Arg.scaled(X2))) }

  testSame("StoreIndexBaseWithScaleI8")  { emit.store(mem(I8,  X1, scaled(X2, W8)),  X0) } { asm.strb (W0, M(X1, X2)) }
  testSame("StoreIndexBaseWithScaleI16") { emit.store(mem(I16, X1, scaled(X2, W16)), X0) } { asm.strh (W0, M(X1, Arg.scaled(X2))) }
  testSame("StoreIndexBaseWithScaleI32") { emit.store(mem(I32, X1, scaled(X2, W32)), X0) } { asm.str  (W0, M(X1, Arg.scaled(X2))) }
  testSame("StoreIndexBaseWithScaleI64") { emit.store(mem(I64, X1, scaled(X2, W64)), X0) } { asm.str  (X0, M(X1, Arg.scaled(X2))) }
  testSame("StoreIndexBaseWithScaleF32") { emit.store(mem(F32, X1, scaled(X2, W32)), D0) } { asm.str  (S0, M(X1, Arg.scaled(X2))) }
  testSame("StoreIndexBaseWithScaleF64") { emit.store(mem(F64, X1, scaled(X2, W64)), D0) } { asm.str  (D0, M(X1, Arg.scaled(X2))) }

  testSame("MemoryBased0") {
    emit.store(mem(I32, X0, 10), X1)
  } {
    asm.str(W1, M(UNSCALED, X0, 10))
  }

  testSame("MemoryBased1") {
    emit.store(mem(I64, X0, 10), X1)
  } {
    asm.str(X1, M(UNSCALED, X0, 10))
  }

  testSame("MemoryBased2") {
    emit.store(mem(I32, X0, 100000), X1)
  } {
    emit.mov64(tmp1, 25000)
    asm.str(W1, M(X0, Arg.scaled(tmp1)))
  }

  testSame("MemoryBased3") {
    emit.load(X1, mem(I32, X0, 10))
  } {
    asm.ldr(W1, M(X0, 10))
  }

  testSame("MemoryBased4") {
    emit.load(X1, mem(I32, X0, 100000))
  } {
    emit.mov64(X1, 25000)
    asm.ldr(W1, M(X0, Arg.scaled(X1)))
  }

  testSame("MemoryBased5") {
    emit.load(X1, mem(I64, X0, bini("11101011000000000000")))
  } {
    emit.add(X1, X0, bini("11101011000000000000"), WPTR)
    asm.ldr(X1, M(X1, 0))
  }

  testSame("MemoryBased6") {
    emit.store(mem(I64, X0, bini("11101011000000000000")), X1)
  } {
    emit.add(tmp1, X0, bini("11101011000000000000"), WPTR)
    asm.str(X1, M(tmp1, 0))
  }

  testSame("MemoryBased7") {
    emit.store(mem(I64, SP, 100), X0)
  } {
    asm.str(X0, M(UNSCALED, SP, 100))
  }

  testSame("MemoryBased8") {
    emit.load(X0, mem(I64, SP, 100))
  } {
    asm.ldr(X0, M(UNSCALED, SP, 100))
  }

  testSame("MemoryBased9") {
    emit.store(mem(I64, SP, 100), 200)
  } {
    asm.mov(tmp1, 200)
    asm.str(tmp1, M(UNSCALED, SP, 100))
  }

  testSame("MemoryBased10") {
    emit.store(mem(I32, SP, 100), 200)
  } {
    asm.mov(tmp1.asW, 200)
    asm.str(tmp1.asW, M(SP, 100))
  }

  testSame("MemoryBased11") {
    emit.load(SP, mem(PTR, X0, 100))
  } {
    asm.ldr(tmp1, M(UNSCALED, X0, 100))
    asm.mov(SP, tmp1)
  }

  testSame("MemoryBased12") {
    emit.store(mem(PTR, X0, 100), SP)
  } {
    asm.mov(tmp1, SP)
    asm.str(tmp1, M(UNSCALED, X0, 100))
  }

  testSame("MemoryBased13") {
    emit.store(mem(PTR, X0, 100000), SP)
  } {
    asm.mov(tmp1, SP)
    emit.mov64(tmp2, 12500)
    asm.str(tmp1, M(X0, Arg.scaled(tmp2)))
  }

  testSame("MemoryBased14") {
    emit.store(mem(PTR, X0, 100001), SP)
  } {
    asm.mov(tmp1, SP)
    emit.mov64(tmp2, 100001)
    asm.str(tmp1, M(X0, tmp2))
  }

  testSame("MemoryBased15") {
    emit.load(SP, mem(PTR, X0, 100000))
  } {
    emit.mov64(tmp1, 12500)
    asm.ldr(tmp1, M(X0, Arg.scaled(tmp1)))
    asm.mov(SP, tmp1)
  }

  testSame("MemoryBased16") {
    emit.load(SP, mem(PTR, X0, 100001))
  } {
    emit.mov64(tmp1, 100001)
    asm.ldr(tmp1, M(X0, tmp1))
    asm.mov(SP, tmp1)
  }

  testSame("MemoryStatic1") {
    emit.load(X0, mem(I64, symbol, 1000000))
  } {
    asm.adr(X0, symbol)
    emit.mov64(tmp1, 1000000)
    asm.ldr(X0, M(X0, tmp1))
  }

  testSame("MemoryStatic2") {
    emit.store(mem(I64, symbol, 1000000), X0)
  } {
    asm.adr(tmp1, symbol)
    emit.mov64(tmp2, 1000000)
    asm.str(X0, M(tmp1, tmp2))
  }

  testSame("MemoryBaseIndex1") {
    emit.store(mem(I64, X0, scaled(X1, W16), 100), X2)
  } {
    asm.add(tmp1, X0, R(X1, LSL, 1))
    asm.str(X2, M(UNSCALED, tmp1, 100))
  }

  testSame("MemoryBaseIndex2") {
    emit.load(X2, mem(I64, X0, scaled(X1, W16), 100))
  } {
    asm.add(X2, X0, R(X1, LSL, 1))
    asm.ldr(X2, M(UNSCALED, X2, 100))
  }

  testSame("MemoryBaseIndex3") {
    emit.load(D0, mem(F64, X0, scaled(X1, W16), 100))
  } {
    asm.add(tmp1, X0, R(X1, LSL, 1))
    asm.ldr(D0, M(UNSCALED, tmp1, 100))
  }

  testSame("MemoryBaseIndex4") {
    emit.load(X2, mem(I64, X0, scaled(X1, W64), 80))
  } {
    asm.add(X2, X1, 10)
    asm.ldr(X2, M(X0, Arg.scaled(X2)))
  }

  testSame("MemoryBaseIndex5") {
    emit.store(mem(I64, X0, scaled(X1, W64), 80), X2)
  } {
    asm.add(tmp1, X1, 10)
    asm.str(X2, M(X0, Arg.scaled(tmp1)))
  }

  // TODO may replace tmp1 -> X0
  testSame("MemoryBaseIndex6") {
    emit.load(X0, mem(I64, X0, scaled(X1, W16)))
  } {
    asm.add(tmp1, X0, R(X1, LSL, 1))
    asm.ldr(X0, M(tmp1, 0))
  }

  testSame("MemoryBaseIndex7") {
    emit.store(mem(I64, X0, scaled(X1, W16)), X0)
  } {
    asm.add(tmp1, X0, R(X1, LSL, 1))
    asm.str(X0, M(tmp1, 0))
  }

  testSame("MemoryBaseIndex8") {
    emit.load(X0, mem(I64, X0, scaled(X1, W32)))
  } {
    asm.add(tmp1, X0, R(X1, LSL, 2))
    asm.ldr(X0, M(tmp1, 0))
  }

  testSame("MemoryBaseIndex9") {
    emit.store(mem(I64, X0, scaled(X1, W32)), X0)
  } {
    asm.add(tmp1, X0, R(X1, LSL, 2))
    asm.str(X0, M(tmp1, 0))
  }

  testSame("MemoryBaseIndex10") {
    emit.load(X0, mem(I64, X0, scaled(X1, W32), 20))
  } {
    asm.add(tmp1, X0, R(X1, LSL, 2))
    asm.ldr(X0, M(UNSCALED, tmp1, 20))
  }

  testSame("MemoryBaseIndex11") {
    emit.store(mem(I64, X0, scaled(X1, W32), 20), X0)
  } {
    asm.add(tmp1, X0, R(X1, LSL, 2))
    asm.str(X0, M(UNSCALED, tmp1, 20))
  }

  testSame("MemoryBaseIndex12") {
    emit.load(X0, mem(I64, X0, scaled(X1, W64), 20))
  } {
    asm.add(tmp1, X0, 20)
    asm.ldr(X0, M(tmp1, Arg.scaled(X1)))
  }

  testSame("MemoryBaseIndex13") {
    emit.store(mem(I64, X0, scaled(X1, W64), 20), X0)
  } {
    asm.add(tmp1, X0, 20)
    asm.str(X0, M(tmp1, Arg.scaled(X1)))
  }

  testSame("MemoryBaseIndex14") {
    emit.load(X0, mem(I64, X0, scaled(X1, W64), 24))
  } {
    emit.add64(tmp1, X1, 3)
    asm.ldr(X0, M(X0, Arg.scaled(tmp1)))
  }

  testSame("MemoryBaseIndex15") {
    emit.store(mem(I64, X0, scaled(X1, W64), 24), X0)
  } {
    emit.add64(tmp1, X1, 3)
    asm.str(X0, M(X0, Arg.scaled(tmp1)))
  }

  testSame("MemoryBaseIndex16") {
    emit.load(X0, mem(I64, X0, scaled(X1, W64)))
  } {
    asm.ldr(X0, M(X0, Arg.scaled(X1)))
  }

  testSame("MemoryBaseIndex17") {
    emit.store(mem(I64, X0, scaled(X1, W64)), X0)
  } {
    asm.str(X0, M(X0, Arg.scaled(X1)))
  }

  testSame("MemoryBaseIndex18") {
    emit.load(X0, mem(I64, X0, scaled(X1, W64), 20001))
  } {
    emit.add64(tmp1, X0, 20001)
    asm.ldr(X0, M(tmp1, Arg.scaled(X1)))
  }

  testSame("MemoryBaseIndex19") {
    emit.store(mem(I64, X0, scaled(X1, W64), 20001), X0)
  } {
    emit.add64(tmp1, X0, 20001)
    asm.str(X0, M(tmp1, Arg.scaled(X1)))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Store immediate to memory

  testSame("StoreImmediate0_64bit") {
    emit.store(mem(I64, X0, 0), 0)
  } {
    emit.store(mem(I64, X0, 0), XZR)
  }

  testSame("StoreImmediate0_32bit") {
    emit.store(mem(I32, X0, 0), 0)
  } {
    asm.str(W32, WZR, M(X0, 0))
  }

  testSame("StoreImmediate1") {
    emit.store(mem(I64, X0, 0), 1)
  } {
    emit.mov64(tmp1, 1)
    emit.store(mem(I64, X0, 0), tmp1)
  }

  testSame("StoreImmediate_UIntMax") {
    emit.store(mem(U32, X0, 0), 0xFFFFFFFFL)
  } {
    emit.mov32(tmp1, 0xFFFFFFFF)
    emit.store(mem(U32, X0, 0), tmp1)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Move immediate to FP register

  testSame("FloatMov0") {
    emit.fmov32(D0, 1.0f)
  } {
    asm.fmov(S0, 1.0f)
  }

  testSame("FloatMov1") {
    emit.fmov64(D0, 1.0)
  } {
    asm.fmov(D0, 1.0)
  }

  testSame("FloatMov2") {
    emit.fmov32(D0, 42.0f)
  } {
    asm.ldrLiteral(S0, java.lang.Float.floatToRawIntBits(42.0f))
  }

  testSame("FloatMov3") {
    emit.fmov64(D0, 42.0)
  } {
    asm.ldrLiteral(D0, java.lang.Double.doubleToRawLongBits(42.0))
  }

  testSame("FloatMov4") {
    emit.fmov64(D0, +0.0d)
  } {
    asm.fmov(D0, XZR)
  }

  testSame("FloatMov5") {
    emit.fmov64(D0, -0.0d)
  } {
    asm.ldrLiteral(D0, java.lang.Double.doubleToRawLongBits(-0.0d))
  }

  testSame("FloatMov6") {
    emit.fmov32(D0, +0.0f)
  } {
    asm.fmov(S0, WZR)
  }

  testSame("FloatMov7") {
    emit.fmov32(D0, -0.0f)
  } {
    asm.ldrLiteral(S0, java.lang.Float.floatToRawIntBits(-0.0f))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Conditional control transfers

  testBranchSame("BranchIf0") { l =>
    emit.branchIf(EQ, X0, X1, W64, l)
  } { l =>
    asm.cmp(X0, X1)
    asm.b(CC.EQ, l)
  }

  testBranchSame("BranchIf1") { l =>
    emit.branchIf(TESTZ, X0, X1, W64, l)
  } { l =>
    asm.tst(X0, X1)
    asm.b(CC.EQ, l)
  }
  
  testBranchSame("BranchIf2") { l =>
    emit.branchIf(TESTZ, X0, -1L, W64, l)
  } { l =>
    asm.mov(tmp1, -1L)
    asm.tst(X0, tmp1)
    asm.b(CC.EQ, l)
  }

  testBranchSame("BranchIf3") { l =>
    emit.branchIf(EQ, X0, 0, W64, l)
  } { l =>
    asm.cbz(X0, l)
  }

  testBranchSame("BranchIf4") { l =>
    emit.branchIf(NE, X0, 0, W64, l)
  } { l =>
    asm.cbnz(X0, l)
  }

  testBranchSame("BranchIf5") { l =>
    emit.branchIf(EQ, X0, 100, W64, l)
  } { l =>
    asm.cmp(X0, 100)
    asm.b(CC.EQ, l)
  }

  testBranchSame("BranchIf6") { l =>
    emit.branchIf(TESTNZ, X0, 7, W64, l)
  } { l =>
    asm.tst(X0, 7)
    asm.b(CC.NE, l)
  }

  testBranchSame("BranchIf7") { l =>
    emit.branchIf(EQ, X0, 100000, W64, l)
  } { l =>
    emit.mov64(tmp1, 100000)
    asm.cmp(X0, tmp1)
    asm.b(CC.EQ, l)
  }

  testBranchSame("BranchIf8") { l =>
    emit.branchIf(TESTZ, X0, bini("1000"), W64, l)
  } { l =>
    asm.tbz(X0, 3, l)
  }

  testBranchSame("BranchIf9") { l =>
    emit.branchIf(TESTZ, X0, bini("101"), W64, l)
  } { l =>
    asm.mov(tmp1, bini("101"))
    asm.tst(X0, tmp1)
    asm.b(CC.EQ, l)
  }

  testBranchSame("BranchIf10") { l =>
    emit.branchIf(TESTZ, X0, bini("1100"), W64, l)
  } { l =>
    asm.tst(X0, bini("1100"))
    asm.b(CC.EQ, l)
  }

  testBranchSame("Jump") { l =>
    emit.jump(l)
  } { l =>
    asm.b(l)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic with registers

  testSame("AddWithRegs1") {
    emit.add64(X0, X1, X2)
  } {
    asm.add(X0, X1, X2)
  }

  testSame("AddWithRegs2") {
    emit.add32(X0, X1, X2)
  } {
    asm.add(W0, W1, W2)
  }

  testSame("SubWithRegs1") {
    emit.sub(X0, X1, X2, W64)
  } {
    asm.sub(X0, X1, X2)
  }

  testSame("SubWithRegs2") {
    emit.sub(X0, X1, X2, W32)
  } {
    asm.sub(W0, W1, W2)
  }

  testSame("MulWithRegs1") {
    emit.mul(X0, X1, X2, W64)
  } {
    asm.mul(X0, X1, X2)
  }

  testSame("MulWithRegs2") {
    emit.mul(X0, X1, X2, W32)
  } {
    asm.mul(W0, W1, W2)
  }

  testSame("AndWithRegs1") {
    emit.and(X0, X1, X2, W64)
  } {
    asm.and(X0, X1, X2)
  }

  testSame("AndWithRegs2") {
    emit.and(X0, X1, X2, W32)
  } {
    asm.and(W0, W1, W2)
  }

  testSame("OrWithRegs1") {
    emit.or(X0, X1, X2, W64)
  } {
    asm.orr(X0, X1, X2)
  }

  testSame("OrWithRegs2") {
    emit.or(X0, X1, X2, W32)
  } {
    asm.orr(W0, W1, W2)
  }

  testSame("XorWithRegs1") {
    emit.xor(X0, X1, X2, W64)
  } {
    asm.eor(X0, X1, X2)
  }

  testSame("XorWithRegs2") {
    emit.xor(X0, X1, X2, W32)
  } {
    asm.eor(W0, W1, W2)
  }

  testSame("FAddWithRegs1") {
    emit.fadd(D0, D1, D2, W64)
  } {
    asm.fadd(D0, D1, D2)
  }

  testSame("FAddWithRegs2") {
    emit.fadd(D0, D1, D2, W32)
  } {
    asm.fadd(S0, S1, S2)
  }

  testSame("FSubWithRegs1") {
    emit.fsub(D0, D1, D2, W64)
  } {
    asm.fsub(D0, D1, D2)
  }

  testSame("FSubWithRegs2") {
    emit.fsub(D0, D1, D2, W32)
  } {
    asm.fsub(S0, S1, S2)
  }

  testSame("FMulWithRegs1") {
    emit.fmul(D0, D1, D2, W64)
  } {
    asm.fmul(D0, D1, D2)
  }

  testSame("FMulWithRegs2") {
    emit.fmul(D0, D1, D2, W32)
  } {
    asm.fmul(S0, S1, S2)
  }

  testSame("FDivWithRegs1") {
    emit.fdiv(D0, D1, D2, W64)
  } {
    asm.fdiv(D0, D1, D2)
  }

  testSame("FDivWithRegs2") {
    emit.fdiv(D0, D1, D2, W32)
  } {
    asm.fdiv(S0, S1, S2)
  }

  testSame("FMov") {
    emit.fmov32(D0, D1)
  } {
    asm.fmov(S0, S1)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Lea

  testSame("Lea0") {
    emit.lea(X0, mem(NONE, X1, scaled(X2, W32), 24))
  } {
    asm.add(X0, X2, 6)
    asm.add(X0, X1, R(X0, LSL, 2))
  }

  testSame("Lea1") {
    emit.lea(X0, mem(NONE, X1, scaled(X2, W8), 24))
  } {
    asm.add(X0, X2, 24)
    asm.add(X0, X1, R(X0, LSL, 0))
  }

  testSame("Lea2") {
    emit.lea(X0, mem(NONE, X1, scaled(X2, W64), 24))
  } {
    asm.add(X0, X2, 3)
    asm.add(X0, X1, R(X0, LSL, 3))
  }

  testSame("Lea3") {
    emit.lea(X0, mem(NONE, X1, 100))
  } {
    asm.add(X0, X1, 100)
  }

  testSame("Lea4") {
    emit.lea(X0, mem(NONE, directSymbol, 100))
  } {
    asm.adr(X0, directSymbol)
    asm.add(X0, X0, 100)
  }

  testSame("Lea5") {
    emit.lea(X0, mem(NONE, X0, scaled(X1, W8)))
  } {
    asm.add(X0, X0, X1)
  }

  testSame("Lea6") {
    emit.lea(X0, mem(NONE, X0, scaled(X1, W16)))
  } {
    asm.add(X0, X0, R(X1, LSL, 1))
  }

  testSame("Lea7") {
    emit.lea(X0, mem(NONE, X0, scaled(X1, W16), 20))
  } {
    asm.add(tmp1, X1, 10)
    asm.add(X0, X0, R(tmp1, LSL, 1))
  }

  testSame("Lea8") {
    emit.lea(X0, mem(NONE, X2, scaled(X0, W16), 5))
  } {
    asm.add(tmp1, X2, 5)
    asm.add(X0, tmp1, R(X0, LSL, 1))
  }


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers without link

  testSame("JumpDirectSymbol") {
    emit.jump(directSymbol)
  } {
    asm.b(directSymbol)
  }

  testSame("JumpFarSymbol") {
    emit.jump(farSymbol)
  } {
    emit.lea(tmp1, farSymbol)
    emit.jump(tmp1)
  }

  testSame("JumpMemIndirect") {
    emit.jumpIndirect(mem(PTR, X0, 20))
  } {
    emit.load(tmp1, mem(PTR, X0, 20))
    emit.jump(tmp1)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers with link

  testSame("CallDirectSymbol") {
    emit.call(directSymbol)
  } {
    asm.bl(directSymbol)
  }

  testSame("CallFarSymbol") {
    emit.call(farSymbol)
  } {
    emit.lea(LR, farSymbol)
    emit.call(LR)
  }

  testSame("CallMemIndirect") {
    emit.callIndirect(mem(PTR, X0, 20))
  } {
    emit.load(LR, mem(PTR, X0, 20))
    emit.call(LR)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Mem barrier

  testSame("MemBarrierLL") { emit.memBarrier(LOAD_LOAD)   } { asm.dmb(LD) }
  testSame("MemBarrierLS") { emit.memBarrier(LOAD_STORE)  } { asm.dmb(LD) }
  testSame("MemBarrierSL") { emit.memBarrier(STORE_LOAD)  } { asm.dmb(SY) }
  testSame("MemBarrierSS") { emit.memBarrier(STORE_STORE) } { asm.dmb(ST) }
  testSame("MemBarrierSM") { emit.memBarrier(STRICT_MEM)  } {             }

  testSame("MemBarrierLL_LS") { emit.memBarrier(LOAD_LOAD,   LOAD_STORE)  } { asm.dmb(LD) }
  testSame("MemBarrierLS_SL") { emit.memBarrier(LOAD_STORE,  STORE_LOAD)  } { asm.dmb(SY) }
  testSame("MemBarrierSL_SS") { emit.memBarrier(STORE_LOAD,  STORE_STORE) } { asm.dmb(SY) }
  testSame("MemBarrierSS_SM") { emit.memBarrier(STORE_STORE, STRICT_MEM)  } { asm.dmb(ST) }
  testSame("MemBarrierSM_LL") { emit.memBarrier(STRICT_MEM,  LOAD_LOAD)   } { asm.dmb(LD) }


  ///////////////////////////////////////////////////////////////////////////
  // Copy memory

  testSame("copyMem 0") {
    emit.copyMem(mem(X0), mem(X1), 0)
  } {}

  testSame("copyMem 4") {
    emit.copyMem(mem(X0), mem(X1), 4)
  } {
    emit.copyAny(mem(X0).field(U32, 0), mem(X1).field(U32, 0))
  }

  testSame("copyMem 5") {
    emit.copyMem(mem(X0), mem(X1), 5)
  } {
    emit.copyAny(mem(X0).field(U32, 0), mem(X1).field(U32, 0))
    emit.copyAny(mem(X0).field(U8,  4), mem(X1).field(U8,  4))
  }

  testSame("copyMem 8") {
    emit.copyMem(mem(X0), mem(X1), 8)
  } {
    emit.copyAny(mem(X0).field(PTR, 0), mem(X1).field(PTR, 0))
  }

  testSame("copyMem 10") {
    emit.copyMem(mem(X0), mem(X1), 10)
  } {
    emit.copyAny(mem(X0).field(PTR, 0), mem(X1).field(PTR, 0))
    emit.copyAny(mem(X0).field(U16, 8), mem(X1).field(U16, 8))
  }

  testSame("copyMem 16") {
    emit.copyMem(mem(X0), mem(X1), 16)
  } {
    emit.copyAny(mem(X0).field(PTR, 0), mem(X1).field(PTR, 0))
    emit.copyAny(mem(X0).field(PTR, 8), mem(X1).field(PTR, 8))
  }

  testSame("copyMem 23") {
    emit.copyMem(mem(X0), mem(X1), 23)
  } {
    emit.copyAny(mem(X0).field(PTR, 0),  mem(X1).field(PTR, 0))
    emit.copyAny(mem(X0).field(PTR, 8),  mem(X1).field(PTR, 8))
    emit.copyAny(mem(X0).field(U32, 16), mem(X1).field(U32, 16))
    emit.copyAny(mem(X0).field(U16, 20), mem(X1).field(U16, 20))
    emit.copyAny(mem(X0).field(U8,  22), mem(X1).field(U8,  22))
  }
}
