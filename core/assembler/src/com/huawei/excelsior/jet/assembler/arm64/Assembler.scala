/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.AsmError.{error, require}
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.assembler.arm64.Bits.{getZR, noSP}
import com.huawei.excelsior.jet.assembler.arm64.Enums.AddSubOp.*
import com.huawei.excelsior.jet.assembler.arm64.Enums.FP2Op.*
import com.huawei.excelsior.jet.assembler.arm64.Enums.LogicalOp.*
import com.huawei.excelsior.jet.assembler.arm64.Enums.MemOp.*
import com.huawei.excelsior.jet.assembler.arm64.Enums.MemOpX
import com.huawei.excelsior.jet.assembler.arm64.Enums.SelectOp.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.WZR
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.immediates.{BitMaskImm, ShiftedImm16}
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import xscala.util.MathUtils
import xscala.util.MathUtils.*

/** Assembler for ARM64
  *
  * @author orangebyte256
  * @author paul
  */
object Assembler {
  def isValidImmForUnscaledLdrOrStr(offset: Int): Boolean =
    isNBitsSigned(offset, 9)

  def isValidImmForScaledLdrOrStr(offset: Int, width: Width): Boolean = {
    (offset >= 0) && {
      // for instructions working with register wider than 8-bit last bits should be zero
      val log2size = (if (width == WPTR) W64 else width).log2bytes
      isAlignedToNBits(offset, log2size) && isNBits(offset, log2size + 12)
    }
  }

  def isValidImmForLdrOrStr(offset: Int, width: Width): Boolean =
    isValidImmForUnscaledLdrOrStr(offset) || isValidImmForScaledLdrOrStr(offset, width)
}

class Assembler extends AsmEmitter.WithLiterals {
  override def alignCode(alignment: Int): Unit =
    addFixup(new Fixups.CodeAlignment(alignment))

  override protected def symbolLiteralKind = RelocationKind.ADDR64

  ////////////////////////////////////////////////////////////////////////////////
  // Instructions related to arithmetic and conversions

  // - integer addition, subtraction, comparison

  def add(rd: IRegister, rn: IRegister, imm: Int): Unit =
    emit(Bits.addSub(ADD, rd, rn, imm))

  def add(rd: IRegister, rn: IRegister, rm: Arg.RArith): Unit =
    emit(Bits.addSub(ADD, rd, rn, rm))

  def sub(rd: IRegister, rn: IRegister, imm: Int): Unit =
    emit(Bits.addSub(SUB, rd, rn, imm))

  def sub(rd: IRegister, rn: IRegister, rm: Arg.RArith): Unit =
    emit(Bits.addSub(SUB, rd, rn, rm))

  def neg(rd: IRegister, rm: Arg.RArith): Unit =
    sub(rd, getZR(rd), rm)

  def negs(rd: IRegister, rm: Arg.RArith): Unit =
    subs(rd, getZR(rd), rm)

  def adds(rd: IRegister, rn: IRegister, rm: Arg.RArith): Unit =
    emit(Bits.addSub(ADDS, rd, rn, rm))

  def adds(rd: IRegister, rn: IRegister, imm: Int): Unit =
    emit(Bits.addSub(ADDS, rd, rn, imm))

  def subs(rd: IRegister, rn: IRegister, imm: Int): Unit =
    emit(Bits.addSub(SUBS, rd, rn, imm))

  def subs(rd: IRegister, rn: IRegister, rm: Arg.RArith): Unit =
    emit(Bits.addSub(SUBS, rd, rn, rm))

  def cmp(rn: IRegister, imm: Int): Unit =
    subs(getZR(rn), rn, imm)

  def cmp(rn: IRegister, rm: Arg.RArith): Unit =
    subs(getZR(rn), rn, rm)

  // - logical operations: and/or/xor/test, etc.

  def and(rd: IRegister, rn: IRegister, imm: Long): Unit =
    emit(Bits.logical(AND, rd, rn, imm))

  def and(rd: IRegister, rn: IRegister, rm: Arg.RLogical): Unit =
    emit(Bits.logical(AND, N = false, rd, rn, rm))

  def ands(rd: IRegister, rn: IRegister, imm: Long): Unit =
    emit(Bits.logical(ANDS, rd, rn, imm))

  def ands(rd: IRegister, rn: IRegister, rm: Arg.RLogical): Unit =
    emit(Bits.logical(ANDS, N = false, rd, rn, rm))

  def tst(rn: IRegister, imm: Long): Unit =
    ands(getZR(rn), rn, imm)

  def tst(rn: IRegister, rm: Arg.RLogical): Unit =
    ands(getZR(rn), rn, rm)

  def eor(rd: IRegister, rn: IRegister, imm: Long): Unit =
    emit(Bits.logical(EOR, rd, rn, imm))

  def eor(rd: IRegister, rn: IRegister, rm: Arg.RLogical): Unit =
    emit(Bits.logical(EOR, N = false, rd, rn, rm))

  def orr(rd: IRegister, rn: IRegister, imm: Long): Unit =
    emit(Bits.logical(ORR, rd, rn, imm))

  def orr(rd: IRegister, rn: IRegister, rm: Arg.RLogical): Unit =
    emit(Bits.logical(ORR, N = false, rd, rn, rm))

  def orn(rd: IRegister, rn: IRegister, rm: Arg.RLogical): Unit =
    emit(Bits.logical(ORR, N = true, rd, rn, rm))

  // - shifts

  def lsl(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    emit(Bits.lslv(rd, rn, rm))

  def lsr(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    emit(Bits.lsrv(rd, rn, rm))

  def asr(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    emit(Bits.asrv(rd, rn, rm))

  def lsl(rd: IRegister, rn: IRegister, shift: Int): Unit = {
    val dim = rd.width.nbits
    assert(0 <= shift && shift < dim)
    ubfm(rd, rn, (dim - shift) % dim, dim - 1 - shift)
  }

  def lsr(rd: IRegister, rn: IRegister, shift: Int): Unit = {
    val dim = rd.width.nbits
    assert(0 <= shift && shift < dim)
    ubfm(rd, rn, shift, dim - 1)
  }

  def asr(rd: IRegister, rn: IRegister, shift: Int): Unit = {
    val dim = rd.width.nbits
    assert(0 <= shift && shift < dim)
    sbfm(rd, rn, shift, dim - 1)
  }

  // - integer division & multiplication

  def udiv(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    emit(Bits.udiv(rd, rn, rm))

  def sdiv(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    emit(Bits.sdiv(rd, rn, rm))

  def mul(rd: IRegister, rn: IRegister, rm: IRegister): Unit =
    madd(rd, rn, rm, getZR(rd))

  def madd(rd: IRegister, rn: IRegister, rm: IRegister, ra: IRegister): Unit =
    emit(Bits.madd(rd, rn, rm, ra))

  def msub(rd: IRegister, rn: IRegister, rm: IRegister, ra: IRegister): Unit =
    emit(Bits.msub(rd, rn, rm, ra))

  def smaddl(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W, ra: IRegister.X): Unit =
    emit(Bits.smaddl(rd, rn, rm, ra))

  def smull(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W): Unit =
    smaddl(rd, rn, rm, XZR)

  def umaddl(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W, ra: IRegister.X): Unit =
    emit(Bits.umaddl(rd, rn, rm, ra))

  def umull(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W): Unit =
    umaddl(rd, rn, rm, XZR)

  def smulh(rd: IRegister.X, rn: IRegister.X, rm: IRegister.X): Unit =
    emit(Bits.smulh(rd, rn, rm))

  def umulh(rd: IRegister.X, rn: IRegister.X, rm: IRegister.X): Unit =
    emit(Bits.umulh(rd, rn, rm))

  // - bitfield move, extract & insert

  def ubfm(rd: IRegister, rn: IRegister, immr: Int, imms: Int): Unit =
    emit(Bits.ubfm(rd, rn, immr, imms))

  def uxtb(rd: IRegister, rn: IRegister.W): Unit =
    ubfm(rd.asW, rn, 0, 7)    // Note that ubfm(Wd, Wn) == ubfm(Xd, Wn)

  def uxth(rd: IRegister, rn: IRegister.W): Unit =
    ubfm(rd.asW, rn, 0, 15)   // Note that ubfm(Wd, Wn) == ubfm(Xd, Wn)

  def sbfm(rd: IRegister, rn: IRegister, immr: Int, imms: Int): Unit =
    emit(Bits.sbfm(rd, rn, immr, imms))

  def sxtb(rd: IRegister, rn: IRegister.W): Unit =
    sbfm(rd, rn.as(rd.width), 0, 7)

  def sxth(rd: IRegister, rn: IRegister.W): Unit =
    sbfm(rd, rn.as(rd.width), 0, 15)

  def sxtw(rd: IRegister.X, rn: IRegister.W): Unit =
    sbfm(rd, rn.asX, 0, 31)

  // - mov immediate/ireg to ireg

  def tryMovImm(rd: IRegister, imm: Long): Boolean = {
    require(noSP(rd), "mov SP, #imm is not permitted")
    val imm16 = ShiftedImm16.encodeOrNull(imm, rd.width)
    if (imm16 != null) {
      emit(Bits.movz(rd, imm16))
      return true
    }
    val inverseImm16 = ShiftedImm16.encodeOrNull(~imm, rd.width)
    if (inverseImm16 != null) {
      emit(Bits.movn(rd, inverseImm16))
      return true
    }
    val bitMaskImm = BitMaskImm.encodeOrNull(imm, rd.width)
    if (bitMaskImm != null) {
      orr(rd, getZR(rd), imm)
      return true
    }
    false
  }

  def mov(rd: IRegister, imm: Long): Unit = {
    if (!tryMovImm(rd, imm)) {
      error("bad immediate for mov Rd, #imm")
    }
  }

  def movz(rd: IRegister, imm16: Int, shift: Int): Unit =
    emit(Bits.movz(rd, ShiftedImm16.encodeOrNull(imm16, shift, rd.width)))

  def movn(rd: IRegister, imm16: Int, shift: Int): Unit =
    emit(Bits.movn(rd, ShiftedImm16.encodeOrNull(imm16, shift, rd.width)))

  def movk(rd: IRegister, imm16: Int, shift: Int): Unit =
    emit(Bits.movk(rd, ShiftedImm16.encodeOrNull(imm16, shift, rd.width)))

  def mov(rd: IRegister, rm: IRegister): Unit = {
    if (rd == SP || rm == SP) {
      add(rd, rm, 0)
    } else {
      orr(rd, getZR(rd), rm)
    }
  }

  // - conditional selection

  def csel(rd: IRegister, rn: IRegister, rm: IRegister, cond: CC): Unit =
    emit(Bits.select(CSEL, rd, rn, rm, cond))

  def csinc(rd: IRegister, rn: IRegister, rm: IRegister, cond: CC): Unit =
    emit(Bits.select(CSINC, rd, rn, rm, cond))

  def csinv(rd: IRegister, rn: IRegister, rm: IRegister, cond: CC): Unit =
    emit(Bits.select(CSINV, rd, rn, rm, cond))

  def csneg(rd: IRegister, rn: IRegister, rm: IRegister, cond: CC): Unit =
    emit(Bits.select(CSNEG, rd, rn, rm, cond))

  def cinc(rd: IRegister, rn: IRegister, cond: CC): Unit = {
    require(noAL_NW(cond), "bad condition for CINC Rd, Rn, cond")
    csinc(rd, rn, rn, cond.opposite)
  }

  def cinv(rd: IRegister, rn: IRegister, cond: CC): Unit = {
    require(noAL_NW(cond), "bad condition for CINV Rd, Rn, cond")
    csinv(rd, rn, rn, cond.opposite)
  }

  def cneg(rd: IRegister, rn: IRegister, cond: CC): Unit = {
    require(noAL_NW(cond), "bad condition for CNEG Rd, cond")
    csneg(rd, rn, rn, cond.opposite)
  }

  def cset(rd: IRegister, cond: CC): Unit = {
    require(noAL_NW(cond), "bad condition for CSET Rd, cond")
    cinc(rd, getZR(rd), cond)
  }

  def csetm(rd: IRegister, cond: CC): Unit = {
    require(noAL_NW(cond), "bad condition for CSETM Rd, cond")
    cinv(rd, getZR(rd), cond)
  }

  // - various integer data-processing instructions

  def ccmp(rn: IRegister, rm: IRegister, nzcv: Int, cond: CC): Unit =
    emit(Bits.ccmp(rn, rm, nzcv, cond))

  def clz(rd: IRegister, rn: IRegister): Unit =
    emit(Bits.clz(rd, rn))

  def rbit(rd: IRegister, rn: IRegister): Unit =
    emit(Bits.rbit(rd, rn))

  def nop(): Unit =
    emit(Bits.nop)

  ////////////////////////////////////////////////////////////////////////////////
  // Instructions related to memory

  // - unclassified instructions

  def dmb(option: DBOption): Unit =
    emit(Bits.dmb(option))

  def prfm(prfop: Int, m: Arg.Mem): Unit =
    emit(Bits.prfm(prfop, m))

  def adr(rd: IRegister.X, target: Symbol): Unit =
    emit(new Fixups.MovAddr(rd, target))

  /** Getting 32-bit offset in segment via two move instructions. */
  def movOffs32InMethod(dst: IRegister, target: Label): Unit =
    emit(new Fixups.MovOffs32InMethod(dst, target))

  // - load/store single ireg/pair of iregs

  def ldr(signExtend: Boolean, width: Width, rt: Register, m: Arg.Mem): Unit =
    emit(Bits.loadStoreReg(if (signExtend) LDSX else LD, width, rt, m))

  def str(width: Width, rt: Register, m: Arg.Mem): Unit =
    emit(Bits.loadStoreReg(ST, width, rt, m))

  def ldrb(rt: IRegister.W, m: Arg.Mem): Unit = ldr(signExtend = false, W8, rt, m)
  def ldrh(rt: IRegister.W, m: Arg.Mem): Unit = ldr(signExtend = false, W16, rt, m)
  def ldr (rt: Register,    m: Arg.Mem): Unit = ldr(signExtend = false, rt.width, rt, m)

  def ldrsb(rt: IRegister,   m: Arg.Mem): Unit = ldr(signExtend = true, W8, rt, m)
  def ldrsh(rt: IRegister,   m: Arg.Mem): Unit = ldr(signExtend = true, W16, rt, m)
  def ldrsw(rt: IRegister.X, m: Arg.Mem): Unit = ldr(signExtend = true, W32, rt, m)

  def strb(rt: IRegister.W, m: Arg.Mem): Unit = str(W8, rt, m)
  def strh(rt: IRegister.W, m: Arg.Mem): Unit = str(W16, rt, m)
  def str (rt: Register,    m: Arg.Mem): Unit = str(rt.width, rt, m)

  def ldp(rt1: Register, rt2: Register, m: Arg.MemRI): Unit =
    emit(Bits.loadStorePair(LD, rt1, rt2, m))

  def stp(rt1: Register, rt2: Register, m: Arg.MemRI): Unit =
    emit(Bits.loadStorePair(ST, rt1, rt2, m))

  def ldr(rt: Register, target: Label): Unit =
    emit(new Fixups.LdrLiteral(rt, target))

  def ldrsw(rt: IRegister.X, target: Label): Unit =
    emit(new Fixups.LdrswLiteral(rt, target))

  def ldrLiteral(rt: Register, data: Long): Unit =
    ldr(rt, literal(data, rt.width))

  def ldrswLiteral(rt: IRegister.X, data: Int): Unit =
    ldrsw(rt, literal(MathUtils.zeroExtend(data), W32))

  // - special (ordered, exclusive, atomic) memory operations

  def ldar(width: Width, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.LDAR, width, WZR, rt, rn))

  def ldaxr(width: Width, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.LDAXR, width, WZR, rt, rn))

  def ldxr(width: Width, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.LDXR, width, WZR, rt, rn))

  def stlr(width: Width, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.STLR, width, WZR, rt, rn))

  def stlxr(width: Width, rs: IRegister.W, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.STLXR, width, rs, rt, rn))

  def stxr(width: Width, rs: IRegister.W, rt: IRegister, rn: IRegister.X): Unit =
    emit(Bits.loadStoreSpecial(MemOpX.STXR, width, rs, rt, rn))

  def cas(width: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering): Unit =
    emit(Bits.cas(width, rs, rt, rn, ord))

  /** Atomic memory operations:
    *  - LD`op`{A}{L}{B|H} Rs, Rt, [Xn|SP]
    *  - ST`op`{L}{B|H} Rs, [Xn|SP]
    *  - SWP{A}{L}{B|H} Rs, Rt, [Xn|SP]
    *
    * where `op` is ADD | CLR | EOR | SET | SMAX | SMIN | UMAX | UMIN
    */
  def memAtomic(op: MemAtomicOp, width: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering): Unit =
    emit(Bits.memAtomic(op, width, rs, rt, rn, ord))

  /** Atomic memory operations:
    *  - LD`op`{A}{L}{B|H} Rs, Rt, [Xn|SP]
    *
    * where `op` is ADD | CLR | EOR | SET | SMAX | SMIN | UMAX | UMIN
    */
  def ldOP(op: MemAtomicOp, width: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering): Unit = {
    assert(op != MemAtomicOp.SWP)
    memAtomic(op, width, rs, rt, rn, ord)
  }

  /** Atomic memory operations:
    *  - ST`op`{L}{B|H} Rs, [Xn|SP]
    *
    * where `op` is ADD | CLR | EOR | SET | SMAX | SMIN | UMAX | UMIN
    */
  def stOP(op: MemAtomicOp, width: Width, rs: IRegister, rn: IRegister.X, ord: MemoryOrdering): Unit = {
    assert(op != MemAtomicOp.SWP && ord.a == 0)
    memAtomic(op, width, rs, getZR(rs), rn, ord)
  }

  def swp(width: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering): Unit =
    memAtomic(MemAtomicOp.SWP, width, rs, rt, rn, ord)

  ////////////////////////////////////////////////////////////////////////////////
  // Control flow instructions: conditional & unconditional branches

  def br (rn: IRegister.X): Unit = emit(Bits.br(rn))
  def blr(rn: IRegister.X): Unit = emit(Bits.blr(rn))
  def ret(rn: IRegister.X): Unit = emit(Bits.ret(rn))
  def ret(): Unit = ret(LR)

  def b (dst: Symbol): Unit = emit(new Fixups.Jump(dst, false))
  def bl(dst: Symbol): Unit = emit(new Fixups.Jump(dst, true))

  def b(cond: CC, dst: Label): Unit = emit(new Fixups.Branch(dst, cond))

  def cbz (rt: IRegister, label: Label): Unit = emit(new Fixups.CompareBranch(false, rt, label))
  def cbnz(rt: IRegister, label: Label): Unit = emit(new Fixups.CompareBranch(true, rt, label))

  def tbz (rt: IRegister, imm: Int, label: Label): Unit = emit(new Fixups.TestBranch(false, rt, imm, label))
  def tbnz(rt: IRegister, imm: Int, label: Label): Unit = emit(new Fixups.TestBranch(true, rt, imm, label))

  ////////////////////////////////////////////////////////////////////////////////
  // Floating point instructions

  // - moves, binary arithmetics, compares

  def fmov(rd: VFPRegister, imm: Double): Unit     = emit(Bits.fmov(rd, imm))
  def fmov(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.fmov(rd, rn))
  def fmov(rd: VFPRegister, rn: IRegister): Unit   = emit(Bits.fmov(rd, rn))
  def fmov(rd: IRegister,   rn: VFPRegister): Unit = emit(Bits.fmov(rd, rn))

  def fadd(rd: VFPRegister, rn: VFPRegister, rm: VFPRegister): Unit = emit(Bits.fp2(FADD, rd, rn, rm))
  def fsub(rd: VFPRegister, rn: VFPRegister, rm: VFPRegister): Unit = emit(Bits.fp2(FSUB, rd, rn, rm))
  def fmul(rd: VFPRegister, rn: VFPRegister, rm: VFPRegister): Unit = emit(Bits.fp2(FMUL, rd, rn, rm))
  def fdiv(rd: VFPRegister, rn: VFPRegister, rm: VFPRegister): Unit = emit(Bits.fp2(FDIV, rd, rn, rm))

  def fcmp(rn: VFPRegister, rm: VFPRegister): Unit = emit(Bits.fcmp(rn, rm))
  def fcmp(rn: VFPRegister, imm: Double): Unit     = emit(Bits.fcmp(rn, imm))

  // - conversions & unary operations

  def fabs(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.fabs(rd, rn))
  def fneg(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.fneg(rd, rn))
  def fsqrt(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.fsqrt(rd, rn))

  def frintz(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.frintz(rd, rn))

  def fcvt(rd: VFPRegister, rn: VFPRegister): Unit = emit(Bits.fcvt(rd, rn))
  def fcvtzs(rd: IRegister, rn: VFPRegister): Unit = emit(Bits.fcvtzs(rd, rn))

  def scvtf(rd: VFPRegister, rn: IRegister): Unit = emit(Bits.scvtf(rd, rn))
  def ucvtf(rd: VFPRegister, rn: IRegister): Unit = emit(Bits.ucvtf(rd, rn))

  ////////////////////////////////////////////////////////////////////////////////
  // Vector instructions

  /** INS Vd.T[i], Rn */
  def ins(rd: VFPRegister.V, elemWidth: Width, index: Int, rn: IRegister): Unit =
    emit(Bits.ins(rd, elemWidth, index, rn))

  /** MOV Vd.T[i], Rn */
  def mov(rd: VFPRegister.V, index: Int, rn: IRegister): Unit =
    ins(rd, rn.width, index, rn)

  /** UMOV Rd, Vn.T[i] */
  def umov(rd: IRegister, rn: VFPRegister.V, elemWidth: Width, index: Int): Unit =
    emit(Bits.umov(rd, rn, elemWidth, index))

  /** MOV Rd, Vn.T[i] */
  def mov(rd: IRegister, rn: VFPRegister.V, index: Int): Unit =
    umov(rd, rn, rd.width, index)

  /** CNT Vd.T, Vn.T */
  def cnt(rd: VFPRegister.V, rn: VFPRegister.V, vlen: Int): Unit =
    emit(Bits.cnt(rd, rn, vlen))

  /** ADDV Vd, Vn.T */
  def addv(rd: VFPRegister.V, dstWidth: Width, rn: VFPRegister.V, vlen: Int): Unit =
    emit(Bits.addv(rd, dstWidth, rn, vlen))

  ////////////////////////////////////////////////////////////////////////////////
  // Private utils

  private def emit(instr: Int): Unit = seg.putW32(instr)

  private def emit(fixup: Fixup): Unit = addFixup(fixup)

  private def noAL_NW(cond: CC) = (cond != CC.AL) && (cond != CC.NV)
}