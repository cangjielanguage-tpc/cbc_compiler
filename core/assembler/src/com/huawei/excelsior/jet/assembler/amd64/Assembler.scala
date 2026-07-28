/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.amd64.Immediate.fitsTo
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.*
import com.huawei.excelsior.jet.assembler.AsmEmitter
import com.huawei.excelsior.jet.assembler.Fixup
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.amd64.Bits.{check, AM}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind

/** Public interface of the assembler.
  *
  * @author paul
  * @author cypok
  */
final class Assembler(features: Feature*) extends AsmEmitter.WithLiterals { self =>

  def this(feature: Feature) = this(Seq(feature)*)

  private val e: Bits = new Bits(features*) {
    override def seg = segment
    override def addFixup(fixup: Fixup): Unit = self.addFixup(fixup)
  }

  /** x87 FPU instructions. */
  val x87 = new X87(e)

  /** SSE, SSE2, etc. instructions. */
  val sse = new SSE(e)

  /** AVX instructions. */
  val avx = new AVX(e)

  //-------------------------------------------------------------------
  //                         U t i l i t i e s
  //-------------------------------------------------------------------

  override def alignCode(alignment: Int): Unit =
    addFixup(new Fixups.CodeAlignment(alignment))

  override def symbolLiteralKind = RelocationKind.ADDR64

  //-------------------------------------------------------------------
  // General purpose x86/AMD64 instructions.
  // Includes all instructions except x87, SIMD (MMX, SSEx, AVX).
  // Intentionally does not include numerous weird, kludge and useless x86 instructions.
  //-------------------------------------------------------------------

  /** nop */
  def nop(): Unit = e.emitByte(0x90)

  /** Obtaining processor information */
  def cpuid(): Unit = e.emitBytes(0x0f, 0xa2)

  /** REP prefix, should be emitted just before string instruction (use with care) */
  def rep(): Unit = e.emitByte(0xf3)

  /** Lock prefix, should be emitted just before locked operation (use with care) */
  def lock(): Unit = e.emitByte(0xf0)

  /** memory barrier */
  def mfence(): Unit = e.emitBytes(0x0F, 0xAE, 0xF0)

  // Binary arithmetic and comparison

  /** dst <- dst + src */
  def add(dst: AddrMode, src: Register): Unit = e.opR1_A(0x00, src, dst)
  def add(dst: Register, src: AddrMode): Unit = e.opR1_A(0x02, dst, src)
  def add(dst: Register, src: Register): Unit = add(AM(dst), src)
  def add(dst: AddrMode, src: Int): Unit = e.x80Group(0, 0x04, dst, src)
  def add(dst: Register, src: Int): Unit = add(AM(dst), src)

  /** dst <- dst - src */
  def sub(dst: AddrMode, src: Register): Unit = e.opR1_A(0x28, src, dst)
  def sub(dst: Register, src: AddrMode): Unit = e.opR1_A(0x2a, dst, src)
  def sub(dst: Register, src: Register): Unit = sub(AM(dst), src)
  def sub(dst: AddrMode, src: Int): Unit = e.x80Group(5, 0x2c, dst, src)
  def sub(dst: Register, src: Int): Unit = sub(AM(dst), src)

  /** dst <- dst + src + CF */
  def adc(dst: AddrMode, src: Register): Unit = e.opR1_A(0x10, src, dst)
  def adc(dst: Register, src: AddrMode): Unit = e.opR1_A(0x12, dst, src)
  def adc(dst: Register, src: Register): Unit = adc(AM(dst), src)
  def adc(dst: AddrMode, src: Int): Unit = e.x80Group(2, 0x14, dst, src)
  def adc(dst: Register, src: Int): Unit = adc(AM(dst), src)

  /** dst <- dst - src - CF */
  def sbb(dst: AddrMode, src: Register): Unit = e.opR1_A(0x18, src, dst)
  def sbb(dst: Register, src: AddrMode): Unit = e.opR1_A(0x1a, dst, src)
  def sbb(dst: Register, src: Register): Unit = sbb(AM(dst), src)
  def sbb(dst: AddrMode, src: Int): Unit = e.x80Group(3, 0x1c, dst, src)
  def sbb(dst: Register, src: Int): Unit = sbb(AM(dst), src)

  /** dst <- dst & src */
  def and(dst: AddrMode, src: Register): Unit = e.opR1_A(0x20, src, dst)
  def and(dst: Register, src: AddrMode): Unit = e.opR1_A(0x22, dst, src)
  def and(dst: Register, src: Register): Unit = and(AM(dst), src)
  def and(dst: AddrMode, src: Int): Unit = e.x80Group(4, 0x24, dst, src)
  def and(dst: Register, src: Int): Unit = and(AM(dst), src)

  /** dst <- dst | src */
  def or(dst: AddrMode, src: Register): Unit = e.opR1_A(0x08, src, dst)
  def or(dst: Register, src: AddrMode): Unit = e.opR1_A(0x0a, dst, src)
  def or(dst: Register, src: Register): Unit = or(AM(dst), src)
  def or(dst: AddrMode, src: Int): Unit = e.x80Group(1, 0x0c, dst, src)
  def or(dst: Register, src: Int): Unit = or(AM(dst), src)

  /** dst <- dst `^` src */
  def xor(dst: AddrMode, src: Register): Unit = e.opR1_A(0x30, src, dst)
  def xor(dst: Register, src: AddrMode): Unit = e.opR1_A(0x32, dst, src)
  def xor(dst: Register, src: Register): Unit = xor(AM(dst), src)
  def xor(dst: AddrMode, src: Int): Unit = e.x80Group(6, 0x34, dst, src)
  def xor(dst: Register, src: Int): Unit = xor(AM(dst), src)

  /** EFLAGS <- arg1 - arg2 */
  def cmp(arg1: AddrMode, arg2: Register): Unit = e.opR1_A(0x38, arg2, arg1)
  def cmp(arg1: Register, arg2: AddrMode): Unit = e.opR1_A(0x3a, arg1, arg2)
  def cmp(arg1: Register, arg2: Register): Unit = cmp(AM(arg1), arg2)

  def cmp(arg1: AddrMode, arg2: Int): Unit = e.x80Group(7, 0x3c, arg1, arg2)
  def cmp(arg1: Register, arg2: Int): Unit = cmp(AM(arg1), arg2)

  def cmp(arg1: AddrMode, arg2: Immediate): Unit = e.x80Group(7, 0x3c, arg1, arg2)
  def cmp(arg1: Register, arg2: Immediate): Unit = cmp(AM(arg1), arg2)

  /** EFLAGS <- arg1 & arg2 */
  def test(arg1: AddrMode, arg2: Register): Unit = e.opR1_A(0x84, arg2, arg1)
  def test(arg1: Register, arg2: AddrMode): Unit = test(arg2, arg1)
  def test(arg1: Register, arg2: Register): Unit = test(AM(arg1), arg2)

  def test(arg1: AddrMode, arg2: Int): Unit = e.testA_I(arg1, arg2)
  def test(arg1: Register, arg2: Int): Unit = test(AM(arg1), arg2)

  /** dst <- condition ? 1 : 0 */
  def set(cc: CC, dst: AddrMode): Unit = {
    check(e.matchWidth(dst, BYTE))
    e.formatME(0x0f90 + cc.code, 0, dst, BYTE)
  }

  /** dst <- condition ? 1 : 0 */
  def set(cc: CC, dst: Register8): Unit = set(cc, AM(dst))

  // Unary arithmetic

  /** arg <- !arg */
  def not(arg: AddrMode): Unit = e.opA1(0xf6, 2, arg)
  def not(arg: Register): Unit = not(AM(arg))

  /** arg <- -arg */
  def neg(arg: AddrMode): Unit = e.opA1(0xf6, 3, arg)
  def neg(arg: Register): Unit = neg(AM(arg))

  /** RDX:RAX <- RAX * arg (unsigned) */
  def mul(arg: AddrMode): Unit = e.opA1(0xf6, 4, arg)
  def mul(arg: Register): Unit = mul(AM(arg))

  /** RDX:RAX <- RAX * arg (signed) */
  def imul(arg: AddrMode): Unit = e.opA1(0xf6, 5, arg)
  def imul(arg: Register): Unit = imul(AM(arg))

  /** RAX (quotient), RDX (remainder) <- RDX:RAX / arg (unsigned) */
  def div(arg: AddrMode): Unit = e.opA1(0xf6, 6, arg)
  def div(arg: Register): Unit = div(AM(arg))

  /** RAX (quotient), RDX (remainder) <- RDX:RAX / arg (signed) */
  def idiv(arg: AddrMode): Unit = e.opA1(0xf6, 7, arg)
  def idiv(arg: Register): Unit = idiv(AM(arg))

  // imul: 2-, 3-operand forms

  /** dst <- dst * src */
  def imul(dst: Register, src: AddrMode): Unit = e.opR_A(0x0faf, dst, src)
  def imul(dst: Register, src: Register): Unit = imul(dst, AM(src))

  /** dst <- src1 * src2 */
  def imul(dst: Register, src1: AddrMode, src2: Int): Unit = e.imulR_A_I(dst, src1, src2)
  def imul(dst: Register, src1: Register, src2: Int): Unit = imul(dst, AM(src1), src2)

  // Bit instructions

  /** ZF, dst <- index of lowest 1 bit in src */
  def bsf(dst: Register, src: AddrMode): Unit = e.opR_A(0x0fbc, dst, src)
  def bsf(dst: Register, src: Register): Unit = bsf(dst, AM(src))

  /** ZF, dst <- index of highest 1 bit in src */
  def bsr(dst: Register, src: AddrMode): Unit = e.opR_A(0x0fbd, dst, src)
  def bsr(dst: Register, src: Register): Unit = bsr(dst, AM(src))

  /** CF <- bitBase[bitIndex] */
  def bt(bitBase: AddrMode, bitIndex: Register): Unit = e.opR_A(0x0fa3, bitIndex, bitBase)
  def bt(bitBase: Register, bitIndex: Register): Unit = bt(AM(bitBase), bitIndex)
  def bt(bitBase: AddrMode, bitIndex: Int): Unit = e.opA_I8(0x0fba, 4, bitBase, bitIndex)
  def bt(bitBase: Register, bitIndex: Int): Unit = bt(AM(bitBase), bitIndex)

  /** CF <- bitBase[bitIndex];
    * bitBase[bitIndex] <- 1 */
  def bts(bitBase: AddrMode, bitIndex: Register): Unit = e.opR_A(0x0fab, bitIndex, bitBase)
  def bts(bitBase: Register, bitIndex: Register): Unit = bts(AM(bitBase), bitIndex)
  def bts(bitBase: AddrMode, bitIndex: Int): Unit = e.opA_I8(0x0fba, 5, bitBase, bitIndex)
  def bts(bitBase: Register, bitIndex: Int): Unit = bts(AM(bitBase), bitIndex)

  /** CF <- bitBase[bitIndex];
    * bitBase[bitIndex] <- 0 */
  def btr(bitBase: AddrMode, bitIndex: Register): Unit = e.opR_A(0x0fb3, bitIndex, bitBase)
  def btr(bitBase: Register, bitIndex: Register): Unit = btr(AM(bitBase), bitIndex)
  def btr(bitBase: AddrMode, bitIndex: Int): Unit = e.opA_I8(0x0fba, 6, bitBase, bitIndex)
  def btr(bitBase: Register, bitIndex: Int): Unit = btr(AM(bitBase), bitIndex)

  /** CF <- bitBase[bitIndex];
    * bitBase[bitIndex] <- !bitBase[bitIndex] */
  def btc(bitBase: AddrMode, bitIndex: Register): Unit = e.opR_A(0x0fbb, bitIndex, bitBase)
  def btc(bitBase: Register, bitIndex: Register): Unit = btc(AM(bitBase), bitIndex)
  def btc(bitBase: AddrMode, bitIndex: Int): Unit = e.opA_I8(0x0fba, 7, bitBase, bitIndex)
  def btc(bitBase: Register, bitIndex: Int): Unit = btc(AM(bitBase), bitIndex)

  /** dst <- count non-zero bits of src */
  def popcnt(dst: Register, src: AddrMode): Unit = {
    val opsize = e.width(dst, src) // TODO: e.supports(POPCNT)
    check(is248(opsize))
    e.formatMR_SSE(0xF3, 0x0FB8, dst, src, opsize)
  }

  def popcnt(dst: Register, src: Register): Unit = popcnt(dst, AM(src))

  // Moves

  /** temp <- r1; r1 <- r2; r2 <- temp; */
  def xchg(r1: Register, r2: Register): Unit = e.xchgR_R(r1, r2)

  /**
    * atomically: temp <- dst; dst <- src; src <- temp;
    *
    * Locking protocol is automatically implemented for the duration of the exchange operation,
    * regardless of the presence or absence of the LOCK prefix.
    */
  def lockxchg(arg1: AddrMode, arg2: Register): Unit = {
    check(!arg1.isRegister)
    e.opR1_A(0x86, arg2, arg1)
  }

  /** temp <- dst
    * ZF <- (temp == Acc)
    * if (ZF) dst <- src else Acc <- temp
    *
    * This instruction can be used with a LOCK prefix to allow the instruction to be executed atomically.
    */
  def cmpxchg(dst: AddrMode, src: Register): Unit = e.opR1_A(0x0fb0, src, dst)

  /** temp <- dst
    * ZF <- (temp == RDX:RAX)
    * if (ZF) dst <- RCX:RBX else RDX:RAX <- temp
    *
    * This instruction can be used with a LOCK prefix to allow the instruction to be executed atomically.
    */
  def cmpxchg16b(dst: AddrMode): Unit = {
    check(!dst.isRegister && e.matchWidth(dst, Width.W128))
    e.formatME(0x0fc7, 1, dst, Width.W64 /* REX.W = 1 */)
  }

  /** temp <- dst; dst <- dst + src; src <- temp;
    *
    * This instruction can be used with a LOCK prefix to allow the instruction to be executed atomically.
    */
  def xadd(dst: AddrMode, src: Register): Unit = e.opR1_A(0x0fc0, src, dst)

  /** swap bytes in reg */
  def bswap(reg: Register): Unit = {
    val w = e.width(reg)
    check(is48(w))
    e.formatSR(0x0fc8, reg, w)
  }

  /** dst <- addr(src) */
  def lea(dst: Register, src: AddrMode): Unit = {
    check(!src.isRegister)
    e.opR_A(0x8d, dst, src)
  }

  /** dst <- src */
  def mov(dst: AddrMode, src: Register): Unit = e.opR1_A(0x88, src, dst)
  def mov(dst: Register, src: AddrMode): Unit = e.opR1_A(0x8a, dst, src)
  def mov(dst: Register, src: Register): Unit = mov(AM(dst), src)

  /** dst <- imm */
  def mov(dst: AddrMode, imm: Immediate): Unit = e.movA_I(dst, imm)
  def mov(dst: Register, imm: Immediate): Unit = mov(AM(dst), imm)
  def mov(dst: AddrMode, imm: Long): Unit = e.movA_I(dst, imm)
  def mov(dst: Register, imm: Long): Unit = mov(AM(dst), imm)

  // TODO: mov xAX, moffs; mov moffs, xAX; opcodes: a0-a3

  /** dst <- src if condition */
  def cmov(cc: CC, dst: Register, src: AddrMode): Unit = e.opR_A(0x0f40 + cc.code, dst, src)
  def cmov(cc: CC, dst: Register, src: Register): Unit = cmov(cc, dst, AM(src))

  // Move with sign/zero extension

  /** AX <- sign-extend of AL */
  def cbw(): Unit = e.formatS(0x98, WORD)

  /** EAX <- sign-extend of AX */
  def cwde(): Unit = e.formatS(0x98, DWORD)

  /** RAX <- sign-extend of EAX */
  def cdqe(): Unit = e.formatS(0x98, QWORD)

  /** DX:AX <- sign-extend of AX */
  def cwd(): Unit = e.formatS(0x99, WORD)

  /** EDX:EAX <- sign-extend of EAX */
  def cdq(): Unit = e.formatS(0x99, DWORD)

  /** RDX:RAX <- sign-extend of RAX */
  def cqo(): Unit = e.formatS(0x99, QWORD)

  /** dst <- zero-extend of src */
  def movzx(dst: Register, src: AddrMode): Unit = {
    val w = e.width(src)
    check(w == BYTE || w == WORD)
    e.movXxR_A(if (w == BYTE) 0x0fb6 else 0x0fb7, dst, src)
  }

  /** dst <- zero-extend of src */
  def movzx(dst: Register, src: Register): Unit = movzx(dst, AM(src))

  /** dst <- sign-extend of src */
  def movsx(dst: Register, src: AddrMode): Unit = {
    val w = e.width(src)
    check(w == BYTE || w == WORD)
    e.movXxR_A(if (w == BYTE) 0x0fbe else 0x0fbf, dst, src)
  }

  /** dst <- sign-extend of src */
  def movsx(dst: Register, src: Register): Unit = movsx(dst, AM(src))

  /** dst <- sign-extend of src */
  def movsxd(dst: Register, src: AddrMode): Unit = {
    check(e.width(src) == DWORD)
    e.movXxR_A(0x63, dst, src)
  }

  /** dst <- sign-extend of src */
  def movsxd(dst: Register, src: Register): Unit = movsxd(dst, AM(src))

  // String instructions

  /** `width` [RDI] <- `width` [RSI] */
  def movs(width: Width): Unit = {
    check(width == BYTE || is248(width))
    val szbit = if (width == BYTE) 0 else 1
    e.formatS(0xA4 + szbit, width)
  }

  def movsb(): Unit = movs(BYTE)
  def movsw(): Unit = movs(WORD)
  def movsd(): Unit = movs(DWORD)
  def movsq(): Unit = movs(QWORD)

  /** `width` [RDI] <- RAX.as(`width`) */
  def stos(width: Width): Unit = {
    check(width == BYTE || is248(width))
    val szbit = if (width == BYTE) 0 else 1
    e.formatS(0xAA + szbit, width)
  }

  def stosb(): Unit = stos(BYTE)
  def stosw(): Unit = stos(WORD)
  def stosd(): Unit = stos(DWORD)
  def stosq(): Unit = stos(QWORD)

  // Shifts

  /** arg <- rotate arg to the left by count */
  def rol(arg: AddrMode, count: Int): Unit = e.shiftImm(0, arg, count)
  def rol(arg: Register, count: Int): Unit = rol(AM(arg), count)
  def rol(arg: AddrMode, count: Register8): Unit = e.shiftCL(0, arg, count)
  def rol(arg: Register, count: Register8): Unit = rol(AM(arg), count)

  /** arg <- rotate arg to the right by count */
  def ror(arg: AddrMode, count: Int): Unit = e.shiftImm(1, arg, count)
  def ror(arg: Register, count: Int): Unit = ror(AM(arg), count)
  def ror(arg: AddrMode, count: Register8): Unit = e.shiftCL(1, arg, count)
  def ror(arg: Register, count: Register8): Unit = ror(AM(arg), count)

  /** arg <- rotate carry flag and arg to the left by count */
  def rcl(arg: AddrMode, count: Int): Unit = e.shiftImm(2, arg, count)
  def rcl(arg: Register, count: Int): Unit = rcl(AM(arg), count)
  def rcl(arg: AddrMode, count: Register8): Unit = e.shiftCL(2, arg, count)
  def rcl(arg: Register, count: Register8): Unit = rcl(AM(arg), count)

  /** arg <- rotate carry flag and arg to the right by count */
  def rcr(arg: AddrMode, count: Int): Unit = e.shiftImm(3, arg, count)
  def rcr(arg: Register, count: Int): Unit = rcr(AM(arg), count)
  def rcr(arg: AddrMode, count: Register8): Unit = e.shiftCL(3, arg, count)
  def rcr(arg: Register, count: Register8): Unit = rcr(AM(arg), count)

  /** arg <- arg << count */
  def shl(arg: AddrMode, count: Int): Unit = e.shiftImm(4, arg, count)
  def shl(arg: Register, count: Int): Unit = shl(AM(arg), count)
  def shl(arg: AddrMode, count: Register8): Unit = e.shiftCL(4, arg, count)
  def shl(arg: Register, count: Register8): Unit = shl(AM(arg), count)

  /** arg <- arg >>> count (logical shift) */
  def shr(arg: AddrMode, count: Int): Unit = e.shiftImm(5, arg, count)
  def shr(arg: Register, count: Int): Unit = shr(AM(arg), count)
  def shr(arg: AddrMode, count: Register8): Unit = e.shiftCL(5, arg, count)
  def shr(arg: Register, count: Register8): Unit = shr(AM(arg), count)

  /** arg <- arg >> count (arithmetic shift) */
  def sar(arg: AddrMode, count: Int): Unit = e.shiftImm(7, arg, count)
  def sar(arg: Register, count: Int): Unit = sar(AM(arg), count)
  def sar(arg: AddrMode, count: Register8): Unit = e.shiftCL(7, arg, count)
  def sar(arg: Register, count: Register8): Unit = sar(AM(arg), count)

  /** dst <- (dst << count) | (src >>> count) */
  def shld(dst: AddrMode, src: Register, count: Int): Unit = e.doubleShiftImm(0x0fa4, dst, src, count)
  def shld(dst: Register, src: Register, count: Int): Unit = shld(AM(dst), src, count)
  def shld(dst: AddrMode, src: Register, count: Register8): Unit = e.doubleShiftCL(0x0fa5, dst, src, count)
  def shld(dst: Register, src: Register, count: Register8): Unit = shld(AM(dst), src, count)

  /** dst <- (dst >>> count) | (src << count) */
  def shrd(dst: AddrMode, src: Register, count: Int): Unit = e.doubleShiftImm(0x0fac, dst, src, count)
  def shrd(dst: Register, src: Register, count: Int): Unit = shrd(AM(dst), src, count)
  def shrd(dst: AddrMode, src: Register, count: Register8): Unit = e.doubleShiftCL(0x0fad, dst, src, count)
  def shrd(dst: Register, src: Register, count: Register8): Unit = shrd(AM(dst), src, count)

  // Push & Pop

  /** pop dst */
  def pop(dst: GPR): Unit = e.opRW(0x58, dst)
  def pop(dst: AddrMode): Unit = e.opAW(0x8f, 0, dst)

  /** push src */
  def push(src: GPR): Unit = e.opRW(0x50, src)
  def push(src: AddrMode): Unit = e.opAW(0xff, 6, src)

  def push(src: Int): Unit = {
    val immWidth = if (fitsTo(src, BYTE)) BYTE else DWORD
    e.formatB_I(if (immWidth == BYTE) 0x6a else 0x68, src, immWidth)
  }

  def push(src: Immediate): Unit = {
    val immWidth = if (src.fitsTo(BYTE)) BYTE else DWORD
    e.formatB_I(if (immWidth == BYTE) 0x6a else 0x68, src, immWidth)
  }

  /** push EFLAGS */
  def pushf(): Unit = e.emitByte(0x9C)

  // Branches

  def ret(): Unit = ret(0)

  /** pop toBeCleared bytes from stack & pop top of the stack to RIP */
  def ret(toBeCleared: Int): Unit = {
    if (toBeCleared == 0) {
      e.emitByte(0xc3)
    } else {
      e.formatB_I(0xc2, toBeCleared, WORD)
    }
  }

  /** call target */
  def call(target: AddrMode): Unit = e.opAW(0xff, 2, target)
  def call(target: Register): Unit = call(AM(target))

  def call(target: Symbol): Unit =
    e.addFixup(new Fixups.Jump(target, true, e.supports(Feature.SHORTJUMPS)))

  /** unconditional jump to target */
  def jmp(target: AddrMode): Unit = e.opAW(0xff, 4, target)
  def jmp(target: Register): Unit = jmp(AM(target))

  def jmp(target: Symbol): Unit =
    e.addFixup(new Fixups.Jump(target, false, e.supports(Feature.SHORTJUMPS)))

  /** jump to target if condition */
  def jcc(cc: CC, target: Label): Unit =
    e.addFixup(new Fixups.Branch(target, cc, e.supports(Feature.SHORTJUMPS)))

  // Prefetches

  /** Fetches the cache line of data from memory that contains `addr` operand to a location in the 1st
    * or 2nd level cache and invalidates other cached instances of the line. */
  def prefetchw(addr: AddrMode): Unit = {
    check(!addr.isRegister && e.matchWidth(addr, BYTE))
    e.formatME(0x0f0d, 1, addr, BYTE)
  }
}
