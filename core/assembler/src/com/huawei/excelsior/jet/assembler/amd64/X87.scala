/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.amd64.Bits.FIXED_WIDTH
import com.huawei.excelsior.jet.assembler.amd64.FPURegister.ST
import com.huawei.excelsior.jet.assembler.amd64.FPURegister.ST1
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth._
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.WORD
import com.huawei.excelsior.jet.assembler.amd64.Register16.AX
import com.huawei.excelsior.jet.assembler.amd64.Bits.check

/** x87 FPU instructions.
  *
  * @author paul
  */
final class X87 private[amd64](e: Bits) {

  // Binary arithmetic

  /** st(0) <- st(0) + src */
  def fadd(src: AddrMode): Unit = arithM(0, src)

  /** dst <- dst + src */
  def fadd(dst: FPURegister, src: FPURegister): Unit = arithRR(0xc0, 0xc0, dst, src)

  /** st(1) <- st(1) + st(0); pop x87 stack */
  def faddp(): Unit = faddp(ST1, ST)

  /** dst <- dst + src; pop x87 stack */
  def faddp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xc0, dst, src)

  /** st(0) <- st(0) + src */
  def fiadd(src: AddrMode): Unit = iarithM(0, src)

  /** st(0) <- st(0) * src */
  def fmul(src: AddrMode): Unit = arithM(1, src)

  /** dst <- dst * src */
  def fmul(dst: FPURegister, src: FPURegister): Unit = arithRR(0xc8, 0xc8, dst, src)

  /** st(1) <- st(1) * st(0); pop x87 stack */
  def fmulp(): Unit = fmulp(ST1, ST)

  /** dst <- dst * src; pop x87 stack */
  def fmulp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xc8, dst, src)

  /** st(0) <- st(0) * src */
  def fimul(src: AddrMode): Unit = iarithM(1, src)

  /** st(0) <- st(0) - src */
  def fsub(src: AddrMode): Unit = arithM(4, src)

  /** dst <- dst - src */
  def fsub(dst: FPURegister, src: FPURegister): Unit = arithRR(0xe0, 0xe8, dst, src)

  /** st(1) <- st(1) - st(0); pop x87 stack */
  def fsubp(): Unit = fsubp(ST1, ST)

  /** dst <- dst - src; pop x87 stack */
  def fsubp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xe8, dst, src)

  /** st(0) <- st(0) - src */
  def fisub(src: AddrMode): Unit = iarithM(4, src)

  /** st(0) <- src - st(0) */
  def fsubr(src: AddrMode): Unit = arithM(5, src)

  /** dst <- src - dst */
  def fsubr(dst: FPURegister, src: FPURegister): Unit = arithRR(0xe8, 0xe0, dst, src)

  /** st(1) <- st(0) - st(1); pop x87 stack */
  def fsubrp(): Unit = fsubrp(ST1, ST)

  /** dst <- src - dst; pop x87 stack */
  def fsubrp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xe0, dst, src)

  /** st(0) <- src - st(0) */
  def fisubr(src: AddrMode): Unit = iarithM(5, src)

  /** st(0) <- st(0) / src */
  def fdiv(src: AddrMode): Unit = arithM(6, src)

  /** dst <- dst / src */
  def fdiv(dst: FPURegister, src: FPURegister): Unit = arithRR(0xf0, 0xf8, dst, src)

  /** st(1) <- st(1) / st(0); pop x87 stack */
  def fdivp(): Unit = fdivp(ST1, ST)

  /** dst <- dst / src; pop x87 stack */
  def fdivp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xf8, dst, src)

  /** st(0) <- st(0) / src */
  def fidiv(src: AddrMode): Unit = iarithM(6, src)

  /** st(0) <- src / st(0) */
  def fdivr(src: AddrMode): Unit = arithM(7, src)

  /** dst <- src / dst */
  def fdivr(dst: FPURegister, src: FPURegister): Unit = arithRR(0xf8, 0xf0, dst, src)

  /** st(1) <- st(0) / st(1); pop x87 stack */
  def fdivrp(): Unit = fdivrp(ST1, ST)

  /** dst <- src / dst; pop x87 stack */
  def fdivrp(dst: FPURegister, src: FPURegister): Unit = opRT(0xde, 0xf0, dst, src)

  /** st(0) <- src / st(0) */
  def fidivr(src: AddrMode): Unit = iarithM(7, src)

  // Comparisons

  /** cmp st(0), st(1) */
  def fcom(): Unit = fcom(ST1)

  /** cmp st(0), arg */
  def fcom(arg: FPURegister): Unit = opR(0xd8, 0xd0, arg)

  /** cmp st(0), arg */
  def fcom(arg: AddrMode): Unit = arithM(2, arg)

  /** cmp st(0), st(1); pop x87 stack */
  def fcomp(): Unit = fcomp(ST1)

  /** cmp st(0), arg; pop x87 stack */
  def fcomp(arg: FPURegister): Unit = opR(0xd8, 0xd8, arg)

  /** cmp st(0), arg; pop x87 stack */
  def fcomp(arg: AddrMode): Unit = arithM(3, arg)

  /** cmp st(0), st(1); pop x87 stack twice */
  def fcompp(): Unit = e.emitBytes(0xde, 0xe9)

  /** unordered cmp st(0), st(1) */
  def fucom(): Unit = fucom(ST1)

  /** unordered cmp st(0), arg */
  def fucom(arg: FPURegister): Unit = opR(0xdd, 0xe0, arg)

  /** unordered cmp st(0), st(1); pop x87 stack */
  def fucomp(): Unit = fucomp(ST1)

  /** unordered cmp st(0), arg; pop x87 stack */
  def fucomp(arg: FPURegister): Unit = opR(0xdd, 0xe8, arg)

  /** unordered cmp st(0), st(1); pop x87 stack twice */
  def fucompp(): Unit = e.emitBytes(0xda, 0xe9)

  /** int cmp st(0), arg */
  def ficom(arg: AddrMode): Unit = iarithM(2, arg)

  /** int cmp st(0), arg; pop x87 stack */
  def ficomp(arg: AddrMode): Unit = iarithM(3, arg)

  /** cmp x, y and set EFLAGS register */
  def fcomi(x: FPURegister, y: FPURegister): Unit = opTR(0xdb, 0xf0, x, y)

  /** cmp x, y and set EFLAGS register; pop x87 stack */
  def fcomip(x: FPURegister, y: FPURegister): Unit = opTR(0xdf, 0xf0, x, y)

  /** unordered cmp x, y and set EFLAGS register */
  def fucomi(x: FPURegister, y: FPURegister): Unit = opTR(0xdb, 0xe8, x, y)

  /** unordered cmp x, y and set EFLAGS register; pop x87 stack */
  def fucomip(x: FPURegister, y: FPURegister): Unit = opTR(0xdf, 0xe8, x, y)

  /** dst <- src if condition */
  def fcmov(cc: FPUCC, dst: FPURegister, src: FPURegister): Unit = {
    val notCC = cc.code & 1
    val baseCC = cc.code >> 1
    opTR(0xda + notCC, 0xc0 + baseCC * 8, dst, src)
  }

  // Load, store, exchange

  /** temp <- st(0); st(0) <- st(1); st(1) <- temp */
  def fxch(): Unit = fxch(ST1)

  /** temp <- st(0); st(0) <- reg; reg <- temp */
  def fxch(reg: FPURegister): Unit = opR(0xd9, 0xc8, reg)

  /** push src onto x87 stack */
  def fld(src: AddrMode): Unit = {
    check(tryF_M(0xd9, 0xdd, 0, src) ||
          tryME_M(TWORD, 0xdb, 5, src))
  }

  /** push src onto x87 stack */
  def fld(src: FPURegister): Unit = opR(0xd9, 0xc0, src)

  /** push src onto x87 stack */
  def fild(src: AddrMode): Unit = {
    check(tryFI_M(0xdf, 0xdb, 0, src) ||
          tryME_M(QWORD, 0xdf, 5, src))
  }

  /** dst <- st(0) */
  def fst(dst: AddrMode): Unit = check(tryF_M(0xd9, 0xdd, 2, dst))

  /** dst <- st(0) */
  def fst(dst: FPURegister): Unit = opR(0xdd, 0xd0, dst)

  /** dst <- st(0) */
  def fist(dst: AddrMode): Unit = check(tryFI_M(0xdf, 0xdb, 2, dst))

  /** pop from x87 stack into dst */
  def fstp(dst: AddrMode): Unit = {
    check(tryF_M(0xd9, 0xdd, 3, dst) ||
          tryME_M(TWORD, 0xdb, 7, dst))
  }

  /** pop from x87 stack into dst */
  def fstp(dst: FPURegister): Unit = opR(0xdd, 0xd8, dst)

  /** pop from x87 stack into dst */
  def fistp(dst: AddrMode): Unit = {
    check(tryFI_M(0xdf, 0xdb, 3, dst) ||
          tryME_M(QWORD, 0xdf, 7, dst))
  }

  /** truncate and pop from x87 stack into dst */
  def fisttp(dst: AddrMode): Unit = {
    check(e.supports(Feature.SSE3) && (
          tryFI_M(0xdf, 0xdb, 1, dst) ||
          tryME_M(QWORD, 0xdd, 1, dst))
    )
  }

  /** control word <- src */
  def fldcw(src: AddrMode): Unit = opME_M(WORD, 0xd9, 5, src)

  /** dst <- control word */
  def fnstcw(dst: AddrMode): Unit = opME_M(WORD, 0xd9, 7, dst)

  /** dst <- status word */
  def fnstsw(dst: Register16): Unit = {
    check(dst == AX)
    e.emitBytes(0xdf, 0xe0)
  }

  // Miscellenious instructions

  /** push 1.0 onto x87 stack */
  def fld1(): Unit = unary(0xe8)

  /** push log[2](10) onto x87 stack */
  def fldl2t(): Unit = unary(0xe9)

  /** push log[2](e) onto x87 stack */
  def fldl2e(): Unit = unary(0xea)

  /** push pi onto x87 stack */
  def fldpi(): Unit = unary(0xeb)

  /** push log[10](2) onto x87 stack */
  def fldlg2(): Unit = unary(0xec)

  /** push log[e](2) onto x87 stack */
  def fldln2(): Unit = unary(0xed)

  /** push 0.0 onto x87 stack */
  def fldz(): Unit = unary(0xee)

  /** st(0) <- -st(0) */
  def fchs(): Unit = unary(0xe0)

  /** st(0) <- abs(st(0)) */
  def fabs(): Unit = unary(0xe1)

  /** compare st(0) to 0.0 */
  def ftst(): Unit = unary(0xe4)

  /** characterize st(0) */
  def fxam(): Unit = unary(0xe5)

  /** st(0) <- 2^st(0) - 1 */
  def f2xm1(): Unit = unary(0xf0)

  /** st(1) <- st(1) * log[2](st(0)); pop x87 stack */
  def fyl2x(): Unit = unary(0xf1)

  /** st(0) <- tan(st(0)); push 1.0 onto x87 stack */
  def fptan(): Unit = unary(0xf2)

  /** st(1) <- arctan( st(1) / st(0) ); pop x87 stack */
  def fpatan(): Unit = unary(0xf3)

  /** pop x87 stack to temp;
    * push exponent of temp and significand of temp onto x87 stack */
  def fxtract(): Unit = unary(0xf4)

  /** st(0) <- st(0) % st(1) (as IEEE 754) */
  def fprem1(): Unit = unary(0xf5)

  /** st(0) <- st(0) % st(1) */
  def fprem(): Unit = unary(0xf8)

  /** st(1) <- st(1) * log[2](st(0) + 1); pop x87 stack */
  def fyl2xp1(): Unit = unary(0xf9)

  /** st(0) <- sqrt(st(0)) */
  def fsqrt(): Unit = unary(0xfa)

  /** pop x87 stack to temp; push sin(temp) and cos(temp) onto x87 stack */
  def fsincos(): Unit = unary(0xfb)

  /** st(0) <- int(st(0)) */
  def frndint(): Unit = unary(0xfc)

  /** st(0) <- st(0) * 2^int(st(1)) */
  def fscale(): Unit = unary(0xfd)

  /** st(0) <- sin(st(0)) */
  def fsin(): Unit = unary(0xfe)

  /** st(0) <- cos(st(0)) */
  def fcos(): Unit = unary(0xff)

  //-------------------------------------------------------------------
  //                         U t i l i t i e s
  //-------------------------------------------------------------------

  // Fsmth st(i): B+fr format
  private def opR(byte1: Int, byte2: Int, fr: FPURegister): Unit = {
    e.formatFR(byte1, byte2, fr)
  }

  // Fsmth st, st(i): B+fr format
  private def opTR(byte1: Int, byte2: Int, x: FPURegister, y: FPURegister): Unit = {
    check(x == ST)
    opR(byte1, byte2, y)
  }

  // Fsmth st(i), st: B+fr format
  private def opRT(byte1: Int, byte2: Int, x: FPURegister, y: FPURegister): Unit = {
    check(y == ST)
    opR(byte1, byte2, x)
  }

  private def unary(op: Int): Unit = e.emitBytes(0xd9, op)

  // Fsmth st(i), st | st, st(i)
  private def arithRR(opcodeTR: Int, opcodeRT: Int, x: FPURegister, y: FPURegister): Unit = {
    check(x == ST || y == ST)
    if (x == ST) {
      opR(0xd8, opcodeTR, y)
    } else {
      opR(0xdc, opcodeRT, x)
    }
  }

  // F[I]smth mem: ME format; fixed operand size
  // no operand size prefixes required/allowed
  private def opME_M(width: Width, opcode: Int, opcodeExt: Int, am: AddrMode): Unit = {
    check(!am.isRegister && e.matchWidth(am, width))
    e.formatME(opcode, opcodeExt, am, FIXED_WIDTH)
  }

  private def tryME_M(width: Width, opcode: Int, opcodeExt: Int, am: AddrMode): Boolean = {
    if (e.sameWidth(am, width)) {
      opME_M(width, opcode, opcodeExt, am)
      return true
    }
    false
  }

  // Fsmth m32/m64: MEF format
  private def tryF_M(opcode32: Int, opcode64: Int, opcodeExt: Int, am: AddrMode) = {
    tryME_M(DWORD, opcode32, opcodeExt, am) ||
      tryME_M(QWORD, opcode64, opcodeExt, am)
  }

  // FIsmth m16/m32: MEF format
  private def tryFI_M(int16Opcode: Int, int32Opcode: Int, opcodeExt: Int, am: AddrMode) = {
    tryME_M(WORD, int16Opcode, opcodeExt, am) ||
      tryME_M(DWORD, int32Opcode, opcodeExt, am)
  }

  private def arithM(opcodeExt: Int, am: AddrMode): Unit = {
    check(tryF_M(0xd8, 0xdc, opcodeExt, am))
  }

  private def iarithM(opcodeExt: Int, am: AddrMode): Unit = {
    check(tryFI_M(0xde, 0xda, opcodeExt, am))
  }
}
