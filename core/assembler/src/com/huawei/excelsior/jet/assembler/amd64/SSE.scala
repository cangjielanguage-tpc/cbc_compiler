/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.ZERO
import xscala.util.MathUtils.isNBits
import com.huawei.excelsior.jet.assembler.amd64.Bits.{check, AM}

/** SSE Instructions.
  *
  * Currently, only limited subset of SSE instructions is implemented. Add them as needed.
  *
  * @author cypok
  * @author paul
  * @author alexm
  */
final class SSE private[amd64](e: Bits) {

  // Load, store, exchange

  /** dst[63:0] <- src[63:0] as scalar double */
  def movsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x10), dst, src)
  def movsd(dst: AddrMode, src: XMM): Unit = opM_X(W64, pF2(0x11), dst, src)
  def movsd(dst: XMM, src: XMM): Unit = movsd(dst, AM(src))

  /** dst[31:0] <- src[31:0] as scalar float */
  def movss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x10), dst, src)
  def movss(dst: AddrMode, src: XMM): Unit = opM_X(W32, pF3(0x11), dst, src)
  def movss(dst: XMM, src: XMM): Unit = movss(dst, AM(src))

  /** dst[31:0] <- src[31:0] */
  def movd(dst: XMM, src: AddrMode): Unit = opX_RM(W32, p66(0x6E), dst, src)
  def movd(dst: XMM, src: Register): Unit = movd(dst, AM(src))

  def movd(dst: AddrMode, src: XMM): Unit = opRM_X(W32, p66(0x7E), dst, src)
  def movd(dst: Register, src: XMM): Unit = movd(AM(dst), src)

  /** dst[63:0] <- qword src */
  def movq(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF3(0x7E), dst, src)

  /** dst[63:0] <- src[63:0] */
  def movq(dst: XMM, src: Register): Unit = src match {
    case src: XMM => movq(dst, AM(src))
    case _        => opX_RM(W64, p66(0x6E), dst, AM(src))
  }

  /** qword dst <- src[63:0] */
  def movq(dst: AddrMode, src: XMM): Unit = opM_X(W64, p66(0xD6), dst, src)

  /** dst[63:0] <- src[63:0] */
  def movq(dst: Register, src: XMM): Unit = dst match {
    case dst: XMM => movq(dst, AM(src))
    case _        => opRM_X(W64, p66(0x7E), AM(dst), src)
  }

  /** dst[127:0] <- src[127:0] as packed doubles (memory is aligned) */
  def movapd(dst: XMM, src: AddrMode): Unit = opX_XM(W128, p66(0x28), dst, src)
  def movapd(dst: AddrMode, src: XMM): Unit = opM_X(W128, p66(0x29), dst, src)
  def movapd(dst: XMM, src: XMM): Unit = movapd(dst, AM(src))

  /** dst[127:0] <- src[127:0] as packed floats (memory is aligned) */
  def movaps(dst: XMM, src: AddrMode): Unit = opX_XM(W128, pNO(0x28), dst, src)
  def movaps(dst: AddrMode, src: XMM): Unit = opM_X(W128, pNO(0x29), dst, src)
  def movaps(dst: XMM, src: XMM): Unit = movaps(dst, AM(src))

  /** dst[127:0] <- src[127:0] as packed ints (memory is analigned) */
  def movdqu(dst: XMM, src: AddrMode): Unit = opX_XM(W128, pF3(0x6F), dst, src)
  def movdqu(dst: AddrMode, src: XMM): Unit = opM_X(W128, pF3(0x7F), dst, src)


  // Comparisons

  /** cmp arg1[63:0], arg2[63:0] & set EFLAGS */
  def comisd(arg1: XMM, arg2: AddrMode): Unit = opX_XM(W64, p66(0x2F), arg1, arg2)
  def comisd(arg1: XMM, arg2: XMM): Unit = comisd(arg1, AM(arg2))

  /** cmp arg1[31:0], arg2[31:0] & set EFLAGS */
  def comiss(arg1: XMM, arg2: AddrMode): Unit = opX_XM(W32, pNO(0x2F), arg1, arg2)
  def comiss(arg1: XMM, arg2: XMM): Unit = comiss(arg1, AM(arg2))

  /** unordered cmp arg1[63:0], arg2[63:0] & set EFLAGS */
  def ucomisd(arg1: XMM, arg2: AddrMode): Unit = opX_XM(W64, p66(0x2E), arg1, arg2)
  def ucomisd(arg1: XMM, arg2: XMM): Unit = ucomisd(arg1, AM(arg2))

  /** unordered cmp arg1[31:0], arg2[31:0] & set EFLAGS */
  def ucomiss(arg1: XMM, arg2: AddrMode): Unit = opX_XM(W32, pNO(0x2E), arg1, arg2)
  def ucomiss(arg1: XMM, arg2: XMM): Unit = ucomiss(arg1, AM(arg2))


  // Floating point arithmetic: add|sub|mul|div|sqrt

  /** dst[63:0] <- dst[63:0] + src[63:0] */
  def addsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x58), dst, src)
  def addsd(dst: XMM, src: XMM): Unit = addsd(dst, AM(src))

  /** dst[31:0] <- dst[31:0] + src[31:0] */
  def addss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x58), dst, src)
  def addss(dst: XMM, src: XMM): Unit = addss(dst, AM(src))

  /** dst[63:0] <- dst[63:0] - src[63:0] */
  def subsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x5C), dst, src)
  def subsd(dst: XMM, src: XMM): Unit = subsd(dst, AM(src))

  /** dst[31:0] <- dst[31:0] - src[31:0] */
  def subss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x5C), dst, src)
  def subss(dst: XMM, src: XMM): Unit = subss(dst, AM(src))

  /** dst[63:0] <- dst[63:0] * src[63:0] */
  def mulsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x59), dst, src)
  def mulsd(dst: XMM, src: XMM): Unit = mulsd(dst, AM(src))

  /** dst[31:0] <- dst[31:0] * src[31:0] */
  def mulss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x59), dst, src)
  def mulss(dst: XMM, src: XMM): Unit = mulss(dst, AM(src))

  /** dst[63:0] <- dst[63:0] / src[63:0] */
  def divsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x5E), dst, src)
  def divsd(dst: XMM, src: XMM): Unit = divsd(dst, AM(src))

  /** dst[31:0] <- dst[31:0] / src[31:0] */
  def divss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x5E), dst, src)
  def divss(dst: XMM, src: XMM): Unit = divss(dst, AM(src))

  /** dst[63:0] <- sqrt(src[63:0]) */
  def sqrtsd(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x51), dst, src)
  def sqrtsd(dst: XMM, src: XMM): Unit = sqrtsd(dst, AM(src))

  /** dst[31:0] <- sqrt(src[31:0]) */
  def sqrtss(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x51), dst, src)
  def sqrtss(dst: XMM, src: XMM): Unit = sqrtss(dst, AM(src))


  // Bitwise logical

  /** dst <- dst & src */
  def andpd(dst: XMM, src: AddrMode): Unit = opX_XM(W128, p66(0x54), dst, src)
  def andpd(dst: XMM, src: XMM): Unit = andpd(dst, AM(src))

  /** dst <- dst & src */
  def andps(dst: XMM, src: AddrMode): Unit = opX_XM(W128, pNO(0x54), dst, src)
  def andps(dst: XMM, src: XMM): Unit = andps(dst, AM(src))

  /** dst <- dst `^` src */
  def xorpd(dst: XMM, src: AddrMode): Unit = opX_XM(W128, p66(0x57), dst, src)
  def xorpd(dst: XMM, src: XMM): Unit = xorpd(dst, AM(src))

  /** dst <- dst `^` src */
  def xorps(dst: XMM, src: AddrMode): Unit = opX_XM(W128, pNO(0x57), dst, src)
  def xorps(dst: XMM, src: XMM): Unit = xorps(dst, AM(src))

  /** dst <- dst `^` src */
  def pxor(dst: XMM, src: AddrMode): Unit = opX_XM(W128, p66(0xEF), dst, src)
  def pxor(dst: XMM, src: XMM): Unit = pxor(dst, AM(src))


  // Conversions

  /** dst[63/31:0] <- double2int(src[63:0]) */
  def cvttsd2si(dst: Register, src: AddrMode): Unit = opR_XM(W64, pF2(0x2C), dst, src)
  def cvttsd2si(dst: Register, src: XMM): Unit = cvttsd2si(dst, AM(src))

  /** dst[63/31:0] <- float2int(src[31:0]) */
  def cvttss2si(dst: Register, src: AddrMode): Unit = opR_XM(W32, pF3(0x2C), dst, src)
  def cvttss2si(dst: Register, src: XMM): Unit = cvttss2si(dst, AM(src))

  /** dst[31:0] <- double2float(src[63:0]) */
  def cvtsd2ss(dst: XMM, src: AddrMode): Unit = opX_XM(W64, pF2(0x5A), dst, src)
  def cvtsd2ss(dst: XMM, src: XMM): Unit = cvtsd2ss(dst, AM(src))

  /** dst[63:0] <- float2double(src[31:0]) */
  def cvtss2sd(dst: XMM, src: AddrMode): Unit = opX_XM(W32, pF3(0x5A), dst, src)
  def cvtss2sd(dst: XMM, src: XMM): Unit = cvtss2sd(dst, AM(src))

  /** dst[63:0] <- int2double(src[63/31:0]) */
  def cvtsi2sd(dst: XMM, src: AddrMode): Unit = opX_RM(pF2(0x2A), dst, src)
  def cvtsi2sd(dst: XMM, src: Register): Unit = cvtsi2sd(dst, AM(src))

  /** dst[31:0] <- int2float(src[63/31:0]) */
  def cvtsi2ss(dst: XMM, src: AddrMode): Unit = opX_RM(pF3(0x2A), dst, src)
  def cvtsi2ss(dst: XMM, src: Register): Unit = cvtsi2ss(dst, AM(src))

  /** Prefetch the line of data from memory that contains `addr` operand into non-temporal cache
    * structure and into a location close to the processor, minimizing cache pollution. */
  def prefetchnta(addr: AddrMode): Unit = {
    check(!addr.isRegister && e.matchWidth(addr, BYTE))
    e.formatME(0x0f18, 0, addr, BYTE)
  }

  /** Prefetches temporal data into the entire cache hierarchy. */
  def prefetcht0(addr: AddrMode): Unit = {
    check(!addr.isRegister && e.matchWidth(addr, BYTE))
    e.formatME(0x0f18, 1, addr, BYTE)
  }

  /** Prefetches temporal data into the second-level (L2) and higher-level caches,
    * but not into the L1 cache. */
  def prefetcht1(addr: AddrMode): Unit = {
    check(!addr.isRegister && e.matchWidth(addr, BYTE))
    e.formatME(0x0f18, 2, addr, BYTE)
  }

  /** Prefetches temporal data into the third-level (L3) and higher-level caches,
    * but not into the L1 or L2 cache */
  def prefetcht2(addr: AddrMode): Unit = {
    check(!addr.isRegister && e.matchWidth(addr, BYTE))
    e.formatME(0x0f18, 3, addr, BYTE)
  }

  //-------------------------------------------------------------------
  //                         U t i l i t i e s
  //-------------------------------------------------------------------

  private def pF2(op: Int) = opx(0xF2, op)
  private def pF3(op: Int) = opx(0xF3, op)
  private def p66(op: Int) = opx(0x66, op)
  private def pNO(op: Int) = opx(0, op)

  private def opx(prefix: Int, op: Int): Int = opx(prefix, 0x0f, op)

  private def opx(prefix: Int, leading: Int, op: Int): Int = {
    assert(leading != 0 && isNBits(leading, 16))
    assert(isNBits(prefix, 8) && isNBits(op, 8))
    leading << 16 | op << 8 | prefix
  }

  private def getPrefix(opx: Int) = opx & 0xff
  private def getOpcode(opx: Int) = opx >>> 8

  // smth xmm, xmm/mem
  private def opX_XM(width: Width, opx: Int, x: XMM, xm: AddrMode): Unit = {
    check(allowXM(xm, width))
    formatMR_SSE(opx, x, xm, ZERO)
  }

  // smth mem, xmm
  private def opM_X(width: Width, opx: Int, m: AddrMode, x: XMM): Unit = {
    check(allowM(m, width))
    formatMR_SSE(opx, x, m, ZERO)
  }

  // smth r, xmm/mem
  private def opR_XM(memWidth: Width, opx: Int, r: Register, am: AddrMode): Unit = {
    check(allowR(r) && allowXM(am, memWidth))
    formatMR_SSE(opx, r, am, e.width(r))
  }

  // smth xmm, r/m
  private def opX_RM(width: Width, opx: Int, x: XMM, rm: AddrMode): Unit = {
    check(allowRM(rm) && e.matchWidth(rm, width))
    formatMR_SSE(opx, x, rm, width)
  }

  // smth xmm, r/m
  private def opX_RM(opx: Int, x: XMM, rm: AddrMode): Unit = {
    val width = e.width(rm)
    check((width eq W32) || (width eq W64))
    formatMR_SSE(opx, x, rm, width)
  }

  // smth r/m, xmm
  private def opRM_X(width: Width, opx: Int, rm: AddrMode, x: XMM): Unit = {
    opX_RM(width, opx, x, rm)
  }

  private def allowR(r: Register) = r.isInstanceOf[GPR] || r.isInstanceOf[Register32]

  private def allowRM(am: AddrMode) = !am.isRegister || allowR(am.asRegister)

  private def allowXM(am: AddrMode, w: Width) = {
    if (am.isRegister) {
      am.asRegister.isInstanceOf[XMM]
    } else {
      e.matchWidth(am, w)
    }
  }

  private def allowM(am: AddrMode, w: Width) = !am.isRegister && e.matchWidth(am, w)

  private def formatMR_SSE(opx: Int, r: Register, am: AddrMode, operandSize: Width): Unit =
    e.formatMR_SSE(getPrefix(opx), getOpcode(opx), r, am, operandSize)
}
