/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.W64
import com.huawei.excelsior.jet.assembler.amd64.Bits.VEXBits._
import com.huawei.excelsior.jet.assembler.amd64.Bits.check

/** AVX Instructions.
  *
  * Currently, only small subset of AVX instructions is implemented. Add them as needed.
  *
  * @author ikireev
  * @author paul
  */
final class AVX private[amd64](e: Bits) {

  /** dst[127:0] <- qword src */
  def vcvtph2ps(dst: XMM, src: AddrMode): Unit = {
    check(allowXM(src, W64)) // TODO: e.supports(F16C)
    e.formatVEX(L128|pp66|mm0F38|W0|op(0x13), dst, src)
  }

  /** dst[127:0] <- src[63:0] */
  def vcvtph2ps(dst: XMM, src: XMM): Unit = vcvtph2ps(dst, src.toAddrMode)

  /** qword dst <- src[127:0] */
  def vcvtps2ph(dst: AddrMode, src: XMM): Unit = {
    check(allowXM(dst, W64)) // TODO: e.supports(F16C)
    val imm8 = 0 // rounding control mode: round to nearest even
    e.formatVEX_I8(L128|pp66|mm0F3A|W0|op(0x1d), src, dst, imm8)
  }

  /** dst[63:0] <- src[127:0] */
  def vcvtps2ph(dst: XMM, src: XMM): Unit = vcvtps2ph(dst.toAddrMode, src)


  private def shiftx(pp: Int, dst: Register, src: AddrMode, count: Register): Unit = {
    check(allowR(dst) && allowR(count) && allowRM(src)) // TODO: e.supports(BMI2)
    val W = vexW(e.width(dst, count, src))
    e.formatVEX(LZ|pp|mm0F38|W|op(0xF7), dst, src, count)
  }

  /** dst <- src << count */
  def shlx(dst: Register, src: AddrMode, count: Register): Unit = shiftx(pp66, dst, src, count)
  def shlx(dst: Register, src: Register, count: Register): Unit = shlx(dst, src.toAddrMode, count)

  /** dst <- src >>> count */
  def shrx(dst: Register, src: AddrMode, count: Register): Unit = shiftx(ppF2, dst, src, count)
  def shrx(dst: Register, src: Register, count: Register): Unit = shrx(dst, src.toAddrMode, count)

  /** dst <- src >> count */
  def sarx(dst: Register, src: AddrMode, count: Register): Unit = shiftx(ppF3, dst, src, count)
  def sarx(dst: Register, src: Register, count: Register): Unit = sarx(dst, src.toAddrMode, count)

  /** dst <- rotateRight(src, count) */
  def rorx(dst: Register, src: AddrMode, count: Int): Unit = {
    check(allowR(dst) && allowRM(src)) // TODO: e.supports(BMI2)
    val W = vexW(e.width(dst, src))
    e.formatVEX_I8(LZ|ppF2|mm0F3A|W|op(0xF0), dst, src, count & 0xff)
  }

  def rorx(dst: Register, src: Register, count: Int): Unit = rorx(dst, src.toAddrMode, count)

  /** dstHi:dstLo <- umul(src1, rDX) */
  def mulx(dstHi: Register, dstLo: Register, src1: AddrMode): Unit = {
    check(allowR(dstHi) && allowR(dstLo) && allowRM(src1)) // TODO: e.supports(BMI2)
    val W = vexW(e.width(dstHi, dstLo, src1))
    e.formatVEX(LZ|ppF2|mm0F38|W|op(0xF6), dstHi, src1, dstLo)
  }

  def mulx(dstHi: Register, dstLo: Register, src1: Register): Unit = mulx(dstHi, dstLo, src1.toAddrMode)

  //-------------------------------------------------------------------
  //                         U t i l i t i e s
  //-------------------------------------------------------------------

  private def vexW(w: Width): Int = (if (w == W64) 1 else 0) << bitW

  // TODO: remove copypaste with SSE.scala

  private def allowR(r: Register) = r.isInstanceOf[GPR] || r.isInstanceOf[Register32]

  private def allowRM(am: AddrMode) = !am.isRegister || allowR(am.asRegister)

  private def allowXM(am: AddrMode, w: Width) =
    if (am.isRegister) am.asRegister.isInstanceOf[XMM] else e.matchWidth(am, w)
}
