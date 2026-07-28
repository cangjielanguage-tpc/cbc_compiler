/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.BYTE
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.NO_WIDTH
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.WPTR
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.is248
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.widthToString
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.couldBeScaled
import com.huawei.excelsior.jet.assembler.amd64.GPR.RSP

/** Addressing mode; immutable object.
  *
  * @author paul
  * @author cypok
  */
object AddrMode {
  def fromRegister(reg: Register) = // TODO-DECAF: private[amd64]
    new AddrMode(reg.width, reg, null, null, null, 0, null, null)

  def baseFromGPR(reg: GPR) = // TODO-DECAF: private[amd64]
    basedMemory(NO_WIDTH, reg, 0, null)

  private def scaledMemory(width: Width, base: GPR, scale: Width, index: GPR, disp: Int, symbol: Symbol) = {
    assert(index != RSP)
    new AddrMode(width, null, base, scale, index, disp, symbol, null)
  }

  private def basedMemory(width: Width, base: GPR, disp: Int, symbol: Symbol) =
    new AddrMode(width, null, base, null, null, disp, symbol, null)

  private def symbolDisp(width: Width, symbol: Symbol, disp: Int) = {
    assert(symbol != null)
    basedMemory(width, null, disp, symbol)
  }

  private def regDisp(width: Width, base: GPR, disp: Int) = {
    assert(base != null)
    if (disp == 0 && (width == NO_WIDTH)) {
      base.addrModeAsBase
    } else {
      basedMemory(width, base, disp, null)
    }
  }

  private def prefixDisp(width: Width, prefix: Prefix, disp: Int) = {
    assert(prefix != null)
    new AddrMode(width, null, null, null, null, disp, null, prefix)
  }

  ///////////////////////////////////////////////////
  //             AddrMode constructors DSL
  ///////////////////////////////////////////////////

  def couldBeScaled(scale: Width) = (scale == BYTE) || is248(scale)

  case class Scaled(scale: Width, index: GPR) {
    assert(couldBeScaled(scale))
  }

  def scaled(scale: Width, index: GPR) = Scaled(scale, index)

  /** width [base + (scaled.index * scaled.scale) + disp] */
  def M(width: Width, base: GPR, scaled: Scaled, disp: Int): AddrMode =
    scaledMemory(width, base, scaled.scale, scaled.index, disp, null)

  /** width [base + (scaled.index * scaled.scale)] */
  def M(width: Width, base: GPR, scaled: Scaled): AddrMode = M(width, base, scaled, 0)

  /** [base + (scaled.index * scaled.scale) + disp] */
  def M(base: GPR, scaled: Scaled, disp: Int): AddrMode = M(NO_WIDTH, base, scaled, disp)

  /** [base + (scaled.index * scaled.scale)] */
  def M(base: GPR, scaled: Scaled): AddrMode = M(base, scaled, 0)

  /** width [scaled.index * scaled.scale + disp] */
  def M(width: Width, scaled: Scaled, disp: Int): AddrMode = M(width, null, scaled, disp)

  /** width [scaled.index * scaled.scale] */
  def M(width: Width, scaled: Scaled): AddrMode = M(width, null, scaled, 0)

  /** [scaled.index * scaled.scale + disp] */
  def M(scaled: Scaled, disp: Int): AddrMode = M(NO_WIDTH, scaled, disp)

  /** [scaled.index * scaled.scale] */
  def M(scaled: Scaled): AddrMode = M(scaled, 0)

  /** width [r1 + r2 + disp] */
  def M(width: Width, r1: GPR, r2: GPR, disp: Int): AddrMode =
    scaledMemory(width, r1, BYTE, r2, disp, null)

  /** width [r1 + r2] */
  def M(width: Width, r1: GPR, r2: GPR): AddrMode = M(width, r1, r2, 0)

  /** [r1 + r2 + disp] */
  def M(r1: GPR, r2: GPR, disp: Int): AddrMode = M(NO_WIDTH, r1, r2, disp)

  /** [r1 + r2] */
  def M(r1: GPR, r2: GPR): AddrMode = M(r1, r2, 0)

  /** width [base + disp] */
  def M(width: Width, base: GPR, disp: Int): AddrMode = regDisp(width, base, disp)

  /** width [base] */
  def M(width: Width, base: GPR): AddrMode = regDisp(width, base, 0)

  /** [base + disp] */
  def M(base: GPR, disp: Int): AddrMode = regDisp(NO_WIDTH, base, disp)

  /** [base] */
  def M(base: GPR): AddrMode = regDisp(NO_WIDTH, base, 0)

  /** width [symbol + (scaled.index * scaled.scale)] */
  def M(width: Width, symbol: Symbol, scaled: Scaled): AddrMode =
    scaledMemory(width, null, scaled.scale, scaled.index, 0, symbol)

  /** [symbol + (scaled.index * scaled.scale)] */
  def M(symbol: Symbol, scaled: Scaled): AddrMode = M(NO_WIDTH, symbol, scaled)

  /** width [symbol + disp] */
  def M(width: Width, symbol: Symbol, disp: Int): AddrMode = symbolDisp(width, symbol, disp)

  /** width [symbol] */
  def M(width: Width, symbol: Symbol): AddrMode = symbolDisp(width, symbol, 0)

  /** [symbol + disp] */
  def M(symbol: Symbol, disp: Int): AddrMode = symbolDisp(NO_WIDTH, symbol, disp)

  /** [symbol] */
  def M(symbol: Symbol): AddrMode = symbolDisp(NO_WIDTH, symbol, 0)

  /** [disp] */
  def absolute(disp: Int): AddrMode =
    new AddrMode(NO_WIDTH, null, null, null, null, disp, null, null)

  /** fs:[disp] */
  def FS(disp: Int): AddrMode = prefixDisp(NO_WIDTH, Prefix.FS, disp)

  /** gs:[disp] */
  def GS(disp: Int): AddrMode = prefixDisp(NO_WIDTH, Prefix.GS, disp)
}

final class AddrMode private(val width: Width, reg: Register,
                             val base: GPR, val scale: Width, val index: GPR, val disp: Int,
                             val symbol: Symbol, val prefix: Prefix) {
  assert(width != null)
  assert(reg == null || reg.width == width)

  def isRegister = reg != null

  def asRegister = reg

  def as(width: Width) = {
    if (this.width == width) {
      this
    } else if (isRegister) {
      reg.as(width).toAddrMode
    } else {
      new AddrMode(width, reg, base, scale, index, disp, symbol, prefix)
    }
  }

  def disposed(delta: Int) = {
    assert(!isRegister)
    if (delta == 0) {
      this
    } else {
      new AddrMode(width, reg, base, scale, index, disp + delta, symbol, prefix)
    }
  }

  def indexed(elemSize: Width, index: GPR) = {
    assert(this.scale == null && this.index == null)
    assert(couldBeScaled(elemSize))
    new AddrMode(width, reg, base, elemSize, index, disp, symbol, prefix)
  }

  ///////////////////////////////////////////////////
  //             AddrMode string representation
  ///////////////////////////////////////////////////

  override def toString: String = {
    if (isRegister) {
      return asRegister.toString
    }

    def hex(x: Int) = s"${x.toHexString}H"
    def concat(prefix: Any, sep: String, suffix: Any) = (prefix, suffix) match {
      case (null, null) => null
      case (null, y) => s"$y"
      case (x, null) => s"$x"
      case (x, y) => s"$x$sep$y"
    }

    var body = concat(symbol, " + ", base)
    if (index != null) {
      val scaleStr = if (scale == WPTR) "AddrSize" else s"${scale.nbytes}"
      body = concat(body, " + ", s"$scaleStr*$index")
    }
    if (body == null) {
      body = hex(disp)
    } else if (disp > 0) {
      body = s"$body + ${hex(disp)}"
    } else if (disp < 0) {
      body = s"$body - ${hex(-disp)}"
    }
    val wstr = if (width == NO_WIDTH) null else widthToString(width)
    concat(wstr, " ", concat(prefix, ":", s"[$body]"))
  }
}
