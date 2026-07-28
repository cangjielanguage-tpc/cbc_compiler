/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Location.SubReg
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.amd64.Register.Helpers

/** 8-bit width AMD64 register.
  *
  * @author paul
  * @author cypok
  */
enum Register8 extends Register with SubReg[GPR] {
  case AL,  CL,  DL,   BL,   SPL,  BPL,  SIL,  DIL,
       R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B,
       AH,  CH,  DH,   BH

  private val addrMode = AddrMode.fromRegister(this)

  override def width = Width.W8
  override def code = if (isHigh) (ordinal - 16) + 4 else ordinal

  override def toAddrMode = addrMode

  override def as(width: Width) = Helpers.getByWidth(width, this)

  override def asGPR = {
    assert(!isHigh)
    Helpers.getGPR(this)
  }

  override def asReg32 = {
    assert(!isHigh)
    Helpers.getReg32(this)
  }

  override def asReg16 = {
    assert(!isHigh)
    Helpers.getReg16(this)
  }

  override def asReg8 = {
    assert(!isHigh)
    this
  }

  override def asHighReg8 = {
    assert(!isHigh)
    Helpers.getHighReg8(this)
  }

  override def asXMM = shouldNotCallThis("this register is not XMM")

  /** Returns whether this 8-bit register is a high part of teh corresponding 16-bit register, e.g. 'ah'. */
  def isHigh = ordinal >= 16

  /** Returns the full 64-bit register that contains this (high or low) 8-bit register as its part. */
  override def host = if (isHigh) GPR.byCode(ordinal - 16) else asGPR
}

object Register8 {

  /** Gets low-part register by it's code. */
  def byCode(code: Int) = {
    assert(code < 16)
    fromOrdinal(code)
  }

  /** Gets high-part register by it's code. */
  def highByCode(code: Int) = {
    assert(code < 4)
    fromOrdinal(code + 16)
  }
}
