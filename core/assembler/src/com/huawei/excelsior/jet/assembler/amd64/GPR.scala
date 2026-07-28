/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.amd64.Register.Helpers

/** General purpose register of AMD64 architecture.
  *
  * @author paul
  * @author cypok
  */
enum GPR extends Register with IReg {
  case RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI,
       R8,  R9,  R10, R11, R12, R13, R14, R15

  val addrModeFromRegister = AddrMode.fromRegister(this)
  val addrModeAsBase = AddrMode.baseFromGPR(this)

  override def width = Width.W64
  override def code = ordinal

  override def toAddrMode = addrModeFromRegister

  override def as(width: Width) = Helpers.getByWidth(width, this)
  override def asReg32 = Helpers.getReg32(this)
  override def asReg16 = Helpers.getReg16(this)
  override def asReg8 = Helpers.getReg8(this)
  override def asHighReg8 = Helpers.getHighReg8(this)
  override def asGPR = this

  override def asXMM = shouldNotCallThis("this register is not XMM")
}

object GPR {
  def byCode(code: Int) = fromOrdinal(code)
}
