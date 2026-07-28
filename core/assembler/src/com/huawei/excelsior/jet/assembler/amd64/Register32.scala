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

/** 32-bit width AMD64 register.
  *
  * @author paul
  * @author cypok
  */
enum Register32 extends Register with SubReg[GPR] {
  case EAX, ECX, EDX,  EBX,  ESP,  EBP,  ESI,  EDI,
       R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D

  private val addrMode = AddrMode.fromRegister(this)

  override def width = Width.W32
  override def code = ordinal

  override def toAddrMode = addrMode

  override def as(width: Width) = Helpers.getByWidth(width, this)
  override def asGPR = Helpers.getGPR(this)
  override def asReg32 = this
  override def asReg16 = Helpers.getReg16(this)
  override def asReg8 = Helpers.getReg8(this)
  override def asHighReg8 = Helpers.getHighReg8(this)

  override def asXMM = shouldNotCallThis("this register is not XMM")

  /** Returns the full 64-bit register that contains this 32-bit register as its part. */
  override def host = asGPR
}

object Register32 {
  def byCode(code: Int) = fromOrdinal(code)
}
