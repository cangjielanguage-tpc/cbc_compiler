/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AsmError
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Width

/** AMD64 register.
  *
  * @author paul
  * @author cypok
  */
trait Register extends Location {

  /** Returns the unique integer code for the register that can be used in serialization. */
  def code: Int

  /** Returns addressing mode to be used for the register accesses. */
  def toAddrMode: AddrMode

  /** Returns the version of this register for the given width, e.g. 32-bit lower part `eax` of 64-bit register `rax`.
    *
    * Shall only be invoked for general-purpose registers (not XMM ones) that have corresponding register
    * of the given width.
    */
  def as(width: Width): Register

  /** Returns 32-bit version of this register, e.g. `eax`.
    *
    * Shall only be invoked for general-purpose registers, not XMM ones.
    */
  def asReg32: Register32

  /** Returns 16-bit version of this register, e.g. `ax`.
    *
    * Shall only be invoked for general-purpose registers, not XMM ones.
    */
  def asReg16: Register16

  /** Returns (lower) 8-bit version of this register, e.g. `al`.
    *
    * Shall only be invoked for general-purpose registers, not XMM ones.
    */
  def asReg8: Register8

  /** Returns 8-bit register that represents high half of 16-bit register returned by [[# asReg16 ( )]], e.g. `ah`.
    *
    * Shall only be invoked for general-purpose registers, not XMM ones.
    */
  def asHighReg8: Register8

  /** Returns full 64-bit version of this general-purpose register, e.g. `rax`.
    *
    * Shall only be invoked for general-purpose registers, not XMM ones.
    */
  def asGPR: GPR

  /** Returns this XMM register casted to [[XMM]].
    *
    * Shall only be invoked for XMM register, not general-purpose ones.
    */
  def asXMM: XMM
}

object Register {

  /** Helper functions for conversions of registers. */
  object Helpers {

    def getByWidth(width: Width, reg: Register) = width match {
      case Width.W8   => getReg8(reg)
      case Width.W16  => getReg16(reg)
      case Width.W32  => getReg32(reg)
      case Width.W64  => getGPR(reg)
      case Width.WPTR => getGPR(reg)
      case _          => throw new AsmError(s"Unsupported register width: $width")
    }

    def getGPR(reg: Register)      = GPR.byCode(reg.code)
    def getReg32(reg: Register)    = Register32.byCode(reg.code)
    def getReg16(reg: Register)    = Register16.byCode(reg.code)
    def getReg8(reg: Register)     = Register8.byCode(reg.code)
    def getHighReg8(reg: Register) = Register8.highByCode(reg.code)
  }
}
