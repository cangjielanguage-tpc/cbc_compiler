/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64

import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D
import com.huawei.excelsior.jet.assembler.arm64.VFPRegister.D.*
import com.huawei.excelsior.jet.compiler.abi.{CallingConvention, CallingConventionCache, RegFile}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv

/** ARM64 calling conventions.
  *
  * @author gatimosh
  * @author conwor
  */
object CallingConventionArm64 extends CallingConventionCache[X, D] {
  /** Sequence of [[X]] in ABI-sensitive order. In exact this order registers will be saved in prologue (first one
    * will have the lowest address).
    *
    * Please note: in [[ABIAmd64]] the order is reversed: first one will have the highest address. TODO: JET-16801
    *
    * This order is complicated with the following details:
    *   1. [[X29]] and [[LR]] must be the last ones to have the highest addresses to support System V like frame pointer
    *      and have access to caller frame descriptor
    */
  val iRegsInABIOrder = Array(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, IP0, IP1, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, LR)

  val volatileIRegs   = Array(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X18, IP0, IP1, LR)

  val jetIRegs = RegFile(
    availableInABIOrder = iRegsInABIOrder,
    volatiles = volatileIRegs,
    headArea = Array(X0, X1, X2, X3, X4, X5)
  )

  val linuxIRegs = RegFile(
    availableInABIOrder = iRegsInABIOrder,
    volatiles = volatileIRegs,
    headArea = Array(X0, X1, X2, X3, X4, X5, X6, X7)
  )

  val fRegs = RegFile(
    availableInABIOrder = D.values,
    volatiles = Array(D0, D1, D2, D3, D4, D5, D6, D7, D16, D17, D18, D19, D20, D21, D22, D23, D24, D25, D26, D27, D28, D29, D30, D31),
    headArea = Array(D0, D1, D2, D3, D4, D5, D6, D7)
  )

  /** Registers volatile in any call. */
  val alwaysVolatile = Array[AnyReg](IP0, IP1, LR)

  protected def create(sourceCC: CallConv): CallingConvention[X, D] = {
    val iRegs = if (sourceCC.isJET) jetIRegs else linuxIRegs
    CallingConvention(sourceCC, iRegs, fRegs, alwaysVolatile)
  }
}
