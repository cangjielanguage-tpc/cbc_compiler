/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64

import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.compiler.Env.{addressSize, targetOS}
import com.huawei.excelsior.jet.compiler.abi.{CallingConvention, CallingConventionCache, RegFile}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv
import xscala.properties.OS.{LINUX, WINDOWS}

/** AMD64 calling conventions.
  *
  * @author conwor
  * @author cypok
  * @author paul
  */
object CallingConventionAmd64 extends CallingConventionCache[GPR, XMM] {
  /** Sequence of [[GPR]] in ABI-sensitive order. In exact this order registers will be saved in prologue (first one
    * will have the highest address).
    *
    * Please note: in [[ABIArm64]] the order is reversed: first one will have the lowest address. TODO: JET-16801
    *
    * This order is complicated with the following details:
    *   1. [[RSP]] is excluded, shifting the indices of following registers
    *   1. [[RAX]] and [[RBP]] swapped, because:
    *     I. [[RBP]] must have the highest address (be saved right after caller return address) to support System V
    *        like frame pointer and have access to caller frame descriptor
    *     I. It is easier to swap it with [[RAX]] than shift registers, because of [[RCX]] usage in
    *        `ExceptionHandling_trivialHandler`
    */
  private val iRegsInABIOrder = Array(RBP, RCX, RDX, RBX, RAX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15)

  private val fRegsInABIOrder = XMM.values

  val jetIRegs = RegFile(
    availableInABIOrder = iRegsInABIOrder,
    volatiles = Array(RAX, RCX, RDX, RSI, RDI, R8, R9, R10),
    headArea = Array(RCX, RSI, RDX, RDI, R8, R9)
  )
  val jetFRegs = RegFile(
    availableInABIOrder = fRegsInABIOrder,
    volatiles = Array(XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM8, XMM9),
    headArea = Array(XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM8, XMM9)
  )

  val windowsIRegs = RegFile(
    availableInABIOrder = iRegsInABIOrder,
    volatiles = Array(RAX, RCX, RDX, R8, R9, R10, R11),
    headArea = Array(RCX, RDX, R8, R9)
  )
  val windowsFRegs = RegFile(
    availableInABIOrder = fRegsInABIOrder,
    volatiles = Array(XMM0, XMM1, XMM2, XMM3, XMM4, XMM5),
    headArea = Array(XMM0, XMM1, XMM2, XMM3)
  )

  val linuxIRegs = RegFile(
    availableInABIOrder = iRegsInABIOrder,
    volatiles = Array(RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11),
    headArea = Array(RDI, RSI, RDX, RCX, R8, R9)
  )
  val linuxFRegs = RegFile(
    availableInABIOrder = fRegsInABIOrder,
    volatiles = fRegsInABIOrder,
    headArea = Array(XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7)
  )

  protected def create(sourceCC: CallConv): CallingConvention[GPR, XMM] = {
      val (iRegs, fRegs) = (sourceCC.isJET, targetOS) match {
        case (true, _)    => (jetIRegs, jetFRegs)
        case (_, WINDOWS) => (windowsIRegs, windowsFRegs)
        case (_, LINUX)   => (linuxIRegs, linuxFRegs)
      }
      CallingConvention(sourceCC, iRegs, fRegs, Array.empty)
  }

  val windowsShadowSpaceSize = 32

  lazy val unixRSASize = linuxIRegs.headArea.length * addressSize + linuxFRegs.headArea.length * XMM.SIZE
}
