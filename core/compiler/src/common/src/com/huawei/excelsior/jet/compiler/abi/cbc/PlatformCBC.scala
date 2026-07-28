/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.cbc

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR12, IR7}
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.abi.cbc.PlatformCBC.tailReg
import com.huawei.excelsior.jet.compiler.abi.{ABI, Platform}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import xscala.properties.OS

object PlatformCBC {
  private[PlatformCBC] def tailReg(arch: Arch, isStandalone: Boolean): IR = {
    if (isStandalone) {
      // Tail register is *last* volatile register on each platform by convention.
      arch match {
        case AMD64 | CBC => CallingConventionCBC.iRegs.volatiles.last
        case ARM64 => CallingConventionCBCAArch64.iRegs.volatiles.last
      }
    } else {
      IR12
    }
  }
}

class PlatformCBC(os: OS, targetCPUArch: Arch, isStandalone: Boolean) extends Platform[IR, FR, ABI[IR, FR]](CBC, os,
  stackPointer        = null,
  framePointer        = null,
  linkRegister        = null,
  execEnvRegister     = null,
  tailRegister        = tailReg(targetCPUArch, isStandalone),
  frameMiddleRegister = null,
  frameAlignment      = -1,
  forceFrameAlignment = false,
) {
  override def abi(methodType: MethodType) = {
    targetCPUArch match {
      case CBC => new ABICBC(methodType)
      case AMD64 => new ABICBC(methodType) // Handled by i2c & c2i adapters for interpretation mode only (with same number of param-passing regs)
      case ARM64 => new ABICBCAArch64(methodType)
    }
  }
}
