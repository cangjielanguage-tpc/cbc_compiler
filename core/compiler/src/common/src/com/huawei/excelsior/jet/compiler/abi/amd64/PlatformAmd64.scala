/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64

import com.huawei.excelsior.common.Arch.AMD64
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.compiler.abi.Platform
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import xscala.properties.OS

class PlatformAmd64(os: OS) extends Platform[GPR, XMM, ABIAmd64](AMD64, os,
  stackPointer        = RSP,
  framePointer        = RBP,
  linkRegister        = null,
  execEnvRegister     = R15,
  tailRegister        = R14,
  frameMiddleRegister = R13,
  frameAlignment      = 16,
  forceFrameAlignment = false,
) {
  override def abi(methodType: MethodType) = new ABIAmd64(methodType)
}
