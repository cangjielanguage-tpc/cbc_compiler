/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64

import com.huawei.excelsior.common.Arch.ARM64
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.assembler.arm64.{IRegister, VFPRegister}
import com.huawei.excelsior.jet.compiler.abi.Platform
import com.huawei.excelsior.jet.compiler.symlevel.MethodType
import xscala.properties.OS.LINUX

class PlatformArm64 extends Platform[IRegister.X, VFPRegister.D, ABIArm64](ARM64, LINUX,
  stackPointer        = SP,
  framePointer        = X29,
  linkRegister        = LR,
  execEnvRegister     = X28,
  tailRegister        = X26,
  frameMiddleRegister = X27,
  frameAlignment      = 16,
  forceFrameAlignment = true,
) {
  override def abi(methodType: MethodType) = new ABIArm64(methodType)
}
