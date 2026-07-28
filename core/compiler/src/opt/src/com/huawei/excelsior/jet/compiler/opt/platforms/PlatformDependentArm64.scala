/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.assembler.arm64.{IRegister, VFPRegister}
import com.huawei.excelsior.jet.compiler.CompilerEnvironment
import com.huawei.excelsior.jet.compiler.Env.targetPlatform
import com.huawei.excelsior.jet.compiler.abi.arm64.{ABIArm64, FrameArm64, PlatformArm64}

trait PlatformDependentArm64 extends PlatformDependent { self: CompilerEnvironment =>
  type IREG = IRegister.X
  type FREG = VFPRegister.D
  type ABI = ABIArm64
  type FRAME = FrameArm64
  type Platform = PlatformArm64
  
  def platform = targetPlatform.asInstanceOf[Platform]
}
