/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.compiler.CompilerEnvironment
import com.huawei.excelsior.jet.compiler.Env.targetPlatform
import com.huawei.excelsior.jet.compiler.abi.amd64._

trait PlatformDependentAmd64 extends PlatformDependent { self: CompilerEnvironment =>
  type IREG = GPR
  type FREG = XMM
  type ABI = ABIAmd64
  type FRAME = FrameAmd64
  type Platform = PlatformAmd64
  
  def platform = targetPlatform.asInstanceOf[Platform]
}
