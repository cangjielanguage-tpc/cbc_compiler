/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.platforms

import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.compiler.CompilerEnvironment
import com.huawei.excelsior.jet.compiler.Env.targetPlatform
import com.huawei.excelsior.jet.compiler.abi.{ABI => XABI}
import com.huawei.excelsior.jet.compiler.abi.cbc.{FrameCBC, PlatformCBC}

trait PlatformDependentCBC extends PlatformDependent { self: CompilerEnvironment =>
  type IREG = IR
  type FREG = FR
  type ABI = XABI[IR, FR]
  type FRAME = FrameCBC
  type Platform = PlatformCBC

  def platform = targetPlatform.asInstanceOf[Platform]
}
