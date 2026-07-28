/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.Environment.{JC_STANDALONE, TARGET_CPU_ARCH, TARGET_OS}
import com.huawei.excelsior.jet.compiler.abi.cbc.PlatformCBC
import com.huawei.excelsior.jet.compiler.{Compiler, Environment}

/** Platform-independent implementations of abstract methods of [[AOTStarter]].
  * Could be used on any base platform with command line option.
  */
class CBCStarterImpl extends AOTStarter {
  private val targetArch = if (JC_STANDALONE) TARGET_CPU_ARCH else Arch.CBC

  override protected def getPlatform = new PlatformCBC(TARGET_OS, targetArch, JC_STANDALONE)

  override protected def getOptPlatformConfig =
    new com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfigCBC

  override protected def getWrappersPlatformConfig = null
}
