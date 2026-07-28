/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.common.Environment.{LANGUAGE_PACK, TARGET_OS}
import com.huawei.excelsior.common.LanguagePack.CANGJIE
import com.huawei.excelsior.jet.compiler.abi.amd64.PlatformAmd64
import com.huawei.excelsior.jet.compiler.{Compiler, Environment}

import xscala.reflect.ClassManipulation
import ClassManipulation.*

/** Platform-dependent implementations of abstract methods of [[AOTStarter]]. */
class PDStarterImpl extends AOTStarter {
  override protected def getPlatform = new PlatformAmd64(TARGET_OS)

  override protected def getOptPlatformConfig =
    new com.huawei.excelsior.jet.compiler.opt.platforms.PlatformConfigAmd64

  override protected def getWrappersPlatformConfig =
    com.huawei.excelsior.jet.compiler.wrappers.platforms.PlatformConfigAmd64
}
