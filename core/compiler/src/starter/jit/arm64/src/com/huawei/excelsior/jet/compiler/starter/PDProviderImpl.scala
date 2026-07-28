/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.compiler.abi.arm64.PlatformArm64
import com.huawei.excelsior.jet.compiler.starter.JITCompilerProvider.PDProvider
import com.huawei.excelsior.jet.compiler.stubs.arm64.ThunkGeneratorFactoryArm64
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}
import xscala.properties.OS

object PDProviderImpl extends PDProvider {
  override def getPlatform(os: OS) = new PlatformArm64

  override def getThunkGenerator(env: Environment, symbolLinker: SymbolLinker) = new ThunkGeneratorFactoryArm64(env, symbolLinker)
}
