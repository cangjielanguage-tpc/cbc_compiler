/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.compiler.abi.amd64.PlatformAmd64
import com.huawei.excelsior.jet.compiler.starter.JITCompilerProvider.PDProvider
import com.huawei.excelsior.jet.compiler.stubs.amd64.ThunkGeneratorFactoryAmd64
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}
import xscala.properties.OS

object PDProviderImpl extends PDProvider {
  def getPlatform(os: OS) = new PlatformAmd64(os)

  def getThunkGenerator(env: Environment, symbolLinker: SymbolLinker) = new ThunkGeneratorFactoryAmd64(env, symbolLinker)
}
