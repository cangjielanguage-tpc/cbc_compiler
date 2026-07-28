/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.compiler.abi.Platform
import com.huawei.excelsior.jet.compiler.stubs.ThunkGeneratorFactory
import com.huawei.excelsior.jet.compiler.{Compiler, Environment, SymbolLinker}
import xscala.properties.OS

object JITCompilerProvider {

  def getCompiler(env: Environment): Compiler = new PDCompilerImpl().getCompiler(env)

  // FIXME: RTConst

  def getPlatform(os: OS) =
    PDProviderImpl.getPlatform(os)

  def getThunkGenerator(env: Environment, symbolLinker: SymbolLinker) =
    PDProviderImpl.getThunkGenerator(env, symbolLinker)

  def initLanguagePack(): Unit = {
    LanguagePackConfig.init()
  }

  trait PDCompiler {
    def getCompiler(env: Environment): Compiler
  }

  trait PDProvider {
    def getPlatform(os: OS): Platform[_, _, _]
    def getThunkGenerator(env: Environment, symbolLinker: SymbolLinker): ThunkGeneratorFactory
  }
}
