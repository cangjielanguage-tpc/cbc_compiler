/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Pass.Backend
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.wrappers.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.{CodeUnit, Compiler, Environment}

/** Wrapper around a compiler that is used to generate wrappers for Java native methods. */
class CompilerWithNativeWrappers(env: Environment, baseCompiler: Compiler, platformConfig: PlatformConfig) extends Compiler(env) {

  override def printFinalStatistics(): Unit = baseCompiler.printFinalStatistics()

  override def genCode(codeUnit: CodeUnit): Unit = {
    val method = codeUnit.method
    assert(!method.isAbstract)
    assert(!method.isAJReplaced && !method.isCallToManaged && !method.isHookInvoker)

    if (env.getPass != Backend) {
      return
    }

    def ctx(useFramePointer: Boolean) = platformConfig.makeGeneratorContext(env, method, useFramePointer)

    if (method.isNative) {
      new NativeWrapperGenerator(ctx(false)).genBody()
    } else {
      baseCompiler.genCode(codeUnit)
    }
  }
}
