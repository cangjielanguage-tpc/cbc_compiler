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

/** Wrapper around a compiler that is used to generate wrappers for special kinds of methods. */
class CompilerWithAJWrappers(env: Environment, baseCompiler: Compiler, platformConfig: PlatformConfig) extends Compiler(env) {

  override def printFinalStatistics(): Unit = baseCompiler.printFinalStatistics()

  override def genCode(codeUnit: CodeUnit): Unit = {
    val method = codeUnit.method
    assert(!method.isAbstract)
    if (!genWrapper(method)) {
      baseCompiler.genCode(codeUnit)
    }
  }

  private def genWrapper(method: Method): Boolean = {
    if (method.isAJReplaced) {
      // Native methods may be AJReplaced, do not try to generate native wrappers for them
      return false
    }

    if (!(method.isCallToManaged || method.isHookInvoker || method.isNative)) {
      return false
    }

    if (env.getPass != Backend) {
      return true
    }

    def ctx(useFramePointer: Boolean) = platformConfig.makeGeneratorContext(env, method, useFramePointer)

    if (method.isCallToManaged) {
      new CallToManagedGenerator(ctx(false)).genBody()
    } else if (method.isHookInvoker) {
      new HookInvokerGenerator(ctx(true)).genBody()
    } else if (method.isNative) {
      new NativeWrapperGenerator(ctx(false)).genBody()
    } else {
      shouldNotReachHere()
    }
    true
  }
}
