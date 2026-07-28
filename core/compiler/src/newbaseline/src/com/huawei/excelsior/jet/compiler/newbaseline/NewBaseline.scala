/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline

import com.huawei.excelsior.jet.compiler.Pass.Backend
import com.huawei.excelsior.jet.compiler.CodeUnit
import com.huawei.excelsior.jet.compiler.Compiler
import com.huawei.excelsior.jet.compiler.{Env, Environment}
import com.huawei.excelsior.jet.compiler.newbaseline.platforms.PlatformConfig
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodAJCallKind}

class NewBaseline(env: Environment, platformConfig: PlatformConfig) extends Compiler(env) {

  private val generator = new MethodGenerator(env, platformConfig)

  private def isAJMethod(method: Method) = {
    !method.isManaged || method.isAJReplaced || method.isInlineAllAndRemove
      || method.getAJCallKind != MethodAJCallKind.NORMAL
  }

  override def genCode(codeUnit: CodeUnit): Unit = {
    if (env.getPass != Backend) {
      return
    }

    val method = codeUnit.method
    assert(!method.isNative, "should be compiled by wrapper-compiler")
    assert(!isAJMethod(method), "baseline cannot compile AJ")
    assert(!method.getDeclaringClass.isCangjieType, "baseline cannot compile Cangjie")
    assert(!method.getDeclaringClass.isXScalaType, "baseline cannot compile XScala")
    assert(Env.isJIT, "baseline only available as Java JIT")

    if (codeUnit.isVersionedMethod) {
      generator.genVersionedMethod(codeUnit)
    } else {
      generator.genNormalMethod(method)
    }
  }

  override def printFinalStatistics(): Unit = {}
}
