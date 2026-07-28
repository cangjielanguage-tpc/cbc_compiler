/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

object StdLibCompilerModule {
  private var impl: () => Boolean = _

  def setImpl(impl: () => Boolean): Unit = {
    StdLibCompilerModule.impl = impl
  }

  def checkOrCompileStdLib(): Boolean = impl()
}
