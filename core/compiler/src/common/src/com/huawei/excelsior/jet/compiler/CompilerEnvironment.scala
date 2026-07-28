/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.common.XString

/**
 * Compiler environment, part of the IR universe.
 */
trait CompilerEnvironment {

  /** Returns environment for current compilation session. */
  def env: Environment

  implicit def _implicitEnv: Environment = env

  /** Returns type provider for current compilation session. */
  implicit def typeProvider: TypeProvider = env.getTypeProvider

  def statsGlobal: Stats

  def stage[A](st: Stage)(action: => A) = env.stage(st)(action)

  def xstr(s: String) = XString.xstr(s)
}
