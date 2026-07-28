/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.Blame
import com.huawei.excelsior.jet.compiler.opt.jprof.fields.Fields

/** Compiler interface to the results of execution profiling.
  *
  * @author ijorch
  */
object Profile {

  def env_=(newEnv: Environment): Unit = {
    assert(_env == null)
    _env = newEnv
  }
  def env: Environment = {
    assert(_env != null)
    _env
  }
  private var _env: Environment = _

  /** Section of execution profile that blames methods for being often executed. */
  def blame = Blame

  /** Section of execution profile storing run-time info about fields. */
  def fields = Fields
}
