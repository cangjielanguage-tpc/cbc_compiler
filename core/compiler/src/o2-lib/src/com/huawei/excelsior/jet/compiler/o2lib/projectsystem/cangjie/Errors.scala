/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.cangjie

import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{ErrMsg, xiEnvModule as env}

object Errors {

  val NOT_A_PROJECT_ERROR       = ErrMsg473
  val MODULE_NOT_FOUND_ERROR    = ErrMsg2001
  val WRONG_EXTENSION_ERROR     = ErrMsg2002
  val NO_MAIN_ERROR             = ErrMsg2003
  val MULTIPLE_MAINS_ERROR      = ErrMsg2004
  val WRONG_MAIN_EQUATION_ERROR = ErrMsg2005

  val NOT_SUPPORTED_DIRECTIVE_FOR_CANGJIE_PROJECT = ErrMsg2006

  def error(err: ErrMsg, x: Any*): Unit = {
    env.errors.fault(err, x: _*)
  }
}
