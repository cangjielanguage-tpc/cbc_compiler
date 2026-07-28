/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{ErrMsg, xiEnvModule as env}

object ErrorMessages {
  val msg_error_in_file = ErrMsg414 /* %S %d %S */
  val msg_error_in_file_ps = ErrMsg441 /* %S %d %d %S */
  val msg_syntax_error = ErrMsg428
  val msg_type_error = ErrMsg442
  val msg_not_defined = ErrMsg443
  val msg_undefined_option = ErrMsg320
  val msg_error_in_command_line = ErrMsg430 /* %S %S */

  private[projectsystem] def configResToMsg(res: Int): ErrMsg = {
    res match {
      case env.wrongSyntax =>
        msg_syntax_error
      case env.unknownOption =>
        msg_undefined_option
      case env.definedOption =>
        ErrMsg321
      case env.unknownEquation =>
        ErrMsg322
      case env.definedEquation =>
        ErrMsg323
      case env.defineOptionWhenEquationDefined =>
        ErrMsg326
      case env.defineEquationWhenOptionDefined =>
        ErrMsg327
      case _ =>
        throw new AssertionError
    }
  }
}
