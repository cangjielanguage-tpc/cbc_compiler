/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

object CommandLineArguments {
  private val booleanOption = """(?:\+[A-Za-z_][A-Za-z0-9_]*|-[A-Za-z_][A-Za-z0-9_]*(?::?[+-])?)""".r

  def expand(arg: String): Seq[String] = {
    val options = arg.trim.split("\\s+").toSeq
    // Runners may pass several boolean options in one argument. Do not split
    // filenames or equations, whose values may legitimately contain spaces.
    if (options.size > 1 && options.forall(booleanOption.matches)) options else Seq(arg)
  }
}
