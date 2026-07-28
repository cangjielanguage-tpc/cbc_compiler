/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

object Errors {
  class Error(message: String, cause: Throwable) extends Exception(message, cause) {
    def this(message: String) = {
      this(message, null)
    }
  }

  /** Check some requirement of the bitcode/bitstream format. */
  private[bitcode] def require(condition: Boolean, msg: String, args: Any*): Unit = {
    if (!condition) error(s"invalid bitstream: $msg", args: _*)
  }

  /** Check some limitation of this implementation which could be eliminated on demand. */
  private[bitcode] def hopeThat(condition: Boolean, msg: String, args: Any*): Unit = {
    if (!condition) error(s"unexpected bitstream: $msg", args: _*)
  }

  private[bitcode] def error[T](msg: String, args: Any*): Nothing =
    throw new Errors.Error(msg.format(args: _*))
}
