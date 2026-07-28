/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import xscala.io.stderr

/** HLIR error reporting utility.
  *
  * Provides convenient interface for accumulating and reporting parsing errors
  * without immediately crashing the process.
  *
  * @author liontiger
  */
class HLIRErrorReporter(source: String) {
  private var _hasErrors = false

  def parsingError(msg: String, token: AnyRef): Unit = {
    _hasErrors = true
    HLIRErrorReporter.parsingError(msg, token, source)
  }

  def require(condition: Boolean, msg: => String, token: => AnyRef): Unit = {
    if (!condition) {
      parsingError(msg, token)
    }
  }

  def hasErrors = {
    _hasErrors
  }
}

object HLIRErrorReporter {
  def fatal(msg: String, token: AnyRef, source: String): Nothing = {
    parsingError(msg, token, source)
    stderr.printStackTrace(new RuntimeException())
    sys.exit(1)
    shouldNotReachHere()
  }

  def assertion(condition: Boolean, msg: => String, token: => AnyRef, source: => String): Unit = {
    if (!condition) {
      fatal(msg, token, source)
    }
  }

  def parsingError(msg: String, token: AnyRef, source: String): Unit = {
    error(msg)
    stderr.println(s"  while parsing $token at $source")
    stderr.println()
  }

  def error(msg: String): Unit = {
    stderr.println(s"HLIR ERROR: $msg")
  }

  /** Convenient wrapper which allows accumulating of parsing errors in `action`
    * without immediately crashing the process.
    *
    * If there were any errors reported during execution of `action`,
    * the process will exit immediately after.
    */
  def withErrorReporter[T](source: String)(action: HLIRErrorReporter => T): T = {
    val reporter = new HLIRErrorReporter(source)
    try {
      action(reporter)
    } finally {
      if (reporter.hasErrors) {
        sys.exit(1)
      }
    }
  }
}
