/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.java

import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.annotation.nowarn

/** Box of java-specific tools for debug support. */
object JavaDebugToolbox {
  // To make it point to the method declaration line
  def methodSourceLine(ca: Method.CodeAttribute): Int = {
    val sourceLine = ca.firstLineNumber // the first line of method body
    if (sourceLine > 0) {
      // It is still not clear why, but when debugging non-CJ apps gdb stops at the first instruction of prolog.
      // It is better to use 'next' command and stop just after prolog at the first (not second) line of the method.
      // So step out one line here.
      sourceLine - 1
    } else {
      sourceLine
    }
  }
}