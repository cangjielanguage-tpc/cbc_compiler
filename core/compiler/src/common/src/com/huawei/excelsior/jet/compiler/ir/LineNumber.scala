/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

/** Constants and methods related to line numbers.
  *
  * @author alexm
  */
object LineNumber {

  /** Line number is not known initially. */
  val UNKNOWN = -1

  // Avoid using -2 value because it's used to mark native methods in JDK
  // (see java.lang.StackTraceElement.isNativeMethod).

  /** Line number was known but it cannot be represented due to technical limitations. */
  val UNREPRESENTABLE = -3

  /** Not a valid line number. */
  val INVALID = Integer.MIN_VALUE

  def isValid(lineNumber: Int) = lineNumber == UNKNOWN || lineNumber == UNREPRESENTABLE || lineNumber >= 0

  def isKnown(lineNumber: Int) = lineNumber >= 0
}