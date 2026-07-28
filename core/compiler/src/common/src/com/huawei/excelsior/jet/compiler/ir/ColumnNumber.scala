/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

object ColumnNumber {

  /** Column number is not known initially. */
  val UNKNOWN = -1

  /** Not a valid column number. */
  val INVALID = Integer.MIN_VALUE

  def isValid(columnNumber: Int) = columnNumber == UNKNOWN || columnNumber >= 0

  def isKnown(columnNumber: Int) = columnNumber >= 0
}