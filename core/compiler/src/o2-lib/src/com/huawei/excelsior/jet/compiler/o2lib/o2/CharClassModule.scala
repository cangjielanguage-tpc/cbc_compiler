/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.o2

object CharClassModule {
  def isNumeric(ch: Char) = (ch >= '0') && (ch <= '9')

  def isNumeric(ch: Byte) = (ch >= '0') && (ch <= '9')

  def isLetter(ch: Char) = ((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))

  def isLetter(ch: Byte) = ((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))

  def isUpper(ch: Byte) = (ch >= 'A') && (ch <= 'Z')

  def isWhiteSpace(ch: Char) = (ch == ' ') || (ch == '\t')

  def isWhiteSpace(ch: Byte) = (ch == ' ') || (ch == '\t')
}
