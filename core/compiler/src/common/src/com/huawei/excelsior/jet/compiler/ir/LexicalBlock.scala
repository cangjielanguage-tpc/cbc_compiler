/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

case class LexicalBlock(inlineContext: InlineContext, // TODO-DWARF is it really needed?
                        line: Int, column: Int, outer: LexicalBlock) {

  assert(LineNumber.isValid(line) && ColumnNumber.isValid(column))
  assert((outer == null) || (outer.inlineContext == inlineContext)) // TODO-DWARF or they can differ when real inlining happens?

  def toStringShort: String = s"LB[$line:$column]${
    if (outer != null) s", included into ${outer.toStringShort}" else ""
  }"

  override def toString = (if (inlineContext != null) inlineContext.method.getFullName + " " else "") + toStringShort
}