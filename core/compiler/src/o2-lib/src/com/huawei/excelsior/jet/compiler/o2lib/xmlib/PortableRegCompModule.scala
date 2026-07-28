/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import com.huawei.excelsior.jet.common.XString
import xscala.matching.{Pattern, PatternSyntaxException}

object PortableRegCompModule {
  class Expr(private[PortableRegCompModule] val pattern: Pattern)

  case class CompileRes(reg: Expr, res: Int)

  def compile(expr: XString): CompileRes = {
    //convert wildcard to regexp
    val wstr = expr.toString
    val sb = new java.lang.StringBuilder
    wstr foreach {
      case '*' => sb.append(".*")
      case '?' => sb.append(".")
      case '.' => sb.append("\\.")
      case c   => sb.append(c)
    }

    try {
      CompileRes(new Expr(Pattern.compile(sb.toString)), expr.length)
    } catch {
      case e: PatternSyntaxException => CompileRes(null, -e.index)
    }
  }

  def Match(re: Expr, s: XString, pos: Int): Boolean = re.pattern != null && re.pattern.matcher(s.toString.substring(pos)).matches
}
