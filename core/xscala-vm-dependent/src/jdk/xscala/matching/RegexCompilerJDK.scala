/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

import java.util.regex.PatternSyntaxException

private[xscala] final class RegexCompilerJDK extends RegexCompiler {
  override def compile(p: String) = try {
    new PatternJDK(p)
  } catch {
    case e: PatternSyntaxException => throw xscala.matching.PatternSyntaxException(e.getPattern, e.getDescription, e.getIndex)
  }
}
