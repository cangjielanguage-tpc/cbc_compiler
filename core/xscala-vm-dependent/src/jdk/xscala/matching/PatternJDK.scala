/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

private[matching] final class PatternJDK(p: String) extends Pattern {
  private lazy val javaPattern = java.util.regex.Pattern.compile(p)

  override def matcher(s: String) = new Matcher {
    lazy val javaMatcher = javaPattern.matcher(s)

    private inline def wrapExceptions[T](action: => T): T = {
      try (action) catch {
        case e: IllegalStateException => throw Matcher.IllegalStateException(e.getMessage)
      }
    }

    def find: Boolean = javaMatcher.find()
    def matches: Boolean = javaMatcher.matches()

    def groupCount: Int = wrapExceptions { javaMatcher.groupCount() }
    def group(g: Int): String = wrapExceptions { javaMatcher.group(g) }
    def start(g: Int): Int = wrapExceptions { javaMatcher.start(g) }
    def end(g: Int): Int = wrapExceptions { javaMatcher.end(g) }
  }
}
