/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

/** Compiled regular expression that can be matched using the [[unapplySeq]] method.
  *
  * @see [[Pattern]], [[Matcher]]
  */
class Regex private[xscala] (val pattern: Pattern) {
  def unapplySeq(s: String): Option[List[String]] = {
    val m = pattern.matcher(s)
    if (runMatcher(m)) Some(List.tabulate(m.groupCount) { i => m.group(i + 1) })
    else None
  }

  private def runMatcher(m: Matcher): Boolean = m.find
}
