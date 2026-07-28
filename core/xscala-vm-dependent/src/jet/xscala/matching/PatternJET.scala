/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

import xscala.matching.Pcre.*

import scala.annotation.static

private[matching] final class PatternJET(p: String) extends Pattern {
  private implicit def anchor: Anchor[Pattern] = this

  private val handle = Pcre.compile(if (p != null) p else throw new NullPointerException())

  override def finalize(): Unit = {
    Pcre.delete(handle)
  }

  override def matcher(s: String) = new Matcher {
    private implicit def anchor: Anchor[Matcher] = this

    private val scope = SearchScope(handle, s)

    override def finalize(): Unit = {
      scope.free()
    }

    private var first: Int = -1
    private var last: Int = -1
    private var nextOffset: Int = 0

    private inline def searchSucceeded = first >= 0
    private inline def ensureSearchSucceeded[T](inline action: => T): T = {
      if (!searchSucceeded) {
        throw Matcher.IllegalStateException("No match available")
      }
      action
    }

    override def find = {
      val info = handle.search(scope)(nextOffset, fullMatch = false)
      first = info.first // set it, even if not matched, to mark it's not succeeded
      val found = searchSucceeded
      if (found) {
        last = info.last
        nextOffset = info.nextOffset
      }
      found
    }

    override def matches = {
      val info = handle.search(scope)(0, fullMatch = true)
      first = info.first // set it, even if not matched, to mark it's not succeeded
      val matched = searchSucceeded
      if (matched) {
        last = info.last
        nextOffset = info.nextOffset
      }
      matched
    }

    override def groupCount = scope.groupCount
    override def group(g: Int) = ensureSearchSucceeded { s.substring(start(g), end(g)) }
    override def start(g: Int) = ensureSearchSucceeded { if (g == 0) first else scope.start(g) }
    override def end(g: Int) = ensureSearchSucceeded { if (g == 0) last else scope.end(g) }
  }
}
