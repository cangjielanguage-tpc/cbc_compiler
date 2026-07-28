/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

class StringTokenizer(
  private val string: String,
  private val delimiters: Array[Char] = Array(' ', '\t', '\n', '\r', '\f')
) {
  private var position: Int = 0

  def hasMoreTokens: Boolean = {
    val newPosition = skipDelimiters(this.position)
    this.position = newPosition
    position != string.length
  }

  def nextToken(): String = {
    val start = skipDelimiters(this.position)
    if (start == string.length) {
      throw new scala.NoSuchElementException()
    }
    val tokenEnd = skipToken(start)
    this.position = tokenEnd
    string.substring(start, tokenEnd)
  }

  def countTokens: Int = {
    var count = 0
    var pos = this.position
    val length = string.length
    while (pos < length) {
      pos = skipDelimiters(pos)
      if (pos == length) {
        return count
      }
      pos = skipToken(pos)
      count += 1
    }
    count
  }

  private def skipDelimiters(position: Int): Int = {
    var pos = position
    val length = string.length
    while (pos < length && delimiters.contains(string.charAt(pos))) {
      pos += 1
    }
    pos
  }

  private def skipToken(position: Int): Int = {
    var pos = position
    val length = string.length
    while (pos < length && !delimiters.contains(string.charAt(pos))) {
      pos += 1
    }
    pos
  }
}
