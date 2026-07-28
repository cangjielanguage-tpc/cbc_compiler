/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import com.huawei.excelsior.jet.compiler.Env.targetOS
import xscala.util.StringOps.asciiIsWhitespace

class QuotedStringTokenizer(line: String) extends Iterator[String] {
  override val size: Int = line.length
  private var curPos: Int = 0
  private val useLinuxWorkaroundForJET3007 = targetOS.isLinux

  private def removeQuotes(src: String): String = {
    if (!useLinuxWorkaroundForJET3007) {
      return src
    }
    var i = src.indexOf('"')
    if (i == -1) {
      return src
    }
    val foo = new StringBuilder
    val l = src.length
    i = 0
    while (i < l) {
      if ((src.charAt(i) != '"') || (i != 0 && src.charAt(i - 1) == '\\')) {
        foo.append(src.charAt(i))
      }
      i += 1
    }
    foo.toString
  }

  override def hasNext = {
    while (curPos < length && line.charAt(curPos).asciiIsWhitespace) {
      curPos += 1
    }
    curPos < length
  }

  /** null if eof */
  override def next(): String = {
    while (curPos < length && line.charAt(curPos).asciiIsWhitespace) {
      curPos += 1
    }
    if (curPos >= length) {
      return null
    }
    if (line.charAt(curPos) == '"') {
      curPos += 1
      if (curPos >= length) {
        return "\""
      }
      val startToken = curPos
      while (curPos < length && line.charAt(curPos) != '"') {
        curPos += 1
      }
      if (curPos >= length) {
        return line.substring(startToken)
      }
      curPos += 1
      line.substring(startToken, curPos - 1)
    } else {
      val startToken = curPos
      while (curPos < length && !line.charAt(curPos).asciiIsWhitespace) {
        //JET-3007 fix: on Linux quoted strings should be catched as single argument
        // and quotes removed later
        if (useLinuxWorkaroundForJET3007 && line.charAt(curPos) == '"' && (curPos == 0 || line.charAt(curPos - 1) != '\\')) {
          var i = curPos + 1
          while (i < length && line.charAt(i) != '"' && (line.charAt(i - 1) != '\\')) {
            i += 1
          }
          if (i < length) {
            curPos = i
          }
        }
        curPos += 1
      }
      if (curPos >= length) {
        return removeQuotes(line.substring(startToken))
      }
      removeQuotes(line.substring(startToken, curPos))
    }
  }
}
