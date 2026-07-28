/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js

object StringTokenizerModule {

  /** StringTokenizer is a class that provides simple linear tokenization of a String. */
  class StringTokenizer(private val str: XString, delimiters: String) {

    private var pos: Int = 0
    private val len: Int = str.length
    private val dlm: XString = js.newJString(delimiters)

    /**
      * Returns the next number of tokens in the String using
      * the current delimiter set.  This is the number of times
      * nextToken() can return before it will generate an exception.
      * Use of this routine to count the number of tokens is faster
      * than repeatedly calling nextToken() because the substrings
      * are not constructed and returned for each token.
      */
    def countTokens(): Int = {
      var count = 0
      var pos = this.pos
      val length = this.len
      while (pos < length) {
        pos = skipDelimiter(pos)
        if (pos == length) {
          return count
        }
        pos = skipToken(pos)
        count += 1
      }
      count
    }

    /** Returns the next token of the String or null. */
    def nextToken(): XString = {
      this.pos = skipDelimiter(this.pos)
      if (this.pos == this.len) {
        return null
      }
      val start = this.pos
      this.pos = skipToken(this.pos)
      this.str.substring(start, this.pos)
    }

    /** Returns true if more tokens exist. */
    def hasMoreTokens: Boolean = {
      this.pos = skipDelimiter(this.pos)
      this.pos < this.len
    }

    private def skipDelimiter(start: Int): Int = {
      val length: Int = this.len
      var pos = start
      while (pos < length && this.dlm.indexOf(this.str.charAt(pos)) >= 0) {
        pos += 1
      }
      pos
    }

    private def skipToken(start: Int): Int = {
      val length: Int = this.len
      var pos = start
      while (pos < length && this.dlm.indexOf(this.str.charAt(pos)) < 0) {
        pos += 1
      }
      pos
    }
  }


  def newStringTokenizer(s: XString, delim: String): StringTokenizer = StringTokenizer(s, delim)

}
