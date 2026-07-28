/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

import xscala.matching.{Pattern, Regex}

object StringOps {
  extension (s: String) {
    def asciiToUpperCase: String = String(s.toCharArray.mapInPlace(_.asciiToUpperCase))
    def asciiToLowerCase: String = String(s.toCharArray.mapInPlace(_.asciiToLowerCase))

    def asciiCapitalize: String = if (s == null || s.isEmpty || !s.charAt(0).asciiIsLowerCase) {
      s
    } else {
      s.updated(0, s.charAt(0).asciiToUpperCase)
    }

    def toHexOption: Option[Int] = {
      val len = s.length

      if (len == 0) return None

      val first = s.charAt(0)
      val v = first.asciiAsHex

      // TODO: replace with `val (isPositive, hasExplicitSign) = ...` when
      //       ScalaTest compilation mode is supported for xscala-vm-dependent
      var hasExplicitSign = false

      val isPositive = if (v < 0) {
        if (first == '-') { hasExplicitSign = true; false }
        else if (first == '+') { hasExplicitSign = true; true }
        else return None
      } else true

      if (hasExplicitSign && len == 1) return None

      val addBoundary = if (isPositive) -Int.MaxValue else Int.MinValue
      val mulBoundary = addBoundary / 16

      var acc = 0
      var i = if (hasExplicitSign) 1 else 0
      while (i < len) {
        val hex = s.charAt(i).asciiAsHex
        if (hex < 0) return None

        if (acc < mulBoundary) return None
        acc *= 16

        if (acc < addBoundary + hex) return None
        acc -= hex

        i += 1
      }

      Some(if (isPositive) -acc else acc)
    }

    def toUnsignedHexOption: Option[Long] = {
      var result = 0L
      for (x <- s) {
        result <<= 4
        val temp = x.asciiAsHex
        if (temp == -1) {
          return None
        }
        result += temp
      }
      Some(result)
    }

    def r: Regex = new Regex(Pattern.compile(s))
  }

  extension (c: Char) {
    def asciiToUpperCase: Char = if ('a' <= c && c <= 'z') {
      (c - 'a' + 'A').toChar
    } else {
      c
    }

    def asciiToLowerCase: Char = if ('A' <= c && c <= 'Z') {
      (c - 'A' + 'a').toChar
    } else {
      c
    }

    def asciiIsWhitespace: Boolean = c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f'

    def asciiIsLowerCase: Boolean = 'a' <= c && c <= 'z'

    def asciiIsDecimal: Boolean = '0' <= c && c <= '9'

    def asciiAsDecimal: Int = if ('0' <= c && c <= '9') c - '0' else -1

    def asciiAsHex: Int = {
      if ('0' <= c && c <= '9') {
        c - '0'
      } else if ('a' <= c && c <= 'f') {
        c - 'a' + 10
      } else if ('A' <= c && c <= 'F') {
        c - 'A' + 10
      } else {
        -1
      }
    }
  }
}
