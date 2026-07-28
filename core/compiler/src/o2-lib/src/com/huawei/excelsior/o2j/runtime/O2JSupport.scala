/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.o2j.runtime

object O2JSupport {
  // logic left shift of 32-bit integer a by n bits (signed), integer length is specified
  def logicLeftShift(a: Int, length: Int, n: Int): Int = {
    val mask = if (length == 32) {
      0xFFFFFFFF
    } else {
      (1 << length) - 1
    }

    if (n > 0) {
      if (n >= length) {
        0
      } else {
        (a << n) & mask
      }
    } else {
      if (n <= -length) {
        0
      } else {
        (a >>> (-n)) & mask
      }
    }
  }


  // Modula-2/Oberon-2 DIV operation
  def div(a: Int, b: Int): Int = {
    assert(b > 0)
    var c = a / b
    if ((a < 0) && (c * b > a)) c -= 1
    c
  }

  // Modula-2/Oberon-2 MOD operation
  def mod(a: Int, b: Int): Int = {
    assert(b > 0)
    var c = a % b
    if ((a < 0) && (c < 0)) c += b
    c
  }

  // converts 8-bit char value to int
  def convCharToInt(ch: Char) = {
    assert((ch >= 0) && (ch <= 255))
    ch.toInt
  }

  // converts int to 8-bit char value
  def convIntToChar(v: Int) = {
    assert((v >= 0) && (v <= 255))
    v.toChar
  }

  // creates a string constant in byte[] string generation mode
  def byteArrStringConst(s: String): Array[Byte] = {
    val b = new Array[Byte](s.length)
    for (i <- 0 until s.length) {
      val ch = s.charAt(i)
      assert((ch >= 0) && (ch <= 127)) // only ASCII chars allowed in const strings
      b(i) = ch.toByte
    }
    b
  }
}
