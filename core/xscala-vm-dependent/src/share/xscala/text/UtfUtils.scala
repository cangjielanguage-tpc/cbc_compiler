/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

private object UtfUtils {

  inline val EXPECTED_BOM = '\uFEFF'
  inline val REVERSED_BOM = '\uFFFE'
  inline val MIN_HIGH_SURROGATE = '\uD800'
  inline val MAX_HIGH_SURROGATE = '\uDBFF'
  inline val MIN_LOW_SURROGATE = '\uDC00'
  inline val MAX_LOW_SURROGATE = '\uDFFF'
  inline val MIN_SUPPLEMENTARY_CODE_POINT = 0x10000

  inline def toCodePoint(high: Char, low: Char): Int = {
    ((high << 10) + low) + (MIN_SUPPLEMENTARY_CODE_POINT - (MIN_HIGH_SURROGATE << 10) - MIN_LOW_SURROGATE)
  }

  inline def isHighSurrogate(ch: Char): Boolean = (ch >= MIN_HIGH_SURROGATE) && (ch <= MAX_HIGH_SURROGATE)

  inline def isLowSurrogate(ch: Char): Boolean = (ch >= MIN_LOW_SURROGATE) && (ch <= MAX_LOW_SURROGATE)

  inline def isSurrogate(ch: Char): Boolean = (ch >= MIN_HIGH_SURROGATE) && (ch <= MAX_LOW_SURROGATE)

  inline def highSurrogate(codePoint: Int): Char =
    ((codePoint >>> 10) + (MIN_HIGH_SURROGATE - (MIN_SUPPLEMENTARY_CODE_POINT >>> 10))).toChar

  inline def lowSurrogate(codePoint: Int): Char =
    ((codePoint & 0x3FF) + MIN_LOW_SURROGATE).toChar

}
