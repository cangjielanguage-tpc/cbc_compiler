/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

object Float16 {
  def bits(value: Double): Int = {
    val raw = java.lang.Double.doubleToRawLongBits(value)
    val sign = ((raw >>> 48) & 0x8000).toInt
    val exponent = ((raw >>> 52) & 0x7ff).toInt
    val fraction = raw & 0x000fffffffffffffL

    if (exponent == 0x7ff) {
      val payload = if (fraction == 0) 0 else ((fraction >>> 42).toInt | 0x200)
      return sign | 0x7c00 | payload
    }

    val unbiased = exponent - 1023
    if (unbiased > 15) return sign | 0x7c00
    if (unbiased < -25) return sign

    val significand = fraction | (1L << 52)
    val shift = if (unbiased >= -14) 42 else 28 - unbiased
    val truncated = significand >>> shift
    val remainder = significand & ((1L << shift) - 1)
    val halfway = 1L << (shift - 1)
    val increment = remainder > halfway || (remainder == halfway && (truncated & 1) != 0)
    val rounded = truncated.toInt + (if (increment) 1 else 0)
    val exponentBits = if (unbiased >= -14) (unbiased + 14) << 10 else 0
    sign | (exponentBits + rounded)
  }
}
