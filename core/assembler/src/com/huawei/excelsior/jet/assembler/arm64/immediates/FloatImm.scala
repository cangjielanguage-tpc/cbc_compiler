/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import java.lang.Double.{doubleToRawLongBits, longBitsToDouble}
import java.lang.Float.{floatToRawIntBits, intBitsToFloat}

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.Width.W64
import com.huawei.excelsior.jet.assembler.arm64.immediates.FloatImm.unpack
import xscala.util.MathUtils.*

/** Helper class for encoding/decoding a immediate value for floating-point move instructions.
  *
  * Described in C2.2.3
  *
  * @author orangebyte256
  */
object FloatImm {
  private def getE(N: Int) = if (N == 64) 11 else 8

  private def getF(N: Int) = N - getE(N) - 1

  private def unpack(imm8: Long, N: Int): Long = {
    val E = getE(N)
    val F = getF(N)

    val sign: Long = bit(imm8, 7).toLong
    val exp: Long = (bit(imm8, 6) ^ 1) << (E - 1) |
      replicate(bit(imm8, 6), E - 3) << 2 |
      bits(imm8, 4, 5)
    val frac: Long = bits(imm8, 0, 3) << F - 4
    sign << (E + F) | exp << F | frac
  }

  def encodeOrNull(orig: Double, width: Width): FloatImm = {
    val N = width.nbits
    assert(N == 32 || N == 64)
    val value = if (N == 32) {
      floatToRawIntBits(orig.toFloat) & ~(-1L << 32)
    } else {
      doubleToRawLongBits(orig)
    }
    val imm8 = (bit(value, N - 1) << 7 | bits(value >> getF(N) - 4, 0, 6)).toInt
    if (unpack(imm8, N) == value) new FloatImm(imm8) else null
  }

  /** Encode given `imm` with FloatImm. */
  def encode(imm: Double, width: Width): FloatImm = encodeOrNull(imm, width) ensuring (_ != null)

  /** Returns true iff given `imm` can be encoded with FloatImm. */
  def canEncode(imm: Double, width: Width): Boolean = encodeOrNull(imm, width) != null
}

final class FloatImm private(val imm8: Int) {
  assert(isNBits(imm8, 8))

  /** Returns immediate encoded by this ShiftedImm12. */
  def decode(width: Width): Double = {
    assert(width == W32 || width == W64)
    val bits = unpack(imm8, width.nbits)
    if (width == W32) {
      intBitsToFloat(bits.toInt).toDouble
    } else {
      longBitsToDouble(bits)
    }
  }
}