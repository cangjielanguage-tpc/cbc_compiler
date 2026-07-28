/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.*
import xscala.util.MathUtils.*

/** Helper class for encoding/decoding a 16-bit unsigned immediate value, optionally shifted left by
  * {16, 32, 48} bits no more than register size.
  *
  * Described in C3.3.3
  *
  * @author conwor
  */
object ShiftedImm16 {
  /** Encode given width-th bit `imm` with ShiftedImm16 or returns null, if encoding is impossible. */
  def encodeOrNull(imm0: Long, width: Width): ShiftedImm16 = {
    val nbits = width.nbits
    assert(nbits == 32 || nbits == 64)
    val imm = bits(imm0, 0, nbits - 1)
    var k = 0
    while (k < nbits) {
      val x = bits(imm, k, k + 15)
      if ((x << k) == imm) {
        return new ShiftedImm16(k / 16, x.toInt)
      }
      k += 16
    }
    null
  }

  /** Encode given width-th bit `imm` with ShiftedImm16. */
  def encode(imm: Long, width: Width): ShiftedImm16 = encodeOrNull(imm, width) ensuring (_ != null)

  /** Returns true iff given width-th bit `imm` can be encoded with ShiftedImm16. */
  def canEncode(imm: Long, width: Width): Boolean = encodeOrNull(imm, width) != null

  /** Encode given pair of 'shift' and 16-th immediate `imm16` with ShiftedImm16 or returns null, if encoding is impossible. */
  def encodeOrNull(imm16: Int, shift: Int, width: Width): ShiftedImm16 = {
    assert(width == W32 || width == W64)
    val shiftOk = isAligned(shift, 16) && (shift >= 0 && shift < width.nbits)
    if (isNBits(imm16, 16) && shiftOk) new ShiftedImm16(shift / 16, imm16) else null
  }

  /** Encode given pair of 'shift' and 16-th immediate `imm16` with ShiftedImm16. */
  def encode(imm16: Int, shift: Int, width: Width) = encodeOrNull(imm16, shift, width) ensuring (_ != null)

  /** Returns true iff given pair of 'shift' and 16-th immediate `imm16` can be encoded with ShiftedImm16. */
  def canEncode(imm16: Int, shift: Int, width: Width) = encodeOrNull(imm16, shift, width) != null
}

final class ShiftedImm16 private(val hw: Int, val imm16: Int) {
  assert(isNBits(hw, 2))
  assert(isNBits(imm16, 16))

  /** Returns immediate encoded by this ShiftedImm16. */
  def decode(width: Width): Long = {
    assert(width == W32 || width == W64)
    val shift = hw * 16
    if (width == W64) {
      imm16.toLong << shift
    } else {
      (imm16 << shift).toLong
    }
  }
}
