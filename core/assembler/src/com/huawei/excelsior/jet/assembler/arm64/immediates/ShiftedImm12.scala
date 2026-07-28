/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64.immediates

import xscala.util.MathUtils.bits
import xscala.util.MathUtils.isNBits

/** Helper class for encoding/decoding a 12-bit unsigned immediate value, optionally shifted left by 12 bits.
  *
  * Described in C3.3.1
  *
  * @author conwor
  */
object ShiftedImm12 {
  def encodeOrNull(imm: Long): ShiftedImm12 = {
    if (isNBits(imm, 12)) {
      new ShiftedImm12(imm.toInt, 0)
    } else if ((bits(imm, 0, 11) == 0) && isNBits(imm >> 12, 12)) {
      new ShiftedImm12((imm >> 12).toInt, 1)
    } else null
  }

  /** Encode given `imm` with ShiftedImm12. */
  def encode(imm: Long): ShiftedImm12 = encodeOrNull(imm) ensuring (_ != null)

  /** Returns true iff given `imm` can be encoded with ShiftedImm12. */
  def canEncode(imm: Long): Boolean = encodeOrNull(imm) != null
}

final class ShiftedImm12 private(val imm12: Int, val shift: Int) {
  assert(isNBits(imm12, 12))
  assert(isNBits(shift, 1)) // 0 for shift by 0 bits, 1 for shift by 12 bits (convenient for encoding)

  /** Returns immediate encoded by this ShiftedImm12. */
  def decode = imm12.toLong << (shift * 12)
}