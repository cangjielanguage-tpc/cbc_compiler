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

/** Helper functions for encoding/decoding of BitMask immediates.
  *
  * Described in J1.1. (aarch64/instrs/integer/bitmasks/DecodeBitMasks)
  * TODO: describe patterns
  *
  * @author orangebyte256
  */
object BitMaskImm {

  private def checkBitUnequals(part: Long, size: Int): Boolean = {
    var unequals = 0
    for (i <- size - 1 until 0 by -1) {
      if (bit(part, i) != bit(part, i - 1)) {
        unequals += 1
        if (unequals > 2) return false
      }
    }
    unequals > 0
  }

  /** Try to encode given width-th bit `imm` with BitMask.
    * In case of success returns encoding. In case of failure returns null.
    */
  private def encodeOrNull(imm: Long, nbits: Int): BitMaskImm = {
    assert(nbits == 32 || nbits == 64)

    def checkPartRepetitions(part: Long, size: Int): Boolean =
      (0 until nbits by size) forall { from => part == bits(imm, from, from + size - 1) }

    var size = 2
    while (size <= nbits) {
      val part = bits(imm, 0, size - 1)
      if (checkPartRepetitions(part, size) && checkBitUnequals(part, size)) {
        var zeroPos = 0
        for (i <- size - 1 until 0 by -1) {
          val nextbit = bit(part, i - 1)
          if (bit(part, i) != nextbit && nextbit == 0) {
            zeroPos = nbits - i
          }
        }
        val logSize = 6 - log2(size) - 1
        var imms = if (logSize > 0) rightNBits32(logSize) else 0
        imms <<= log2(size) + 1
        imms |= bitCount(part) - 1
        return new BitMaskImm(if (size == 64) 1 else 0, zeroPos % size, imms)
      }
      size *= 2
    }
    null
  }

  private def nbits(w: Width) = if (w == WPTR) W64.nbits else w.nbits

  def encodeOrNull(imm: Long, width: Width): BitMaskImm = encodeOrNull(imm, nbits(width))

  /** Encode given width-th bit `imm` with BitMaskImm. */
  def encode(imm: Long, width: Width): BitMaskImm = encodeOrNull(imm, nbits(width)) ensuring (_ != null)

  /** Returns true iff given width-th bit `imm` can be encoded with BitMask. */
  def canEncode(imm: Long, width: Width): Boolean = encodeOrNull(imm, width) != null
}

final class BitMaskImm private[immediates](val N: Int, val immr: Int, val imms: Int) {
  assert(isNBits(N, 1))
  assert(isNBits(immr, 6))
  assert(isNBits(imms, 6))

  /** Returns immediate encoded by this BitMaskImm. */
  def decode(width: Width): Long = {
    assert(width == W32 || width == W64)

    // Compute log2 of element size
    val len = log2(N << 6 | (imms ^ 0x3f))
    assert(1 <= len && len <= (width.log2bytes + 3))

    // Determine S, R parameters
    val levels = rightNBits32(len)

    // For logical immediates an all-ones value of S is reserved
    // since it would generate a useless all-ones result (many times)
    assert((imms & levels) != levels)
    val S = imms & levels
    val R = immr & levels

    val esize = 1 << len
    val welem = rightNBits64(S + 1)

    // Following code implements next pseudo-code from spec: ROR(Replicate(welem), R)
    // It has changes order of operations cause Replicate(ROR(welem, R)) and ROR(Replicate(welem), R) are equals

    // construct value without rotation by repeating pattern (Replicate)
    var wmask = 0L
    for (k <- 0 until 64 by esize) {
      wmask |= welem << k
    }
    // make ROR
    wmask = java.lang.Long.rotateRight(wmask, R)
    if (width == W32) wmask.toInt else wmask
  }
}