/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{ImmEXT, Width, getImmext}
import xscala.util.MathUtils.{bitsSigned, isNBitsSigned, rightNBits64, signExtend}

import scala.annotation.tailrec

/** Implements compact encoding of 32/64-bit signed immediate,
  * so that it can be represented as a shorter `iK [+ immext] and rotCnt`.
  *
  * Algorithm's steps by example of 32-bit immediate `0xFFFF_0FFF` (that is actually representable as `0xFFFFFFFF_FFFF_0FFF`):
  *
  * 1) immediate "''normalization''", so that the least significant bit is different from the most significant bit (if possible)
  * <pre>normalized immediate = 0xFFFFFFFF_FFFF_FFF0</pre>
  * 2) finding longest same-bit sequence (for both 0 and 1) and its position
  *
  * <pre>0: longest seq. `size = 4` and positions of seq. (as bit mask) `= 0x8`</pre>
  * <pre>1: longest seq. `size = 28` and positions of seq. (as bit mask) `= 0x80000000`</pre>
  * The following steps are applied for both 0-bit and 1-bit seq (but this example will stick on 1-bit seq.):
  *
  * 3) rotate immediate the way, that same-bit seq. goes first, then unique bits go after, and calculate corresponding rotation amount
  *
  * 4) then we need to adjust rotation count (and resulted immediate) to ISA-12 `rotCnt` step-size `(w.nbits / 16)`
  *
  * so now, we have immediate representation as `0xFFFFFFFF_FFFF_FFF0` and `rotCnt = 0x14`
  *
  * 5) then we choose: if 32-bit original immediate is actually representable more compact way
  * (with old ISA-12 spec. there only one possible compact layout for this -- `i16` with `rotCnt`, without `immhi`),
  * with regard of sign bit (relying on the fact that immediate will be sign-extended after decoding),
  * then we encode it like this
  *
  * as a result, we have `i16 = 0xFFF0` and `rotCnt = 0xA` (as it is `0x14 / (W32.nbits / 16)`)
  */
object SignedImmCompactEncoding {

  case class EncodedImmParts(t4: Int, iK: Int, k: K, immext: Option[ImmEXT]) {
    def encodedImmBits: Int = SignedImmCompactEncoding.encodedImmBits(k)

    def signBit: Int = {
      k match {
        case K0 =>
          (t4 >> 3) & 1
        case K8 | K16 =>
          (iK >> (k.bits - 1)) & 1
      }
    }

    def decodeImm(w: Width): Long = {
      (w: @unchecked) match {
        case Width.W32 =>
          k match {
            case K0 | K8 =>
              assert(k == K8 || iK == 0)
              (if (immext.isDefined) immext.get.decodeImmEXT(w) else 0) + (signExtend((iK << 4) | t4, encodedImmBits).toLong & rightNBits64(w.nbits))
            case K16 =>
              java.lang.Integer.rotateRight((if (immext.isDefined) immext.get.decodeImmEXT(w) else 0).toInt + signExtend(iK, k.bits), t4 * 2).toLong & rightNBits64(w.nbits)
          }
        case Width.W64 =>
          k match {
            case K0 | K8 =>
              assert(k == K8 || iK == 0)
              val low32 = signExtend(((iK << 4) | t4).toLong, encodedImmBits)
              val hi32 = if (immext.isDefined) immext.get.decodeImmEXT(w) else 0
              low32 + hi32
            case K16 =>
              val low32 = bitsSigned(iK.toLong, 0, k.bits - 1)
              val hi32 = if (immext.isDefined) immext.get.decodeImmEXT(w) else 0
              java.lang.Long.rotateRight(low32 + hi32, t4 * 4)
          }
      }
    }
  }

  def encodedImmBits(k: K): Int = k.bits + (k match {
    case K0 | K8 => 4 // in case of immediate encoded into (i0|i8):t4
    case _ => 0
  })

  private case class SameBitsSeqInfo(bitsSize: Int, bitsPoses: Long)
  
  /** Returns `( rotCnt as t4, imm as iK, K, immext )`. */
  def calculateMemoryCompactImm(imm: Long, w: Width): EncodedImmParts = {
    assert(w == Width.W32 || w == Width.W64)
    val shiftStepsCount = 16 // hyper constant (defined by ISA-12 spec.)
    assert(Width.W32.nbits / shiftStepsCount == 2)
    assert(Width.W64.nbits / shiftStepsCount == 4)
    val (normShiftNum, normImm) = differentiateLSBbyMSB(imm, w)
    val (seqsOf0, seqsOf1) = findLongestSeqOfBits(normImm, w)

    def calculateRotCntAndShortImm(sameBitsInfo: SameBitsSeqInfo): (Int, Int, Long) = {
      val shortImm = rotateLeftImm(normImm, w.nbits - getNumberOfSetMSB(sameBitsInfo.bitsPoses), w)
      val shift = (2 * w.nbits - getNumberOfSetMSB(sameBitsInfo.bitsPoses) - normShiftNum) % w.nbits
      assert(shift >= 0 && shift < w.nbits)
      val shiftAdjustCount = (w: @unchecked) match {
        case Width.W32 => (2 - shift % 2) % 2 // shift - (shift / 2) * 2
        case Width.W64 => (4 - shift % 4) % 4 // shift - (shift / 4) * 2
      }
      if (shiftAdjustCount != 0) {
        val newShortImm = rotateLeftImm(shortImm, shiftAdjustCount, w)
        (sameBitsInfo.bitsSize - shiftAdjustCount, (shift + shiftAdjustCount) % w.nbits, newShortImm)
      } else {
        (sameBitsInfo.bitsSize, shift, shortImm)
      }
    }

    val (maxSameBitsSeqsSize, maxRotCnt, minShortImm) =
      Seq(calculateRotCntAndShortImm(seqsOf0), calculateRotCntAndShortImm(seqsOf1)).maxBy(_._1 /* max by sameBitsSeqsSize */)

    def getResult(rotCnt: Int, shortImm: Int, uniqBitsSeqSize: Int): EncodedImmParts = {
      val t4 = rotCnt / (w.nbits / shiftStepsCount)
      val resultIK = bitsSigned(shortImm, 0, uniqBitsSeqSize)
      (uniqBitsSeqSize: @unchecked) match {
        case _ if uniqBitsSeqSize < EncodedImmParts(0, 0, K8, None).encodedImmBits && rotCnt == 0 =>
          (resultIK >> 4) & 0xFF match {
            case result if result != 0 =>
              EncodedImmParts(resultIK & 0xF, result, K8, None)
            case _ =>
              EncodedImmParts(resultIK & 0xF, 0, K0, None)
          }
        case _ if uniqBitsSeqSize < K16.bits => // 0..14, as 15th bit should be in sequence of the same bits (as it is sign bit)
          EncodedImmParts(t4, resultIK & 0xFFFF, K16, None)
      }
    }

    if (maxSameBitsSeqsSize > w.nbits - K16.bits) { // t4 + i0/i8/i16 (condition is `= 16`, as sign bit is required to be the same as each of sameBitsSeq)
      getResult(maxRotCnt, minShortImm.toInt, w.nbits - maxSameBitsSeqsSize)
    } else {
      val newShortImm = rotateRightImm(minShortImm, maxRotCnt, w)
      val lowestPart = newShortImm & rightNBits64(16)
      val immhi = newShortImm >>> 16
      if (isNBitsSigned(lowestPart, encodedImmBits(K0))) { // t4 + i0 + appropriate immext
        val resultIK = (lowestPart & 0xF).toInt
        EncodedImmParts(resultIK, 0, K0, getImmext(immhi + ((resultIK >>> 3) & 1)))
      } else if (isNBitsSigned(lowestPart, encodedImmBits(K8))) { // t4 + i8 + appropriate immext
        val resultIK = ((lowestPart >> 4) & 0xFF).toInt
        EncodedImmParts((lowestPart & 0xF).toInt, resultIK, K8, getImmext(immhi + ((resultIK >>> 7) & 1)))
      } else { // i16 + immext.u/s i48
        val resultIK = (lowestPart & 0xFFFF).toInt
        EncodedImmParts(0, resultIK, K16, getImmext(immhi + ((resultIK >>> 15) & 1)))
      }
    }
  }

  private def rotateRightImm(x: Long, rotCnt: Int, w: Width): Long = (w: @unchecked) match {
    case Width.W32 =>
      java.lang.Integer.rotateRight(x.toInt, rotCnt) & rightNBits64(32)
    case Width.W64 =>
      java.lang.Long.rotateRight(x, rotCnt)
  }

  private def rotateLeftImm(x: Long, rotCnt: Int, w: Width): Long = (w: @unchecked) match {
    case Width.W32 =>
      java.lang.Integer.rotateLeft(x.toInt, rotCnt) & rightNBits64(32)
    case Width.W64 =>
      java.lang.Long.rotateLeft(x, rotCnt)
  }

  /** Calculates the number at which first and last bit are different,
    * and determines corresponding rotation count (to right).
    *
    * @example
    * {{{
    *   0b0001110 => 0b0000111
    *   0b0111000 => 0b1110000
    *   0b0011100 => 0b0000111
    *   ----------------------
    *   0x8000101 => 0xC000080
    * }}}
    * */
  private def differentiateLSBbyMSB(x: Long, w: Width): (Int, Long) = {
    @tailrec
    def differentiateLSBbyMSBImpl(xRighted: Long, xLefted: Long, iterationNum: Int): (Int, Long) = {
      val (newXRighted, newXLefted) = (rotateRightImm(xRighted, 1, w), rotateLeftImm(xLefted, 1, w))
      if ((xRighted & 0x1) != ((xRighted >>> (w.nbits - 1)) & 0x1)) {
        (iterationNum, xRighted)
      } else if ((xLefted & 0x1) != ((xLefted >>> (w.nbits - 1)) & 0x1)) {
        (-iterationNum, xLefted)
      } else {
        differentiateLSBbyMSBImpl(newXRighted, newXLefted, iterationNum + 1)
      }
    }


    w match {
      // When number is repeated '0b01' (or '0b10') sequence
      case Width.W32 if x == 0xAAAAAAAA          || x == 0x55555555          || x == 0 || x == 0xFFFFFFFFFFFFFFFFL => (0, x)
      case Width.W64 if x == 0xAAAAAAAAAAAAAAAAL || x == 0x5555555555555555L || x == 0 || x == 0xFFFFFFFFFFFFFFFFL => (0, x)

      case _ => differentiateLSBbyMSBImpl(x, x, 0)
    }
  }


  /** Returns same bits seqs size and poses relatively for both 0 and 1 bits.
    * The bits poses is a binary mask in which 1 stands only where the largest sequence (of zeroes or ones) begins.
    *
    * @example
    * {{{
    *   0xFFF0 (as i16):
    *
    *   longest seq of zeros: size = 4  and positions = 0x8
    *   number:  0b1111_1111_1111_0000
    *   bitmask: 0b0000_0000_0000_1000
    *   
    *   longest seq of ones:  size = 12 and positions = 0x8000
    *   number:  0b1111_1111_1111_0000
    *   bitmask: 0b1000_0000_0000_0000
    * }}}
    * */
  private def findLongestSeqOfBits(x: Long, w: Width): (SameBitsSeqInfo, SameBitsSeqInfo) = {
    @tailrec
    def findLongestSeqOfBitsImpl(x: Long, xSeqSize: Int): SameBitsSeqInfo = {
      val (newX, newXSeqSize) = ((x & (x << 1)) & rightNBits64(w.nbits), xSeqSize + 1)
      if (newX == 0) {
        SameBitsSeqInfo(newXSeqSize, x)
      } else {
        if (xSeqSize > w.nbits) {
          SameBitsSeqInfo(newXSeqSize, x)
        } else {
          findLongestSeqOfBitsImpl(newX, newXSeqSize)
        }
      }
    }

    val infoFor0 = findLongestSeqOfBitsImpl(~x & rightNBits64(w.nbits), 0)
    val infoFor1 = findLongestSeqOfBitsImpl(x & rightNBits64(w.nbits), 0)
    (infoFor0, infoFor1)
  }

  /** Returns first (idx + 1) of ones in number
    * 
    * @example 0b1 => 1, 0b100 => 3, 0b0 => 0
    */
  private def getNumberOfSetMSB(x: Long): Int = java.lang.Long.numberOfTrailingZeros(x) match {
    case 64 => 0
    case x  => x + 1
  }
}