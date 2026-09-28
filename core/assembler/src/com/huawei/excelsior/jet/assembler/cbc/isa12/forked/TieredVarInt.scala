/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import java.lang.Long.numberOfLeadingZeros
import xscala.util.MathUtils

/** Utility methods for encoding of variable length integers.
 *  Uses bit-encoding.
 *
 *  A value is written as a sequence of chunks of growing size. Every chunk but
 *  the last one is prefixed with a continuation bit, so the reader knows when
 *  the value ends:
 *
 *  {{{
 *  | tier | chunk | payload | value bits | stream bits | bytes | up to  |
 *  |------|-------|---------|------------|-------------|-------|--------|
 *  |  1   |   4   |    3    |      3     |      4      |   1   |   7    |
 *  |  2   |   4   |    3    |      6     |      8      |   1   |  2^6-1 |
 *  |  3   |   8   |    7    |     13     |     16      |   2   |  2^13-1|
 *  |  4   |  16   |   15    |     28     |     32      |   4   |  2^28-1|
 *  |  5   |  36   |   36    |     64     |     68      |   9   |  2^64-1|
 *  }}}
 *
 *  The last tier needs no continuation bit, hence its whole chunk is payload
 *  and the full unsigned 64-bit range is encodable. The most significant value
 *  bits go into the first chunk, i.e. values are big-endian, matching the bit
 *  order of [[BitStream]]. The tail of the last byte is zero padded.
 *
 *  Signed values are encoded via [[zigzag]] mapping onto unsigned ones.
 */
object TieredVarInt {

  /** Bit size of every tier chunk, including the continuation bit. */
  val chunkBits: Array[Int] = Array(4, 4, 8, 16, 36)

  /** Bit size of the payload of every tier chunk. The last chunk carries no
    * continuation bit. */
  val payloadBits: Array[Int] = Array(3, 3, 7, 15, 36)

  /** Cumulative amount of value bits encoded by tiers up to and including
    * every tier. */
  val valueBits: Array[Int] = scanCumulative(payloadBits)

  /** Cumulative amount of stream bits used by tiers up to and including every
    * tier, that is, the exact bit size of a value of that tier. */
  val streamBits: Array[Int] = scanCumulative(chunkBits)

  /** Index of the last tier. */
  val lastTier: Int = chunkBits.length - 1

  private def scanCumulative(sizes: Array[Int]): Array[Int] = {
    var acc = 0
    sizes.map { s => acc += s; acc }
  }

  /** Returns zig-zag encoded `x`, mapping signed values onto unsigned ones so
    * that small magnitudes stay small. */
  def zigzag(x: Long): Long = (x << 1) ^ (x >> 63)

  /** Returns a value restored from its [[zigzag]] form. */
  def unzigzag(u: Long): Long = (u >>> 1) ^ -(u & 1L)

  /** Returns an amount of significant bits of the unsigned value `u`. */
  def significantBits(u: Long): Int = 64 - numberOfLeadingZeros(u)

  /** Returns a tier `u` is encoded with. */
  def tierOf(u: Long): Int = {
    val bits = significantBits(u)
    var tier = 0
    while (bits > valueBits(tier)) {
      tier += 1
    }
    tier
  }

  /** Returns a bit size of the unsigned value `u`, that is, an amount of bits
    * [[encode]] writes for it. */
  def bitSize(u: Long): Int = streamBits(tierOf(u))

  /** Returns a byte size of the unsigned value `u`, rounding it up. */
  def byteSize(u: Long): Int = (bitSize(u) + 7) / 8

  /** Returns a bit size of the signed value `x`. */
  def bitSizeSigned(x: Long): Int = bitSize(zigzag(x))

  /** Returns a byte size of the signed value `x`, rounding it up. */
  def byteSizeSigned(x: Long): Int = byteSize(zigzag(x))

  /** Writes the unsigned value `u` into the bit stream. */
  def encode(bs: BitStream, u: Long): BitStream = {
    val tier = tierOf(u)
    val total = valueBits(tier)
    var shift = total
    var t = 0
    while (t <= tier) {
      shift -= payloadBits(t)
      if (t != lastTier) {
        bs.w1(t != tier)
      }
      bs.write((u >>> shift) & MathUtils.rightNBits64(payloadBits(t)), payloadBits(t))
      t += 1
    }
    bs
  }

  /** Reads the unsigned value written by [[encode]]. */
  def decode(in: BitReader): Long = {
    var value = 0L
    var t = 0
    var last = false
    while (!last) {
      last = t == lastTier || in.r1() == 0
      value = (value << payloadBits(t)) | in.read(payloadBits(t))
      t += 1
    }
    value
  }

  /** Writes the signed value `x` into the bit stream. */
  def encodeSigned(bs: BitStream, x: Long): BitStream = encode(bs, zigzag(x))

  /** Reads the signed value written by [[encodeSigned]]. */
  def decodeSigned(in: BitReader): Long = unzigzag(decode(in))

  /** Appends the unsigned value `u` to the byte stream, padding it up to byte
    * boundary. */
  def write(stream: ByteStream, u: Long): ByteStream = stream.bitsRounded(encode(_, u))

  /** Appends the signed value `x` to the byte stream, padding it up to byte
    * boundary. */
  def writeSigned(stream: ByteStream, x: Long): ByteStream = write(stream, zigzag(x))
}

/**
  * Counterpart of [[BitStream]]: reads back bits written in most significant
  * bit first order.
  */
class BitReader(bytes: Seq[Int], private var _pos: Int = 0) {
  private var bitPos: Int = 0

  /** An offset of the next bit to read, in bits from the stream start. */
  def pos: Int = _pos * 8 + bitPos

  def read(bits: Int): Long = {
    assert(0 <= bits && bits <= 64)
    var value = 0L
    var remaining = bits
    while (remaining > 0) {
      val available = 8 - bitPos
      val taken = math.min(available, remaining)
      val byte = bytes(_pos) & 0xff
      val chunk = (byte >>> (available - taken)) & MathUtils.rightNBits32(taken)
      value = (value << taken) | chunk
      remaining -= taken
      bitPos += taken
      if (bitPos == 8) {
        bitPos = 0
        _pos += 1
      }
    }
    value
  }

  def r1(): Int = read(1).toInt

  /** Skips padding up to the next byte boundary. */
  def align(): BitReader = {
    if (bitPos != 0) {
      bitPos = 0
      _pos += 1
    }
    this
  }
}
