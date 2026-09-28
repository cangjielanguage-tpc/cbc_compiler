/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{BitReader, BitStream, ByteStream, TieredVarInt}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * @author liontiger
  */
class TieredVarIntSuite extends AnyFunSuite {

  private class CollectStream extends ByteStream {
    val bytes: ArrayBuffer[Int] = ArrayBuffer.empty

    override def write8(x: Int): ByteStream = {
      bytes += (x & 0xff)
      this
    }
  }

  private def encodeBytes(u: Long): Seq[Int] =
    TieredVarInt.encode(new BitStream, u).sendRounded()

  private def encodeSignedBytes(x: Long): Seq[Int] =
    TieredVarInt.encodeSigned(new BitStream, x).sendRounded()

  /** Encoded bytes of `values` written one after another into a single bit stream. */
  private def encodeAll(values: Seq[Long]): Seq[Int] = {
    val bs = new BitStream
    values.foreach(TieredVarInt.encode(bs, _))
    bs.sendRounded()
  }

  private def decodeAll(bytes: Seq[Int], count: Int): Seq[Long] = {
    val in = new BitReader(bytes)
    val result = (0 until count).map(_ => TieredVarInt.decode(in))
    in.align()
    in.pos shouldBe bytes.length * 8
    result
  }

  private def hex(bytes: Seq[Int]): Seq[Int] = bytes.map(_ & 0xff)

  private val assertionsEnabled: Boolean = classOf[BitStream].desiredAssertionStatus

  /** The largest value of every tier and the first value of the next one. */
  private val boundaryValues: Seq[Long] = {
    val upper = TieredVarInt.valueBits.toIndexedSeq.map(bits => if (bits == 64) -1L else (1L << bits) - 1)
    upper.flatMap(u => Seq(u - 1, u)) ++ upper.dropRight(1).map(_ + 1)
  }.distinct

  private val interestingValues: Seq[Long] = (boundaryValues ++ Seq(
    0x1L, 0xfL, 0x1234L, 0x12345L, 0x12345678L, 0x123456789abcdefL,
    0x5555555555555555L, 0xaaaaaaaaaaaaaaaaL, 0x8000000000000000L, Long.MaxValue, -1L
  )).distinct

  private val randomValues: Seq[Long] = {
    val rnd = new Random(20260928L)
    Array.fill(2000)(rnd.nextLong()).toIndexedSeq
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Tier table

  test("tierTable") {
    TieredVarInt.chunkBits.toIndexedSeq shouldBe IndexedSeq(4, 4, 8, 16, 36)
    TieredVarInt.payloadBits.toIndexedSeq shouldBe IndexedSeq(3, 3, 7, 15, 36)
    TieredVarInt.valueBits.toIndexedSeq shouldBe IndexedSeq(3, 6, 13, 28, 64)
    TieredVarInt.streamBits.toIndexedSeq shouldBe IndexedSeq(4, 8, 16, 32, 68)
    TieredVarInt.lastTier shouldBe 4
    // every chunk but the last one is prefixed with a continuation bit
    (TieredVarInt.chunkBits.toIndexedSeq zip TieredVarInt.payloadBits).dropRight(1).foreach { case (chunk, payload) =>
      chunk shouldBe payload + 1
    }
  }

  test("tierOfKnownValues") {
    TieredVarInt.tierOf(0L) shouldBe 0
    TieredVarInt.tierOf(7L) shouldBe 0
    TieredVarInt.tierOf(8L) shouldBe 1
    TieredVarInt.tierOf(63L) shouldBe 1
    TieredVarInt.tierOf(64L) shouldBe 2
    TieredVarInt.tierOf(8191L) shouldBe 2
    TieredVarInt.tierOf(8192L) shouldBe 3
    TieredVarInt.tierOf((1L << 28) - 1) shouldBe 3
    TieredVarInt.tierOf(1L << 28) shouldBe 4
    TieredVarInt.tierOf(-1L) shouldBe 4
  }

  test("tierOfBoundaries") {
    for (tier <- TieredVarInt.chunkBits.indices) {
      val bits = TieredVarInt.valueBits(tier)
      val max = if (bits == 64) -1L else (1L << bits) - 1
      withClue(s"tier=$tier: ") {
        TieredVarInt.tierOf(max) shouldBe tier
        if (tier != TieredVarInt.lastTier) {
          // the first value of the next tier
          TieredVarInt.tierOf(max + 1) shouldBe tier + 1
        }
        if (tier != 0) {
          // the last value of the previous tier
          val prevMax = (1L << TieredVarInt.valueBits(tier - 1)) - 1
          TieredVarInt.tierOf(prevMax) shouldBe tier - 1
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Size calculation

  test("sizeKnownValues") {
    TieredVarInt.bitSize(0L) shouldBe 4
    TieredVarInt.bitSize(7L) shouldBe 4
    TieredVarInt.bitSize(8L) shouldBe 8
    TieredVarInt.bitSize(63L) shouldBe 8
    TieredVarInt.bitSize(64L) shouldBe 16
    TieredVarInt.bitSize(8191L) shouldBe 16
    TieredVarInt.bitSize(8192L) shouldBe 32
    TieredVarInt.bitSize(1L << 28) shouldBe 68
    TieredVarInt.bitSize(-1L) shouldBe 68

    TieredVarInt.byteSize(0L) shouldBe 1
    TieredVarInt.byteSize(63L) shouldBe 1
    TieredVarInt.byteSize(64L) shouldBe 2
    TieredVarInt.byteSize(8192L) shouldBe 4
    TieredVarInt.byteSize(1L << 28) shouldBe 9
  }

  test("sizeMatchesEncoding") {
    for (u <- interestingValues ++ randomValues) {
      val bytes = encodeBytes(u)
      withClue(s"u=$u: ") {
        TieredVarInt.byteSize(u) shouldBe bytes.length
        // written bits are padded up to the next byte at most
        assert(TieredVarInt.bitSize(u) <= bytes.length * 8)
        assert(bytes.length * 8 - TieredVarInt.bitSize(u) < 8)
        val in = new BitReader(bytes)
        TieredVarInt.decode(in) shouldBe u
        in.pos shouldBe TieredVarInt.bitSize(u)
      }
    }
  }

  test("signedSize") {
    for (x <- interestingValues ++ randomValues) {
      val bytes = encodeSignedBytes(x)
      withClue(s"x=$x: ") {
        TieredVarInt.byteSizeSigned(x) shouldBe bytes.length
        TieredVarInt.bitSizeSigned(x) shouldBe TieredVarInt.bitSize(TieredVarInt.zigzag(x))
      }
    }
    // zig-zag keeps small magnitudes within the first tier
    for (x <- -3L to 3L) {
      TieredVarInt.bitSizeSigned(x) shouldBe 4
    }
    TieredVarInt.bitSizeSigned(7L) shouldBe 8
    TieredVarInt.bitSizeSigned(-8L) shouldBe 8
    TieredVarInt.bitSizeSigned(8L) shouldBe 8
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Encoding

  test("encodeKnownValues") {
    val expected: Seq[(Long, Seq[Int])] = Seq(
      0L                    -> Seq(0x00),
      1L                    -> Seq(0x10),
      7L                    -> Seq(0x70),
      8L                    -> Seq(0x90),
      15L                   -> Seq(0x97),
      63L                   -> Seq(0xf7),
      64L                   -> Seq(0x88, 0x40),
      4096L                 -> Seq(0xc8, 0x00),
      8191L                 -> Seq(0xff, 0x7f),
      8192L                 -> Seq(0x88, 0x80, 0x20, 0x00),
      0x12345L              -> Seq(0x88, 0x82, 0x23, 0x45),
      (1L << 28) - 1        -> Seq(0xff, 0xff, 0x7f, 0xff),
      (1L << 28)            -> Seq(0x88, 0x80, 0x80, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00),
      0x123456789abcdefL    -> Seq(0x88, 0xa4, 0xb4, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0),
      Long.MaxValue         -> Seq(0xbf, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0),
      -1L                   -> Seq(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0)
    )
    for ((u, bytes) <- expected) {
      withClue(s"u=$u: ") {
        encodeBytes(u) shouldBe hex(bytes)
      }
    }
  }

  test("encodeSignedKnownValues") {
    val expected: Seq[(Long, Seq[Int])] = Seq(
      0L                    -> Seq(0x00),
      1L                    -> Seq(0x20),
      -1L                   -> Seq(0x10),
      3L                    -> Seq(0x60),
      -3L                   -> Seq(0x50),
      7L                    -> Seq(0x96),
      -7L                   -> Seq(0x95),
      8L                    -> Seq(0xa0),
      -8L                   -> Seq(0x97),
      31L                   -> Seq(0xf6),
      -32L                  -> Seq(0xf7),
      63L                   -> Seq(0x88, 0x7e),
      -64L                  -> Seq(0x88, 0x7f),
      Long.MaxValue         -> Seq(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xe0),
      Long.MinValue         -> Seq(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xf0)
    )
    for ((x, bytes) <- expected) {
      withClue(s"x=$x: ") {
        encodeSignedBytes(x) shouldBe hex(bytes)
      }
    }
  }

  test("paddingIsZero") {
    // tier 1 uses 4 bits, the rest of the byte is zero padding
    encodeBytes(1L) shouldBe Seq(0x10)
    // tier 5 uses 68 bits, padded up to 72
    TieredVarInt.bitSize(1L << 28) shouldBe 68
    encodeBytes(1L << 28).last shouldBe 0x00
  }

  test("decodeRoundtripUnsigned") {
    for (u <- interestingValues ++ randomValues) {
      withClue(s"u=$u: ") {
        TieredVarInt.decode(new BitReader(encodeBytes(u))) shouldBe u
      }
    }
  }

  test("decodeRoundtripSigned") {
    val values = (-300L to 300L) ++
      Seq(0L, 1L, -1L, 7L, -8L, 1L << 27, -(1L << 27) - 1, Long.MaxValue, Long.MinValue) ++ randomValues
    for (x <- values) {
      withClue(s"x=$x: ") {
        TieredVarInt.decodeSigned(new BitReader(encodeSignedBytes(x))) shouldBe x
      }
    }
  }

  test("multipleValuesInOneStream") {
    val values = Seq(0L, 7L, 8L, 63L, 64L, 8191L, 8192L, 1L << 28, -1L, 0x123456789abcdefL)
    decodeAll(encodeAll(values), values.length) shouldBe values
  }

  test("multipleRandomValuesInOneStream") {
    val values = randomValues.take(200)
    decodeAll(encodeAll(values), values.length) shouldBe values
  }

  test("writeToByteStream") {
    val stream = new CollectStream
    val values = Seq(1L, 63L, 8192L, 1L << 28, 7L)
    values.foreach(TieredVarInt.write(stream, _))
    stream.bytes.length shouldBe values.map(TieredVarInt.byteSize).sum
    // every value is byte aligned, so it can be read back from its own reader
    var pos = 0
    for (u <- values) {
      withClue(s"u=$u: ") {
        TieredVarInt.decode(new BitReader(stream.bytes.toSeq, pos)) shouldBe u
      }
      pos += TieredVarInt.byteSize(u)
    }
  }

  test("writeSignedToByteStream") {
    val stream = new CollectStream
    val values = Seq(-1L, 0L, 1L, -8L, 63L, -(1L << 27) - 1)
    values.foreach(TieredVarInt.writeSigned(stream, _))
    stream.bytes.length shouldBe values.map(TieredVarInt.byteSizeSigned).sum
    var pos = 0
    for (x <- values) {
      withClue(s"x=$x: ") {
        TieredVarInt.decodeSigned(new BitReader(stream.bytes.toSeq, pos)) shouldBe x
      }
      pos += TieredVarInt.byteSizeSigned(x)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Zig-zag

  test("zigzag") {
    val expected: Seq[(Long, Long)] = Seq(
      0L -> 0L, -1L -> 1L, 1L -> 2L, -2L -> 3L, 2L -> 4L,
      63L -> 126L, -64L -> 127L, Long.MaxValue -> -2L, Long.MinValue -> -1L
    )
    for ((x, u) <- expected) {
      withClue(s"x=$x: ") {
        TieredVarInt.zigzag(x) shouldBe u
        TieredVarInt.unzigzag(u) shouldBe x
      }
    }
  }

  test("zigzagIsInvertible") {
    for (x <- interestingValues ++ randomValues) {
      TieredVarInt.unzigzag(TieredVarInt.zigzag(x)) shouldBe x
    }
    for (u <- interestingValues ++ randomValues) {
      TieredVarInt.zigzag(TieredVarInt.unzigzag(u)) shouldBe u
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // BitStream

  test("bitStreamOneByte") {
    val bs = new BitStream
    bs.w4(0xa).w4(0x5)
    bs.size shouldBe 8
    bs.send() shouldBe Seq(0xa5)
  }

  test("bitStreamRounded") {
    val bs = new BitStream
    bs.w1(true).w2(2)
    bs.size shouldBe 3
    bs.sendRounded() shouldBe Seq(0xc0)
  }

  test("bitStreamMoreThanByte") {
    val bs = new BitStream
    bs.w16(0x1234).w36(0x555555555L).w8(0xff)
    bs.size shouldBe 60
    bs.sendRounded() shouldBe Seq(0x12, 0x34, 0x55, 0x55, 0x55, 0x55, 0x5f, 0xf0)
  }

  test("bitStreamFullLong") {
    val bs = new BitStream
    bs.write(-1L, 64)
    bs.size shouldBe 64
    bs.send() shouldBe Seq.fill(8)(0xff)
  }

  test("bitStreamSendChecksRounding") {
    if (!assertionsEnabled) {
      cancel("assertions are disabled in the test JVM, run it with -ea")
    }
    val rounded = new BitStream
    rounded.w4(1).w4(2)
    rounded.send() shouldBe Seq(0x12)

    val trailing = new BitStream
    trailing.w1(true)
    assertThrows[AssertionError](trailing.send())
    trailing.sendRounded() shouldBe Seq(0x80)
  }

  test("bitStreamWriteRejectsOverflow") {
    if (!assertionsEnabled) {
      cancel("assertions are disabled in the test JVM, run it with -ea")
    }
    assertThrows[AssertionError](new BitStream().write(8, 3))
    assertThrows[AssertionError](new BitStream().write(-1L, 63))
    assertThrows[AssertionError](new BitStream().write(1L, 65))
  }

  test("byteStreamBits") {
    val stream = new CollectStream
    stream.bits(_.w4(0xf).w4(0x1)).bitsRounded(_.w1(true).w1(false))
    stream.bytes.toSeq shouldBe Seq(0xf1, 0x80)

    val rounded = new CollectStream
    rounded.bitsRounded(_.w1(true).w4(0xf))
    rounded.bytes.toSeq shouldBe Seq(0xf8)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // BitReader

  test("bitReader") {
    val bytes = Seq(0xa5, 0xc0, 0xff)
    val in = new BitReader(bytes)
    in.read(4) shouldBe 0xa
    in.read(4) shouldBe 0x5
    in.pos shouldBe 8
    in.read(3) shouldBe 6
    in.align()
    in.pos shouldBe 16
    in.r1() shouldBe 1
    in.read(7) shouldBe 0x7f
    in.pos shouldBe bytes.length * 8
  }

  test("bitReaderOffset") {
    val in = new BitReader(Seq(0x00, 0x97, 0xb7, 0x00), 1)
    TieredVarInt.decode(in) shouldBe 15L
    in.pos shouldBe 16
    in.align()
    in.pos shouldBe 16
    TieredVarInt.decode(in) shouldBe 31L
    in.pos shouldBe 24
  }

  test("bitReaderFuzz") {
    val rnd = new Random(20260928L)
    for (trial <- 0 until 200) {
      val widths = (0 until 50).map(_ => 1 + rnd.nextInt(64))
      val values = widths.map(bits => if (bits == 64) rnd.nextLong() else rnd.nextLong() & ((1L << bits) - 1))
      val bs = new BitStream
      (values zip widths).foreach { case (v, bits) => bs.write(v, bits) }
      val total = widths.sum
      bs.size shouldBe total
      val bytes = bs.sendRounded()
      bytes.length shouldBe (total + 7) / 8
      val in = new BitReader(bytes)
      withClue(s"trial=$trial: ") {
        (values zip widths).foreach { case (v, bits) => in.read(bits) shouldBe v }
      }
      in.pos shouldBe total
    }
  }
}
