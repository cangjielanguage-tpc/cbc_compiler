/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common.io

import org.scalatest.Assertions.assertResult
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{an, shouldBe}
import xscala.io.*

/**
  * @author liontiger
  */

class TestLEB128Encoder extends AnyFunSuite {

  private def testEncodeDecode(unsigned: Boolean, values: Int*): Unit = {
    val out = new ByteBuffer()
    for (v <- values) {
      if (unsigned) out.putULEB(v) else out.putSLEB(v)
    }
    val in = DataInput.from(out)
    for (v <- values) {
      val decoded = if (unsigned) in.getULEB() else in.getSLEB()
      decoded shouldBe v
    }
    in.available shouldBe 0
  }

  private def testEncodeDecodeLong(unsigned: Boolean, values: Long*): Unit = {
    val out = new ByteBuffer()
    for (v <- values) {
      if (unsigned) out.putULEB(v) else out.putSLEB(v)
    }
    val in = DataInput.from(out)
    for (v <- values) {
      val decoded = if (unsigned) in.getULEBLong() else in.getSLEBLong()
      decoded shouldBe v
    }
    in.available shouldBe 0
  }

  private def testEncodeLongDecodeInt(unsigned: Boolean, success: Boolean, exceptionExpected: Boolean, values: Long*): Unit = {
    val out = new ByteBuffer()
    for (v <- values) {
      if (unsigned) out.putULEB(v) else out.putSLEB(v)
    }
    val in = DataInput.from(out)
    for (v <- values) {
      if (success) {
        val decoded = if (unsigned) in.getULEB() else in.getSLEB()
        decoded shouldBe v
      } else {
        if (exceptionExpected) {
          an[AssertionError] should be thrownBy {
            if (unsigned) in.getULEB() else in.getSLEB()
          }
        } else {
          val decoded = if (unsigned) in.getULEB() else in.getSLEB()
          assert(decoded != v)
        }
      }
    }
    in.available shouldBe 0
  }

  private def testSizeCalculation(unsigned: Boolean, value: Long): Unit = {
    class Counter(var count: Int) {
      def consume(unused: Int): Unit = count += 1
    }

    val counter = Counter(0)

    if (unsigned) {
      val size = LEB128Encoder.calcSizeULEB128(value)
      LEB128Encoder.encodeULEB128(value, counter.consume)
      counter.count shouldBe size
    } else {
      val size = LEB128Encoder.calcSizeSLEB128(value)
      LEB128Encoder.encodeSLEB128(value, counter.consume)
      counter.count shouldBe size
    }
  }

  private def testOne(unsigned: Boolean): Unit = {
    testEncodeDecode(unsigned, 0)
    testEncodeDecode(unsigned, 1)
    testEncodeDecode(unsigned, 2)
    testEncodeDecode(unsigned, 0x7f)
    testEncodeDecode(unsigned, 0x80)
    testEncodeDecode(unsigned, 0x82)
    testEncodeDecode(unsigned, 0xff)
    testEncodeDecode(unsigned, 0x100)
    testEncodeDecode(unsigned, 0xabc)
    testEncodeDecode(unsigned, 0x12345678)
    testEncodeDecode(unsigned, 0x789abcde)
    testEncodeDecode(unsigned, Integer.MAX_VALUE)
    testEncodeDecode(unsigned, -1)
    testEncodeDecode(unsigned, -2)
    testEncodeDecode(unsigned, -0x7f)
    testEncodeDecode(unsigned, -0x80)
    testEncodeDecode(unsigned, -0x82)
    testEncodeDecode(unsigned, -0xff)
    testEncodeDecode(unsigned, -0x100)
    testEncodeDecode(unsigned, -0xabc)
    testEncodeDecode(unsigned, 0x87654321)
    testEncodeDecode(unsigned, Integer.MIN_VALUE)
    testEncodeDecodeLong(unsigned, 0)
    testEncodeDecodeLong(unsigned, 1)
    testEncodeDecodeLong(unsigned, 2)
    testEncodeDecodeLong(unsigned, 0x7f)
    testEncodeDecodeLong(unsigned, 0x80)
    testEncodeDecodeLong(unsigned, 0x82)
    testEncodeDecodeLong(unsigned, 0xff)
    testEncodeDecodeLong(unsigned, 0x100)
    testEncodeDecodeLong(unsigned, 0xabc)
    testEncodeDecodeLong(unsigned, 0x12345678)
    testEncodeDecodeLong(unsigned, 0x789abcde)
    testEncodeDecodeLong(unsigned, 0x87654321)
    testEncodeDecodeLong(unsigned, 0x6789abcdefL)
    testEncodeDecodeLong(unsigned, 0x456789abcdefL)
    testEncodeDecodeLong(unsigned, 0x23456789abcdefL)
    testEncodeDecodeLong(unsigned, 0x123456789abcdefL)
    testEncodeDecodeLong(unsigned, 0x123456789abcdef0L)
    testEncodeDecodeLong(unsigned, java.lang.Long.MAX_VALUE)
    testEncodeDecodeLong(unsigned, -1)
    testEncodeDecodeLong(unsigned, -2)
    testEncodeDecodeLong(unsigned, -0x7f)
    testEncodeDecodeLong(unsigned, -0x80)
    testEncodeDecodeLong(unsigned, -0x82)
    testEncodeDecodeLong(unsigned, -0xff)
    testEncodeDecodeLong(unsigned, -0x100)
    testEncodeDecodeLong(unsigned, -0xabc)
    testEncodeDecodeLong(unsigned, -0x87654321)
    testEncodeDecodeLong(unsigned, -0x6789abcdefL)
    testEncodeDecodeLong(unsigned, -0x456789abcdefL)
    testEncodeDecodeLong(unsigned, -0x23456789abcdefL)
    testEncodeDecodeLong(unsigned, -0x123456789abcdefL)
    testEncodeDecodeLong(unsigned, -0x123456789abcdef0L)
    testEncodeDecodeLong(unsigned, 0x8123456789abcdefL)
    testEncodeDecodeLong(unsigned, java.lang.Long.MIN_VALUE)

    testEncodeLongDecodeInt(unsigned, true, false, 0)
    testEncodeLongDecodeInt(unsigned, true, false, 1)
    testEncodeLongDecodeInt(unsigned, true, false, 2)
    testEncodeLongDecodeInt(unsigned, true, false, 0x789a)
    testEncodeLongDecodeInt(unsigned, true, false, 0x789abc)
    testEncodeLongDecodeInt(unsigned, true, false, 0x789abcdeL)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -1)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -2)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -0x321)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -0x789a)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -0x789abc)
    testEncodeLongDecodeInt(unsigned, !unsigned, true, -0x789abcde)
    testEncodeLongDecodeInt(unsigned, false, !unsigned, 0x87654321L)
    testEncodeLongDecodeInt(unsigned, false, true, 0x6789abcdefL)
    testEncodeLongDecodeInt(unsigned, false, true, java.lang.Long.MIN_VALUE)
    testEncodeLongDecodeInt(unsigned, false, true, java.lang.Long.MAX_VALUE)
  }

  private def testMultiple(unsigned: Boolean): Unit = {
    testEncodeDecode(unsigned, 1, 0xaabb, -1, 0x12345678, 0, 0, 0x80, 0x7f, -42, 1917)
  }

  private def testSizeCalculation(unsigned: Boolean): Unit = {
    Array(
      1, 20, 300, 4000, 50000, -1, -20, -300, -4000, -5000,
      0x8123456789abcdefL, -0x8123456789abcdefL,
      0x7f, 0x80, -0x7f, -0x80,
      java.lang.Integer.MAX_VALUE, java.lang.Integer.MIN_VALUE,
      java.lang.Long.MAX_VALUE, java.lang.Long.MIN_VALUE
    ).foreach(testSizeCalculation(unsigned, _))
  }

  test("OneULEB128") {
    testOne(true)
  }

  test("MultipleULEB128") {
    testMultiple(true)
  }

  test ("OneSLEB128") {
    testOne(false)
  }

  test ("MultipleSLEB128") {
    testMultiple(false)
  }

  test ("SizeULEB128") {
    testSizeCalculation(false)
  }

  test ("SizeSLEB128") {
    testSizeCalculation(true)
  }
}
