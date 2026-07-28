/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import xscala.util.ULong

import scala.math.abs
import scala.util.Random


class ULongSuite extends AnyFunSuite with Matchers {

  val limitD = 1000
  val limitN = 1000

  val limit = 1000

  def testWith(genN: => ULong, genD: => ULong): Unit = {
    for (_ <- 0 to limitD;
         d = genD;
         if d != ULong(0);
         _ <- 0 to limitN) {
      checkValues(genN, d)
    }
  }

  def checkValues(n: ULong, d: ULong): Unit = {
    withClue(s"(n, d) = ($n, $d)") {
      val q = n / d
      val r = n % d
      assertResult(r < d)
      assertResult(q <= n)
      q * d + r should be(n)
    }
  }

  def checkZeroDivision(n: ULong): Unit = {
    a [ArithmeticException] should be thrownBy{
      n / ULong(0)
    }

    a [ArithmeticException] should be thrownBy{
      n % ULong(0)
    }
  }

  test("Random test") {
    val random = new Random(System.currentTimeMillis())
    testWith(ULong(random.nextLong()), ULong(random.nextLong()))
  }

  test("Large cases") {
    val random = new Random(System.currentTimeMillis())
    testWith((ULong(1) << 63) + ULong(abs(random.nextLong())), (ULong(1) << 63) + ULong(abs(random.nextLong())))
  }

  test("Middle cases") {
    val random = new Random(System.currentTimeMillis())
    testWith((ULong(1) << 63) + ULong(abs(random.nextLong())), ULong(abs(random.nextInt())))
  }

  test("Small cases") {
    var i: ULong = -1
    def getNext: ULong = {
      i += 1
      i
    }

    val random = new Random(System.currentTimeMillis())
    testWith((ULong(1) << 63) + ULong(abs(random.nextLong())), getNext)
  }

  test("Zero division") {
    checkZeroDivision(100)
    checkZeroDivision((ULong(1) << 63) + 100)
  }
}
