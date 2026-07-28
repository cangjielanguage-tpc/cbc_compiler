/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.pdb.archive

import com.huawei.excelsior.jet.common.XString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import xscala.util.MathUtils

import scala.annotation.tailrec
import scala.collection.mutable

/** Randomized unit tests for `MemIndex`.
  *
  * @author paul
  */
class MemIndexAdvancedSuite extends AnyFunSuite {

  abstract class MyRandom {
    def startSeed: Long

    protected def nextBitsImpl(width: Int): Int

    /** Generates the next `width`-bit pseudorandom number,
      * where `width` should be in range `[1, 32]`.
      */
    def nextBits(width: Int): Int = {
      assert(width >= 1 && width <= 32)
      nextBitsImpl(width)
    }

    /** Returns the next pseudorandom, uniformly distributed `Int` value. */
    def nextInt(): Int = nextBits(32)

    /** Returns the next pseudorandom, uniformly distributed `Double` value
      * from the range `[0.0, 1.0)`.
      */
    def nextDouble(): Double = {
      val k = 1.0 / (1L << 53)
      ((nextBits(26).toLong << 27) + nextBits(27)) * k
    }

    private var nextReadyGaussian = Option.empty[Double]

    /** Returns the next pseudorandom, Gaussian ("normally") distributed
      * `Double` value with mean `0.0` and standard deviation `1.0`.
      */
    def nextGaussian(): Double = nextReadyGaussian match {
      case Some(value) => nextReadyGaussian = None; value
      case None =>
        // See Knuth, ACP, Section 3.4.1 Algorithm C.
        @tailrec def retry(): Double = {
          val v1, v2 = 2 * nextDouble() - 1 // between -1 and 1
          val s = v1 * v1 + v2 * v2
          if (s != 0 && s < 1) {
            val m = StrictMath.sqrt(-2 * StrictMath.log(s) / s)
            nextReadyGaussian = Some(v2 * m)
            v1 * m
          } else {
            retry()
          }
        }
        retry()
    }
  }

  /* java-based MyRandom impl
  def newRandom(): MyRandom = new MyRandom {
    var startSeed = 0L

    class JRND extends java.util.Random() {
      override def setSeed(seed: Long): Unit = {
        startSeed = seed
        super.setSeed(seed)
      }
      def nextBits(width: Int) = next(width)
    }

    val jrnd = new JRND
    protected def nextBitsImpl(width: Int) = jrnd.nextBits(width)
  }
  */

  // TODO: move into xscala.util.Random
  def newRandom(): MyRandom = new MyRandom {
    val startSeed: Long = xscala.time.unixNanoseconds

    private val multiplier = 3202034522624059733L
    private val addend = 0x421L
    private var seed = startSeed

    protected def nextBitsImpl(width: Int): Int = {
      val nextValue = multiplier * seed + addend
      seed = nextValue
      MathUtils.bits(nextValue, 48 - width, 47).toInt // get `width` middle bits
    }
  }

  private class RndTable { self =>
    val rnd = newRandom()
    println(s"seed: 0x${rnd.startSeed.toHexString}L")

    val log = false
    val doStats = true

    var namesCount = 0
    var appendCount = 0

    val str2id = mutable.HashMap.empty[XString, Int]
    val idx = new MemIndex

    def checkContents(): Unit = {
      val ser = SerialIndex.from(idx)
      for ((name, id) <- str2id) {
        idx.find(name) shouldBe id
        ser.find(name) shouldBe id
      }
      println(s"names: $namesCount, duplicates: ${appendCount - namesCount}")
      if (doStats) {
        idx.stats.print(null)
        ser.stats.print(null)
      }
      System.out.flush()
    }

    def log(msg: String): Unit = {
      if (log) println(msg)
    }

    def append(name: XString): Unit = {
      var msg = s"append[$appendCount]: \"$name\" => "
      appendCount += 1
      var id = namesCount + 1
      val res = idx.add(name, id, false)
      if (res == Index.NO_ENTRY) {
        namesCount += 1
        msg += s"new $id"
      } else {
        id = res
        msg += s"found $res"
      }
      log(msg)
      str2id.put(name, id) match {
        case None => res shouldBe 0
        case Some(oldID) =>
          id shouldBe oldID
          (res == 0) shouldBe false
      }
    }

    def randomLength: Int = {
      val maxLength = 1000
      val sigma = 1.0
      val R = 100.0

      @tailrec def retry(): Double = {
        // To generate mainly short strings here we use
        // segment [0, R) of distribution lnN(0, sigma^2)
        var x = rnd.nextGaussian() // x : N(0, 1)
        x = x * sigma // x : N(0, sigma^2)
        x = Math.exp(x) // x : lnN(0, sigma^2)
        if (x >= R) retry() else x
      }

      val x = retry()
      Math.ceil(x / R * maxLength).toInt
    }

    def randomByte(alphaWidth: Int) = {
      import Integer.numberOfLeadingZeros as nlz

      assert(alphaWidth >= 2 && alphaWidth <= 256)
      val z = nlz(alphaWidth - 1)

      @tailrec def retry(): Int = {
        val x = rnd.nextInt() >>> z
        if (x < 0 || x >= alphaWidth) retry() else x
      }

      val x = retry()
      val r = if (alphaWidth > 64) x else if (alphaWidth > 32) x + '0' else x + 'a'
      r.toByte
    }

    def randomString(length: Int, alphaWidth: Int) = {
      XString.fill(length) { randomByte(alphaWidth) }
    }

    def populate(strCount: Int, alphaWidth: Int): Unit = {
      for (_ <- 0 until strCount) {
        append(randomString(randomLength, alphaWidth))
      }
    }
  }

  test("PopulateWithRandomStrings") {
    val t = new RndTable
    t.populate(400000, 64)
    t.checkContents()
  }
}
