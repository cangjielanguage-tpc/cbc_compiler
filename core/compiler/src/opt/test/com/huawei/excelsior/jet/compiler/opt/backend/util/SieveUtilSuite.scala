/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.util

import com.huawei.excelsior.jet.compiler.CompilerSuite

import scala.collection.mutable

class SieveUtilSuite extends CompilerSuite with SieveUtil {

  import Sieve._

  private val _callsCount = new mutable.HashMap[Any, Int]()
  private def callsCount(f: Any) = _callsCount.getOrElse(f, 0)
  private def incCall(f: Any): Unit = _callsCount(f) = callsCount(f) + 1

  private val isEven: Function[Int, Boolean]        = { x => incCall(isEven);       x % 2 == 0 }
  private val isNegative: Function[Int, Boolean]    = { x => incCall(isNegative);   x < 0 }
  private val alwaysTrue: Function[Int, Boolean]    = { x => incCall(alwaysTrue);   true }
  private val rem3: Function[Int, Int]              = { x => incCall(rem3);         (x + 30000) % 3 }

  private val bigSequence = Seq(-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6)

  override def beforeEach(): Unit = {
    bigSieve.clear()
    _callsCount.clear()
  }

  val isEvenSieve = Sieve(root(isEven))
  val isNegativeSieve = Sieve(root(isNegative))
  val alwaysTrueSieve = Sieve(root(alwaysTrue))

  /////////////////////////////////////////////
  val rem3Sieve = Sieve(
                  root(rem3, 3)                       ||
        leaf  |  alwaysTrueSieve  |  leaf             )


  /////////////////////////////////////////////
  val bigCombinedSieve = Sieve(
                  root(isNegative)                    ||
        isEvenSieve    |    rem3Sieve                 )


  /////////////////////////////////////////////
  val sieveWithDuplicate = Sieve(
                  root(rem3, 3)                       ||
              dup(isNegativeSieve, 3)                 )


  /////////////////////////////////////////////
  val bigSieve = Sieve(

                    root(isNegative)                                          ||

          isEven            |                     (rem3, 3)                   ||

      leaf   |  leaf        |       leaf    |     alwaysTrue    |     leaf    ||

                                                leaf  |   leaf                )


  /////////////////////////////////////////////
  // Test for basic operations
  /////////////////////////////////////////////

  test("sieve best elements and get them immediately") {
    bigSieve.sift(-18)  shouldBe Some(-18)
    bigSieve.sift(-24)  shouldBe Some(-24)

    callsCount(isNegative)  shouldBe 2
    callsCount(isEven)      shouldBe 2
    callsCount(rem3)        shouldBe 0
    callsCount(alwaysTrue)  shouldBe 0
  }

  test("sieve some not best elements and get them by one") {
    bigSieve.sift(-19)  shouldBe None
    bigSieve.sift(-30)  shouldBe Some(-30)
    bigSieve.sift(6)    shouldBe None

    callsCount(isNegative)  shouldBe 3
    callsCount(isEven)      shouldBe 2
    callsCount(rem3)        shouldBe 0 // ! 0, because 6 is not the best and we will not apply filters without the need
    callsCount(alwaysTrue)  shouldBe 0

    // Get previously put elements by their quality
    bigSieve.get()  shouldBe Some(-19)
    bigSieve.get()  shouldBe Some(6)
    bigSieve.get()  shouldBe None

    callsCount(isNegative)  shouldBe 3
    callsCount(isEven)      shouldBe 2
    callsCount(rem3)        shouldBe 1 // ok, check it
    callsCount(alwaysTrue)  shouldBe 0
  }

  test("sieve some not best elements and get all equally-best") {
    // Put some not best elements and get nothing
    bigSieve.sift(6)    shouldBe None
    bigSieve.sift(8)    shouldBe None
    bigSieve.sift(9)    shouldBe None

    // Get all equally-best elements from previously put
    bigSieve.getAllBest()   shouldBe Seq(6, 9)
  }

  test("bug in allBest") {
    bigSieve.sift(3)    shouldBe None
    bigSieve.sift(6)    shouldBe None
    bigSieve.sift(5)    shouldBe None
    bigSieve.sift(8)    shouldBe None

    bigSieve.getAllBest()   shouldBe Seq(3, 6)
    bigSieve.getAllBest()   shouldBe Seq(5, 8)
  }


  /////////////////////////////////////////////
  // Tests for massive operations
  /////////////////////////////////////////////

  test("select from simple sieve") {
    isEvenSieve.selectFrom(Seq(1, 0, 3, 2), 4) shouldBe Seq(0, 2) ++ Seq(1, 3)

    isEvenSieve.selectFrom(Seq(0, 1, 2, 3), 4) shouldBe Seq(0, 2) ++ Seq(1, 3)
    isEvenSieve.selectFrom(Seq(0, 1, 2, 3), 3) shouldBe Seq(0, 2) ++ Seq(1)
    isEvenSieve.selectFrom(Seq(0, 1, 2, 3), 2) shouldBe Seq(0, 2)
    isEvenSieve.selectFrom(Seq(0, 1, 2, 3), 1) shouldBe Seq(0)
  }

  test("select from simple sieve with lazy filters application") {
    isEvenSieve.selectFrom(Seq(0, 1, 2, 3), 1) shouldBe Seq(0)
    callsCount(isEven) shouldBe 1
  }

  test("best leaf from simple sieve") {
    isEvenSieve.allBestFrom(Seq(0, 1, 2, 3)) shouldBe Seq(0, 2)
    isEvenSieve.allBestFrom(Seq(19, 37)) shouldBe Seq(19, 37)
  }

  for ((sieve, name) <- Seq(bigSieve, bigCombinedSieve).zip(Seq("big", "combined"))) {
    test(s"select from $name - 1") {
      sieve.selectFrom(bigSequence, 13) shouldBe
        Seq(-6, -4, -2) ++    // negative && even
          Seq(-5, -3, -1) ++  // negative && !even
          Seq(0, 3, 6) ++     // !negative && rem3 == 0
          Seq(1, 4) ++        // !negative && rem3 == 1 && alwaysTrue
          Seq() ++            // !negative && rem3 == 1 && !alwaysTrue
          Seq(2, 5)           // !negative && rem3 == 2
    }

    test(s"select from $name - 2") {
      sieve.selectFrom(bigSequence, 8) shouldBe
        Seq(-6, -4, -2) ++    // negative && even
          Seq(-5, -3, -1) ++  // negative && !even
          Seq(0, 3)           // !negative && rem3 == 0

      callsCount(isNegative)  shouldBe 13
      callsCount(isEven)      shouldBe 6
      callsCount(rem3)        shouldBe 4
      callsCount(alwaysTrue)  shouldBe 0
    }

    test(s"select from $name - 3") {
      sieve.selectFrom(bigSequence, 5) shouldBe
        Seq(-6, -4, -2) ++    // negative && even
          Seq(-5, -3)         // negative && !even

      callsCount(isNegative)  shouldBe 13
      callsCount(isEven)      shouldBe 6
      callsCount(rem3)        shouldBe 0
      callsCount(alwaysTrue)  shouldBe 0
    }

    test(s"select from $name - 4") {
      sieve.selectFrom(bigSequence, 2) shouldBe Seq(-6, -4)

      callsCount(isNegative)  shouldBe 3
      callsCount(isEven)      shouldBe 3
      callsCount(rem3)        shouldBe 0
      callsCount(alwaysTrue)  shouldBe 0
    }

    test(s"select from $name - 6") {
      sieve.selectFrom(Seq(1, 5, -4), 3) shouldBe Seq(-4, 1, 5)

      callsCount(isNegative)  shouldBe 3
      callsCount(isEven)      shouldBe 1
      callsCount(rem3)        shouldBe 2
      callsCount(alwaysTrue)  shouldBe 1
    }

    test(s"select from $name - 7") {
      sieve.allBestFrom(bigSequence) shouldBe Seq(-6, -4, -2)

      callsCount(isNegative)  shouldBe 13
      callsCount(isEven)      shouldBe 6
      callsCount(rem3)        shouldBe 0
      callsCount(alwaysTrue)  shouldBe 0
    }

    test(s"select from $name - 8") {
      sieve.allBestFrom(Seq(1, 2, 4, 5)) shouldBe Seq(1, 4)

      callsCount(isNegative)  shouldBe 4
      callsCount(isEven)      shouldBe 0
      callsCount(rem3)        shouldBe 4
      callsCount(alwaysTrue)  shouldBe 2
    }

    test(s"select from $name - 9") {
      sieve.allBestFrom(Seq(5)) shouldBe Seq(5)

      callsCount(isNegative)  shouldBe 1
      callsCount(isEven)      shouldBe 0
      callsCount(rem3)        shouldBe 1
      callsCount(alwaysTrue)  shouldBe 0
    }
  }

  test("select from duplicated sieve") {
    sieveWithDuplicate.selectFrom(bigSequence, 13) shouldBe
      Seq(-6, -3) ++ Seq(0, 3, 6) ++
      Seq(-5, -2) ++ Seq(1, 4) ++
      Seq(-4, -1) ++ Seq(2, 5)
  }

  test("bug of selection more elements than collection size") {
    assertThrows[AssertionError] {
      bigSieve.selectFrom(Seq(1), 2)
    }
  }

}
