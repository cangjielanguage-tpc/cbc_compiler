/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.jet.CommonSuite

import java.util.ConcurrentModificationException
import scala.collection.mutable

/**
 * Tests for Worklist.
 */
class WorklistSuite extends CommonSuite {

  private var worklist: Worklist[Int] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    worklist = Worklist.empty[Int]
  }

  test("simple test") {
    worklist ++= Seq(1, 2)
    worklist.snapshot should be (Seq(1, 2))
  }

  test("add with repetitions") {
    worklist ++= Seq(1, 2, 3, 2, 1)
    worklist.snapshot should be (Seq(1, 2, 3))
  }

  test("add while drain") {
    worklist += 1

    val buf = mutable.Buffer[Int]()
    for (x <- worklist.drain) {
      buf += x
      if (x < 5) {
        worklist += x * 2
      }
    }
    buf should be (Seq(1, 2, 4, 8))
  }

  test("add while accumulate") {
    worklist += 1

    for (x <- worklist.accumulate) {
      if (x < 5) {
        worklist += x * 2
      }
    }
    worklist.snapshot should be (Seq(1, 2, 4, 8))
  }

  test("remove while accumulate") {
    worklist ++= Seq(1, 2, 3, 4, 5)

    val buf = mutable.Buffer[Int]()
    for (x <- worklist.accumulate) {
      buf += x
      if (x == 3) {
        Seq(1, 2, 3, 4, 6, 7) foreach worklist.remove
      }
    }
    buf should be (Seq(1, 2, 3, 5))
    worklist.snapshot should be (Seq(5))
  }

  test("swap") {
    worklist ++= Seq(1, 2, 3)
    val other = Worklist(4, 5, 6)
    worklist.swap(other)
    worklist.snapshot should be (Seq(4, 5, 6))
    other.snapshot should be (Seq(1, 2, 3))
  }

  test("drainTo(empty)") {
    worklist ++= Seq(1, 2, 3)
    val other = Worklist.empty[Int]
    worklist.drainTo(other)
    worklist.isEmpty should be (true)
    other.snapshot should be (Seq(1, 2, 3))
  }

  test("drainTo(nonEmpty)") {
    worklist ++= Seq(1, 2, 3)
    val other = Worklist(3, 4)
    worklist.drainTo(other)
    worklist.isEmpty should be (true)
    other.snapshot should be (Seq(3, 4, 1, 2))
  }

  test("swap while accumulate") {
    val buf1 = mutable.Buffer[Int]()
    val buf2 = mutable.Buffer[Int]()

    worklist ++= Seq(2, 3, 4)
    val other = Worklist(-1, 42, 7, 13, 3, 7)

    for (x <- worklist.accumulate) {
      buf1 += x
      if (x == 3) {
        for (y <- other.accumulate) {
          buf2 += y
          if (y == 7) worklist.swap(other)
        }
      }
    }

    buf1 should be (Seq(2, 3, -1, 42, 7, 13, 3))
    buf2 should be (Seq(-1, 42, 7, 2, 3, 4, 2, 3, 4))
  }

  test("mutate while iterate") {
    worklist ++= Seq(1, 2, 3)

    for (x <- worklist) {
      worklist += x
    }

    intercept[ConcurrentModificationException] {
      for (x <- worklist) {
        worklist += 42
      }
    }

    for (x <- worklist) {
      worklist -= (-x)
    }

    intercept[ConcurrentModificationException] {
      for (x <- worklist) {
        worklist -= 42
      }
    }

    intercept[ConcurrentModificationException] {
      val other = Worklist(100, 101)
      for (x <- worklist) {
        worklist.swap(other)
      }
    }

    intercept[ConcurrentModificationException] {
      for (x <- worklist) {
        worklist.clear()
      }
    }
  }

}
