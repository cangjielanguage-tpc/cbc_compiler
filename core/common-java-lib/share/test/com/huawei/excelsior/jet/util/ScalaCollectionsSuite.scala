/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.ScalaCollections.*

import scala.collection.mutable
import scala.math.PartialOrdering

class ScalaCollectionsSuite extends CommonSuite {
  test("groupBy") {
    groupBy(Seq(1, 2, 3))(_ % 2 == 0) shouldBe Map(true -> Seq(2), false -> Seq(1, 3))
  }

  test("groupMap") {
    groupMap(Seq(1, 2, 3))(_ % 2 == 0)(_ * 2) shouldBe Map(true -> Seq(4), false -> Seq(2, 6))
  }

  test("groupMapReduce") {
    groupMapReduce(Seq(1, 2, 3))(_ % 2 == 0)(_ * 2)(_ + _) shouldBe Map(true -> 4, false -> 8)
  }

  test("sequence") {
    sequence(Seq()) shouldBe Some(Seq())
    sequence(Seq(Some(1), Some(2))) shouldBe Some(Seq(1, 2))
    sequence(Seq(None)) shouldBe None
    sequence(Seq(Some(1), None, Some(2))) shouldBe None
  }

  test("lastElement") {
    lastElement(Seq()) shouldBe None
    lastElement(Seq(1, 2, 3)) shouldBe Some(3)
  }

  test("insertAt") {
    insertAt(Seq(), 0, 1).toSeq shouldBe Seq(1)
    insertAt(Seq(1), 0, 2).toSeq shouldBe Seq(2, 1)
    insertAt(Seq(2, 1), 1, 3).toSeq shouldBe Seq(2, 3, 1)
    insertAt(Seq(2, 3, 1), 3, 4).toSeq shouldBe Seq(2, 3, 1, 4)
  }

  test("removeAt") {
    removeAt(Seq(), 0).toSeq shouldBe Seq()
    removeAt(Seq(1), 0).toSeq shouldBe Seq()
    removeAt(Seq(1, 2), 0).toSeq shouldBe Seq(2)
    removeAt(Seq(1, 2), 1).toSeq shouldBe Seq(1)
  }

  test("minimal/maximalElements") {
    implicit val divisors: PartialOrdering[Int] = partialOrderingBy((x, y) => x <= y && y % x == 0)

    minimalElements(Seq(2, 3, 4, 5, 6)).toSeq shouldBe Seq(2, 3, 5)
    maximalElements(Seq(2, 3, 4, 5, 6)).toSeq shouldBe Seq(4, 5, 6)

    minimalElements(Seq(1, 2, 3, 4, 5, 6)).toSeq shouldBe Seq(1)
    maximalElements(Seq(1, 2, 3, 4, 5, 6)).toSeq shouldBe Seq(4, 5, 6)

    minimalElements(Seq(2, 3, 4, 5, 6, 2, 5, 6)).toSeq shouldBe Seq(2, 3, 5, 2, 5)
    maximalElements(Seq(2, 3, 4, 5, 6, 2, 5, 6)).toSeq shouldBe Seq(4, 5, 6, 5, 6)
  }

  test("partialOrderingBy - weird equivalence") {
    val ord: PartialOrdering[Int] = partialOrderingBy((x, y) => x <= y + 1)

    ord.equiv(0, 1) shouldBe true
    ord.tryCompare(0, 1) shouldBe Some(0)
  }

  test("map with") {
    mapWith[Int, Int](Seq.empty) { x => x % 2 } shouldBe Map()

    mapWith(Seq(1, 2, 3, 4)) { x => x } shouldBe Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4)

    mapWith(Seq(1, 2, 3, 4)) { x => x % 2 } shouldBe Map(1 -> 1, 2 -> 0, 3 -> 1, 4 -> 0)

    mapWith(Seq(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)) { x => x % 2 } shouldBe Map(1 -> 1, 2 -> 0, 3 -> 1, 4 -> 0)

    (mapWith(Seq(1, 2, 3, 4)) { x => x }).toSeq shouldBe Seq((1, 1), (2, 2), (3, 3), (4, 4))
  }

  test("collect duplicates") {
    collectDuplicates(Seq(1, 1, 1)).toSeq shouldBe Seq(1, 1)
    collectDuplicates(Seq(1, 1, 2, 3, 2)).toSeq shouldBe Seq(1, 2)

    collectDuplicatesBy(Seq((1, 2), (2, 2), (3, 2)))(_._2).toSeq shouldBe Seq((2, 2), (3, 2))
  }

  test("zipMap") {
    val xs = Seq(1, 2, 3, 4)
    val ys = Seq(5, 6, 7, 8)
    zipMap(xs, ys)(_ + _).toSeq shouldBe (xs zip ys map (_ + _))
  }

  test("aggregate") {
    aggregate(Seq[Int]())(_ < _) shouldBe Seq()
    aggregate(Seq(1))(_ < _) shouldBe Seq(Seq(1))
    aggregate(Seq(1, 2, 3, 1, 2, 3))(_ < _) shouldBe Seq(Seq(1, 2, 3), Seq(1, 2, 3))
  }
  
  test("aggregateFirst") {
    aggregateFirst(Seq[Int]())(_ < _) shouldBe (Seq(), Seq())
    aggregateFirst(Seq(1))(_ < _) shouldBe (Seq(1), Seq())
    aggregateFirst(Seq(1, 2, 3, 1, 2, 3))(_ < _) shouldBe (Seq(1, 2, 3), Seq(1, 2, 3))
  }
}
