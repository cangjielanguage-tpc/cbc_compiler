/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.jet.CommonSuite
import org.scalactic.source

class DisjointSetSuite extends CommonSuite {
  private val defaultSize = 5


  def testUnionFind(name: String)(action: DisjointSet[Int] => Unit)(implicit pos: source.Position): Unit = {
    for ((set, setName) <- Seq(
      (new DisjointSet.ofInt(defaultSize), "int[5]"),
      (DisjointSet.empty[Int], "normal")
    )) {
      test(s"$name ($setName)") {
        action(set)
      }
    }
  }

  testUnionFind("one element") { set =>
    set.find(1) shouldBe 1
  }

  testUnionFind("two elements union") { set =>
    set.find(1) shouldBe 1
    set.union(1, 2)
    set.find(1) shouldBe 2

    for (i <- 1 to 2) {
      set.equivElements(i).toSet shouldBe Set(1, 2)
    }
  }

  testUnionFind("three elements union, forward build") { set =>
    set.union(1, 2)
    set.union(2, 3)

    set.find(1) shouldBe 3
    set.find(2) shouldBe 3

    for (i <- 1 to 3) {
      set.equivElements(i).toSet shouldBe Set(1, 2, 3)
    }
  }

  testUnionFind("three elements union, backward build") { set =>
    set.union(2, 3)
    set.union(1, 2)

    set.find(1) shouldBe 3
    set.find(2) shouldBe 3

    for (i <- 1 to 3) {
      set.equivElements(i).toSet shouldBe Set(1, 2, 3)
    }
  }

  testUnionFind("multiple elements union") { set =>
    set.unionAll(Seq(1, 2, 3, 4))

    for (i <- 1 to 4) {
      set.find(i) shouldBe 1
      set.equivElements(i).toSet shouldBe Set(1, 2, 3, 4)
    }
  }

  def testEquiv(name: String)(eq: Equiv[Int])(xs: Int*)(classes: Seq[Int]*): Unit = {
    test(s"test equiv ($name)") {
      val set = DisjointSet.from(xs)(eq)
      set.equivClasses.size shouldBe classes.size
      for ((i, c) <- set.equivClasses zip classes) {
        set.equivElements(i) should beIterator (c: _*)
      }
    }
  }

  testEquiv("universal")(Equiv.universal)(0, 1, 2, 3)(
    Seq(0),
    Seq(1),
    Seq(2),
    Seq(3),
  )

  testEquiv("all false")((_, _) => false)(0, 1, 2, 3)(
    Seq(0),
    Seq(1),
    Seq(2),
    Seq(3),
  )

  testEquiv("all true")((_, _) => true)(0, 1, 2, 3)(
    Seq(0, 1, 2, 3),
  )

  testEquiv("mod 2")(Equiv.by(_ % 2))(0, 1, 2, 3)(
    Seq(0, 2),
    Seq(1, 3),
  )

  testEquiv("mod 3")(Equiv.by(_ % 3))(0, 1, 2, 3)(
    Seq(0, 3),
    Seq(1),
    Seq(2),
  )

  test("bad equiv case (non-reflexive and non-transitive)") {
    val eq: Equiv[Int] = _ % 2 != _ % 2
    val set = DisjointSet.from(1 to 3)(eq)

    eq.equiv(1, 1) shouldBe false
    eq.equiv(1, 2) shouldBe true
    eq.equiv(1, 3) shouldBe false
    eq.equiv(2, 1) shouldBe true
    eq.equiv(2, 2) shouldBe false
    eq.equiv(2, 3) shouldBe true

    // Resulting set equivalence may be different from the original one,
    // if the original was not a proper equivalence relation (reflexive, symmetric, transitive).
    set.equiv(1, 1) shouldBe true
    set.equiv(1, 2) shouldBe true
    set.equiv(1, 3) shouldBe true
    set.equiv(2, 1) shouldBe true
    set.equiv(2, 2) shouldBe true
    set.equiv(2, 3) shouldBe true
  }

  test("bad equiv case (asymmetric)") {
    val eq: Equiv[Int] = {
      case (1, 2) => true
      case (2, 1) => false
      case (x, y) => x == y
    }
    val set = DisjointSet.from(1 to 2)(eq)

    eq.equiv(1, 1) shouldBe true
    eq.equiv(1, 2) shouldBe true
    eq.equiv(2, 1) shouldBe false
    eq.equiv(2, 2) shouldBe true

    // Resulting set equivalence may be different from the original one,
    // if the original was not a proper equivalence relation (reflexive, symmetric, transitive).
    set.equiv(1, 1) shouldBe true
    set.equiv(1, 2) shouldBe true
    set.equiv(2, 1) shouldBe true
    set.equiv(2, 2) shouldBe true
  }
}

