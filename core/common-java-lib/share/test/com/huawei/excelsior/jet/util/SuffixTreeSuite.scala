/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.jet.CommonSuite

/**
 * Tests for SuffixTree.
 */
class SuffixTreeSuite extends CommonSuite {

  private var root: SuffixTree[Int] = _

  private def newRoot() = new SuffixTree(0, null)

  override def beforeEach(): Unit = {
    super.beforeEach()
    root = newRoot()
  }

  private def add(elems: Int*): SuffixTree[Int] = {
    val p = root.prepend(elems.toIndexedSeq)

    var x = p
    for (i <- 0 until elems.length) {
      x = x.parent
    }
    x should be (root)

    p.toRoot.toSeq should be (elems.toSeq)

    p.getRoot should be (root)

    p
  }

  private def checkSuffix(p1: SuffixTree[Int], p2: SuffixTree[Int], suffixLength: Int): Unit = {
    def toPathArr(p: SuffixTree[Int]): Array[SuffixTree[Int]] = {
      val s1 = new Array[SuffixTree[Int]](p.toRoot.length)
      var x = p
      for (i <- s1.indices) {
        s1(i) = x
        x = x.parent
      }
      s1
    }

    val a1 = toPathArr(p1)
    val a2 = toPathArr(p2)

    a1.length should be >= suffixLength
    a2.length should be >= suffixLength
    a1.drop(a1.length - suffixLength) should equal (a2.drop(a2.length - suffixLength))
  }

  private def checkChildren(p: SuffixTree[Int], elems: Int*): Unit = {
    p.getChildren.map(_.elem).toSet should be (Set(elems*))
  }

  test("simple test") {
    add(0)
  }

  test("one line") {
    add(0, 1, 2, 3, 4, 5)
  }

  test("two lines") {
    val p1 = add(0, 1, 2, 3, 4, 5)
    val p2 = add(6, 7, 8, 3, 4, 5)
    checkSuffix(p1, p2, 3)
  }

  test("equals") {
    val p1 = add(0, 1, 2)
    val p2 = add(0, 1, 2)
    p1 should be (p2)
  }

  test("tree") {
    val p1 = add(0, 1, 2, 3, 4, 5)
    val p2 = add(6, 7, 8, 3, 4, 5)
    val p3 = add(         3, 4, 5)
    val p4 = add(   9, 8, 3, 4, 5)
    val p5 = add(1, 2, 3, 4, 5, 5)
    checkSuffix(p1, p2, 3)
    checkSuffix(p2, p3, 3)
    checkSuffix(p2, p4, 4)
    checkSuffix(p1, p5, 1)

    checkChildren(p3, 2, 8)
  }

  test("forest") {
    val p1 = add(0, 1, 2)
    val p2 = add(0, 1, 3)
    val p3 = add(4, 5, 2)
    val p4 = add(4, 5, 3)
    checkSuffix(p1, p2, 0)
    checkSuffix(p1, p3, 1)
    checkSuffix(p2, p4, 1)
    checkSuffix(p3, p4, 0)
  }

  test("null element") {
    val objRoot = SuffixTree.newRoot[String]()
    intercept[NullPointerException] {
      objRoot.prepend(IndexedSeq("abc", null, "def"))
    }
  }

  test("removing elements") {
    add(2, 1, 0)
    add(4, 1, 0)
    add(7, 6, 0)
    add(8, 6, 0)

    val zero = add(0)

    checkChildren(zero, 1, 6)

    root.retainAll(_ % 2 == 0)

    checkChildren(zero, 6)

    val six = add(6, 0)
    checkChildren(six, 8)
  }

  test("toRoot mapper") {
    val res = add(0, 1, 2).toRoot.map(_.toString)
    res.toSeq should be (Seq("0", "1", "2"))
  }

}
