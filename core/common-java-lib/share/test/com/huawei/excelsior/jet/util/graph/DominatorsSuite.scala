/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL
import com.huawei.excelsior.jet.util.graph.Dominators

/** Tests for dominators calculation.
 */
class DominatorsSuite extends CommonSuite with IntGraphBuilderDSL {

  var dominators: Dominators[N] = _

  private def updateOne(node: N, newIdom: N = graph.invalidNode): Unit = {
    dominators.tryUpdateOne(node, newIdom) should be (true)
  }

  private def calcDominatorsOver(start: SubGraph): Unit = {
    makeGraph(start)
    dominators = graph.dominators
  }

  private def checkNoIDom(node: N): Unit = {
    dominators.idom(node) should be (graph.invalidNode)
  }

  private def checkIDom(node: N, expectedIDom: N): Unit = {
    dominators.idom(node) should be (expectedIDom)
  }

  private def checkDom(node: N, expectedIDom: N): Unit = {
    checkIDom(node, expectedIDom)

    // check consistency of idom(node) and doms(node)
    val doms = dominators.doms(node).toList
    for (List(x, y) <- doms.sliding(2)) {
      checkIDom(x, y)
    }
    checkNoIDom(doms.last)
  }

  def checkStrictDom(res: Boolean, strictDom: N, node: N): Unit = {
    dominators.strictlyDominates(strictDom, node) should be (res)
  }

  def checkCompare(n1: N, n2: N, res: Int): Unit = {
    dominators.compare(n1, n2) should be (res)
  }

  def checkUnordered(n1: N, n2: N): Unit = {
    dominators.tryCompare(n1, n2) should be (None)
    a[Throwable] should be thrownBy { dominators.compare(n1, n2) }
  }

  def checkMax(n1: N, n2: N, res: N): Unit = {
    dominators.max(n1, n2) should be (res)
  }

  def checkMin(n1: N, n2: N, res: N): Unit = {
    dominators.min(n1, n2) should be (res)
  }
  
  def checkDepth(n: N, res: Int): Unit = {
    dominators.depth(n) should be (res)
  }
  
  def checkGCA(n1: N, n2: N, res: N): Unit = {
    dominators.nearest(n1, n2) should be (res)
  }
  
  def checkRange(from: N, to: N, res: N*): Unit = {
    val range = dominators.range(from, to).toSeq
    range should have size (res.size)
    for ((x, y) <- range zip res) {
      x should be (y)
    }
    val reversedRange = dominators.reversedRange(to, from).toSeq
    reversedRange should have size (res.size)
    for ((x, y) <- reversedRange zip res.reverse) {
      x should be (y)
    }
  }

  def checkChildren(node: N, cs: N*): Unit = {
    val childs = dominators.children(node).toSet
    val csSet = cs.toSet
    childs should have size (csSet.size)
    for (x <- childs) {
      csSet should contain (x)
    }
  }

  test("simple two nodes") {
    calcDominatorsOver(0 -> 1)

    checkDom(1, 0)
  }

  test("simple nodes with unreachable") {
    calcDominatorsOver(0 -> 1)

    checkStrictDom(true, 0, 2)
    checkStrictDom(true, 1, 2)
    checkStrictDom(false, 2, 0)
    checkStrictDom(false, 2, 1)
  }

  test("simple several nodes") {
    calcDominatorsOver(0 -> 1 -> 2 -> 3 -> 4)
    checkNoIDom(0)
    checkDom(1, 0)
    checkDom(2, 1)
    checkDom(3, 2)
    checkDom(4, 3)

    dominators.strictDoms(4).toSeq should be (Seq(3, 2, 1, 0))
  }

  test("diamond") {
    calcDominatorsOver(0 -> (1 || 2) -> 3)
    checkDom(1, 0)
    checkDom(2, 0)
    checkDom(3, 0)
  }

  test("cylce") {
    calcDominatorsOver(wd(0 -> 1 -> 2) -> 3)
    checkDom(1, 0)
    checkDom(2, 1)
    checkDom(3, 0)
  }

  test("big & complex") {
    // 0 - 3 - 5
    //  \   \ //
    //   1   4
    //    \ //
    //     2
    calcDominatorsOver(0 -> ((1 -> 2 -> 4) || (3 -> ((4 -> (2 || 5))|| (5 -> 4)))))
    Seq(1, 2, 3, 4, 5) foreach { checkDom(_, 0) }
  }

  test("irreducible") {
    calcDominatorsOver(0 -> ((dw(1 -> 2 -> 3) -> 4) || 2))
    checkDom(1, 0)
    checkDom(2, 0)
    checkDom(3, 2)
    checkDom(4, 3)
  }

  test("strict domination") {
    calcDominatorsOver(0 -> ((1 -> 2) || 3))

    checkStrictDom(true, 0, 1)
    checkStrictDom(true, 1, 2)
    checkStrictDom(true, 0, 2)
    checkStrictDom(false, 0, 0)
    checkStrictDom(false, 1, 1)
    checkStrictDom(false, 2, 1)
    checkStrictDom(false, 2, 3)
    checkStrictDom(false, 3, 2)
    checkStrictDom(false, 1, 3)
    checkStrictDom(false, 3, 1)
  }

  test("compare") {
    calcDominatorsOver(0 -> 1 -> ((2 -> (4 || 5)) || (3 -> (6 || 7))))

    for ((dom, ns) <- Seq((0, Seq(1, 2, 3, 4, 5, 6, 7)),
                          (1, Seq(2, 3, 4, 5, 6, 7)),
                          (2, Seq(4, 5)),
                          (3, Seq(6, 7)));
         n <- ns) {
      checkCompare(dom, dom, 0)
      checkCompare(dom, n, -1)
      checkCompare(n, dom, 1)
    }

    for (x <- Seq(2, 4, 5); y <- Seq(3, 6, 7)) {
      checkUnordered(x, y)
      checkUnordered(y, x)
    }
  }

  test("max&min") {
    calcDominatorsOver(0 -> 1 -> ((2 -> (4 || 5)) || (3 -> (6 || 7))))

    for ((dom, ns) <- Seq((0, Seq(0, 1, 2, 3, 4, 5, 6, 7)),
                          (1, Seq(1, 2, 3, 4, 5, 6, 7)),
                          (2, Seq(2, 4, 5)),
                          (3, Seq(3, 6, 7)));
         n <- ns) {
      checkMin(dom, n, dom)
      checkMax(dom, n, n)
    }

    for (x <- Seq(2, 4, 5); y <- Seq(3, 6, 7)) {
      a[Throwable] should be thrownBy { dominators.max(x, y) }
      a[Throwable] should be thrownBy { dominators.max(y, x) }
    }
  }
  
  test("depth") {
    calcDominatorsOver(0 -> 1 -> ((2 -> (4 || 5)) || (3 -> (6 || 7))))
    checkDepth(0, 1)
    checkDepth(1, 2)
    checkDepth(2, 3)
    checkDepth(3, 3)
    checkDepth(4, 4)
    checkDepth(5, 4)
    checkDepth(6, 4)
    checkDepth(7, 4)
  }
  
  test("find GCA") {
    calcDominatorsOver(0 -> 1 -> ((2 -> (4 || 5)) || (3 -> (6 || 7))))

    for (n <- 0 until 8) {
      checkGCA(0, n, 0)
      checkGCA(n, n, n)
    }

    for (n <- 1 until 8) {
      checkGCA(1, n, 1)
    }
    
    for (n <- Seq(2, 4, 5); m <- Seq(3, 6, 7)) {
      checkGCA(n, m, 1)
    }

    checkGCA(4, 5, 2)
    checkGCA(6, 7, 3)
  }
  
  test("range") {
    calcDominatorsOver(0 -> 1 -> ((2 -> (4 || 5)) || (3 -> (6 || 7))))

    for (n <- 0 until 8) {
      checkRange(n, n, n)
    }

    checkRange(4, 0,   4, 2, 1, 0)
    checkRange(5, 0,   5, 2, 1, 0)
    checkRange(6, 0,   6, 3, 1, 0)
    checkRange(7, 0,   7, 3, 1, 0)

    checkRange(4, 1,   4, 2, 1)
    checkRange(5, 1,   5, 2, 1)
    checkRange(6, 1,   6, 3, 1)
    checkRange(7, 1,   7, 3, 1)

    checkRange(2, 0,   2, 1, 0)
    checkRange(3, 0,   3, 1, 0)

    checkRange(2, 1,   2, 1)
    checkRange(3, 1,   3, 1)

    for (n <- Seq(2, 4, 5); m <- Seq(3, 6, 7)) {
      a[Throwable] should be thrownBy { dominators.range(n, m) }
    }
  }

  test("children") {
    calcDominatorsOver(0 -> (1 || 2) -> 3 -> 4)

    checkChildren(0,   1, 2, 3)
    checkChildren(1    )
    checkChildren(2    )
    checkChildren(3,   4)
    checkChildren(4    )
  }

  test("simple update") {
    calcDominatorsOver(0 -> 1)
    checkDom(1, 0)
    graph.addEdges(0 -> 2)
    a[Throwable] should be thrownBy { checkDom(2, 0) }
    updateOne(2)
    checkDom(2, 0)
  }

  test("update with 2 preds") {
    calcDominatorsOver(0 -> (1 || 2))
    checkDom(1, 0)
    checkDom(2, 0)
    graph.addEdges((1 || 2) -> 3)
    a[Throwable] should be thrownBy { checkDom(3, 0) }
    updateOne(3)
    checkDom(3, 0)
  }

  test("multiple update") {
    calcDominatorsOver(0 -> (1 || !2) -> 3)
    checkDom(1, 0)
    checkDom(2, 0)
    checkDom(3, 1)

    graph.removeEdges(0 -> 2)
    updateOne(2)
    checkDom(1, 0)
    a[Throwable] should be thrownBy { checkDom(2, 0) }
    checkDom(3, 1)

    graph.addEdges(3 -> 2)
    updateOne(2)
    checkDom(1, 0)
    checkDom(2, 3)
    checkDom(3, 1)
  }

  test("update with backward branch") {
    calcDominatorsOver(0 -> 1)
    checkDom(1, 0)
    a[Throwable] should be thrownBy { checkDom(2, 0) }
    graph.addEdges(1 -> 2)
    updateOne(2, 0)
    checkDom(1, 0)
    checkDom(2, 0)
  }

  test("unreachable code") {
    calcDominatorsOver((0 -> 1) |>| (5 -> 6 -> 1))
    checkDom(1, 0)
    dominators.dominates(0, 6) shouldBe true
    dominators.dominates(1, 6) shouldBe true
    dominators.dominates(5, 6) shouldBe true
    dominators.dominates(6, 5) shouldBe true
  }

  test("long chain") {
    val N = 10000
    var g: SubGraph = 0 -> 1
    for (k <- 2 to N) { g = g -> k }
    calcDominatorsOver(g)
    for (k <- 0 to N) {
      dominators.dominates(0, k) shouldBe true
      dominators.dominates(k, 0) shouldBe (k == 0)
      dominators.dominates(k, N-k) shouldBe (k <= N-k)
    }
  }
}

