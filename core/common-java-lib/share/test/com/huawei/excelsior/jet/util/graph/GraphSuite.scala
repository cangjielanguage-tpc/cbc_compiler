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
import com.huawei.excelsior.jet.util.graph.{BiGraph, Graph}

/**
 * Tests for graph methods.
 */
class GraphSuite extends CommonSuite with IntGraphBuilderDSL {

  private def checkEquals(x: SubGraph, y: SubGraph): Unit = {
    makeGraph(x) should beTopologicallyEqual (makeGraph(y))
  }

  private def checkNotEquals(x: SubGraph, y: SubGraph): Unit = {
    makeGraph(x) shouldNot beTopologicallyEqual (makeGraph(y))
  }

  private def checkReverse(x: SubGraph)(sink: Int)(y: SubGraph): Unit = {
    makeGraph(x).reverse(sink) should beTopologicallyEqual (makeGraph(y))
  }

  private def calcSize(x: SubGraph) = {
    val g = makeGraph(x)
    g.collectReachableFrom(g.start).size
  }

  private def subGraphOf(graph: SubGraph, entry: Int, anchors: Int*) = {
    BiGraph.anchorSubGraph(makeGraph(graph), entry, anchors: _*)
  }

  private var _graph: Graph[Int] = _

  private def init(x: SubGraph): Unit = {
    _graph = makeGraph(x)
  }

  test("topologically equation 1") {
    checkEquals(
      0 -> 1 -> 0,
      dw(0 -> 1))
  }

  test("topologically equation 2") {
    checkEquals(
      0 -> 1 -> 0,
      wd(0 -> 1))
  }

  test("topologically equation 3") {
    checkEquals(
      0 -> 1 -> 0 -> 2,
      wd(0 -> 1) -> 2)
  }

  test("topologically equation 4") {
    checkNotEquals(
      0 -> 1 -> 0 -> 2,
      dw(0 -> 1) -> 2)
  }

  test("topologically equation 5") {
    checkEquals(
      0 -> ((1 -> 0) || 2),
      wd(0 -> 1) -> 2)
  }

  test("topologically equation 6") {
    checkNotEquals(
      0 -> ((1 -> (3 || 4)) || 2) -> 5,
      0 -> (1 || (2 -> (3 || 4))) -> 5)
  }

  test("topologically equation 7") {
    checkEquals(
      dw(0 -> ((dw(1 -> 2) -> 3) || (dw(4 -> dw(5) -> 6) -> dw(7 -> 8)))),
      dw(5 -> ((dw(8 -> 1) -> 4) || (dw(7 -> dw(0) -> 3) -> dw(2 -> 6)))))
  }

  test("topologically equation 8") {
    checkNotEquals(
      dw(0 -> ((dw(1 -> 2) -> 3) || (dw(4 -> dw(5) -> 6) -> dw(7 -> 8)))),
      dw(5 -> ((dw(8 -> 1) -> 5) || (dw(7 -> dw(0) -> 3) -> dw(2 -> 6)))))
  }

  test("size 1") {
    calcSize(0) should be (1)
  }

  test("size 2") {
    calcSize(0 -> 1) should be (2)
  }

  test("size 3") {
    calcSize(0 -> 1 -> 0 -> 2) should be (3)
  }

  test("size 4") {
    calcSize(0 -> ((1 -> 2 -> 4) || (3 -> ((4 -> (2 || 5))|| (5 -> 4))))) should be (6)
  }

  test("sub graph simple") {
    subGraphOf(0 -> (1 || (2 -> 3 -> ((4 -> 5 -> 6) || (7 -> 8 -> 9)))) -> 10,
      3, 5, 8) should be (Set(3, 4, 5, 7, 8))
  }

  test("sub graph with loop header") {
    subGraphOf(0 -> dw(1 -> (2 || (3 -> 4 -> 5)) -> 6) -> 7,
      1, 4) should be (Set(1, 3, 4))
  }

  test("sub graph with loop pre-header") {
    subGraphOf(0 -> dw(1 -> (2 || (3 -> 4 -> 5)) -> 6) -> 7,
      0, 4) should be (Set(0, 1, 2, 3, 4, 5, 6))
  }

  test("version graph with multiple switch") {
    // this test shows why 'anchor sub graph' builds from anchors to entry
    // otherwise all branches of switch will be included in subgraph
    subGraphOf(0 -> ((1 -> 2) || (3 -> 4) || 5 || 6) -> 7,
      0, 1, 4) should be (Set(0, 1, 3, 4))
  }

  test("reverse") {
    checkReverse (0 -> 1 -> 2) (2) (2 -> 1 -> 0)
    checkReverse (0 -> 1 -> 2) (1) (1 -> 0)
    checkReverse (0 -> 1 -> 2) (0) (0)

    checkReverse (0 -> (1 || 2) -> 3) (3) (3 -> (1 || 2) -> 0)
    checkReverse (0 -> (1 || 2) -> 3) (1) (1 -> 0)

    checkReverse (0 -> (1 || dw(2 -> 3))) (1) (1 -> 0)
    checkReverse (0 -> (1 || dw(2 -> 3))) (2) ((2 -> 0) |>| wd(2 -> 3))
    checkReverse (0 -> (1 || dw(2 -> 3))) (3) (3 -> 2 -> (0 || 3))
  }

}