/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph.ordering

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.Seq

/** Tests for post order calculation.
  *
  * @see [[com.huawei.excelsior.jet.compiler.newbaseline.frontend.TopSortSuite]]
  */
class TopSortSuite extends CommonSuite with IntGraphBuilderDSL {

  var topSort: TopSort[N] = _

  private def calc0(start: SubGraph, makeTopSort: MutableGraph => TopSort[N]): Unit = {
    makeGraph(start)
    topSort = makeTopSort(graph)
    for (x <- topSort.order) topSort.order(topSort.number(x)) should be (x)
  }

  private def calcTopSortOf(start: SubGraph): Unit = {
    calc0(start, _.topSort)
  }

  private def calcTopSortOf(start: SubGraph, startNodes: N*): Unit = {
    calc0(start, _.topSort(startNodes.iterator))
  }

  private def sortedSeq = topSort.order.toSeq
  
  private def lt(x: N, y: N) = topSort.lt(x, y)

  test("simple several nodes") {
    calcTopSortOf(0 -> 1 -> 2 -> 3 -> 4)
    sortedSeq should be (Seq(0, 1, 2, 3, 4))
  }

  test("some dolled nodes") {
    calcTopSortOf(1 -> 3 -> 4)
    sortedSeq should be (Seq(1, 3, 4))
  }

  test("diamond") {
    calcTopSortOf(0 -> (1 || 2) -> 3)
    sortedSeq should be (Seq(0, 1, 2, 3))
    lt(0, 1) should be (true)
    lt(0, 2) should be (true)
    lt(1, 3) should be (true)
    lt(2, 3) should be (true)
  }

  test("cycle") {
    calcTopSortOf(0 -> wd(1 -> 2 -> 3) -> 4)
    sortedSeq should be (Seq(0, 1, 2, 3, 4))
  }

  test("irreducible") {
    calcTopSortOf(0 -> ((dw(1 -> 2 -> 3) -> 4) || 2))
    sortedSeq should be (Seq(0, 2, 3, 1, 4))
  }

  test("triangle-1") {
    calcTopSortOf(0 -> ((1 -> 2) || 2))
    sortedSeq should be (Seq(0, 1 ,2))
  }

  test("triangle-2") {
    calcTopSortOf(0 -> (2 || (1 -> 2)))
    sortedSeq should be (Seq(0, 1 ,2))
  }

  test("several-start-nodes") {
    calcTopSortOf(0 -> ((1 -> 2) || dw(3 -> 4 -> 5) -> 6) -> 7, 1, 4)
    sortedSeq should be (Seq(1, 2, 4, 5, 3, 6, 7))
  }

}

