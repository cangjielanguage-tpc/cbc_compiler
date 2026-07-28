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
import com.huawei.excelsior.jet.util.graph.ordering.DepthFirstSearch

/** Tests for DFS algorithm.
 */
class DepthFirstSearchSuite extends CommonSuite with IntGraphBuilderDSL {

  var dfs: DepthFirstSearch[N] = _

  private def calcDFSOf(start: SubGraph): Unit = {
    makeGraph(start)
    dfs = DepthFirstSearch(graph)
    for (x <- dfs.order) dfs.order(dfs.number(x)) should be (x)
  }

  private def isAncestor(x: Int, y: Int) = {
    dfs.isAncestor(x, y)
  }

  private def sortedSeq = dfs.order.toSeq

  private def lt(x: N, y: N) = dfs.lt(x, y)

  test("simple several nodes") {
    calcDFSOf(0 -> 1 -> 2 -> 3 -> 4)
    sortedSeq should be (Seq(0, 1, 2, 3, 4))
    isAncestor(3, 4) should be (true)
    isAncestor(4, 3) should be (false)
    isAncestor(4, 4) should be (true)
  }

  test("some dolled nodes") {
    calcDFSOf(1 -> 3 -> 4)
    sortedSeq should be (Seq(1, 3, 4))
  }

  test("diamond") {
    calcDFSOf(0 -> (1 || 2) -> 3)
    sortedSeq should (be (Seq(0, 1, 3, 2))
                   or be (Seq(0, 2, 3, 1)))
    isAncestor(0, 3) should be (true)
    lt(0, 1) should be (true)
    lt(0, 2) should be (true)
    lt(0, 3) should be (true)
  }

  test("cylce") {
    calcDFSOf(0 -> wd(1 -> 2 -> 3) -> 4)
    sortedSeq should (be (Seq(0, 1, 2, 3, 4))
                   or be (Seq(0, 1, 4, 2, 3)))
    isAncestor(1, 2) should be (true)
    isAncestor(1, 3) should be (true)
    isAncestor(1, 4) should be (true)
    isAncestor(3, 1) should be (false)
  }

  test("irreducible") {
    calcDFSOf(0 -> ((dw(1 -> 2 -> 3) -> 4) || 2))
    sortedSeq should (be (Seq(0, 1, 2, 3, 4))
                   or be (Seq(0, 2, 3, 4, 1))
                   or be (Seq(0, 2, 3, 1, 4)))
    isAncestor(0, 2) should be (true)
    isAncestor(0, 3) should be (true)
    isAncestor(3, 4) should be (true)
  }
}

