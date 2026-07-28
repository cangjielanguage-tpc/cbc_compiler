/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.newbaseline.testutils.DSLs.{BlockGraph, BlockGraphBuilderDSL}

/** Tests for top sort and unreachable blocks elimination.
  *
  * @see [[com.huawei.excelsior.jet.compiler.opt.graph.ordering.TopSortSuite]]
  */
class TopSortSuite extends CompilerSuite with BlockGraphBuilderDSL {

  var sortedSeq: collection.Seq[Block] = _

  private def checkTopSortOf(start: => SubGraph, expectedOrders: => collection.Seq[collection.Seq[Block]], extraCheck: () => Unit): Unit = {
    val BlockGraph(entry, blocks) = makeGraph(start)
    sortedSeq = TopSort.sortAndRemoveUnreachable(blocks, entry, null)
    expectedOrders should contain (sortedSeq)
    if (extraCheck != null) {
      extraCheck()
    }
  }

  private def testTwice(name: String, start: => SubGraph, expectedOrders: => collection.Seq[collection.Seq[Block]], extraCheck: () => Unit = null): Unit = {
    test(name) {
      // first we check original graph, which may be covered by fast-path
      checkTopSortOf(start, expectedOrders, extraCheck)
    }
    test(name + " (without fast-path)") {
      // than we construct bad graph which is not covered by fast path (add some unreachable code)
      checkTopSortOf(start |>| (998 -> 999), expectedOrders, extraCheck)
    }
  }

  private def bs(xs: Int*): collection.Seq[Block] = xs map b

  testTwice("simple several nodes",
    0 -> 1 -> 2 -> 3 -> 4,
    Seq(bs(0, 1, 2, 3, 4)))

  testTwice("some dolled nodes",
    1 -> 3 -> 4,
    Seq(bs(1, 3, 4)))

  testTwice("diamond",
    0 -> (1 || 2) -> 3,
    Seq(bs(0, 1, 2, 3),
      bs(0, 2, 1, 3)))

  testTwice("cycle",
    0 -> wd(1 -> 2 -> 3) -> 4,
    Seq(bs(0, 1, 2, 3, 4),
      bs(0, 1, 4, 2, 3)))

  testTwice("irreducible",
    0 -> ((dw(1 -> 2 -> 3) -> 4) || 2),
    Seq(bs(0, 1, 2, 3, 4),
      bs(0, 2, 3, 4, 1),
      bs(0, 2, 3, 1, 4)))

  testTwice("triangle-1",
    0 -> ((1 -> 2) || 2),
    Seq(bs(0, 1 ,2)))

  testTwice("triangle-2",
    0 -> (2 || (1 -> 2)),
    Seq(bs(0, 1 ,2)))

  testTwice("trivial with bad bytecode order (anti-fast-path)",
    0 -> 5 -> (3 || (8 -> 3)),
    Seq(bs(0, 5, 8, 3)))

  testTwice("simple unreachable",
    (0 -> 1 -> 2) |>| (9 -> 2),
    Seq(bs(0, 1 ,2)),
    () => {
      (b(2).inputs map (_.block): collection.Seq[Block]) should be(Seq(b(1)))
      b(9).end.outputs should be(empty)
    })

  testTwice("unreachable subgraph",
    (0 -> 1 -> 2) |>| (3 -> (4 || 5) -> 6 -> (7 || 2)),
    Seq(bs(0, 1 ,2)),
    () => {
      (b(2).inputs map (_.block): collection.Seq[Block]) should be(Seq(b(1)))
      b(6).end.outputs should be(Seq(b(7)))
    })

}

