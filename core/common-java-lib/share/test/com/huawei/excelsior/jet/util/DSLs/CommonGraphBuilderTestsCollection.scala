/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.graph.BiGraph
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.language.implicitConversions

/** Common tests for all graph builders.
  */
trait CommonGraphBuilderTestsCollection extends CommonSuite with IntGraphBuilderDSL {

  type NN // avoid collision with N from GraphBuilderDSL
  type G <: BiGraph[NN]

  def make(start: SubGraph): Unit

  def g: G

  // for calling g.succs(node)
  implicit def int2n(node: Int): NN

  // for graph building
  implicit def int2graph(node: Int): SubGraph


  test("graph with many enters") {
    intercept[Throwable] {
      make((3 || 4) -> 5)
    }
  }

  test("straight graph") {
    // 0 - 1 - 2
    make(0 -> 1 -> 2)
    g.succs(0) should beIterator[NN] (1)
    g.preds(1) should beIterator[NN] (0)
    g.succs(1) should beIterator[NN] (2)
    g.preds(2) should beIterator[NN] (1)
    g.succs(2) should be(empty)
  }

  test("conditional graph") {
    //   0
    //  / \
    // 1   2
    //  \ /
    //   3
    make(0 -> (1 || 2) -> 3)
    g.succs(0) should beIterator[NN] (1, 2)
    for (i <- 1 to 2) {
      g.preds(i) should beIterator[NN] (0)
      g.succs(i) should beIterator[NN] (3)
    }
    g.preds(3) should beIterator[NN] (1, 2)
    g.succs(3) should be(empty)
  }

  test("self-looped graph") {
    // 0 -> 1 -
    //      |  |
    //       --
    make(0 -> dw(1))
    g.preds(1) should beIterator[NN] (0, 1)
    g.succs(1) should beIterator[NN] (1)
  }

  test("graph with do-while loop") {
    // 0 - 1 - 2 - 3 - 4
    //      \     /
    //        ---
    make(0 -> dw(1 -> 2 -> 3) -> 4)
    g.preds(1) should beIterator[NN] (0, 3)
    g.succs(1) should beIterator[NN] (2)
    g.succs(3) should beIterator[NN] (1, 4)
  }

  test("graph with while-do loop") {
    // 0 - 1 - 4
    //     |\
    //     2-3
    make(0 -> wd(1 -> 2 -> 3) -> 4)
    g.preds(1) should beIterator[NN] (0, 3)
    g.succs(1) should beIterator[NN] (2, 4)
    g.succs(3) should beIterator[NN] (1)
  }

  test("graph with loop with two back-edges") {
    // 0 - 1 = 2
    //      \\
    //        3
    make(0 -> dw(1 -> (2 || 3)))
    g.preds(1) should beIterator[NN] (0, 2, 3)
  }

  test("simple graph with end") {
    make(0 -> end)
    g.succs(0) should be(empty)
  }

  test("hard graph with end") {
    val sub = dw(3 -> ((4 -> end) || (5 -> (6 || 7))))
    make(0 -> (1 || 2) -> sub)
    g.preds(1) should beIterator[NN] (0)
    g.preds(2) should beIterator[NN] (0)
    g.preds(3) should beIterator[NN] (1, 2, 6, 7)
    g.preds(4) should beIterator[NN] (3)
    g.preds(5) should beIterator[NN] (3)
    g.preds(6) should beIterator[NN] (5)
    g.preds(7) should beIterator[NN] (5)
  }

  test("graph with parallel edges") {
    make((0 -> 1 -> 2 -> 3) |>| (1 -> 2))
    g.succs(1) should have size (2)
    g.succs(1) should beIterator[NN] (2, 2)
    g.preds(2) should have size (2)
    g.preds(2) should beIterator[NN] (1, 1)
  }

  test("graph with custom loop with two exits") {
    //            -------- 5
    //          /   /
    // 0 - 1 - 2 - 3 - 4
    //      \         /
    //        -------
    make(0 -> lp(1 -> 2 -> 3 -> 4, exits(2, 3)) -> 5)
    g.preds(1) should beIterator[NN] (0, 4)
    g.preds(4) should beIterator[NN] (3)
    g.preds(5) should beIterator[NN] (2, 3)
  }

  test("graph with node with many exits") {
    make(0 -> (1 || 2 || 3 || 4) -> 5)
    for (i <- 1 to 4) {
      g.preds(i) should beIterator[NN] (0)
    }
    g.preds(5) should beIterator[NN] (1, 2, 3, 4)
  }
}
