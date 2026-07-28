/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.LocalNodesBuilder

/**
 * Tests for NodeReplaceOptimizer.
 */
class NodeReplaceOptimizerSuite extends CompilerSuite
                                   with LocalNodesBuilder {

  private def replace(pairs: (Int, Int)*) = bulkReplace {
    for ((x, y) <- pairs) {
      replaceTransitively(getLNode(x), getLNode(y))
    }
  }

  private def p(x: Int, y: Int): (Int, Int) = (x, y)

  private def checkReplaced(pairs: (Node, Node)*): Unit = {
    for (p <- pairs) {
      p._1.deref should equal (p._2)
    }
  }

  test("simple") {
    lNode(3, 0, 1)
    lNode(4, 2, 0)

    replace(p(2, 1))

    checkLNodes(0, 1, 3, 4)
    checkLNodeArgs(3, 0, 1)
    checkLNodeArgs(4, 1, 0)
  }

  test("one optimizations level") {
    lNode(3, 0, 1)
    lNode(4, 0, 2)

    replace(p(2, 1))

    checkLNodes(0, 1, 3)
    checkLNodeArgs(3, 0, 1)
  }

  test("two optimizations level") {
    lNode(3, 0, 1)
    lNode(4, 0, 2)
    lNode(5, 0, 3)
    lNode(6, 0, 4)
    lNode(7, 5, 6)

    replace(p(2, 1))

    checkLNodes(0, 1, 3, 5, 7)
    checkLNodeArgs(3, 0, 1)
    checkLNodeArgs(5, 0, 3)
    checkLNodeArgs(7, 5, 5)
  }

  test("some cases") {
    // case 1
    lNode(3, 0, 1)
    lNode(4, 0, 2)
    lNode(5, 0, 3)
    lNode(6, 0, 4)
    lNode(7, 5, 6)

    // case 2
    lNode(11, 8, 9)
    lNode(12, 8, 10)

    replace(p(2, 1), p(10, 9))

    checkLNodes(0, 1, 3, 5, 7, 8, 9, 11)
    checkLNodeArgs(3, 0, 1)
    checkLNodeArgs(5, 0, 3)
    checkLNodeArgs(7, 5, 5)
    checkLNodeArgs(11, 8, 9)
  }

  test("simple pyramid") {
    lNode(3, 0, 1)
    lNode(4, 1, 2)

    lNode(8, 5, 6)
    lNode(9, 6, 7)

    replace(p(5, 0), p(6, 1), p(7, 2))

    checkLNodes(0, 1, 2, 3, 4)
    checkLNodeArgs(3, 0, 1)
    checkLNodeArgs(4, 1, 2)
  }

  test("cheops pyramids") {
    lNode(3, 0, 1)
    lNode(4, 1, 2)
    lNode(5, 3, 4)

    lNode(9, 6, 7)
    lNode(10, 7, 8)

    lNode(14, 11, 12)
    lNode(15, 12, 13)

    lNode(19, 16, 17)
    lNode(20, 17, 18)

    replace(p(6, 0), p(7, 1), p(8, 2), p(11, 0), p(12, 1), p(13, 2), p(16, 0), p(17, 1), p(18, 2))

    checkLNodes(0, 1, 2, 3, 4, 5)
    checkLNodeArgs(3, 0, 1)
    checkLNodeArgs(4, 1, 2)
    checkLNodeArgs(5, 3, 4)
  }

  test("double replacement") {
    val n0 = lNode(0)
    val n1 = lNode(1, 2, 3)
    val n4 = lNode(4, 5, 1)
    val n6 = lNode(6, 7, 4)
    lNode(8, 9, 6)

    replace(p(6, 4), p(4, 1), p(1, 0))

    checkLNodes(0, 2, 3, 5, 7, 8, 9)
    checkLNodeArgs(8, 9, 0)

    checkReplaced((n6, n0), (n4, n0), (n1, n0))
  }

}