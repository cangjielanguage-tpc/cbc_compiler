/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class ControlNodeDominationSuite extends CompilerSuite with GlobalNodesBuilder {

  def checkUniversalProperties(): Unit = {
    removeHandlerAnchors()
    eliminateUnreachableCode()

    for (x <- all[ControlNode] if x.block.reachable) {
      val ix = x.idom
      if (ix != null) {
        ix shouldNot be (x)
        (ix dominates x) should be (true)
      }

      for (y <- all[ControlNode] if y.block.reachable) {
        (x dominates y) should be ((x nearestDom y) == x)

        (x nearestDom y) should be (y nearestDom x)

        if (x != y && (x dominates y)) {
          (y dominates x) should be (false)
        }

        if (ix != null && y != ix && y != x) {
          ((ix dominates y) && (y dominates x)) should be (false)
        }
      }
    }
  }

  def x = n("x").asInstanceOf[SpinalNode]
  def y = n("y").asInstanceOf[SpinalNode]
  def z = n("z").asInstanceOf[SpinalNode]
  def c = n("c").asInstanceOf[SpinalNode]

  test("simple") {
    makeCFG(
      0@@(
        "x=spinal()",
        "y=spinal()",
        "z=spinal()"
      ))

    checkUniversalProperties()

    (x dominates y) should be (true)
    (y dominates y) should be (true)
    (z dominates y) should be (false)
    (y dominates b(0).blockEnd) should be (true)

    x.idom should be (b(0))
    y.idom should be (x)
    b(0).blockEnd.idom should be (z)
    entryBlock.idom should be (null)

    (x nearestDom x) should be (x)
    (x nearestDom y) should be (x)
    (y nearestDom b(0).blockEnd) should be (y)
  }

  test("diamond") {
    makeCFG(
      0@@
        "x=spinal()" ->
      (1@@"y=spinal()" || 2@@"z=spinal()") ->
      3)

    val if0 = b(0).blockEnd.asInstanceOf[If]
    assert(if0.trueBlock == b(1) && if0.falseBlock == b(2))

    checkUniversalProperties()

    (x dominates y) should be (true)
    (z dominates y) should be (false)
    (y dominates z) should be (false)
    (x dominates if0) should be (true)
    (x dominates if0.trueExit) should be (true)
    (if0 dominates b(1)) should be (true)
    (if0 dominates b(2)) should be (true)
    (if0.trueExit dominates b(1)) should be (true)
    (if0.trueExit dominates b(2)) should be (false)

    y.idom should be (b(1))
    b(1).idom should be (if0.trueExit)
    b(3).idom should be (if0)
    if0.trueExit.idom should be (if0)

    (x nearestDom y) should be (x)
    (y nearestDom z) should be (if0)
    (y nearestDom b(3)) should be (if0)
    (if0 nearestDom if0.falseExit) should be (if0)
    (if0.trueExit nearestDom if0.falseExit) should be (if0)
    (if0.trueExit nearestDom b(1)) should be (if0.trueExit)
    (if0.trueExit nearestDom b(2)) should be (if0)
  }

  test("single throwing") {
    makeCFG(
      0@@(
        "x=spinal()",
        "y=xspinal()",
        "z=spinal()"
      ))

    checkUniversalProperties()

    (x dominates y.xpoint) should be (true)
    (y.xpoint dominates y) should be (false)

    z.idom should be (y)
    y.xpoint.idom should be (y)

    (x nearestDom y.xpoint) should be (x)
    (y nearestDom y.xpoint) should be (y)
    (z nearestDom y.xpoint) should be (y)
  }

  test("single throwing and catching") {
    makeCFG(
      0@@(
        "x=spinal()",
        "y=xspinal()",
        "z=spinal()"
      ) ->
      xb(1)@@
        "c=spinal()"
      )

    checkUniversalProperties()

    (x dominates c) should be (true)
    (y dominates c) should be (true)
    (y.xpoint dominates c) should be (true)
    (z dominates c) should be (false)

    c.idom should be (xb(1))
    xb(1).idom should be (y.xpoint)
    y.xpoint.idom should be (y)

    (x nearestDom c) should be (x)
    (z nearestDom c) should be (y)
    (y.xpoint nearestDom c) should be (y.xpoint)
  }

  test("double throwing") {
    makeCFG(
      0@@(
        "x=spinal()",
        "y=xspinal()",
        "z=xspinal()"
      ) ->
      1)

    checkUniversalProperties()

    (y dominates z.xpoint) should be (true)
    (y.xpoint dominates z.xpoint) should be (false)

    z.xpoint.idom should be (z)

    (y.xpoint nearestDom z.xpoint) should be (y)
  }

  test("double throwing and catching") {
    makeCFG(
      0@@(
        "x=spinal()",
        "y=xspinal()",
        "z=xspinal()"
      ) ->
      xb(1)@@
        "c=spinal()"
      )

    checkUniversalProperties()

    (y dominates c) should be (true)
    (y.xpoint dominates c) should be (false)
    (z.xpoint dominates c) should be (false)

    xb(1).idom should be (y)
    y.xpoint.idom should be (y)

    (y.xpoint nearestDom c) should be (y)
    (z.xpoint nearestDom c) should be (y)
  }

}
