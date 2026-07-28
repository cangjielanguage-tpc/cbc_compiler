/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.SwitchDiamondAbsorption
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}

class SwitchDiamondAbsorptionSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with CFGTransformationDSL
     with SwitchDiamondAbsorption {

  startPhase(CompilerPhase.PostInline)

  override def transformation(): Unit = {
    while (absorbSwitchDiamonds() | completeSSA()) { }
  }

  override def nodeWeight(n: Node) = 0.0

  override def makeDebug = false

  def addSwitchSelector(block: Block, selector: Node): Unit = {
    val switch = block.blockEnd.asInstanceOf[Switch]
    assert(switch.cases == (1 to switch.cases.size))
    switch.selector = selector
  }

  def init(ifBlock: Block, switchBlock: Block)(value: Int): Unit = {
    val x = addNode()
    addCondition(ifBlock, x, IConst(value), Condition.EQ)
    addSwitchSelector(switchBlock, x)
  }

  def fullDiamond = 0 -> 1 -> (2 || 22) -> 3 -> (4 || 5 || 6) -> 7

  def halfDiamond = 0 -> 1 -> (2 -> 3 || !3) -> (4 || 5 || 6) -> 7
  def halfDiamondWithGlue = 0 -> 1@@"x" -> (2@@("y", "u2=use(x)") -> 3@@"p=phi(x,y)" || !3) ->
    (4@@"u4=use(p)" || 5@@"u5=use(p)" || 6@@"u6=use(p)") -> 7

  def absorbed = 0 -> 1 -> 31 -> 3 -> (4 || (2 -> 33 -> 5) || 6) -> 7

  test("absorption") {
    beforeWithPre(halfDiamond, {
      init(1, 3)(1)
    })
    after(absorbed)
  }

  test("absorption with glue code") {
    before(halfDiamondWithGlue, {
      init(1, 3)(1)
    }, {
      n("u2").asInstanceOf[FakeSpinalUnary].inValue should be (n("x"))
      n("u4").asInstanceOf[FakeSpinalUnary].inValue should be (n("x"))
      n("u5").asInstanceOf[FakeSpinalUnary].inValue should be (n("y"))
      n("u6").asInstanceOf[FakeSpinalUnary].inValue should be (n("x"))
    })
    after(absorbed)
  }

  test("no absorption - spinal glue code") {
    beforeWithPre(halfDiamondWithGlue, {
      init(1, 3)(1)
      addSomeCtrlNode(3)
    })
    after(halfDiamondWithGlue)
  }

  test("no absorption - full diamond") {
    beforeWithPre(fullDiamond, {
      init(1, 3)(1)
    })
    after(fullDiamond)
  }

  test("no absorption - no matching case") {
    beforeWithPre(halfDiamond, {
      init(1, 3)(10)
    })
    after(halfDiamond)
  }
}
