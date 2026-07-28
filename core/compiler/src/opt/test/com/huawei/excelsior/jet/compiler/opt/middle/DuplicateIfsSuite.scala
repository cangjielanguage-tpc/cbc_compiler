/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.types.Guards.CHABitGuard

class DuplicateIfsSuite extends CompilerSuite
  with GlobalNodesBuilder
  with IdempotentOperationsOptimizer {

  startPhase(CompilerPhase.PostInline)

  override def parsableAttributes() = Seq(
    new SimpleAttribute("obj")({ case Seq() => addObjNode() }),
    new SimpleAttribute("tau")({ case Seq(obj) => TypeTest(CHABitGuard, TauInfo.Static)(obj) })
  ) ++ super.parsableAttributes()

  def checkSelector(selector: Node, blocks: Block*): Unit = {
    blocks foreach (_.blockEnd.asInstanceOf[If].selector shouldBe selector)
  }

  test("If with same condition in true branch") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)") -> 1@@"if(c)" -> (2@@"if(c)" -> (4 || 5) || 3) -> 6)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(True(), 2)
  }

  test("If with reversed condition in false branch") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)") -> 1@@"if(c)" ->
      (2 || (3@@"if(not(c))" -> (5 || 6))) -> 4)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(True(), 3)
  }

  test("If with reversed condition in true branch") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)") -> 1@@"if(c)" ->
      (2@@"if(not(c))" -> (5 || 6) || 3) -> 4)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(False(), 2)
  }

  test("If with different predicates") {
    makeCFG(0@@("x", "y", "c", "c1=cmp(x,y)", "c2=cmp(x,c)") -> 1@@"if(c1)" -> (2@@"if(c2)" -> (4 || 5) ||
      3@@"if(not(c2))" -> (6 || 7)) -> 8)

    optimizeDuplicateIfs() shouldBe false
  }

  test("Many If's with similar predicates") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)") -> 1@@"if(c)" -> (2@@"if(c)" -> (4@@"if(not(c))" -> (9 || 10) || 5) ||
      3@@"if(not(c))" -> (6@@"if(c)" -> (11 || 12) || 7)) -> 8)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(True(), 2, 3)
    checkSelector(False(), 4, 6)
  }

  test("If with phi in true branch") {
    makeCFG(0 -> (1@@"x=bc(true)" || 2@@"y=bc(false)") -> 4@@("p=phi(x,y)", "if(p)") ->
      (6@@"if(p)" -> (8 || 9) || 7) -> 10)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(True(), 6)
  }

  test("If with reversed phi in true branch") {
    makeCFG(0 -> (1@@"x=bc(true)" || 2@@"y=bc(false)") -> 4@@("p=phi(x,y)", "if(p)") ->
      (6@@"if(not(p))" -> (8 || 9) || 7) -> 10)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(False(), 6)
  }

  test("If with tautest") {
    makeCFG(0@@"x=obj()" -> 1@@("t=tau(x)", "if(t)") ->
      (2@@"if(not(t))" -> (4 || 5) || 3@@"if(t)" -> (7 || 8)) -> 6)

    optimizeDuplicateIfs() shouldBe true
    checkSelector(False(), 2, 3)
  }
}