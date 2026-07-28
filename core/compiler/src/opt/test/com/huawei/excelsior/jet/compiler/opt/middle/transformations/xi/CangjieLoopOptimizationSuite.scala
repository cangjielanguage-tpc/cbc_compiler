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
import com.huawei.excelsior.jet.compiler.opt.ir.ConstBranchElimination
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.middle.{DCEComponent, SimplifyComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, CFGTransformationDSL, GlobalNodesBuilder}

class CangjieLoopOptimizationSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with CFGTransformationDSL
    with ConstBranchElimination
    with UCEComponent
    with DCEComponent
    with SimplifyComponent
    with IRTransformationsCollection
    with CangjieLoopOptimization {

  startPhase(CompilerPhase.PostInline)

  override def transformation(): Unit = {
    while (simplifyResidualCangjieForInLoops() | completeSSA() | simplifyIR() | eliminateConstBranches() |
      eliminateUnreachableCode() | eliminateDeadCode() | normalizeAllLoops() | transform(BlocksConnectionTransformation)) { }
  }

  override def nodeWeight(n: Node) = 0.0

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("cmpne")({
        case Seq(l, r) => Cmp(l.tpe, Condition.NE)(l, r)
      }),

    ) ++ super.parsableAttributes()
  }

  test("residual loop - trivial") {
    beforeWithPost(0@@("t=true()", "x=ic(1)") -> wd(1@@("p=phi(t,c)", "i=phi(x,a)", "a=add(i,ic(1))", "c=cmpne(ic(10),i)", "b=if(p)") -> 2) -> 3@@"ret(i)", {
      all[Phi].filter(_.tpe == ConditionType) shouldBe empty
      n("b").asInstanceOf[If].selector shouldBe n("c")
    })
    after(dw(2) -> 3)
  }

  test("residual loop - complex") {
    beforeWithPost(0@@("t=true()", "x=ic(1)") -> wd(1@@("p=phi(t,c)", "i=phi(x,a)", "a=add(i,ic(1))", "c=cmpne(ic(10),i)", "b=if(p)") -> 2 -> (3@@"spinal()" || 4) -> 5) -> 6@@"ret(i)", {
      all[Phi].filter(_.tpe == ConditionType) shouldBe empty
      n("b").asInstanceOf[If].selector shouldBe n("c")
    })
    after(dw(2 -> (3 || 4) -> 5) -> 6)
  }

  test("residual loop negative - many phi uses") {
    before(0@@("t=true()", "x=ic(1)") -> wd(1@@("p=phi(t,c)", "i=phi(x,a)", "a=add(i,ic(1))", "c=cmpne(ic(10),i)", "if(p)") -> 2@@"use(p)" -> (3@@"spinal()" || 4) -> 5) -> 6@@"ret(i)")
    after(wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
  }

  test("residual loop negative - many cmp uses") {
    before(0@@("t=true()", "x=ic(1)") -> wd(1@@("p=phi(t,c)", "i=phi(x,a)", "a=add(i,ic(1))", "c=cmpne(ic(10),i)", "if(p)") -> 2@@"use(c)" -> (3@@"spinal()" || 4) -> 5) -> 6@@"ret(i)")
    after(wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
  }

  test("residual loop - regression codehub #582") {
    // var cond = true
    // var first = true
    // while (cond) {
    //   if (first) {
    //     first = false
    //   } else {
    //     i ++
    //   }
    //   cond = i < 100
    // }
    beforeWithPost(
        0 @@ ("t=true()", "f=false()", "z=ic(0)") ->
        wd(1 @@ ("i_i=phi(z,i_j)", "c_i=phi(t,cmp)", "f_i=phi(t,f_j)", "b=if(c_i)") ->
          2 ->
          (3 ||
           4 @@ "add=spinal()") ->
          5 @@ ("i_j=phi(i_i,add)", "cmp=cmpne(i_j,ic(100))", "f_j=phi(f,f_i)")
        ) ->
        6 @@ "ret(i_i)", {

      n("b").asInstanceOf[If].selector shouldBe n("cmp")
    })
    after(dw(2 -> (3 || 4) -> 5) -> 6)
  }

}
