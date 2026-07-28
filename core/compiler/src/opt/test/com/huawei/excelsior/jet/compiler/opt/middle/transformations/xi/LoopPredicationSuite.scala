/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.options.BoolOption.LoopPredication
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.UCEComponent
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}

class LoopPredicationSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with CFGTransformationDSL
     with IRTransformationsCollection
     with UCEComponent
     with LoopPredication {

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(LoopPredication)
  }

  override def transformation(): Unit = {
    while (predicateLoops() | completeSSA() | eliminateUnreachableCode()) { }
  }

  override def nodeWeight(n: Node) = 0.0

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new UnnamedAttribute(() => addObjNode()),

    ) ++ super.parsableAttributes()
  }

  test("predication - simple") {
    beforeWithPost(0@@"x" -> dw(1@@"nc(x)" -> (2@@"spinal()" || 3@@"spinal()") -> 4) -> 5, {
      0.blockEnd shouldBe an[If]
      val predicate = 0.blockEnd.asInstanceOf[If]
      predicate.selector should matchPattern {
        case Cmp(Condition.NE, l, Null()) if l == n("x") =>
      }

      transform(EmptyBlocksElimination) // hack for simpler after template and Halt check

      val coldGoto = predicate.falseBlock.blockEnd
      coldGoto shouldBe a[Goto]
      coldGoto.asInstanceOf[Goto].target.blockEnd shouldBe a[Halt]

      all[NullCheck] shouldBe empty
    })
    after(0 -> ((dw(1 -> 10 -> (2 || 3) -> 4) -> 5) || 66 -> 67))
  }

  test("predication - hard") {
    beforeWithPost(0@@"x" -> dw(1 -> (2@@"spinal()" || 3@@"spinal()") -> 4@@"nc(x)") -> 5, {
      0.blockEnd shouldBe an[If]
      val predicate = 0.blockEnd.asInstanceOf[If]
      predicate.selector should matchPattern {
        case Cmp(Condition.NE, l, Null()) if l == n("x") =>
      }

      transform(EmptyBlocksElimination) // hack for simpler after template and Halt check

      val coldGoto = predicate.falseBlock.blockEnd
      coldGoto shouldBe a[Goto]
      // it's hard to check for halt
      //coldGoto.asInstanceOf[Goto].target.blockEnd shouldBe a[Halt]

      all[NullCheck] shouldBe empty
    })
    after(0 -> ((dw(1 -> (2 || 3) -> 4) -> 5) || (11 -> 12 -> (22 || 33) -> 44)))
  }

  test("predication - JET-13257") {
    before(0@@"x" -> !dw(1@@"nc(x)" -> (2@@"spinal()" || 3@@"spinal()") -> 4@@("xspinal()", "ret(ic(0))") -> xb(5)), {
      removeHandlerAnchors()
    }, {
      0.blockEnd shouldBe an[If]
      val predicate = 0.blockEnd.asInstanceOf[If]
      predicate.selector should matchPattern {
        case Cmp(Condition.NE, l, Null()) if l == n("x") =>
      }

      transform(EmptyBlocksElimination) // hack for simpler after template and Halt check

      val coldGoto = predicate.falseBlock.blockEnd
      coldGoto shouldBe a[Goto]
      coldGoto.asInstanceOf[Goto].target.blockEnd shouldBe a[Halt]

      all[NullCheck] shouldBe empty
    })
    after(0 -> (!dw(1 -> 10 -> (2 || 3) -> 4 -> (!44 || xb(5))) || 66 -> 67))
  }

  test("no predication - no dominating check") {
    before(0@@"x" -> dw(1 -> (2@@"nc(x)" || 3) -> 4) -> 5)
    after(0 -> dw(1 -> (2 || 3) -> 4) -> 5)
  }

}
