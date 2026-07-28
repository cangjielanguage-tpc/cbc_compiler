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
import com.huawei.excelsior.jet.compiler.types.Guards.CHABitGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.options.BoolOption._

import scala.util.chaining.scalaUtilChainingOps

class GradientInvariantLiftingSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with CFGTransformationDSL
    with GradientInvariantLifting {

  override def isPGOHost = true

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(GradientInvariantLifting)
  }

  override def transformation(): Unit = {
    while (liftGradientInvariants()) { }
  }

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("const")({ case Seq() => Fake(ConditionType) tap setCondition }),
      new SimpleAttribute("tau")({ case Seq(obj) => TypeTest(CHABitGuard, TauInfo.Static)(obj) tap setCondition }),
      new SimpleAttribute("vp")({ case Seq() => GradientVersioningPoint() tap setCondition }),
      new SimpleAttribute("obj")({ case Seq() => addObjNode() }),
    ) ++ super.parsableAttributes()
  }

  def br(b: Block) = b.blockEnd.asInstanceOf[If]

  def checkPredicate(b: Block, p: Node): Unit = {
    br(b).selector shouldBe p
  }

  test("no optimization without cold code") {
    before(0@@"vp()" -> (1@@"const()" -> (2 || 3) || 4/*cold*/) -> 5)
    after(0 -> (1 -> (2 || 3) || 4) -> 5)
  }

  test("no optimization without cold code (loop invariant)") {
    before(0@@"vp()" -> (wd(1 -> 2@@"const()" -> (3 || 4) -> 5) -> 6 || 7/*cold*/) -> 8)
    after(0 -> (wd(1 -> 2 -> (3 || 4) -> 5) -> 6 || 7) -> 8)
  }

  test("no optimization without cold code (loop invariant exit)") {
    before(0@@"vp()" -> (wd(1@@"const()" -> 2) -> 3 || 4/*cold*/) -> 5)
    after(0 -> (wd(1 -> 2) -> 3 || 4) -> 5)
  }

  test("no optimization of cold branch") {
    before(0@@"vp()" -> (1 || 2/*cold*/@@"const()" -> (3 || 4@@"coldcode()")) -> 5)
    after(0 -> (1 || 2 -> (3 || 4)) -> 5)
  }

  test("no optimization after merge point") {
    before(0@@"vp()" -> (1 || 2/*cold*/) -> 3@@"const()" -> (4 || 5@@"coldcode()") -> 6)
    after(0 -> (1 || 2) -> 3 -> (4 || 5) -> 6)
  }

  // Note: trying to lift unreachable invariants here can lead to very unexpected problems
  test("no optimization of unreachable") {
    before(0@@"vp()" -> (1 || 2/*cold*/) |>| 3@@"const()" -> (4 || 5@@"coldcode()") -> 1)
    after(0 -> (1 || 2))
  }

  test("no optimization of unreachable point") {
    before(0 |>| 1@@"p=vp()" -> (2@@"a=const()" -> (3 || 4@@"coldcode()") || 5/*cold*/) -> 0)
    after(0)
  }

  test("no optimization of reachable and unreachable points") {
    before(0@@"vp()" -> (1 || 2/*cold*/) |>| 3@@"vp()" -> (4 || 5@@"coldcode()") -> 1)
    after(0 -> (1 || 2))
  }

  test("single invariant") {
    beforeWithPost(0@@"p=vp()" -> (1@@"a=const()" -> (2 || 3@@"coldcode()") || 4/*cold*/) -> 5, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "p")
    })
    after(0 -> (10 -> (1 -> 2 || 4) || !4) -> 5)
  }

  test("single invariant inverted") {
    beforeWithPost(0@@"p=vp()" -> (1@@"a=const()" -> (2@@"coldcode()" || 3) || 4/*cold*/) -> 5, {
      checkPredicate(0, "a") // Note that predicate is not negated, but true and false paths are swapped.
      checkPredicate(br(0).falseBlock, "p")
    })
    after(0 -> (!4 || 10 -> (1 -> 3 || 4)) -> 5)
  }

  test("single tau invariant") {
    beforeWithPost(0@@("p=vp()", "x=obj()") -> (1@@"t=tau(x)" -> (2 || 3@@"coldcode()") || 4/*cold*/) -> 5, {
      checkPredicate(0, Cmp(TRefType, Condition.NE)("x", AnyNull(TRefType)))
      checkPredicate(br(0).trueBlock, TypeTest(CHABitGuard, TauInfo.Static)(br(0).trueBlock, "x"))
      checkPredicate(br(br(0).trueBlock).trueBlock, "p")
    })
    after(0 -> (10 -> (11 -> (1 -> 2 || 4) || !4) || !4) -> 5)
  }

  test("single invariant with unreachable point") {
    beforeWithPost(0@@"p=vp()" -> (1@@"a=const()" -> (2 || 3@@"coldcode()") || 4/*cold*/) -> 5 |>| 6@@"vp()" -> (7 || 8@@"coldcode()") -> 1, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "p")
    })
    after(0 -> (10 -> (1 -> 2 || 4) || !4) -> 5)
  }

  test("consequent invariants") {
    beforeWithPost(0@@"p=vp()" -> (1@@"a=const()" -> (2@@"b=const()" -> (3 || 4@@"coldcode()") || 5@@"coldcode()") || 6/*cold*/) -> 7, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "b")
      checkPredicate(br(br(0).trueBlock).trueBlock, "p")
    })
    after(0 -> (10 -> (20 -> (1 -> 2 -> 3 || 6) || !6) || !6) -> 7)
  }

  test("parallel invariants") {
    beforeWithPost(0@@"p=vp()" -> (1 -> (2@@"a=const()" -> (3 || 4@@"coldcode()") || 5@@"b=const()" -> (6 || 7@@"coldcode()")) || 8/*cold*/) -> 9, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "b")
      checkPredicate(br(br(0).trueBlock).trueBlock, "p")
    })
    after(0 -> (20 -> (50 -> (1 -> (2 -> 3 || 5 -> 6) || 8) || !8) || !8) -> 9)
  }

  test("loop invariant") {
    beforeWithPost(0@@"p=vp()" -> (wd(1 -> 2@@"a=const()" -> (3 || 4@@"coldcode()") -> 5) -> 6 || 7/*cold*/) -> 8, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "p")
    })
    after(0 -> (10 -> (wd(1 -> 2 -> 3 -> 5) -> 6 || 7) || !7) -> 8)
  }

  test("loop invariant exit") {
    beforeWithPost(0@@"p=vp()" -> (wd(1@@"a=const()" -> 2) -> 3@@"coldcode()" || 4/*cold*/) -> 5, {
      checkPredicate(0, "a")
      checkPredicate(br(0).trueBlock, "p")
    })
    after(0 -> (10 -> (!wd(1 -> 2) || 4) || !4) -> 5)
  }

  test("loop invariant exit inverted") {
    beforeWithPost(0@@"p=vp()" -> (wd(1@@"a=const()" -> 2@@"coldcode()") -> 3 || 4/*cold*/) -> 5, {
      checkPredicate(0, "a") // Note that predicate is not negated, but true and false paths are swapped.
      checkPredicate(br(0).falseBlock, "p")
    })
    after(0 -> (!4 || 10 -> (1 -> 3 || 4)) -> 5)
  }

}
