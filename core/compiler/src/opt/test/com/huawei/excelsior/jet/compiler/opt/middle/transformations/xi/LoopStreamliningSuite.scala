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

class LoopStreamliningSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with CFGTransformationDSL
    with LoopStreamlining {

  override def isPGOHost = true

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(LoopStreamlining)
  }

  override def transformation(): Unit = {
    while (streamlineLoops()) { }
  }

  override def makeDebug = false

  test("no optimization without cold code") {
    before(0 -> wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
    after(0 -> wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
  }

  test("no optimization without diamond - 1") {
    before(0 -> wd(1 -> 2) -> 3@@"coldcode()")
    after(0 -> wd(1 -> 2) -> 3)
  }

  test("no optimization without diamond - 2") {
    before(0 -> wd(1 -> 2@@"coldcode()") -> 3)
    after(0 -> wd(1 -> 2) -> 3)
  }

  // Note: can be optimized if needed
  test("no optimization of clinit without other diamonds") {
    before(0 -> wd(1@@"clinit(A)" -> 2) -> 3)
    after(0 -> wd(1 -> 2) -> 3)
  }

  test("single diamond") {
    before(0 -> wd(1 -> 2 -> (3 || 4@@"coldcode()") -> 5) -> 6)
    after(0 -> lp(100 -> (
      101 -> 102 -> !wd(10 -> 20 -> 30 -> 50)
        || 103 -> 104 -> 1 -> 2 -> (3 || 4) -> 5
      ), exits(1, 10)) -> 6
      |>| 20 -> 4)
  }

  test("multiple diamonds") {
    before(0 -> wd(1 -> 2 -> (3 || 4@@"coldcode()") -> 5 -> (6 || 7@@"coldcode()") -> 8) -> 9)
    after(0 -> lp(100 -> (
      101 -> 102 -> !wd(10 -> 20 -> 30 -> 50 -> 60 -> 80)
        || 103 -> 104 -> 1 -> 2 -> (3 || 4) -> 5 -> (6 || 7) -> 8
      ), exits(1, 10)) -> 9
      |>| 20 -> 4 |>| 50 -> 7)
  }

  test("single diamond with clinit") {
    before(0 -> wd(1@@"clinit(A)" -> 2 -> (3 || 4@@"coldcode()") -> 5) -> 6)
    after(0 -> lp(100 -> (
      101 -> 102 -> !wd(10 -> 11 -> 12 -> 20 -> 30 -> 50)
        || 103 -> 104 -> 1 -> (13 || 14 -> 15) -> 16 -> 2 -> (3 || 4) -> 5
      ), exits(12, 16)) -> 6
      |>| 10 -> 14
      |>| 20 -> 4)
  }

  test("iteration continuation") {
    beforeWithPost(0@@"x" -> (wd(1@@"p=phi(x,y)" -> 2@@"y=add(p,ic(1))" -> (3 || 4@@"coldcode()") -> 5) || 6) -> 7, {
      val spine = 0.blockEnd.asInstanceOf[If].trueBlock.spine.toSeq
      spine should have (length (1))
      val assign = spine.head
      assign should matchPattern {
        case AssignVar(_, N("x")) =>
      }
      n("x").uses.toSeq shouldBe Seq(assign)
    })
    after(0 -> (
      90 -> lp(100 -> (
        101 -> 102 -> !wd(10 -> 11 -> (20 || !91) -> 30 -> 50)
          || 103 -> 104 -> 1 -> 12 -> (2 || !92) -> (3 || 4) -> 5
        ), exits(91, 92))
        || 6
      ) -> 7
      |>| 20 -> 4)
  }

}
