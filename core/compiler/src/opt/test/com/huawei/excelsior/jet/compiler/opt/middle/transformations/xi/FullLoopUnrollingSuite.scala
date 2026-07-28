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
import com.huawei.excelsior.jet.compiler.opt.middle.{SimplifyComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.options.NumOption.FullyUnrollableLoopMinDepth

class FullLoopUnrollingSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with CFGTransformationDSL
    with ConstBranchElimination
    with UCEComponent
    with SimplifyComponent
    with FullLoopUnrolling {

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.define(FullyUnrollableLoopMinDepth, 1)
  }

  override def transformation(): Unit = {
    while (fullyUnrollLoops() | completeSSA() | simplifyIR() | eliminateConstBranches()) { }
  }

  override def nodeWeight(n: Node) = 0.0

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("idx_0_1")({
        case Seq() => addInductiveVariable(IConst(0), Condition.LT, IConst(1), IConst(1))
      }),

      new SimpleAttribute("idx_0_2")({
        case Seq() => addInductiveVariable(IConst(0), Condition.LT, IConst(2), IConst(1))
      }),

      new SimpleAttribute("idx_0_3")({
        case Seq() => addInductiveVariable(IConst(0), Condition.LT, IConst(3), IConst(1))
      }),

    ) ++ super.parsableAttributes()
  }

  def addIndex(from: Node, to: Node) = addInductiveVariable(1, from, Condition.LE, to, ic(1))

  def x = addNode()
  def eval(n: Node) = n match {
    case IConst(x) => s"$x"
    case _ => s"#${n.id}"
  }

  def simpleLoop = 0 -> wd(1 -> 2) -> 9

  for ((from, to, res) <- Seq(
    (0, 0, () => 0 -> 10 -> 1 -> 2 -> 90 -> 9),
    (0, 1, () => 0 -> 10 -> 1 -> 2 -> 11 -> 22 -> 90 -> 9),
    (0, 2, () => 0 -> 10 -> 1 -> 2 -> 11 -> 22 -> 111 -> 222 -> 90 -> 9),
  )) {
    test(s"full unroll from $from to $to") {
      beforeWithPre(simpleLoop, {
        addIndex(ic(from), ic(to))
      })
      after(res())
    }
  }

  test("no full unroll half symbolic range") {
    beforeWithPre(simpleLoop, {
      addIndex(ic(0), addNode())
    })
    after(simpleLoop)
  }

  test("no full unroll symbolic range") {
    beforeWithPre(simpleLoop, {
      addIndex(addNode(), addNode())
    })
    after(simpleLoop)
  }

  test("fully unroll consecutive loops (JET-12147)") {
    before(0 -> wd(1@@("idx_0_2()") -> 2) -> wd(3@@("idx_0_1()") -> 4) -> 9)
    after(0 -> 10 -> 1 -> 2 -> 11 -> 22 -> 90 -> 30 -> 31 -> 3 -> 4 -> 900 -> 9)
  }

  test("fully unroll same consecutive loops") {
    before(0 -> wd(1@@("idx_0_2()") -> 2) -> wd(3@@("idx_0_2()") -> 4) -> 9)
    after(0 -> 10 -> 1 -> 2 -> 11 -> 22 -> 90 -> 30 -> 31 -> 3 -> 4 -> 33 -> 44 -> 900 -> 9)
  }

}
