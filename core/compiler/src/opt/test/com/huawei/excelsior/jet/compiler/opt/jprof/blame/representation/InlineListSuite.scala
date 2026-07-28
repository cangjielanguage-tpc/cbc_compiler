/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.InlineContextID
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeMethod

class InlineListSuite extends CompilerSuite {
  val topBCPos = 41
  val fm1 = new FakeMethod("m1"); val m1CallSiteBCPos = 42
  val fm2 = new FakeMethod("m2"); val m2CallSiteBCPos = 43
  val fm3 = new FakeMethod("m3");

  val ic =
    InlineContext.newInlined(fm1,
      12, m1CallSiteBCPos, InlineContext.newInlined(fm2,
        22, m2CallSiteBCPos, InlineContext.newRoot(fm3)))
  val pos = BytecodePosition(topBCPos, 123, 456, ic)

  val m1 = Method.fromSymlevel(fm1)
  val m2 = Method.fromSymlevel(fm2)
  val m3 = Method.fromSymlevel(fm3)

  val ms = Array(InlineList.JProfEntry(m1, m1CallSiteBCPos), InlineList.JProfEntry(m2, m2CallSiteBCPos))
  val icID = Some(InlineContextID(0, 2))

  test("IL by IC") {
    InlineList.reversed(pos) shouldBe InlineList(pos).reverse
  }

  test("IL by icID") {
    InlineList.reversed(ms, icID, m3, topBCPos) shouldBe InlineList(ms, icID, m3, topBCPos).reverse
  }

  test("IL by IC == IL by icID") {
    InlineList(pos) shouldBe InlineList(ms, icID, m3, topBCPos)
  }
}
