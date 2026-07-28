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
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels.Important
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.options.BoolOption.SwitchAggregation

class SwitchAggregationSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with CFGTransformationDSL
     with SwitchAggregation {

  startPhase(CompilerPhase.PostInline)

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(SwitchAggregation)
  }

  override def transformation(): Unit = {
    while (aggregateSwitches()) { }
  }

  override def makeDebug = false

  test("aggregate two") {
    beforeWithPost(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@"if(cmp(x,ic(1)))"-> (4 || 5)) -> 6, {
      1.blockEnd shouldBe a [Switch]
      1.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(2, 1)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 4 || 5) -> 6)
  }

  test("aggregate two negated") {
    beforeWithPost(0@@"x" -> 1@@"if(cmpne(x,ic(2)))" -> (3@@"if(cmp(x,ic(1)))"-> (4 || 5) || 2) -> 6, {
      1.blockEnd shouldBe a [Switch]
      1.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(2, 1)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (4 || 5 || 2) -> 6)
  }

  test("aggregate three") {
    beforeWithPost(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@"if(cmp(x,ic(1)))" -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6, {
      1.blockEnd shouldBe a [Switch]
      1.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(2, 1, 3)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 4 || 7 || 8) -> 6)
  }

  test("aggregate three with arbitrary code above top branch") {
    beforeWithPost(0@@"x" -> 1@@("use(x)", "use(x)", "if(cmp(x,ic(2)))") -> (2 || 3@@"if(cmp(x,ic(1)))" -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6, {
      1.blockEnd shouldBe a [Switch]
      1.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(2, 1, 3)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 4 || 7 || 8) -> 6)
  }

  test("no aggregation (single)") {
    before(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3) -> 6)
    after(0 -> 1 -> (2 || 3) -> 6)
  }

  test("no aggregation (different selectors)") {
    before(0@@("x", "y") -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@"if(cmp(y,ic(1)))" -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6)
    after(0 -> 1 -> (2 || 3 -> (4 || 5 -> (7 || 8))) -> 6)
  }

  test("no aggregation (non-const)") {
    before(0@@("x", "y") -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@"if(cmp(x,y))" -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6)
    after(0 -> 1 -> (2 || 3 -> (4 || 5 -> (7 || 8))) -> 6)
  }

  test("no aggregation (same const)") {
    before(0@@"x" -> 1@@"if(cmp(x,ic(1)))" -> (2 || 3@@"if(cmp(x,ic(1)))" -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6)
    after(0 -> 1 -> (2 || 3 -> (4 || 5 -> (7 || 8))) -> 6)
  }

  test("no aggregation (controlled use)") {
    beforeWithPost(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@("read()", "if(cmp(x,ic(1)))") -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6, {
      3.blockEnd shouldBe a [Switch]
      3.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(1, 3)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 3 -> (4 || 7 || 8)) -> 6)
  }

  test("no aggregation (memory use)") {
    before(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@("r=read()", "if(cmp(x,ic(1)))") -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6, {
      withIncrementalGCM {
        eliminateCrossBlockMemoryEdges()
      }
      n("r").asInstanceOf[HasInControl].inCtrl = b(1)
      checkIRConsistency(Important)
    }, {
      3.blockEnd shouldBe a [Switch]
      3.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(1, 3)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 3 -> (4 || 7 || 8)) -> 6)
  }

  test("no aggregation (glue code)") {
    beforeWithPost(0@@"x" -> 1@@"if(cmp(x,ic(2)))" -> (2 || 3@@("use(x)", "if(cmp(x,ic(1)))") -> (4 || 5@@"if(cmp(x,ic(3)))" -> (7 || 8))) -> 6, {
      3.blockEnd shouldBe a[Switch]
      3.blockEnd.asInstanceOf[Switch].cases shouldBe Seq(1, 3)
      checkIRConsistency(Important)
    })
    after(0 -> 1 -> (2 || 3 -> (4 || 7 || 8)) -> 6)
  }
}
