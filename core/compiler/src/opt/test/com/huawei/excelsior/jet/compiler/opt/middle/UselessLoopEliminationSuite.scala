/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement

class UselessLoopEliminationSuite extends CompilerSuite
                                     with CFGTransformationDSL
                                     with GlobalNodesBuilder
                                     with UselessLoopElimination {

  def transformation(): Unit = {
    while (eliminateUselessLoops() | evaluateUselessLoops() | eliminateZeroLoops()) {}
  }

  val makeDebug = false

  private def addInductiveVariable(): Node = {
    addInductiveVariable(1, IConst(0), Condition.LT, IConst(100), IConst(1))
  }

  private def addInductiveVariableNE(): Node = {
    addInductiveVariable(1, IConst(0), Condition.NE, IConst(100), IConst(1))
  }

  private def addEmptyInductiveVariable(header: Block = 1, use: Block = null): Node = {
    addInductiveVariableWithUse(header, if (use == null) header else use, IConst(0), Condition.LT, IConst(0), IConst(1))
  }

  test("elimination of useless counted loop") {
    beforeWithPre(0 -> wd(1) -> 2, {
      addInductiveVariable()
    })
    after(0 -> 2)
  }

  test("elimination of useless counted cold loop") {
    beforeWithPre(0 -> wd(1@@("coldcode()")) -> 2, {
      addInductiveVariable()
    })
    after(0 -> 2)
  }

  test("elimination of useless counted loop with split non-critical backward edge") {
    beforeWithPre(0 -> wd(1 -> 2) -> 3, {
      addInductiveVariable()
    })
    after(0 -> 3)
  }

  test("no elimination of useless counted loop with multiple blocks (not supported yet)") {
    beforeWithPre(0 -> wd(1 -> 2 -> 3) -> 4, {
      addInductiveVariable()
    })
    after(0 -> wd(1 -> 2 -> 3) -> 4)
  }

  test("no elimination of useful counted loop with spine") {
    beforeWithPre(0 -> wd(1@@("write()")) -> 2, {
      addInductiveVariable()
    })
    after(0 -> wd(1) -> 2)
  }

  test("no elimination of useful counted loop with uses below") {
    beforeWithPre(0 -> wd(1) -> 2, {
      val p = addInductiveVariable()
      addResult(2, entryMemory, p)
    })
    after(0 -> wd(1) -> 2)
  }

  test("evaluation and elimination of useful counted loop with uses below") {
    before(0 -> wd(1) -> 2, {
      val p = addInductiveVariableNE()
      addResult(2, entryMemory, p)
    }, {
      Return.unique.get.inValue shouldBe IConst(100)
    })
    after(0 -> 2)
  }

  test("no elimination of useless counted loop with multiple exits") {
    beforeWithPre(0 -> wd(1 -> 2) -> 3 |>| 2 -> 3, {
      addInductiveVariable()
    })
    after(0 -> wd(1 -> 2) -> 3 |>| 2 -> 3)
  }

  test("no elimination of useless uncounted loop - 1") {
    before(0 -> wd(1) -> 2)
    after(0 -> wd(1) -> 2)
  }

  test("no elimination of useless uncounted loop - 2") {
    before(0 -> wd(1))
    after(0 -> wd(1))
  }

  test("no elimination of useless infinite loop") {
    before(0 -> !wd(1))
    after(0 -> !wd(1))
  }

  test("elimination of zero loop") {
    beforeWithPre(0 -> wd(1 -> 2@@("write()")) -> 3, {
      addEmptyInductiveVariable()
    })
    after(0 -> 1 -> 3)
  }

  test("elimination of zero loop with multiple exits") {
    beforeWithPre((0 -> wd(1 -> 2 -> 3@@("write()")) -> 4) |>| 2 -> 4, {
      addEmptyInductiveVariable()
      addEmptyInductiveVariable(use = b(2))
    })
    after(0 -> 1 -> 4)
  }

  test("elimination of zero loop with non-header exit") {
    beforeWithPre(0 -> dw(1@@("write()") -> 2) -> 3, {
      addEmptyInductiveVariable(1, 2)
    })
    after(0 -> 1@@("write()") -> 2 -> 3)
  }
}
