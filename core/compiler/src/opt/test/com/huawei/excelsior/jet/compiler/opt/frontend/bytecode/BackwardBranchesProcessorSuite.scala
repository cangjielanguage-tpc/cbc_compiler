/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.CFGTransformationDSL

/**
 * Tests for BackwardBranchesProcessor.
 */
class BackwardBranchesProcessorSuite extends CompilerSuite
                                        with CFGTransformationDSL
                                        with BackwardBranchesProcessor {

  def transformation(): Unit = {
    processBackwardBranches()
  }

  val makeDebug = false

  test("simple line graph test") {
    before(0 -> 1 -> 2)
    after (0 -> 1 -> 2) // TODO: method noChanges()
  }

  test("test with ifs") {
    before(0 -> (1 || (2 -> (3 || 4) -> 5)) -> 6)
    after (0 -> (1 || (2 -> (3 || 4) -> 5)) -> 6)
  }

  test("test with simple loop") {
    before(0 -> dw(1 -> 2) -> 3)
    after (0 -> dw(1 -> 2) -> 3)
  }

  test("test with included loops") {
    before(0 -> dw(1 -> dw(2 -> 3) -> 4) -> 5)
    after (0 -> dw(1 -> dw(2 -> 3) -> 4) -> 5)
  }

  test("test with some normal predecessors") {
    before(0 -> (1 || 2) ->      dw(3 -> 4) -> 5)
    after (0 -> (1 || 2) -> 6 -> dw(3 -> 4) -> 5)
  }

  test("hard test with some normal predecessors") {
    before(0 -> (1 || (2 -> (3 || (4 -> (5 || 6))))) ->       dw(7 -> 8) -> 9)
    after (0 -> (1 || (2 -> (3 || (4 -> (5 || 6))))) -> 10 -> dw(7 -> 8) -> 9)
  }

  test("test with one normal predecessor and some backward branches (wd)") {
    before(0 -> wd(1 -> 2 -> (3 || 4)     ) -> 5)
    after (0 -> wd(1 -> 2 -> (3 || 4) -> 6) -> 5)
  }

  test("test with one normal predecessor and some backward branches (dw)") {
    before(0 -> dw(1 -> (2 || 3)) -> 4)
    after (0 -> lp(1 -> (2 || 3) -> 5, exits(2, 3)) -> 4)
  }

  test("test with some normal predecessors and some backward branches") {
    before(0 -> (1 || 2)      -> wd(3 -> 4 -> (5 || 6)     ) -> 7)
    after (0 -> (1 || 2) -> 8 -> wd(3 -> 4 -> (5 || 6) -> 9) -> 7)
  }

  test("test parallel back edges") {
    before((0 -> dw(1 -> 2) -> 3) |>| (2 -> 1))
    after ((0 -> lp(1 -> 2 -> 4, exits(2)) -> 3) |>| (2 -> 4))
  }

  test("test with a lot of included do-while cycles") {
    before(0 -> (1 || (2 -> (3 || 4))) ->       dw(      dw(      dw(5 -> 6) -> 7) -> 8) -> 9)
    after (0 -> (1 || (2 -> (3 || 4))) -> 10 -> dw(11 -> dw(12 -> dw(5 -> 6) -> 7) -> 8) -> 9)
  }

  test("test some cases") {
    before(0 -> (1 || (2 -> (3 || 4))) ->       dw(      dw(      dw(5 -> 6) -> 7) -> 8) -> 9 -> (10 || 11) ->       wd(12 -> 13 -> (14 || 15)      ) -> 16)
    after (0 -> (1 || (2 -> (3 || 4))) -> 19 -> dw(20 -> dw(21 -> dw(5 -> 6) -> 7) -> 8) -> 9 -> (10 || 11) -> 17 -> wd(12 -> 13 -> (14 || 15) -> 18) -> 16)
  }

  test("test self-cycle") {
    before(0 -> dw(1))
    after (0 -> dw(1))
  }

  test("test self-cycle with some normal predecessors") {
    before(0 -> (1 || 2) ->      dw(3))
    after (0 -> (1 || 2) -> 4 -> dw(3))
  }

  test("test self-cylce with some normal predecessors and some another backward branches") {
    before(0 -> (1 || 2) ->      dw(     dw(3) -> 4))
    after (0 -> (1 || 2) -> 5 -> dw(6 -> dw(3) -> 4))
  }

  test("test with some nested levels") {
    before(0 -> wd(      lp(      dw(1 -> 2) -> 3 -> (4 || 5 -> 6)      , exits(6)) -> 7          ) -> 8)
    after (0 -> lp(11 -> lp(12 -> dw(1 -> 2) -> 3 -> (4 || 5 -> 6) -> 13, exits(6)) -> 7, exits(1)) -> 8)
  }

}