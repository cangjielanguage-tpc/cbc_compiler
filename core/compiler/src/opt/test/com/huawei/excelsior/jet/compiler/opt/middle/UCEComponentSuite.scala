/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import transformations.IRTransformationsCollection

/**
 * Tests for UCEComponent
 */
class UCEComponentSuite extends CompilerSuite
                           with GlobalNodesBuilder
                           with DCEComponent
                           with UCEComponent
                           with IRTransformationsCollection {

  test("elimination with phi") {
    makeCFG((0@@("x", "y", "z") -> (1 || 2) -> 4@@("p=phi(x,y,z)", "ret(p)") ) |>| (UB -> 3 -> 4)) // 0 -> 3 was removed
    b(4).predBlocks.toSet should be (bSet(1, 2, 3))
    eliminateUnreachableCode()
    b(4).predBlocks.toSet should be (bSet(1, 2))
    n("p").asInstanceOf[Phi].argsSeq should be (Seq(n("x"), n("y")))
  }

  test("diamond elimination") {
    makeCFG((0 -> 1 -> 3) |>| (UB -> 2 -> 3)) // 0 -> 2 was removed
    b(3).predBlocks.toSet should be (bSet(1, 2))
    eliminateUnreachableCode()
    b(3).predBlocks.toSet should be (bSet(1))
  }

  test("loop elimination") {
    makeCFG((0 -> 5) |>| (dw(2 -> 3 -> 4) -> 5) |>| (UB -> 2)) // 0 -> 2 was removed
    b(5).predBlocks.toSet should be (bSet(0, 4))
    eliminateUnreachableCode()
    b(5).predBlocks.toSet should be (bSet(0))
  }

  test("irreducible loop sruvival") {
    makeCFG((0 -> 1 -> ((3 -> 4) || 4)) |>| dw(2 -> 3))
    b(3).predBlocks.toSet should be (bSet(1, 2))
    eliminateUnreachableCode()
    b(3).predBlocks.toSet should be (bSet(1, 2))
  }

  test("after DCE - bug with return statement elimination") {
    makeCFG(0@@("x", "y", "z", "if(cmp(x,y))") -> (1 || 2) -> 3@@("ret(z)"))
    transform(EmptyBlocksElimination)

    b(0).succBlocks.size should be (2)
    b(0).succBlocks.toSet.size should be (1)

    eliminateDeadCode()
    eliminateUnreachableCode()

    Return.unique.isDefined should be (true)
  }

  test("after DCE elimination simple") {
    makeCFG(0@@("x", "y", "if(cmp(x,y))") -> (1 || (2@@("if(cmp(y,x))") -> (4 || 5))) -> 3@@("ret(x)"))
    eliminateDeadCode()
    eliminateUnreachableCode()
    transform(BlocksConnectionTransformation, EmptyBlocksElimination, MultiEdgeElimination)
    entryBlock.succBlocks.size should be (0)
  }

  test("after DCE elimination hard") {
    makeCFG(0@@("x", "y", "q", "r", "s", "t", "if(cmp(x,y))") -> (1 || (2@@("if(cmp(q,r))") -> (3 ||4) -> 5@@("if(cmp(s,t))") -> (6 || 7))) -> 8@@("ret(x)"))
    eliminateDeadCode()
    eliminateUnreachableCode()
    transform(BlocksConnectionTransformation, EmptyBlocksElimination, MultiEdgeElimination)
    entryBlock.succBlocks.size should be (0)
  }

}
