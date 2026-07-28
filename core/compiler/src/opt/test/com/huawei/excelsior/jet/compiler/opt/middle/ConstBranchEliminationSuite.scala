/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.ir.ConstBranchElimination
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class ConstBranchEliminationSuite extends CompilerSuite
                                     with GlobalNodesBuilder
                                     with ConstBranchElimination {

  private def diamondWithCondition(condition: Node) = {
    makeCFG(0 -> (1 || 2) -> 3)
    val branch = (0: Block).blockEnd.asInstanceOf[If]
    branch.selector = condition
    branch
  }

  test("non-constant branch") {
    diamondWithCondition(Cmp(IntType, Condition.EQ)(Param(IntType, 37), IConst(0)))
    eliminateConstBranches() should be (false)
    (0: Block).succBlocks.size should be (2)
  }

  test("null branch") {
    diamondWithCondition(null)
    eliminateConstBranches() should be (false)
    (0: Block).succBlocks.size should be (2)
  }

  test("true branch") {
    val branch = diamondWithCondition(True())
    val (live, dead) = (branch.trueBlock, branch.falseBlock)
    eliminateConstBranches() should be (true)
    (0: Block).succBlocks.toSeq should be (Seq(live))
    dead.predBlocks.toSeq should be (List(unreachableBar))
  }

  test("false branch") {
    val branch = diamondWithCondition(False())
    val (live, dead) = (branch.falseBlock, branch.trueBlock)
    eliminateConstBranches() should be (true)
    (0: Block).succBlocks.toSeq should be (Seq(live))
    dead.predBlocks.toSeq should be (List(unreachableBar))
  }
}
