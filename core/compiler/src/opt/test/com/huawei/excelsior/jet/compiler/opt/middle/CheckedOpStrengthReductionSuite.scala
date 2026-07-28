/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class CheckedOpStrengthReductionSuite extends CompilerSuite with GlobalNodesBuilder with CheckedOpStrengthReduction {
  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("cmplt")({ case Seq(l, r) => Cmp(LongType, Condition.LT)(l, r) }),
      new SimpleAttribute("cmple")({ case Seq(l, r) => Cmp(LongType, Condition.LE)(l, r) }),
      new SimpleAttribute("cmpgt")({ case Seq(l, r) => Cmp(LongType, Condition.GT)(l, r) }),
      new SimpleAttribute("cmpge")({ case Seq(l, r) => Cmp(LongType, Condition.GE)(l, r) }),
      new SimpleAttribute("cadd")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.ADD, signed = true, managed = true)(l, r) }),
      new SimpleAttribute("csub")({ case Seq(l, r) => CheckedOp(LongType, Width.W64, CheckedOp.Kind.SUB, signed = true, managed = true)(l, r) }),
      new SimpleAttribute("arg")({ case Seq() => Fake(LongType) }),
    ) ++ super.parsableAttributes()
  }

  test("left < right") {
    makeCFG(
      1@@("x=arg()", "y=arg()", "cond=cmplt(x,y)", "if(cond)") -> (
        2@@(
        "xadd=cadd(x,lc(1))",
        "ysub=csub(y,lc(1))"
        ) ||
        3
      )
    )

    optimizeRedundantCheckedOp()
    n("xadd") should not be a [CheckedOp]
    n("ysub") should not be a [CheckedOp]
  }

  test("left <= right") {
    makeCFG(
      1@@("x=arg()", "y=arg()", "cond=cmple(x,y)", "if(cond)") -> (
        2 ||
        3@@(
            "xsub=csub(x,lc(1))",
            "yadd=cadd(y,lc(1))",
        )
      )
    )

    optimizeRedundantCheckedOp()
    n("xsub") should not be a [CheckedOp]
    n("yadd") should not be a [CheckedOp]
  }

  test("left > right") {
    makeCFG(
      1@@("x=arg()", "y=arg()", "cond=cmpgt(x,y)", "if(cond)") -> (
        2@@(
          "xsub=csub(x,lc(1))",
          "yadd=cadd(y,lc(1))"
        ) ||
        3
      )
    )
    
    optimizeRedundantCheckedOp()
    n("xsub") should not be a [CheckedOp]
    n("yadd") should not be a [CheckedOp]
  }

  test("left >= right") {
    makeCFG(
      1@@("x=arg()", "y=arg()", "cond=cmpge(x,y)", "if(cond)") -> (
        2 ||
        3@@(
          "xadd=cadd(x,lc(1))",
          "ysub=csub(y,lc(1))",
        )
      )
    )

    optimizeRedundantCheckedOp()
    n("xadd") should not be a [CheckedOp]
    n("ysub") should not be a [CheckedOp]
  }

  test("more than one use which limits") {
    makeCFG(
      1@@("x=arg()", "y=arg()", "cond1=cmplt(x,y)", "cond2=cmplt(x,lc(10))", "if(cond1)") -> (
        (
          2@@"if(cond2)" -> (
            4@@(
              "xadd=cadd(x,lc(1))",
              "ysub=csub(y,lc(1))"
            ) ||
            5
          )
        ) || 
        3
      )
    )

    optimizeRedundantCheckedOp()
    n("xadd") should not be a [CheckedOp]
    n("ysub") should not be a [CheckedOp]
  }
}
