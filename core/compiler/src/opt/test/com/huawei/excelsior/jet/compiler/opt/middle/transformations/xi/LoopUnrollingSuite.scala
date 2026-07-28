/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, GlobalNodesBuilder}

class LoopUnrollingSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with LoopUnrolling {

  def e(tpe: Type) = EmptyValueRange(tpe)
  def c(tpe: Type, from: Long, to: Long, evidence: ControlNode = null) = ConstValueRange(tpe, from, to, evidence)

  val I = IntType
  val L = LongType

  for (t <- Seq(I, L); ((range, step, unrolledStep, unrolledRange), pos) <- Seq(
    tp(c(t, 1, 10),  1, 2,  c(t, 1, 10)),
    tp(c(t, 1, 10),  1, 3,  c(t, 1, 9)),
    tp(c(t, 1, 10),  1, 4,  c(t, 1, 8)),
    tp(c(t, 1, 10),  1, 5,  c(t, 1, 10)),
    tp(c(t, 1, 10),  1, 8,  c(t, 1, 8)),
    tp(c(t, 1, 10),  1, 9,  c(t, 1, 9)),
    tp(c(t, 1, 10),  1, 10, c(t, 1, 10)),
    tp(c(t, 1, 10),  1, 11, e(t)),
    tp(c(t, 1, 10),  1, 16, e(t)),

    tp(c(t, 1, 10), -1, 2,  c(t, 1, 10)),
    tp(c(t, 1, 10), -1, 3,  c(t, 2, 10)),
    tp(c(t, 1, 10), -1, 4,  c(t, 3, 10)),
    tp(c(t, 1, 10), -1, 5,  c(t, 1, 10)),
    tp(c(t, 1, 10), -1, 8,  c(t, 3, 10)),
    tp(c(t, 1, 10), -1, 9,  c(t, 2, 10)),
    tp(c(t, 1, 10), -1, 10, c(t, 1, 10)),
    tp(c(t, 1, 10), -1, 11, e(t)),
    tp(c(t, 1, 10), -1, 16, e(t)),

    tp(c(t, 0, 10),  1, 2,  c(t, 0, 9)),
    tp(c(t, 0, 10),  1, 3,  c(t, 0, 8)),
    tp(c(t, 0, 10),  1, 4,  c(t, 0, 7)),
    tp(c(t, 0, 10),  1, 5,  c(t, 0, 9)),
    tp(c(t, 0, 10),  1, 8,  c(t, 0, 7)),
    tp(c(t, 0, 10),  1, 9,  c(t, 0, 8)),
    tp(c(t, 0, 10),  1, 10, c(t, 0, 9)),
    tp(c(t, 0, 10),  1, 11, c(t, 0, 10)),
    tp(c(t, 0, 10),  1, 16, e(t)),

    tp(c(t, 0, 10), -1, 2,  c(t, 1, 10)),
    tp(c(t, 0, 10), -1, 3,  c(t, 2, 10)),
    tp(c(t, 0, 10), -1, 4,  c(t, 3, 10)),
    tp(c(t, 0, 10), -1, 5,  c(t, 1, 10)),
    tp(c(t, 0, 10), -1, 8,  c(t, 3, 10)),
    tp(c(t, 0, 10), -1, 9,  c(t, 2, 10)),
    tp(c(t, 0, 10), -1, 10, c(t, 1, 10)),
    tp(c(t, 0, 10), -1, 11, c(t, 0, 10)),
    tp(c(t, 0, 10), -1, 16, e(t)),

  )) {
    test(s"unrolled value ranges ($range, $step, $unrolledStep)") {
      calcUnrolledValueRange(range, step, unrolledStep, entryBlock) shouldBe unrolledRange
    }
  }

}
