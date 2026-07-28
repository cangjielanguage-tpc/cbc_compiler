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
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.types.Guards.PointGuard
import org.scalactic.source

import scala.util.chaining.scalaUtilChainingOps

class PairedRawDataAccessOptimizationSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with PairedRawDataAccessOptimization {

  override def parsableAttributes() = {
    Seq(
      new UnnamedAttribute(() => addObjNode()),
      new SimpleAttribute("addr")({ case Seq() => addNode(AddrType) }),

      new SimpleAttribute("acquire")({ case Seq(array) => AcquireRawData(array) }),
      new SimpleAttribute("release")({ case Seq(array, pointer) => ReleaseRawData(array, pointer) }),

    ) ++ super.parsableAttributes()
  }

  def testPaired(name: String, cfg: => SubGraph)(paired: String*)(nonPaired: String*)(implicit pos: source.Position): Unit = {
    test(name) {
      makeCFG(cfg)

      val pairedNodes = paired map n.apply
      val nonPairedNodes = nonPaired map n.apply

      if (optimizePairedRawDataAccesses()) {
        // we do not recursively optimize in these tests
        optimizePairedRawDataAccesses() shouldBe false
      }

      pairedNodes foreach { _ should not be Symbol("committed") }
      nonPairedNodes foreach { _ shouldBe Symbol("committed") }
    }
  }

  testPaired("single acquire",
    0@@"o" -> 1@@"a=acquire(o)")(

    // no optimized
  )(
    "a"
  )

  testPaired("single acquire with no exits",
    0@@"o" -> 1@@"a=acquire(o)" -> !wd(2))(

    // no optimized
  )(
    "a"
  )

  testPaired("single release",
    0@@("o", "p=addr()") -> 1@@"r=release(o,p)")(

    // no optimized
  )(
    "r"
  )

  testPaired("single release with no exits",
    0@@("o", "p=addr()") -> 1@@"r=release(o,p)" -> !wd(2))(

    // no optimized
  )(
    "r"
  )

  testPaired("simple pair",
    0@@("o", "a=acquire(o)", "r=release(o,a)"))(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("simple pair with no exits",
    0@@"o" -> 1@@"a=acquire(o)" -> 2@@"r=release(o,a)" -> !wd(3))(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("bad pair - different pointers",
    0@@("o", "a=acquire(o)", "p=addr()") -> 1@@"r=release(o,p)")(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad pair - different arrays",
    0@@("o1", "o2", "a=acquire(o1)") -> 1@@"r=release(o2,a)")(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("cfg pair",
    0@@("o", "a=acquire(o)") -> (1 || 2) -> 3@@"r=release(o,a)")(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("cfg pair with uses",
    0@@("o", "a=acquire(o)") -> (1@@"use(o)" || 2@@"use(a)") -> 3@@"r=release(o,a)")(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("cfg pair with handler - 1",
    0@@("o", "a=acquire(o)") -> (1 -> (xb(2) || !4) || 3) -> 4@@"r=release(o,a)")(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("cfg pair with handler - 2",
    0@@("o", "a=acquire(o)") -> 1 -> (xb(2)@@"r1=release(o,a)" || 3@@"r2=release(o,a)") -> 4)(

    "a", "r1", "r2"
  )(
    // all optimized
  )

  testPaired("cfg pair with handler - 3",
    0@@("o", "a=acquire(o)") -> (1 -> (xb(2)@@"r1=release(o,a)" || !3@@"r2=release(o,a)") || 4@@"r3=release(o,a)") -> 5)(

    "a", "r1", "r2", "r3"
  )(
    // all optimized
  )

  testPaired("cfg pair with loop",
    0@@"o" -> 1@@"a=acquire(o)" -> wd(2) -> 3@@"r=release(o,a)" -> 4)(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("cfg pair with infinite loop",
    0@@("o", "a=acquire(o)") -> (!wd(1) || 2) -> 3@@"r=release(o,a)")(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("cfg pair with multiple releases",
    0@@("o", "a=acquire(o)") -> (1@@"r1=release(o,a)" || 2@@"r2=release(o,a)") -> 3)(

    "a", "r1", "r2"
  )(
    // all optimized
  )

  testPaired("parallel pair - same object",
    0@@"o" -> (1@@"a1=acquire(o)" -> 2@@"r1=release(o,a1)" || 3@@"a2=acquire(o)" -> 4@@"r2=release(o,a2)") -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("parallel pair - different objects",
    0@@("o1", "o2") -> (1@@"a1=acquire(o1)" -> 2@@"r1=release(o1,a1)" || 3@@"a2=acquire(o2)" -> 4@@"r2=release(o2,a2)") -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("sequential pair - same object",
    0@@"o" -> 1@@"a1=acquire(o)" -> 2@@"r1=release(o,a1)" -> 3@@"a2=acquire(o)" -> 4@@"r2=release(o,a2)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("sequential pair - different objects",
    0@@("o1", "o2") -> 1@@"a1=acquire(o1)" -> 2@@"r1=release(o1,a1)" -> 3@@"a2=acquire(o2)" -> 4@@"r2=release(o2,a2)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("nested pair - same object",
    0@@"o" -> 1@@"a1=acquire(o)" -> 2@@"a2=acquire(o)" -> 3@@"r2=release(o,a2)" -> 4@@"r1=release(o,a1)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("nested pair - different objects",
    0@@("o1", "o2") -> 1@@"a1=acquire(o1)" -> 2@@"a2=acquire(o2)" -> 3@@"r2=release(o2,a2)" -> 4@@"r1=release(o1,a1)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("overlapping pair - same object",
    0@@"o" -> 1@@"a1=acquire(o)" -> 2@@"a2=acquire(o)" -> 3@@"r1=release(o,a1)" -> 4@@"r2=release(o,a2)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("overlapping pair - different objects",
    0@@("o1", "o2") -> 1@@"a1=acquire(o1)" -> 2@@"a2=acquire(o2)" -> 3@@"r1=release(o1,a1)" -> 4@@"r2=release(o2,a2)" -> 5)(

    "a1", "r1", "a2", "r2"
  )(
    // all optimized
  )

  testPaired("bad pair - not enough releases",
    0@@("o", "a=acquire(o)") -> (1@@"r=release(o,a)" || 2) -> 3)(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad pair - not enough releases with no exits - 1",
    0@@("o", "a=acquire(o)") -> (1@@"r=release(o,a)" || 2) -> !wd(3))(

    // no optimized
  )(
    "a", "r"
  )

  // Shouldn't optimize but does, because endless loops are hard to analyze.
  testPaired("bad pair - not enough releases with no exits - 2",
    0@@("o", "a=acquire(o)") -> (1@@"r=release(o,a)" -> !wd(2) || 3 -> !wd(4)))(

    "a", "r"
  )(
    // no optimized
  )

  testPaired("bad pair - not enough releases through handler",
    0@@("o", "a=acquire(o)") -> 1 -> (2@@"r=release(o,a)" || xb(3)) -> 4)(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad pair - not enough releases through default handler",
    0@@("o", "a=acquire(o)", "xspinal()") -> 1@@"r=release(o,a)")(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("loop pair",
    0@@"o" -> dw(1@@"a=acquire(o)" -> 2@@"r=release(o,a)") -> 3)(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("loop pair with no exits",
    0@@"o" -> dw(1@@"a=acquire(o)" -> 2@@"r=release(o,a)") -> !wd(3))(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("bad loop pair - not enough releases",
    0@@"o" -> wd(1@@"a=acquire(o)" -> 2@@"r=release(o,a)") -> 3)(

    // no optimized
  )(
    "a", "r"
  )

  // Shouldn't optimize but does, because endless loops are hard to analyze.
  testPaired("bad loop pair - not enough releases with no exits",
    0@@"o" -> wd(1@@"a=acquire(o)" -> 2@@"r=release(o,a)") -> !wd(3))(

    "a", "r"
  )(
    // all optimized
  )

  testPaired("bad loop pair - looping acquire",
    0@@"o" -> dw(1@@"a=acquire(o)") -> 2@@"r=release(o,a)" -> 3)(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad loop pair - looping acquire with no exits",
    0@@"o" -> dw(1@@"a=acquire(o)") -> 2@@"r=release(o,a)" -> !wd(3))(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad loop pair - looping release",
    0@@"o" -> 1@@"a=acquire(o)" -> dw(2@@"r=release(o,a)") -> 3)(

    // no optimized
  )(
    "a", "r"
  )

  testPaired("bad loop pair - looping release with no exits",
    0@@"o" -> 1@@"a=acquire(o)" -> dw(2@@"r=release(o,a)") -> !wd(3))(

    // no optimized
  )(
    "a", "r"
  )

  // Can be optimized, but currently is not.
  testPaired("cfg pair - phi",
    0@@"o" -> (1@@"a1=acquire(o)" || 2@@"a2=acquire(o)") -> 3@@("p=phi(a1,a2)", "r=release(o,p)"))(

    // no optimized
  )(
    "a1", "a2", "r"
  )

  // Can be optimized, but currently is not.
  testPaired("cfg pair - phi with no exits",
    0@@"o" -> (1@@"a1=acquire(o)" || 2@@"a2=acquire(o)") -> 3@@("p=phi(a1,a2)", "r=release(o,p)") -> !wd(4))(

    // no optimized
  )(
    "a1", "a2", "r"
  )

  // Can be optimized, but currently is not.
  testPaired("cfg pair - phi with no exits (evil)",
    0@@"o" -> (1@@"a1=acquire(o)" -> (!99@@"r1=release(o,a1)" || 98) -> 97 || 2@@"a2=acquire(o)") -> 3@@("p=phi(a1,a2)", "r2=release(o,p)") -> !wd(4))(

    // no optimized
  )(
    "a1", "r1", "a2", "r2"
  )

  // Can be optimized, but currently is not.
  testPaired("cfg pair - phi different arrays",
    0@@("o1", "o2") -> (1@@"a1=acquire(o1)" || 2@@"a2=acquire(o2)") -> 3@@("op=phi(o1,o2)", "p=phi(a1,a2)", "r=release(op,p)"))(

    // no optimized
  )(
    "a1", "a2", "r"
  )

  testPaired("bad pair - phi different pointers",
    0@@"o" -> (1@@"a1=acquire(o)" || 2@@"a2=addr()") -> 3@@("p=phi(a1,a2)", "r=release(o,p)"))(

    // no optimized
  )(
    "a1", "r"
  )

  testPaired("bad pair - consecutive acquire",
    0@@("o", "a1=acquire(o)", "a2=acquire(o)") -> (1 || 2) -> 3@@("p=phi(a1,a2)", "r=release(o,p)"))(

    // no optimized
  )(
    "a1", "a2", "r"
  )

  testPaired("bad pair - consecutive acquire with no exits",
    0@@("o", "a1=acquire(o)", "a2=acquire(o)") -> (1 || 2) -> 3@@("p=phi(a1,a2)", "r=release(o,p)") -> !wd(4))(

    // no optimized
  )(
    "a1", "a2", "r"
  )

  testPaired("bad pair - consecutive release",
    0@@"o" -> 1@@"a=acquire(o)" -> 2@@("r1=release(o,a)", "r2=release(o,a)"))(

    // no optimized
  )(
    "a", "r1", "r2"
  )

  testPaired("bad pair - consecutive release with no exits",
    0@@"o" -> 1@@"a=acquire(o)" -> 2@@("r1=release(o,a)", "r2=release(o,a)") -> !wd(4))(

    // no optimized
  )(
    "a", "r1", "r2"
  )

  testPaired("bad pair - consecutive acquire and release",
    0@@("o", "a1=acquire(o)", "a2=acquire(o)") -> (1 || 2) -> 3@@("p=phi(a1,a2)", "r1=release(o,p)", "r2=release(o,p)"))(

    // no optimized
  )(
    "a1", "a2", "r1", "r2"
  )

  testPaired("bad pair - consecutive acquire and release with no exits",
    0@@("o", "a1=acquire(o)", "a2=acquire(o)") -> (1 || 2) -> 3@@("p=phi(a1,a2)", "r1=release(o,p)", "r2=release(o,p)") -> !wd(4))(

    // no optimized
  )(
    "a1", "a2", "r1", "r2"
  )

}
