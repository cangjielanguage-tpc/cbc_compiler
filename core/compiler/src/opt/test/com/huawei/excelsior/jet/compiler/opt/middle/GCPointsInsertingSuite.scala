/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*

/**
 * Tests for inserting gc-points
 *
 * @author ikireev
 * @author dbg
 */
class GCPointsInsertingSuite extends CompilerSuite
  with GlobalNodesBuilder
  with GCPointsInserting
  with IRTransformationsCollection
  with BackEndAmd64 {

  override def beforeEach(): Unit = {
    super.beforeEach()
    _rootMethod = null
  }

  /** Checks that there is unique GCPoint in the given node. */
  private def hasGCPoint(b: Block) =
    b.blockEnd.inCtrl.isInstanceOf[GCPoint] && (collect[GCPoint](b.spine).length == 1)

  test("gc-point inserting before return") {
    rootMethod.setCallConv(RTCALL)

    makeCFG(0 @@("x") -> 1 @@ ("ret(x)"))

    addGCPoints()

    hasGCPoint(b(1)) should be (true)
    all[GCPoint].length should be (1)
  }

  test("gc-point inserting in epilogue") {
    makeCFG(0 @@("x") -> 1 @@ ("ret(x)"))

    addGCPoints()

    hasGCPoint(b(1)) should be (false)
    all[GCPoint].length should be (0)
  }

  test("gc-point inserting on backward branch") {
    makeCFG(0 -> dw(1 -> 2))

    addGCPoints()

    all[GCPoint].length should be (1)
    hasGCPoint(b(2)) should be (true)
  }

  test("gc-points inserting on backward branches and before return") {
    rootMethod.setCallConv(RTCALL)

    makeCFG(0 @@("x") -> 1 -> 2 -> wd(3 -> 4) -> dw(5 -> 6) -> 7 @@ ("ret(x)"))

    addGCPoints()

    all[GCPoint].length should be (3)
    hasGCPoint(b(7)) should be (true)
  }

  test("gc-points inserting on backward branches and before return 2") {
    rootMethod.setCallConv(RTCALL)

    makeCFG(0 -> dw(1 -> 2 -> 3) -> dw(4 -> wd(5 -> 6) -> 7) -> 8)

    addGCPoints()

    all[GCPoint].length should be (4)
    hasGCPoint(b(8)) should be (true)
  }

  test("no gc-points needed") {
    makeCFG(0 -> 1)

    addBlockEnd(1) { case (ctrl, memory) =>
      val thrw = Throw(ctrl, memory, Fake(TRefType))
      Halt.empty()(thrw, thrw)
    }

    addGCPoints()

    all[GCPoint].isEmpty should be (true)
  }

  // TODO: make tests for GCPoints inserting into counted and not counted loops

}