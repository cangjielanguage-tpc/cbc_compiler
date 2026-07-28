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
import com.huawei.excelsior.jet.compiler.opt.lowering.amd64.LoweringAmd64
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.ScalesAmd64
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class Log2ViaLzcntIntelSuite extends CompilerSuite
        with GlobalNodesBuilder
        with LoweringAmd64 with Optimize with ScalesAmd64 {

  startPhase(CompilerPhase.Lowering)

  test("typical log2 implementation (regression test)") {
    makeCFG(0)
    val ret = (0: Block).blockEnd.asInstanceOf[Return]

    val x = addNode()
    val log2x = Sub(IConst(31), BitCount.leadingZeros(IntType, x))
    ret.inValue = log2x

    doLowering()

    ret.inValue should be (BitCount.highestBit(IntType, x))
  }

}
