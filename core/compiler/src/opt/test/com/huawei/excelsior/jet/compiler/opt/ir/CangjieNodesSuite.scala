/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.assembler.cbc.{CbcTypeKind, FieldReference}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.RTSProc.JR_FatalError
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, MethodAccessKind}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{CLASS, RECORD}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.*
import com.huawei.excelsior.jet.compiler.symlevel.{CallKind, SignatureType}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class CangjieNodesSuite extends CompilerSuite with GlobalNodesBuilder with TypesToolbox {

  test("DerivedPtr.Proxy phi web") {
    makeCFG((0 -> wd(1 -> (2 || 3) -> 4) -> 5))

    val x = Fake(AddrType)
    val y = Fake(AddrType)
    val phi = Phi.cyclic(AddrType)(1, phi => Seq(x, Phi(AddrType)(4, phi, y)))
    DerivedPtr.Proxy.resolve(phi) should matchPattern {
      case p @ Phi(B(1), _: DerivedPtr.Local, Phi(B(4), q, _: DerivedPtr.Local)) if p == q =>
    }
  }

  test("DerivedPtr.Proxy phi web with ReinterpretCast") {
    makeCFG((0 -> wd(1 -> (2 || 3) -> 4) -> 5))

    val x = Fake(TRefType)
    val y = Fake(TRefType)
    val rx = ReinterpretCast(TRefType, AddrType)(x)
    val ry = ReinterpretCast(TRefType, AddrType)(y)
    val phi = Phi.cyclic(AddrType)(1, phi => Seq(rx, Phi(AddrType)(4, phi, ry)))
    DerivedPtr.Proxy.resolve(phi) should matchPattern {
      case p @ Phi(B(1), `x`, Phi(B(4), q, `y`)) if p == q =>
    }
  }

}
