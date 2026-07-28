/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.{ADD, LSL}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, GlobalNodesBuilder}
import org.scalatest.Assertions
import org.scalatest.Assertions.assertThrows

class IRTypeInferenceSuite extends CompilerSuite with Identities with GlobalNodesBuilder with ArithNodesDSL with Types {
  
  private def node(tpe: Type): Node = Proxy(tpe)(entryBlock)
  
  test("add") {
    Add(node(AddrType), node(ExecEnvType)).tpe should be (AddrType)
    Add(node(ExecEnvType), node(AddrType)).tpe should be (AddrType)
    Add(node(LongType), node(LongType)).tpe should be (LongType)
    Add(node(DoubleType), node(DoubleType)).tpe should be (DoubleType)

    an[AssertionError] should be thrownBy { Add(node(LongType), node(IntType)) }
    an[AssertionError] should be thrownBy { Add(node(LongType), node(DoubleType)) }
  }

  test("shift") {
    Shift(LSL, node(IntType), node(IntType)).tpe should be(IntType)
    Shift(LSL, node(LongType), node(IntType)).tpe should be(LongType)
    Shift(LSL, node(AddrType), node(IntType)).tpe should be(AddrType)

    Shift(LSL, node(LongType), node(IntType)).tpe should not be(IntType)
    an[AssertionError] should be thrownBy { Shift(ADD, node(AddrType), node(IntType)).tpe should be(AddrType)  }
  }

}
