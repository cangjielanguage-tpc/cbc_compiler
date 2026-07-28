/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

abstract class PostProcessSuite extends CompilerSuite with GlobalNodesBuilder {

  // Tests require that node points are ordered according to their definitions.
  override def parsableAttributes() = Seq(
    new SimpleAttribute("s")({
      case Seq() => FakeSpinal(IntType)()
      case Seq(x, y) => FakeSpinalBinary(IntType)(x, y)
      case _ => shouldNotReachHere()
    }),
    new SimpleAttribute("xs")({ case Seq() => FakeSpinalX(IntType)() }),
  ) ++ super.parsableAttributes()

}
