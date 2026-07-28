/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.intrinsics

import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.compiler.intrinsics.IntrinsicWithBody
import com.huawei.excelsior.jet.compiler.intrinsics.IntrinsicWithBody.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.PartialFunction.condOpt

trait IntrinsicsCBC extends Intrinsics { self: Universe =>
  override def loadIntrinsicWithBody(target: Method, itype: IntrinsicWithBody, caller: Method, args: Seq[Node]): Option[Node] = condOpt(itype) {
    case Half_fromFloat => ValueConvert(F32, F16)(args: _*)
    case Half_toFloat   => ValueConvert(F16, F32)(args: _*)

  } orElse {
    super.loadIntrinsicWithBody(target, itype, caller, args)
  }


}
