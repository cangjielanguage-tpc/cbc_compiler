/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.ir.{ColumnNumber}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

private[bytecode] trait Toolbox { self: Universe =>

  private[bytecode] def currentMethodPos(bytecodeOffset: Int) = {
    val ic = currentInlineContext
    BytecodePosition(bytecodeOffset, ic.method.codeAttribute.findLineNumber(bytecodeOffset), ColumnNumber.UNKNOWN, ic)
  }

  private[bytecode] def currentMethodSyntheticPos =
    BytecodePosition(currentInlineContext)

}
