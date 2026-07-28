/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind as BTK

enum NodeType {
  case
    ADDR, TREF, THIN,
    INT, FLOAT,
    LONG, DOUBLE,
    LONG_DOUBLE_2

  def isFP = this == FLOAT || this == DOUBLE

  def toAsm = this match {
    case ADDR | THIN | TREF => AsmType.PTR
    case INT => AsmType.I32
    case FLOAT => AsmType.F32
    case LONG => AsmType.I64
    case DOUBLE => AsmType.F64
    case LONG_DOUBLE_2 => shouldNotReachHere(this)
  }
}

object NodeType {

  def by(`type`: BytecodeTypeKind) = `type` match {
    case BTK.BOOLEAN | BTK.BYTE | BTK.SHORT | BTK.CHAR | BTK.INT => INT
    case BTK.LONG => LONG
    case BTK.FLOAT => FLOAT
    case BTK.DOUBLE => DOUBLE
    case BTK.CLASS | BTK.ARRAY => TREF
    case BTK.THIN => THIN
    case BTK.VOID => shouldNotReachHere(`type`)
  }
}
