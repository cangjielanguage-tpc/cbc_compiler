/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind.*

/** Conversion bytecode operations
  *
  * @author cypok
  */
enum ConvertOp(val srcKind: BytecodeTypeKind, val dstKind: BytecodeTypeKind) {
  case I2L extends ConvertOp(INT, LONG)
  case I2F extends ConvertOp(INT, FLOAT)
  case I2D extends ConvertOp(INT, DOUBLE)
  case L2I extends ConvertOp(LONG, INT)
  case L2F extends ConvertOp(LONG, FLOAT)
  case L2D extends ConvertOp(LONG, DOUBLE)
  case F2I extends ConvertOp(FLOAT, INT)
  case F2L extends ConvertOp(FLOAT, LONG)
  case F2D extends ConvertOp(FLOAT, DOUBLE)
  case D2I extends ConvertOp(DOUBLE, INT)
  case D2L extends ConvertOp(DOUBLE, LONG)
  case D2F extends ConvertOp(DOUBLE, FLOAT)
  case I2B extends ConvertOp(INT, BYTE)
  case I2C extends ConvertOp(INT, CHAR)
  case I2S extends ConvertOp(INT, SHORT)
}
