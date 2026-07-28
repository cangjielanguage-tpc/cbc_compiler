/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Width.*

/** Low-level types used in assembler instructions and objects.
  *
  * @author conwor
  */
enum AsmType(val width: Width, val signed: Boolean) {
  // Use only for addressing mode construction for LEA instruction
  case NONE extends AsmType(WNONE, true)
  
  case I8 extends AsmType(W8, true)
  case U8 extends AsmType(W8, false)
  
  case I16 extends AsmType(W16, true)
  case U16 extends AsmType(W16, false)
  
  case I32 extends AsmType(W32, true)
  case U32 extends AsmType(W32, false)
  
  case I64 extends AsmType(W64, true)
  case U64 extends AsmType(W64, false)
  
  case F16 extends AsmType(W16, true)
  case F32 extends AsmType(W32, true)
  case F64 extends AsmType(W64, true)
  
  case PTR extends AsmType(WPTR, true)

  def isPrimitive = this match {
    case I8  | U8  | I16 | U16 |
         I32 | U32 | I64 | U64 |
         F16 | F32 | F64  => true
    case NONE | PTR => false
  }
  def isPointer = this == PTR

  def isFloatingPoint = this == F16 || this == F32 || this == F64

  def isIntegral = this match {
    case I8  | U8  | I16 | U16 |
         I32 | U32 | I64 | U64 => true
    case _ => false
  }

  def isShortIntegral = this match {
    case I8 | U8 | I16 | U16 => true
    case _ => false
  }

  def sizeInBytes = this.width.nbytes
  def sizeInBits = this.width.nbits

  def isSigned   = this == I8 || this == I16 || this == I32 || this == I64
  def isUnsigned = this == U8 || this == U16 || this == U32 || this == U64
}

object AsmType {
  def integral(width: Width, signed: Boolean) = (width: @unchecked) match {
    case W8  => if (signed) I8  else U8
    case W16 => if (signed) I16 else U16
    case W32 => if (signed) I32 else U32
    case W64 => if (signed) I64 else U64
  }
}
