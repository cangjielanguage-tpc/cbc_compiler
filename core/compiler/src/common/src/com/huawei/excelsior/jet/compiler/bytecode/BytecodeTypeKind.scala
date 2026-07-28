/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.addressLog2Size

enum BytecodeTypeKind(val bcSignatureChar: Char, val width: Width, _bcTypeKind: Int) {
  case VOID    extends BytecodeTypeKind('V', W0, 0)
  case BOOLEAN extends BytecodeTypeKind('Z', W8, 4)
  case BYTE    extends BytecodeTypeKind('B', W8, 8)
  case SHORT   extends BytecodeTypeKind('S', W16, 9)
  case CHAR    extends BytecodeTypeKind('C', W16, 5)
  case INT     extends BytecodeTypeKind('I', W32, 10)
  case LONG    extends BytecodeTypeKind('J', W64, 11)
  case FLOAT   extends BytecodeTypeKind('F', W32, 6)
  case DOUBLE  extends BytecodeTypeKind('D', W64, 7)
  case CLASS   extends BytecodeTypeKind('L', WPTR, 1)
  case ARRAY   extends BytecodeTypeKind('[', WPTR, 3)
  case THIN    extends BytecodeTypeKind('L', WPTR, -1)

  /** Corresponding value for [[com.huawei.excelsior.jet.runtime.typedesc.TypeKind]].
    * Used by [[com.huawei.excelsior.jet.jit.interpreter.Interpreter]].
    */
  private val bcTypeKind = _bcTypeKind.toByte

  def isPrimitive = !isReference
  def isReference = isTraceableReference || (this == THIN)
  def isTraceableReference = (this == CLASS) || (this == ARRAY)

  def isFloatingPoint = (this == FLOAT) || (this == DOUBLE)
  def isIntegral = isShortIntegral || (this == INT) || (this == LONG)
  def isShortIntegral = (this == BOOLEAN) || (this == BYTE) || (this == SHORT) || (this == CHAR)

  def isVoid = this == VOID

  /** Values of JVMLong types occupy two units of memory in bytecode's stack machine
    * (i.e. a pair of local variables or two slots on the operand stack).
    *
    * @return true iff the type is JVMLong.
    */
  def is2Slots = (this == LONG) || (this == DOUBLE)

  /** Returns number of Java expression slots occupied by value of this type */
  def nslots = if (is2Slots) 2 else 1

  /** Returns log2 size of type kind in memory */
  def log2Size = this match {
    case BOOLEAN | BYTE => 0
    case CHAR | SHORT => 1
    case INT | FLOAT => 2
    case ARRAY | CLASS | THIN => addressLog2Size
    case DOUBLE | LONG => 3
    case VOID => shouldNotReachHere(this)
  }

  /** Returns size of type kind in memory. */
  def size = 1 << log2Size

  /** Returns kind which is used instead of `this` kind in bytecode instructions.
    * E.g. all short integral types are represented as integers in method's bytecode.
    */
  def toBytecodeApproximation = if (isShortIntegral) {
    INT
  } else if (isReference) {
    CLASS
  } else {
    this
  }

  def toAsm = this match {
    case BOOLEAN | BYTE => AsmType.I8
    case SHORT          => AsmType.I16
    case CHAR           => AsmType.U16
    case INT            => AsmType.I32
    case LONG           => AsmType.I64
    case FLOAT          => AsmType.F32
    case DOUBLE         => AsmType.F64
    case CLASS | THIN | ARRAY => AsmType.PTR
    case VOID => shouldNotReachHere(this)
  }
}

object BytecodeTypeKind {
  def fromBCSignatureChar(ch: Byte) = {
    val kind = ch match {
      case 'Z' => BOOLEAN
      case 'B' => BYTE
      case 'S' => SHORT
      case 'C' => CHAR
      case 'I' => INT
      case 'J' => LONG
      case 'F' => FLOAT
      case 'D' => DOUBLE
      case 'L' => CLASS // interfaces are not recognized
      case '[' => ARRAY
      case 'V' => VOID
    }
    kind ensuring (_.bcSignatureChar == ch)
  }
}
