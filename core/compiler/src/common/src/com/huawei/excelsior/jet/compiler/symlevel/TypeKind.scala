/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.{addressLog2Size, addressSize}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind

import scala.annotation.tailrec

/** Kind of some Java type
  *
  * @author cypok
  * @author paul
  */
enum TypeKind(private val bcSignatureChar: Char, val width: Width, _basicType: Int, _bcTypeKind: Int) {
  case VOID      extends TypeKind('V', W0, 0, 0)
  case BOOLEAN   extends TypeKind('Z', W8, 2, 4)
  case BYTE      extends TypeKind('B', W8, 1, 8)
  case SHORT     extends TypeKind('S', W16, 4, 9)
  case CHAR      extends TypeKind('C', W16, 3, 5)
  case INT       extends TypeKind('I', W32, 5, 10)
  case LONG      extends TypeKind('J', W64, 6, 11)
  case FLOAT     extends TypeKind('F', W32, 7, 6)
  case DOUBLE    extends TypeKind('D', W64, 8, 7)
  case CLASS     extends TypeKind('L', WPTR, 9, 1)
  case INTERFACE extends TypeKind('L', WPTR, 9, 2)
  case ARRAY     extends TypeKind('[', WPTR, 9, 3)
  case THIN      extends TypeKind('L', WPTR, -1, -1)
  case RECORD    extends TypeKind('X', WPTR, -1, -1)

  private val basicType = _basicType.toByte
  private val bcTypeKind = _bcTypeKind.toByte

  /** Returns log2 size of type kind in memory. */
  def log2Size = this match {
    case BOOLEAN | BYTE => 0
    case CHAR | SHORT => 1
    case INT | FLOAT => 2
    case ARRAY | INTERFACE | CLASS | THIN | RECORD => addressLog2Size
    case DOUBLE | LONG => 3
    case VOID => shouldNotReachHere(this.toString)
  }

  /** Returns size of type kind in memory. */
  def size = this match {
    case VOID => 0
    case _ => 1 << log2Size
  }

  def alignment = this match {
    case VOID => 1
    case RECORD => shouldNotReachHere()
    case _ => size
  }

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

  def isClass = this == CLASS
  def isPrimitive = !isReference && (this != RECORD)
  def isReference = isTraceableReference || (this == THIN)
  def isTraceableReference = (this == CLASS) || (this == ARRAY) || (this == INTERFACE)

  def isInterface = this == INTERFACE
  def isThin = this == THIN
  def isVoid = this == VOID

  def isFloatingPoint = (this == FLOAT) || (this == DOUBLE)
  def isIntegral = isShortIntegral || (this == INT) || (this == LONG)
  def isShortIntegral = (this == BOOLEAN) || (this == BYTE) || (this == SHORT) || (this == CHAR)

  def getBCSignatureChar = bcSignatureChar

  def getBasicType = {
    assert(basicType != -1)
    basicType
  }

  def getBCTypeKind = bcTypeKind

  def toAsm = (this: @unchecked) match {
    case VOID           => AsmType.NONE
    case BOOLEAN | BYTE => AsmType.I8
    case SHORT          => AsmType.I16
    case CHAR           => AsmType.U16
    case INT            => AsmType.I32
    case LONG           => AsmType.I64
    case FLOAT          => AsmType.F32
    case DOUBLE         => AsmType.F64
    case CLASS | THIN | INTERFACE | RECORD | ARRAY => AsmType.PTR
  }
}

object TypeKind {
  // JET-14216: Hard-coded because this value might be used by the interpreter before this class is initialized.
  val COUNT = 14

  assert(COUNT == TypeKind.values.length)

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
      case _ => shouldNotReachHere(s"Unexpected signature char '$ch'")
    }
    assert(kind.bcSignatureChar == ch)
    kind
  }

  def byPrimTypeCode(primTypeCode: Int) = {
    val kind = primTypeCode match {
      case 0 => VOID
      case 2 => BOOLEAN
      case 1 => BYTE
      case 4 => SHORT
      case 3 => CHAR
      case 5 => INT
      case 6 => LONG
      case 7 => FLOAT
      case 8 => DOUBLE
      case _ => shouldNotReachHere(String.valueOf(primTypeCode))
    }
    assert(kind.getBasicType == primTypeCode)
    kind
  }

  def fromBytecode(btk: BytecodeTypeKind) = btk match {
    case BytecodeTypeKind.BOOLEAN => BOOLEAN
    case BytecodeTypeKind.BYTE => BYTE
    case BytecodeTypeKind.SHORT => SHORT
    case BytecodeTypeKind.CHAR => CHAR
    case BytecodeTypeKind.INT => INT
    case BytecodeTypeKind.LONG => LONG
    case BytecodeTypeKind.FLOAT => FLOAT
    case BytecodeTypeKind.DOUBLE => DOUBLE
    case BytecodeTypeKind.CLASS => CLASS
    case BytecodeTypeKind.ARRAY => ARRAY
    case BytecodeTypeKind.THIN => THIN
    case BytecodeTypeKind.VOID => VOID
  }

  def address = addressSize match {
    case 4 => INT
    case 8 => LONG
  }

  def primitives = TypeKind.values.filter(_.isPrimitive)
}
