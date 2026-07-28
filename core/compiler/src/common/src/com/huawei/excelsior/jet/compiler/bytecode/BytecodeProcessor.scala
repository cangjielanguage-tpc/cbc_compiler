/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Interface for processor over JVM instructions
  *
  * @author paul
  */
trait BytecodeProcessor {

  /** Start to process next instruction that starts from offset and ends before nextOffset.
    *
    * @param offset     instruction offset.
    * @param nextOffset next instruction offset.
    */
  def startInstruction(offset: Int, nextOffset: Int): Unit = {}

  def finishInstruction(): Unit = {}

  def pushCPEntry(index: Int): Unit

  def pushConst(`type`: BytecodeTypeKind, value: Int): Unit

  def pushLocal(`type`: BytecodeTypeKind, index: Int): Unit
  def storeLocal(`type`: BytecodeTypeKind, index: Int): Unit

  def arithOp(`type`: BytecodeTypeKind, op: ArithOp): Unit
  def convert(op: ConvertOp): Unit
  def stackOp(op: Bytecode): Unit

  def increment(local: Int, delta: Int): Unit

  def arrayGet(`type`: BytecodeTypeKind): Unit
  def arrayPut(`type`: BytecodeTypeKind): Unit

  def fieldOp(index: Int, akind: FieldAccessKind): Unit
  def invoke(index: Int, akind: MethodAccessKind): Unit

  def monitorEnter(): Unit
  def monitorExit(): Unit

  def doNew(index: Int): Unit
  def instanceOf(index: Int): Unit
  def checkCast(index: Int): Unit
  def doThrow(): Unit

  def newPrimitiveArray(`type`: BytecodeTypeKind): Unit
  def newObjectArray(index: Int): Unit
  def newMultiObjectArray(index: Int, dimNum: Int): Unit

  def arrayLength(): Unit

  def unaryIf(`type`: BytecodeTypeKind, op: CompareOp, bc: Int): Unit
  def binaryIf(`type`: BytecodeTypeKind, op: CompareOp, bc: Int): Unit
  def jump(bc: Int): Unit
  def jsr(bc: Int): Unit
  def ret(`var`: Int): Unit

  def doReturn(`type`: BytecodeTypeKind, isLastBytecode: Boolean): Unit

  def nop(): Unit = {}

  def tableSwitch(bcDefault: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit
  def lookupSwitch(bcDefault: Int, matches: Array[Int], bcTargets: Array[Int]): Unit
}
