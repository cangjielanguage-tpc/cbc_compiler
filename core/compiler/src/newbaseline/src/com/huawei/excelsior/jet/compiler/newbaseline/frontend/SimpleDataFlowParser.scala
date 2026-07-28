/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.parsing.BlockDataFlowParser.RET_ADDR_KIND
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodAJCallKind, MethodType, SignatureType}

abstract class SimpleDataFlowParser[V](method: Method, block: Block, slots: Slots)
  extends BaseParser[V](method, block, slots) {

  private val declClass = method.getDeclaringClass

  protected def newValue(`type`: BytecodeTypeKind): V

  protected def useValue(value: V): Unit

  private def pushNewValue(`type`: BytecodeTypeKind): Unit = {
    val value = newValue(`type`)
    push(`type`, value)
  }

  override def pushCPEntry(index: Int): Unit = {
    import Tag.*;
    cp.getTag(index) match {
      case INTEGER =>
        pushNewValue(BytecodeTypeKind.INT)
      case FLOAT =>
        pushNewValue(BytecodeTypeKind.FLOAT)
      case STRING | CLASS | METHOD_TYPE | METHOD_HANDLE =>
        pushNewValue(BytecodeTypeKind.CLASS)

      case LONG =>
        pushNewValue(BytecodeTypeKind.LONG)
      case DOUBLE =>
        pushNewValue(BytecodeTypeKind.DOUBLE)

      case tag =>
        shouldNotReachHere(tag)
    }
  }

  override def pushConst(`type`: BytecodeTypeKind, value: Int): Unit = {
    pushNewValue(`type`)
  }

  override def arithOp(`type`: BytecodeTypeKind, op: ArithOp): Unit = {
    if (op == ArithOp.NEG) {
      // only one arg
    } else if (op.isShift) {
      // second arg is a shift distance
      useValue(pop(BytecodeTypeKind.INT))
    } else {
      useValue(pop(`type`))
    }
    useValue(pop(`type`))

    pushNewValue(if (op.isCmp) BytecodeTypeKind.INT else `type`)
  }

  override def convert(op: ConvertOp): Unit = {
    useValue(pop(op.srcKind))
    pushNewValue(op.dstKind)
  }

  override def increment(local: Int, delta: Int): Unit = {
    useValue(read(BytecodeTypeKind.INT, local))
    write(BytecodeTypeKind.INT, local, newValue(BytecodeTypeKind.INT))
  }

  override def arrayGet(`type`: BytecodeTypeKind): Unit = {
    useValue(pop(BytecodeTypeKind.INT))
    useValue(pop(BytecodeTypeKind.ARRAY))
    pushNewValue(`type`)
  }

  override def arrayPut(`type`: BytecodeTypeKind): Unit = {
    useValue(pop(`type`))
    useValue(pop(BytecodeTypeKind.INT))
    useValue(pop(BytecodeTypeKind.ARRAY))
  }

  override def fieldOp(index: Int, akind: FieldAccessKind): Unit = {
    val sigType = typeProvider.resolveSingleElementSignature(cp.getRefSignature(index), declClass)
    fieldOp(akind, sigType.jbcKind)
  }

  private def fieldOp(akind: FieldAccessKind, `type`: BytecodeTypeKind): Unit = {
    if (akind.isStatic) {
      if (akind.isWrite) {
        useValue(pop(`type`))
      } else {
        pushNewValue(`type`)
      }
    } else {
      if (akind.isWrite) {
        useValue(pop(`type`))
        useValue(pop(BytecodeTypeKind.CLASS))
      } else {
        useValue(pop(BytecodeTypeKind.CLASS))
        pushNewValue(`type`)
      }
    }
  }

  private def popAndUseValue(kind: BytecodeTypeKind) = {
    val value = pop(kind)
    useValue(value)
    value
  }

  override def invoke(index: Int, akind: MethodAccessKind): Unit = {
    val methodType = MethodType.forJava(cp.getRefSignature(index), typeProvider, declClass)
      .insertReceiverType(SignatureType.fromSymType(typeProvider.getObjectType), akind.hasObjectArg)

    if ((akind != MethodAccessKind.DYNAMIC) && !cp.isMethodSignaturePolymorphic(index)) {
      val methodAccess = cp.getMethodReference(index, akind)
      if (methodAccess.getResult == ConstantPoolAccessResult.OK) {
        val target = methodAccess.getObject.method
        assert(!target.isIntrinsicCall)
      }
    }

    invoke(methodType)
  }

  private def invoke(methodType: MethodType): Unit = {
    val paramCount = methodType.parameterCount
    for (i <- paramCount - 1 to 0 by -1) {
      val value = popAndUseValue(methodType.parameterType(i).jbcKind)
    }

    val returnKind = methodType.returnType.jbcKind
    if (!returnKind.isVoid) {
      pushNewValue(returnKind)
    }
  }

  override def monitorEnter(): Unit = {
    useValue(pop(BytecodeTypeKind.CLASS))
  }

  override def monitorExit(): Unit = {
    useValue(pop(BytecodeTypeKind.CLASS))
  }

  override def doNew(index: Int): Unit = {
    pushNewValue(BytecodeTypeKind.CLASS)
  }

  override def instanceOf(index: Int): Unit = {
    useValue(pop(BytecodeTypeKind.CLASS))
    pushNewValue(BytecodeTypeKind.INT)
  }

  override def checkCast(index: Int): Unit = {
    val x = pop(BytecodeTypeKind.CLASS)
    useValue(x)
    push(BytecodeTypeKind.CLASS, x)
  }

  override def doThrow(): Unit = {
    useValue(pop(BytecodeTypeKind.CLASS))
    clearStack()
  }

  override def newPrimitiveArray(`type`: BytecodeTypeKind): Unit = {
    useValue(pop(BytecodeTypeKind.INT))
    pushNewValue(BytecodeTypeKind.ARRAY)
  }

  override def newObjectArray(index: Int): Unit = {
    useValue(pop(BytecodeTypeKind.INT))
    pushNewValue(BytecodeTypeKind.ARRAY)
  }

  override def newMultiObjectArray(index: Int, dimNum: Int): Unit = {
    for (i <- 0 until dimNum) {
      useValue(pop(BytecodeTypeKind.INT))
    }
    pushNewValue(BytecodeTypeKind.ARRAY)
  }

  override def arrayLength(): Unit = {
    useValue(pop(BytecodeTypeKind.CLASS))
    pushNewValue(BytecodeTypeKind.INT)
  }

  override def unaryIf(`type`: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = {
    useValue(pop(`type`))
  }

  override def binaryIf(`type`: BytecodeTypeKind, op: CompareOp, bc: Int): Unit = {
    useValue(pop(`type`))
    useValue(pop(`type`))
  }

  override def jump(bc: Int): Unit = {}

  override def jsr(bc: Int): Unit = {
    pushNewValue(RET_ADDR_KIND)
  }

  override def ret(`var`: Int): Unit = {
    // we ignore saved ret addr, because subroutines are inlined
  }

  override def doReturn(`type`: BytecodeTypeKind, isLastBytecode: Boolean): Unit = {
    if (!`type`.isVoid) {
      useValue(pop(`type`))
    }
    clearStack()
  }

  override def tableSwitch(bcDefault: Int, lowMatch: Int, highMatch: Int, bcTargets: Array[Int]): Unit = {
    useValue(pop(BytecodeTypeKind.INT))
  }

  override def lookupSwitch(bcDefault: Int, matches: Array[Int], bcTargets: Array[Int]): Unit = {
    useValue(pop(BytecodeTypeKind.INT))
  }
}
