/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute.ExceptionTableTraverser

object MethodCodeAttribute {
  trait ExceptionTableTraverser {
    def hasNext: Boolean

    def queryNext(): Unit

    /** Start (offset in method's bytecode, inclusive) of current protected region. */
    def startPC: Int

    /** End (offset in method's bytecode, exclusive) of current protected region. */
    def endPC: Int

    /** Handler (offset in method's bytecode) for current protected region. */
    def handlerPC: Int

    /** A constant pool index of Class_info structure represents
      * class of exceptions handled by current protected region.
      *
      * @return 0 for "catch_all" regions.
      */
    def catchTypeIndex: Int

    /** Name of class of exceptions handled by current protected region.
      *
      * @return null for "catch_all" regions.
      */
    def catchTypeName: XString
  }

  abstract class ExceptionTableTraverserArrayImpl[T](array: Array[T]) extends ExceptionTableTraverser {
    private var index = -1
    private val endIndex = if (array == null) -1 else array.length - 1

    def startPC(x: T): Int

    def endPC(x: T): Int

    def handlerPC(x: T): Int

    def catchTypeIndex(x: T): Int

    def catchTypeName(x: T): XString

    override def startPC: Int = startPC(array(index))

    override def endPC: Int = endPC(array(index))

    override def handlerPC: Int = handlerPC(array(index))

    override def catchTypeIndex: Int = catchTypeIndex(array(index))

    override def catchTypeName: XString = catchTypeName(array(index))

    override final def hasNext = index < endIndex

    override final def queryNext(): Unit = {
      assert(hasNext)
      index += 1
    }
  }
}

trait MethodCodeAttribute {
  /** Maximum stack size of the method. */
  def maxStack: Int

  /** Maximum locals count of the method. */
  def maxLocals: Int

  /** Length (number of bytes) of the method's bytecode. */
  def bytecodeLength: Int

  /** Bytecode array. Contains bytecode of the method starting from [[bytecodeStart]]. */
  def bytecodeArray: Array[Byte]

  /** Start of the method's bytecode in the bytecode array. */
  def bytecodeStart: Int

  /** Returns true if this method has non empty exception table. */
  def hasExceptionTable: Boolean

  /** Entries of the exception table of the method wrapped into traverser. */
  def getExceptionTableTraverser: ExceptionTableTraverser

  /** Content of 'StackMapTable' attribute of this Code attribute. */
  def stackMapTable: Array[Byte]
}
