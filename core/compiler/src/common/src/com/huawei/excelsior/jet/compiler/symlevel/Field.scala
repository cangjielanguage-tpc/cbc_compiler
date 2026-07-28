/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.VOLATILE

/** Some Java class field.
  *
  * @author cypok
  */
abstract class Field extends Member with ConstantPoolObject {

  def isStringTable: Boolean

  def isVolatile = getJavaModifiers contains VOLATILE

  /** Check if the normal field is @Flat or not */
  def isAJFlat: Boolean

  def size: Int
  def alignment: Int

  def getType: SignatureType
  
  def getSignature: SignatureType = getType

  protected def getOffset: Int

  /** Get the offset of a field in object */
  final def getInstanceFieldOffset = {
    assert(!isStatic)
    getOffset
  }

  /** Get the offset of a field in static bundle */
  final def getStaticFieldOffset = {
    assert(isStatic)
    getOffset
  }

  /** Get symbol of the normal static field */
  def getStaticFieldSymbol: Symbol

  /** Returns index of this field in class file of declaring class. */
  def getFieldIndex: Int

  override def getMemberIndex: Int = getFieldIndex

  // Note: it is intentionally different from getFullName to avoid confusion in future
  override final def toString = s"field $getFullName"

  override def getFullName = getDeclaringClass.getName + (if (isStatic) "." else "#") + getName

  /** Returns unique number of this field in scope of host class. */
  def getUniqueNumberInClass: Int

  /** Returns unique number of this field in global scope. */
  def getUniqueNumber: Long = (getDeclaringClass.getUniqueNumber.toLong << 32) + getUniqueNumberInClass

  /** Returns true iff `this` field is static field initialized from constant pool value. */
  def hasInitialValue: Boolean

  def getInitialValue: ConstValues.ConstValue

  def getCJAnnotationFactory: Method
}
