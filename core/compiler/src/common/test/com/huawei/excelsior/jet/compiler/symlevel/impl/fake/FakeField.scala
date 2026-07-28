/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.{DeferredAccessInfo, ErrorAccessInfo}
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.symlevel.*

/** Field with setters for all getters
  *
  * @author cypok
  */
object FakeField {
  def apply(field: java.lang.reflect.Field) =
    new FakeField(field.getName, SignatureType.fromSymType(FakeType.create(field.getType)))
      .setJavaModifiers(Modifiers(field.getModifiers))
}

class FakeField(name: String = "fake", `type`: SignatureType = SignatureType.fromSymType(FakeType(TypeKind.CLASS))) extends Field with ConstantPool.Access[Field] { self =>
  private var flat = false
  override def isAJFlat = flat
  def setAJFlat(flat: Boolean): FakeField = {
    this.flat = flat
    this
  }

  override def isStringTable = shouldNotCallThis()

  override def size: Int = shouldNotCallThis()

  override def alignment: Int = shouldNotCallThis()

  override def getPermanent: PermanentMember = new PermanentMember {
    override def get: Field = self
  }

  private var javaModifiers = Modifiers.EMPTY
  override def getJavaModifiersValue = javaModifiers.value
  def setJavaModifiers(javaModifiers: Modifiers): FakeField = {
    this.javaModifiers = javaModifiers
    this
  }

  def getCJModifiers: Modifiers = Modifiers.EMPTY

  private var deferred = false
  def setDeferred(): FakeField = {
    deferred = true
    this
  }

  override def getType = `type`

  private var offset = 0
  override def getOffset = offset
  def setOffset(offset: Int): FakeField = {
    this.offset = offset
    this
  }

  private var declaringClass = FakeType("Fake", TypeKind.CLASS)
  override def getDeclaringClass = declaringClass
  private[fake] def setDeclaringClass(declaringClass: FakeType): FakeField = {
    this.declaringClass = declaringClass
    this
  }

  override def getUniqueNumberInClass = 0

  override def hasInitialValue = false
  override def getInitialValue: ConstValues.ConstValue = null

  private var staticFieldSymbol: Symbol = _
  override def getStaticFieldSymbol = staticFieldSymbol
  def setStaticFieldSymbol(staticFieldSymbol: Symbol): Unit = {
    this.staticFieldSymbol = staticFieldSymbol
  }

  override def getFieldIndex: Int = shouldNotCallThis()

  override def getXName: XString = XString.ascii(name)

  override def getResult: ConstantPoolAccessResult = if (deferred) ConstantPoolAccessResult.DEFERRED else ConstantPoolAccessResult.OK
  override def getObject: FakeField = this
  override def getError: ErrorAccessInfo = shouldNotCallThis()
  override def getDeferredInfo: DeferredAccessInfo = shouldNotCallThis()

  override def getDebugType: DebugType = null

  override def shouldBeGenerated: Boolean = shouldNotCallThis()

  override def getExportedName: XString = shouldNotCallThis()

  override def getExternalName: XString = shouldNotCallThis()

  override def isOverloaded: Boolean = false

  override def getCHIRDef = None

  override def getCJAnnotationFactory: Method = null
}