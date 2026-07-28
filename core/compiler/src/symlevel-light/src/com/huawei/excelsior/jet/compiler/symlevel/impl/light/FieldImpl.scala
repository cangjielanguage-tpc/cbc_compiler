/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.StringTable
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*

class FieldImpl(val o2f: pcOModule.Field) extends Field with Symbol with SymLevelObject with ConstantPool.Access[Field] {
  override def o2object: pcOModule.Field = o2f

  override def getJavaModifiersValue = o2f.getJavaModifiers.toInt

  override def getCJModifiers = {
    assert(getDeclaringClass.isCangjieType)
    Modifiers(o2f.getCJModifiers.toInt)
  }

  private def asInstance = o2f.asInstanceOf[pcOModule.InstanceField]

  private def asStatic = o2f.asInstanceOf[pcOModule.StaticField]

  override def isAJFlat = o2f.isAJFlat

  override def size = o2f.size
  override def alignment = o2f.alignment

  override def getType = o2f.sig

  override protected def getOffset = o2f.getOffset

  override def getStaticFieldSymbol = {
    asStatic
    this
  }

  override def getFieldIndex = o2f.getNumberInClassFile

  override def getUniqueNumberInClass = o2f.lref

  override def hasInitialValue = isStatic && asStatic.hasInitialValue

  override def getInitialValue = {
    assert(hasInitialValue)
    assert(isStatic)
    asStatic.value
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: FieldImpl => memberEquals(this.o2f, that.o2f)
    case _ => false
  }

  override def hashCode = memberHashCode(o2f)

  override def getDeclaringClass = classByO2Object(o2f.getDeclaringClass)

  override def getXName = o2name(o2f)

  override def getResult = if (o2f.getDeclaringClass.isShielded) ConstantPoolAccessResult.DEFERRED else ConstantPoolAccessResult.OK

  override def getObject = this
  override def getError = shouldNotCallThis()

  override def getDeferredInfo = shouldNotCallThis("DeferredInfo is under development yet")

  override def getCPPLinkageName  = o2f.cppLinkageName
  override def getSourceName      = o2f.sourceName
  override def getSourceFile      = o2f.sourceFile
  override def getSourceLine      = o2f.sourceLine
  override def getDebugType       = o2f.debugType

  override def setCPPLinkageName(name: XString)   : Unit = o2f.cppLinkageName = name
  override def setSourceName(name: XString)       : Unit = o2f.sourceName = name
  override def setSourceFile(file: XString)       : Unit = o2f.sourceFile = file
  override def setSourceLine(line: Int)           : Unit = o2f.sourceLine = line
  override def setDebugType(debugType: DebugType) : Unit = o2f.debugType = debugType

  override def ownsSegment = o2f.ownsSegment

  override def getPermanent: PermanentMember = new PermanentMemberImpl(o2f.getRef) {
    override def get: Field = new FieldImpl(ref.getField)
  }

  override def shouldBeGenerated: Boolean = isStatic && asStatic.shouldBeGenerated

  override def getExportedName = if (o2f.isExported) o2f.getExportedName else null

  override def getExternalName = if (o2f.isExternal) o2f.getExternalName else null

  override def isOverloaded: Boolean = o2f.isOverloaded

  override def getCHIRDef = o2f.getCHIRDef

  override def isStringTable: Boolean = o2f.isInstanceOf[StringTable]

  override def getCJAnnotationFactory: Method = {
    val factory = o2f.getCJAnnotationFactory
    if (factory == null) {
      null
    } else {
      methodByO2Object(factory)
    }
  }
}
