/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.classfile.NameAndSigComparable
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{FINAL, PRIVATE, PROTECTED, PUBLIC, STATIC, SYNCHRONIZED, SYNTHETIC}
import com.huawei.excelsior.jet.compiler.symlevel.indy.CHIRDef

trait Member extends ConstantPoolObject {
  def getXName: XString

  /** Java access modifiers of this method. The [[Modifiers]] class should be used to decode. */
  def getJavaModifiersValue: Int

  def getJavaModifiers: Modifiers = Modifiers(getJavaModifiersValue)
  def getCJModifiers: Modifiers

  def isPrivate =   getJavaModifiers contains PRIVATE
  def isProtected = getJavaModifiers contains PROTECTED
  def isPublic =    getJavaModifiers contains PUBLIC
  def isStatic =    getJavaModifiers contains STATIC
  def isFinal =     getJavaModifiers contains FINAL
  def isSynthetic = getJavaModifiers contains SYNTHETIC

  /** Get the declaring class */
  def getDeclaringClass: ClassType

  /** Returns index of this member in class file of declaring class. */
  def getMemberIndex: Int

  /** Get Java member name. */
  final def getName: String = getXName.toString

  /** Returns member signature as defined in source. */
  def getSignature: Signature

  /** Get full Java member name with class specification and signature if it has one. */
  def getFullName: String

  /** Provides permanent mirror of the member that survives between compilation sessions. */
  def getPermanent: PermanentMember

  def getCPPLinkageName: XString = shouldNotCallThis("not supported in current compiler configuration")
  def getSourceName: XString =     shouldNotCallThis("not supported in current compiler configuration")
  def getSourceFile: XString =     shouldNotCallThis("not supported in current compiler configuration")
  def getSourceLine: Int =         shouldNotCallThis("not supported in current compiler configuration")

  def setCPPLinkageName(name: XString): Unit = shouldNotCallThis("not supported in current compiler configuration")
  def setSourceName(name: XString): Unit =     shouldNotCallThis("not supported in current compiler configuration")
  def setSourceFile(name: XString): Unit =     shouldNotCallThis("not supported in current compiler configuration")
  def setSourceLine(line: Int): Unit =         shouldNotCallThis("not supported in current compiler configuration")

  /** Returns debug type of `this` member (field type or method return type) if debug information exists. Otherwise returns null. */
  def getDebugType: DebugType = shouldNotCallThis("not supported in current compiler configuration")

  /** Set debug type for member (type of field or method return type). */
  def setDebugType(debugType: DebugType): Unit = shouldNotCallThis("not supported in current compiler configuration")

  def hasSourceFile = {
    val name = getSourceFile
    name != null && !name.isEmpty
  }

  def hasSourceName = {
    val name = getSourceName
    name != null && !name.isEmpty
  }

  /** Returns true when there is a compiler-generated binary object (code or static data) for this member. */
  def shouldBeGenerated: Boolean

  /** Returns exported name or null if the member is not exported or does not have exported name. */
  def getExportedName: XString

  /** Returns external name or null if the member is not external or does not have external name. */
  def getExternalName: XString

  def isOverloaded: Boolean

  def getCHIRDef: Option[CHIRDef]
}
