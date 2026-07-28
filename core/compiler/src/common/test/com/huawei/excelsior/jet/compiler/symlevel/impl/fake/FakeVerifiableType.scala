/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.verifier.{AbstractVerifier, VerifiableMethod, VerifiableType}

import scala.collection.mutable

class FakeVerifiableType(private[fake] val impl: FakeType) extends VerifiableType {
  val verificationImports: mutable.Set[FakeType] = mutable.Set.empty

  override def isAJType = impl.isAJType

  override def isAnonymous = impl.isAnonymous

  override def isClass = impl.isClass

  override def isClassOrInterface = impl.isClassOrInterface

  override def isDeferred = impl.isDeferred

  override def isHierarchyRoot = impl.isHierarchyRoot

  override def isInterface = impl.isInterface

  override def isSamePackage(that: VerifiableType) = impl.isSamePackage(that.asInstanceOf[FakeVerifiableType].impl)

  override def isUnloadable = impl.isUnloadable

  override def isAssignableFrom(`type`: VerifiableType) = impl.isAssignableFrom(`type`.asInstanceOf[FakeVerifiableType].impl)

  override def containsProtectedField(fieldName: XString, fieldSig: XString) = shouldNotCallThis()

  override def containsProtectedMethod(fieldName: XString, fieldSig: XString) = shouldNotCallThis()

  override def getAbsentSuper: VerifiableType = null

  override def getDeclaredMethods: Iterator[VerifiableMethod] =
    impl.declaredFakeMethods.map(m => new FakeVerifiableMethod(m).asInstanceOf[VerifiableMethod])

  override def getDeclaredSuperInterfaces: Iterator[VerifiableType] =
    impl.implInterfs.map(intfc => new FakeVerifiableType(intfc).asInstanceOf[VerifiableType]).iterator

  override def getHostClass: VerifiableType = null

  override def getSuperClass = new FakeVerifiableType(asClassType(impl.getSuperClassSig)(typeProvider = new FakeEnvironment).asInstanceOf[FakeType])

  override def getClassConstantPool: ConstantPool = impl.getClassConstantPool

  override def getClassInheritanceLevel: Int = impl.getClassInheritanceLevel

  override def getVerificationInfo: AbstractVerifier.Info = impl.getVerificationInfo

  override def hasVerificationInfo = impl.hasVerificationInfo

  override def getXName = impl.getXName

  override def resolveTypeByName(tp: TypeProvider, name: XString) = new FakeVerifiableType(tp.resolveTypeByName(impl, name).asInstanceOf[FakeType])

  override def addVerificationPair(builder: AbstractVerifier.InfoBuilder, to: VerifiableType, context: VerifiableMethod): Unit = {
    builder.addVerificationPair(impl, to.asInstanceOf[FakeVerifiableType].impl, context)
  }

  override def addVerificationImport(importedType: VerifiableType): Unit = importedType match {
    case importedType: FakeVerifiableType => verificationImports += importedType.impl
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: FakeVerifiableType => impl.equals(that.impl)
    case _ => false
  }

  override def hashCode: Int = impl.hashCode
}