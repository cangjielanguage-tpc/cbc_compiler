/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field, GenericInfo, Method, MethodSignature, SignatureType, Type}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import xscala.io.Path

trait SymLevelBuilder {
  def env: Environment

  def getSource: String
  def getSourceForSymlevel: String = Path(getSource).name

  def build(): Type

  // TODO-MODIFIERS: refactor this
  def addPackage(name: String, modifiers: Int): ClassType
  def addBitcodeDeferredPackage(name: String): ClassType
  def addPackageField(name: String, sig: SignatureType, modifiers: Int): Field
  def addPackageMethod(name: String, sig: MethodSignature, exportedName: String, llvmIdx: Int, modifiers: Int, genericInfo: GenericInfo, hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean, isCFunc: Boolean): Method
  def setStaticFieldConstValue(f: Field, initValue: Long): Unit
  def addPackageInit(name: String, sig: MethodSignature, llvmIdx: Int): Method
  def addGlobalInit(name: String, sig: MethodSignature, llvmIdx: Int): Method
  def addIntrinsicMethod(name: String, sig: MethodSignature, llvmIdx: Int, shouldBeGenerated: Boolean): Method
  def addExternalCMethod(name: String, sig: MethodSignature, vararg: Boolean, llvmIdx: Int): Method
  def addCMethod(name: String, sig: MethodSignature, vararg: Boolean, llvmIdx: Int): Method

  def addRecord(pkg: Type, name: String, genericInfo: GenericInfo): ClassType

  def addArraySlice(pkg: Type, elemTypeOpt: Option[SignatureType]): ClassType

  def addRawArray(pkg: Type, elemType: SignatureType): ClassType

  def addBox(pkg: Type, baseType: SignatureType): ClassType

  def addClass(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, isCangjieLambda: Boolean, genericInfo: GenericInfo): ClassType
  def addBitcodeDeferredType(pkg: Type, name: String, isCangjie: Boolean, isRecord: Boolean, isInterface: Boolean, genericInfo: GenericInfo): ClassType

  /** Returns true if this class is a fresh one, false if this class is a duplicate. */
  def startClassFilling(clazz: ClassType, superClass: RefClassType, superinterfaces: Array[RefInterfaceType]): Boolean
  def startSyntheticClassFilling(clazz: ClassType): Boolean
  def addClassField(name: String, sig: SignatureType, modifiers: Int): Field
  def addClassMethod(name: String, sig: MethodSignature, exportedName: String, llvmIdx: Int, modifiers: Int, genericInfo: GenericInfo, hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean): Method

  def addInterface(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, genericInfo: GenericInfo): ClassType

  /** Returns true if this class is a fresh one, false if this class is a duplicate. */
  def startInterfaceFilling(iface: ClassType, superinterfaces: Array[RefInterfaceType]): Boolean

  def addImport(importer: Type, importee: Type): Unit

  def addCJAnnotationFactoryForClass(clazz: ClassType, factory: Method): Unit
  def addCJAnnotationFactoryForMethod(method: Method, factory: Method): Unit
  def addCJAnnotationFactoryForField(field: Field, factory: Method): Unit
  def addCJAnnotationFactoriesForParameters(method: Method, factories: Array[Method]): Unit

  def addVArray(pkg: Type, name: String, elemType: SignatureType): ClassType
}
