/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType as RefClassType, InterfaceType as RefInterfaceType}
import xscala.io.Path

trait CHIRSymLevelBuilder {
  def env: Environment

  def build(): Unit

  // TODO-MODIFIERS: refactor this
  def addPackage(name: String): ClassType

  def addRecord(pkg: Type, name: String, genericInfo: GenericInfo): ClassType

  def addClass(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, isCangjieLambda: Boolean, genericInfo: GenericInfo): ClassType

  def setSuperclass(clazz: ClassType, superclass: RefClassType): Unit
  def setSuperinterfaces(clazz: ClassType, superinterfaces: Array[RefInterfaceType]): Unit
  
  def setVTable(clazz: ClassType, vtable: CHIRVTable): Unit

  def addField(clazz: ClassType, name: String, sig: SignatureType, exportedName: String, modifiers: Int): Field
  def addMethod(clazz: ClassType, name: String, sig: MethodSignature, exportedName: String, modifiers: Int, genericInfo: GenericInfo,
                hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean, isCFunc: Boolean,
                hasOuterTypeInfo: Boolean, genericFuncParamsCount: Int, isMutWrapper: Boolean): Method

  def markAsConstructor(method: Method): Unit
  def markAsPackageInit(method: Method): Unit
  def markAsPackageLiteralInit(method: Method): Unit

  def markAsCHIRDef(clazz: ClassType): Unit
  def markAsCHIRDef(field: Field, id: Int): Unit
  def markAsCHIRDef(method: Method, id: Int): Unit

  def addInterface(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, genericInfo: GenericInfo): ClassType

}
