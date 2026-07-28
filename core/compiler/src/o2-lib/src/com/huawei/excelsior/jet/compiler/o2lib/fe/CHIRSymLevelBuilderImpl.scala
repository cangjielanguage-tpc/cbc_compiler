/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.Language
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRSymLevelBuilder, CHIRVTable, CangjieSymLevelMaker}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.{Env, TypeProvider}
import xscala.io.stderr
import xscala.util.Set32

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// TODO-MODIFIERS: refactor this
class CHIRSymLevelBuilderImpl extends CHIRSymLevelBuilder {

  var srcFD: xiFilesModule.FileDescriptor = _

  private val classes = ArrayBuffer.empty[pcOModule.Class]

  override def env: LightweightEnvironment = LightweightEnvironment.getInstance

  private implicit val typeProvider: TypeProvider = env

  override def build(): Unit = {
    pcOModule.withSymCacheGCProhibited {
      // We must delay symcache GC because it might be triggered in the
      // middle of symlevel writing process, when not all classes are written to disk.
      // This will invalidate references to already dumped members, which can still
      // be used in in-memory classes (e.g. in replacement FEXT).

      for (c <- classes) {
        SymLevelBuilderModule.preprocessClass(c)
      }
      for (c <- classes) {
        SymLevelBuilderModule.processClass(c)
      }
    }
  }

  private def newClass(name: XString, modifiers: Modifiers, isCangjie: Boolean, isCangjieLambdaClass: Boolean = false): pcOModule.Class = {
    val clazz = SymLevelBuilderModule.newClass(pcNamesModule.newClassName(name), modifiers, srcFD)
    this.classes += clazz
    if (isCangjie) {
      clazz.markAsCangjieType()

      if (isCangjieLambdaClass) {
        clazz.markAsCangjieLambdaBaseClass()
        clazz.markAsEvacuatedType()
      }
    } else {
      clazz.markAsJavaAnnotatedCangjieClass()
    }
    if (!languagePack.supports(Language.JAVA)) {
      clazz.markAsNoJavaClass()
    }
    clazz
  }

  private def addClass0(pkg: Type, name: XString, modifiers: Modifiers, isCangjie: Boolean, isCangjieLambdaClass: Boolean = false,
                       genericInfo: GenericInfo): pcOModule.Class = {
    val clazz = newClass(name, modifiers, isCangjie, isCangjieLambdaClass)
    if (isCangjie) {
      clazz.cangjiePackage = typeToO2Class(pkg)
    }

    if (genericInfo != GenericInfo.none) {
      clazz.markAsUniversalGeneric()
      clazz.addGenericInfo(genericInfo)
    }

    clazz
  }

  override def addPackage(name: String): ClassType = {
    // TODO: support updating package modifiers if needed
    val p = newClass(XString(name), Modifiers(PUBLIC), isCangjie = true)
    p.cangjiePackage = p
    classByO2Object(p) ensuring (findClass(name) == _)
  }

  private def findClass(name: String): Type = {
    env.getTypeProvider.findClass(XString(name), loadPDB = true)
  }

  override def addRecord(pkg: Type, name: String, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      return asClassType(alreadyAdded)
    }

    val record = addClass0(pkg, xname, Modifiers(PUBLIC), isCangjie = true, genericInfo = genericInfo)
    record.markAsRecord()

    classByO2Object(record) ensuring (findClass(name) == _)
  }

  override def addClass(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, isCangjieLambdaClass: Boolean, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      // There could be identical classes defined in different packages, it's OK.
      return asClassType(alreadyAdded)
    }

    classByO2Object(addClass0(pkg, xname, Modifiers(modifiers), isCangjie, isCangjieLambdaClass, genericInfo)) ensuring (findClass(name) == _)
  }

  override def setSuperclass(clazz: ClassType, superclass: RefClassType): Unit = {
    assert(superclass != null)
    typeToO2Class(clazz).setSuperClass(superclass)
  }

  override def setSuperinterfaces(clazz: ClassType, superinterfaces: Array[RefInterfaceType]): Unit = {
    assert(superinterfaces != null)
    typeToO2Class(clazz).setSuperInterfaces(superinterfaces)
  }

  override def setVTable(clazz: ClassType, vtable: CHIRVTable): Unit = {
    typeToO2Class(clazz).setCHIRVTable(vtable)
  }

  override def addField(clazz: ClassType, name: String, sig: SignatureType, exportedName: String, modifiers: Int): Field = {
    val dup = clazz.findDeclaredFieldOrNull(XString(name), sig)
    if (dup != null) {
      // TODO: checks?
      dup

    } else {
      val f = SymLevelBuilderModule.addField(typeToO2Class(clazz), XString(name), sig, Set32(modifiers))
      if (sig.isRecord) {
        f.markAsAJFlat()
      }

      if (exportedName != null) {
        f.markAsExported(XString(exportedName))
      }
      
      fieldByO2Object(f)
    }
  }

  override def addMethod(clazz: ClassType, name: String, sig: MethodSignature, exportedName: String, modifiers: Int, genericInfo: GenericInfo,
                         hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean, isCFunc: Boolean,
                         hasOuterTypeInfo: Boolean, genericFuncParamsCount: Int, isMutWrapper: Boolean) = {
    val dup = clazz.findDeclaredMethodOrNull(XString(name), sig)
    if (dup != null) {
      // TODO: checks?
      dup

    } else {
      val m = SymLevelBuilderModule.addMethod(typeToO2Class(clazz), XString(name), sig, Set32(modifiers),
        hasUGDesc, hasThisTypeInfoParam, isCFunc, hasOuterTypeInfo, genericFuncParamsCount, isMutWrapper)

      if (genericInfo != GenericInfo.none) {
        m.markAsUniversalGeneric()
        m.addGenericInfo(genericInfo)
      }

      if (exportedName != null) {
        m.markAsExported(XString(exportedName))
      }

      methodByO2Object(m)
    }
  }

  override def markAsConstructor(method: Method): Unit = {
    val m = getO2Method(method)
    m.markAsConstructor()
    if (m.getDeclaringClass.isRecord) {
      m.markAsRecordConstructor()
    }
  }

  override def markAsPackageInit(method: Method): Unit = {
    getO2Method(method).markAsPackageInit()
  }

  override def markAsPackageLiteralInit(method: Method): Unit = {
    getO2Method(method).markAsPackageLiteralInit()
  }

  override def addInterface(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded == null) {
      classByO2Object(addClass0(pkg, xname, Modifiers(modifiers) + INTERFACE, isCangjie, genericInfo = genericInfo)) ensuring (findClass(name) == _)
    } else {
      // There could be identical classes defined in different packages, it's OK.
      asClassType(alreadyAdded)
    }
  }

  override def markAsCHIRDef(clazz: ClassType): Unit = {
    typeToO2Class(clazz).markAsCHIRDef()
  }

  override def markAsCHIRDef(field: Field, id: Int): Unit = {
    fieldToO2Field(field).addCHIRDef(srcFD.getName, id)
  }

  override def markAsCHIRDef(method: Method, id: Int): Unit = {
    getO2Method(method).addCHIRDef(srcFD.getName, id)
  }

}
