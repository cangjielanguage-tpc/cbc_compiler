package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.toSeq
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRItemProvider, CustomTypeDefImpl, HasAnnotationsImpl, HasAttributesImpl}

abstract class CustomTypeDefImpl(d: CustomTypeDef)(using provider: CHIRItemProvider) extends CHIR.CustomTypeDef
  with HasAnnotationsImpl(d.base) with HasAttributesImpl(d.base.attributes) {
  
  def packageName: String = d.packageName
  def identifier: String = d.identifier
  def srcCodeIdentifier: String = d.srcCodeIdentifier
  def instanceVars: Seq[CHIR.InstanceVar] = {
    for (m <- d.instanceMemberVarsVector.toSeq) yield {
      InstanceVarImpl(m)
    }
  }
  def staticVars: Seq[CHIR.GlobalVar] = {
    for (idx <- d.staticMemberVarsVector.toSeq) yield {
      provider.getValue[CHIR.GlobalVar](idx).get
    }
  }
  def methods: Seq[CHIR.Func] = {
    for (idx <- d.methodsVector.toSeq) yield {
      provider.getValue[CHIR.Func](idx).get
    }
  }
  def vtables: Seq[CHIR.VTable] = {
    for (v <- d.vtableVector.toSeq) yield {
      VTableImpl(v)
    }
  }
  def implementedInterfaces: Seq[CHIR.ClassType] = {
    for (idx <- d.implementedInterfacesVector.toSeq) yield {
      provider.getType[CHIR.ClassType](idx).get
    }
  }
}

final class EnumDefImpl(e: EnumDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.EnumDef {
  def tpe: CHIR.EnumType = provider.getType[CHIR.EnumType](e.base.`type`).get
  def nonExhaustive: Boolean = e.nonExhaustive
  def ctors: Seq[CHIR.EnumCtor] = {
    for (idx <- e.ctorsVector.toSeq) yield {
      EnumCtorImpl(idx)
    }
  } 
}

final class ClassDefImpl(c: ClassDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(c.base) with CHIR.ClassDef {
  def tpe: CHIR.ClassType = provider.getType[CHIR.ClassType](c.base.`type`).get
  def isClass: Boolean = c.isClass
  def superClass: Option[CHIR.ClassType] = provider.getType[CHIR.ClassType](c.superClass)
}

final class StructDefImpl(s: StructDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(s.base) with CHIR.StructDef {
  def tpe: CHIR.StructType = provider.getType[CHIR.StructType](s.base.`type`).get
}

final class ExtendDefImpl(e: ExtendDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.ExtendDef {
  def tpe: CHIR.Type = provider.getType[CHIR.Type](e.extendedType).get
  def genericTypeParams: Seq[CHIR.GenericType] = {
    for (idx <- e.genericParamsVector.toSeq) yield {
      provider.getType[CHIR.GenericType](idx).get
    }
  }
}