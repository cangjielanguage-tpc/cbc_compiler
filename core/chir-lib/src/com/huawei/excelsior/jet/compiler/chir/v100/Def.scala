package com.huawei.excelsior.jet.compiler.chir.v100

import com.huawei.excelsior.jet.compiler.chir.*
import com.huawei.excelsior.jet.compiler.chir.v100.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v100.CHIRUtils.{toSeq, toTypeSeq, toValueSeq}

abstract class CustomTypeDefImpl(d: CustomTypeDef)(using provider: CHIRItemProvider) extends CHIR.CustomTypeDef
  with HasAnnotationsImpl(d.base) with HasAttributesImpl(d.base.attributes) {
  
  def packageName: String = d.packageName
  def identifier: String = d.identifier
  def srcCodeIdentifier: String = d.srcCodeIdentifier
  def instanceVars: Seq[CHIR.InstanceVar] = d.instanceMemberVarsVector.toSeq
  def staticVars = d.staticMemberVarsVector.toValueSeq[CHIR.GlobalVar]
  def methods = d.methodsVector.toValueSeq[CHIR.Func]
  def vtables: Seq[CHIR.VTable] = d.vtableVector.toSeq
  def implementedInterfaces = d.implementedInterfacesVector.toTypeSeq[CHIR.ClassType]
}

final class EnumDefImpl(e: EnumDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.EnumDef {
  def tpe: CHIR.EnumType = provider.getType[CHIR.EnumType](e.base.`type`).get
  def nonExhaustive: Boolean = e.nonExhaustive
  def ctors: Seq[CHIR.EnumCtor] = e.ctorsVector.toSeq
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
  def genericTypeParams = e.genericParamsVector.toTypeSeq[CHIR.GenericType]
}