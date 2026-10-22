package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.*
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v13002.CHIRUtils.{toTypeSeq, toValueSeq}

abstract class CustomTypeDefImpl(d: CustomTypeDefT)(using val provider: CHIRItemProvider) extends CHIR.CustomTypeDef
  with HasAnnotationsImpl with HasAttributesImpl {
  def base: BaseT = d.getBase
  def attrs: Long = base.getAttributes
  def packageName: String = d.getPackageName
  def identifier: String = d.getIdentifier
  def srcCodeIdentifier: String = d.getSrcCodeIdentifier
  def instanceVars: Seq[CHIR.InstanceVar] = Option(d.getInstanceMemberVars).toSeq.flatten.map(InstanceVarImpl(_))
  def staticVars = d.getStaticMemberVars.toValueSeq[CHIR.GlobalVar]
  def methods = d.getMethods.toValueSeq[CHIR.Func]
  def vtables: Seq[CHIR.VTable] = Option(d.getVtable).toSeq.flatten.map(VTableImpl(_))
  def implementedInterfaces = d.getImplementedInterfaces.toTypeSeq[CHIR.ClassType]
}

final class EnumDefImpl(e: EnumDefT)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.getBase) with CHIR.EnumDef {
  def tpe: CHIR.EnumType = provider.getType[CHIR.EnumType](e.getBase.getType).get
  def nonExhaustive: Boolean = e.getNonExhaustive
  def ctors: Seq[CHIR.EnumCtor] = Option(e.getCtors).toSeq.flatten.map(EnumCtorImpl(_))
}

final class ClassDefImpl(c: ClassDefT)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(c.getBase) with CHIR.ClassDef {
  def tpe: CHIR.ClassType = provider.getType[CHIR.ClassType](c.getBase.getType).get
  def isClass: Boolean = c.getIsClass
  def superClass: Option[CHIR.ClassType] = provider.getType[CHIR.ClassType](c.getSuperClass)
}

final class StructDefImpl(s: StructDefT)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(s.getBase) with CHIR.StructDef {
  def tpe: CHIR.StructType = provider.getType[CHIR.StructType](s.getBase.getType).get
}

final class ExtendDefImpl(e: ExtendDefT)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.getBase) with CHIR.ExtendDef {
  def tpe: CHIR.Type = provider.getType[CHIR.Type](e.getExtendedType).get
  def genericTypeParams = e.getGenericParams.toTypeSeq[CHIR.GenericType]
}
