package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{ClassDef, CustomTypeDef, EnumDef, ExtendDef, StructDef}

abstract class CustomTypeDefImpl(d: CustomTypeDef)(using provider: CHIRItemProvider) extends CHIR.CustomTypeDef
  with HasAnnotationsImpl(d.base) with HasAttributesImpl(d.base.attributes) {

  override def packageName(): String = d.packageName
  override def identifier(): String = d.identifier
  override def srcCodeIdentifier(): String = d.srcCodeIdentifier
  override def instanceVars: Seq[CHIR.InstanceVar] = {
    for (idx <- 0 until d.instanceMemberVarsLength) yield {
      InstanceVarImpl(d.instanceMemberVars(idx))
    }
  }
  override def staticVars: Seq[CHIR.GlobalVar] = {
    for (idx <- 1 to d.staticMemberVarsLength) yield {
      provider.getValue[CHIR.GlobalVar](idx).get
    }
  }
  override def methods(): Seq[CHIR.Func] = ???
  override def vTables(): Seq[CHIR.VTable] = ???
  override def implementedInterfaces(): Seq[CHIR.ClassType] = ???
}

final class EnumDefImpl(e: EnumDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.EnumDef {
  override def tpe(): CHIR.EnumType = ???
  override def nonExhaustive(): Boolean = e.nonExhaustive()
  override def ctors(): Seq[CHIR.EnumCtor] = ???
}

final class ClassDefImpl(c: ClassDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(c.base) with CHIR.ClassDef {
  override def tpe(): CHIR.ClassType = ???
  override def isClass(): Boolean = ???
  override def superClass(): Option[CHIR.ClassType] = ???
}

final class StructDefImpl(s: StructDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(s.base) with CHIR.StructDef {
  override def tpe(): CHIR.StructType = ???
}

final class ExtendDefImpl(e: ExtendDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.ExtendDef {
  override def tpe(): CHIR.CustomType = ???
  override def genericTypeParams(): Seq[CHIR.GenericType] = ???
}