package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.toSeq
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRItemProvider, CustomTypeDefImpl, HasAnnotationsImpl, HasAttributesImpl}

abstract class CustomTypeDefImpl(d: CustomTypeDef)(using provider: CHIRItemProvider) extends CHIR.CustomTypeDef
  with HasAnnotationsImpl(d.base) with HasAttributesImpl(d.base.attributes) {
  
  override def tpe: CHIR.CustomType = provider.getType[CHIR.CustomType](d.`type`).get
  override def packageName: String = d.packageName
  override def identifier: String = d.identifier
  override def srcCodeIdentifier: String = d.srcCodeIdentifier
  override def instanceVars: Seq[CHIR.InstanceVar] = {
    for (m <- d.instanceMemberVarsVector.toSeq) yield {
      InstanceVarImpl(m)
    }
  }
  override def staticVars: Seq[CHIR.GlobalVar] = {
    for (idx <- d.staticMemberVarsVector.toSeq) yield {
      provider.getValue[CHIR.GlobalVar](idx).get
    }
  }
  override def methods: Seq[CHIR.Func] = {
    for (idx <- d.methodsVector.toSeq) yield {
      provider.getValue[CHIR.Func](idx).get
    }
  }
  override lazy val vTables: Seq[CHIR.VTable] = {
    for (v <- d.vtableVector.toSeq) yield {
      VTableImpl(v)
    }
  }
  override def implementedInterfaces: Seq[CHIR.ClassType] = {
    for (idx <- d.implementedInterfacesVector.toSeq) yield {
      provider.getType[CHIR.ClassType](idx).get
    }
  }
}

final class EnumDefImpl(e: EnumDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.EnumDef {
  override def tpe: CHIR.EnumType = super.tpe.asInstanceOf[CHIR.EnumType]
  override def nonExhaustive: Boolean = e.nonExhaustive
  override def ctors: Seq[CHIR.EnumCtor] = {
    for (idx <- e.ctorsVector.toSeq) yield {
      EnumCtorImpl(idx)
    }
  } 
}

final class ClassDefImpl(c: ClassDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(c.base) with CHIR.ClassDef {
  override def tpe: CHIR.ClassType = super.tpe.asInstanceOf[CHIR.ClassType]
  override def isClass: Boolean = c.isClass
  override def superClass: Option[CHIR.ClassType] = provider.getType[CHIR.ClassType](c.superClass) 
}

final class StructDefImpl(s: StructDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(s.base) with CHIR.StructDef {
  override def tpe: CHIR.StructType = super.tpe.asInstanceOf[CHIR.StructType]
}

final class ExtendDefImpl(e: ExtendDef)(using provider: CHIRItemProvider) extends CustomTypeDefImpl(e.base) with CHIR.ExtendDef {
  override def genericTypeParams: Seq[CHIR.GenericType] = {
    for (idx <- e.genericParamsVector.toSeq) yield {
      provider.getType[CHIR.GenericType](idx).get
    }
  }
}