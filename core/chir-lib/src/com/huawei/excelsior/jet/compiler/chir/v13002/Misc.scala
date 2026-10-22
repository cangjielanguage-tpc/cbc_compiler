package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v13002.CHIRUtils.toTypeSeq

trait HasAnnotationsImpl extends HasAnnotations {
  def base: BaseT
  implicit def provider: CHIRItemProvider
  def annotations: Seq[CHIR.Annotation] =
    Option(base).toSeq.flatMap(b => Option(b.getAnnos).toSeq.flatten).flatMap { a =>
      a.getValue match {
        case x: IsAutoEnvClassT => Some(IsAutoEnvClassImpl(x))
        case x: OverrideSrcFuncTypeT => Some(OverrideSrcFuncTypeImpl(x))
        case x: WrappedRawMethodT => Some(WrappedRawMethodImpl(x))
        case _ => None
      }
    }
}

final class IsAutoEnvClassImpl(i: IsAutoEnvClassT) extends CHIR.IsAutoEnvClass {
  def value: Boolean = i.getValue
}

final class OverrideSrcFuncTypeImpl(o: OverrideSrcFuncTypeT)(using provider: CHIRItemProvider) extends CHIR.OverrideSrcFuncType {
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](o.getType).get
}

final class WrappedRawMethodImpl(w: WrappedRawMethodT)(using provider: CHIRItemProvider) extends CHIR.WrappedRawMethod {
  def rawMethod: CHIR.Func = provider.getValue[CHIR.Func](w.getRawMethod).get
}

trait HasAttributesImpl extends HasAttributes {
  def attrs: Long
  def attributes: Seq[CHIR.Attribute] = {
    CHIR.Attribute.values.toIndexedSeq.filter { attr =>
      (attrs & (1L << attr.ordinal)) != 0L
    }
  }
}

trait HasDeclaringDefImpl extends HasDeclaringDef {
  def gv: GlobalValueT
  implicit def provider: CHIRItemProvider
  def declaringDef: Option[CHIR.CustomTypeDef] = provider.getDef[CHIR.CustomTypeDef](gv.getDeclaredParent)
}

final class InstanceVarImpl(m: MemberVarInfoT)(using provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl {
  def attrs: Long = m.getAttributes
  def tpe: CHIR.Type = provider.getType[CHIR.Type](m.getType).get
  def name: String = m.getName
}

final class VTableImpl(v: VTableInTypeT)(using provider: CHIRItemProvider) extends CHIR.VTable {
  def srcParentType: CHIR.ClassType = provider.getType[CHIR.ClassType](v.getSrcParentType).get
  def vMethods: Seq[CHIR.VMethod] = Option(v.getVirtualMethods).toSeq.flatten.map(VMethodImpl(_))
}

final class VMethodImpl(v: VirtualMethodInfoT)(using provider: CHIRItemProvider) extends CHIR.VMethod with HasAttributesImpl {
  def attrs: Long = v.getAttributes
  def name: String = v.getFuncName
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](v.getSigType).get
  def instance: CHIR.Func = provider.getValue[CHIR.Func](v.getInstance).get
  def genericTypeParams = v.getMethodGenericTypeParams.toTypeSeq[CHIR.Type]
  def originalType: CHIR.FuncType = provider.getType[CHIR.FuncType](v.getOriginalType).get
  def parentType: CHIR.Type = provider.getType[CHIR.Type](v.getParentType).get
  def returnType: CHIR.Type = provider.getType[CHIR.Type](v.getReturnType).get
}

final class EnumCtorImpl(e: EnumCtorInfoT)(using provider: CHIRItemProvider) extends CHIR.EnumCtor {
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](e.getFuncType).get
}
