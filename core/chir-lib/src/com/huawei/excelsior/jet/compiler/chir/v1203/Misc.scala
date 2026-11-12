package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.toTypeSeq

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

private object AttributeValues {
  val all: Vector[CHIR.Attribute] = CHIR.Attribute.values.toVector
}

trait HasAttributesImpl extends EditableCHIR.HasAttributes {
  protected def setAttributes(mask: Long): Unit
  final def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit =
    setAttributes(attributes.foldLeft(0L)((bits, a) => bits | (1L << a.ordinal)))

  def attrs: Long
  private var cachedMask = 0L
  private var cachedAttributes: Seq[CHIR.Attribute] = Seq.empty
  def attributes: Seq[CHIR.Attribute] = {
    val mask = attrs
    if (mask != cachedMask) {
      cachedAttributes = AttributeValues.all.filter(attr => (mask & (1L << attr.ordinal)) != 0L)
      cachedMask = mask
    }
    cachedAttributes
  }
}

trait HasDeclaringDefImpl extends HasDeclaringDef {
  def gv: GlobalValueT
  implicit def provider: CHIRItemProvider
  def declaringDef: Option[CHIR.CustomTypeDef] = provider.getDef[CHIR.CustomTypeDef](gv.getDeclaredParent)
}

final class InstanceVarImpl private[v1203] (private[v1203] val m: MemberVarInfoT)(using val provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl {
  def attrs: Long = m.getAttributes
  protected def setAttributes(mask: Long): Unit = m.setAttributes(mask)
  def tpe: CHIR.Type = provider.getType[CHIR.Type](m.getType).get
  def name: String = m.getName
}

final class VTableImpl(v: VTableInTypeT)(using provider: CHIRItemProvider) extends CHIR.VTable {
  def srcParentType: CHIR.ClassType = provider.getType[CHIR.ClassType](v.getSrcParentType).get
  def vMethods: Seq[CHIR.VMethod] = Option(v.getVirtualMethods).toSeq.flatten.map(VMethodImpl(_))
}

final class VMethodImpl private[v1203] (private[v1203] val v: VirtualMethodInfoT)(using val provider: CHIRItemProvider) extends CHIR.VMethod with HasAttributesImpl {
  def attrs: Long = v.getAttributes
  protected def setAttributes(mask: Long): Unit = v.setAttributes(mask)
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

final class FuncSigImpl(f: FuncSigInfoT)(using provider: CHIRItemProvider) extends CHIR.FuncSig {
  final def equalTo(that: CHIR.FuncSig): Boolean = that match {
    case _: CHIR.Func | _: CHIR.VMethod => false
    case _ => this.name == that.name && this.tpe == that.tpe && this.genericTypeParams == that.genericTypeParams
  }

  def name: String = f.getFuncName
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](f.getFuncType).get
  def genericTypeParams: Seq[CHIR.Type] = f.getGenericTypeParams.toTypeSeq[CHIR.Type]
}
