package com.huawei.excelsior.jet.compiler.chir.v13002

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.v13002.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v13002.CHIRUtils.{toSeq, toTypeSeq}

trait HasAnnotationsImpl extends HasAnnotations {
  def base: Base
  implicit def provider: CHIRItemProvider
  
  lazy val annotations: Seq[CHIR.Annotation] = {
    val annos = base.annosVector
    (0 until annos.length).collect {
      case i if base.annosType(i) != Annotation.NONE =>
        val obj = base.annosType(i) match {
          case Annotation.needCheckArrayBound => new NeedCheckArrayBound
          case Annotation.needCheckCast => new NeedCheckCast
          case Annotation.debugLocationInfoForWarning => new DebugLocation
          case Annotation.generatedFromForIn => new GeneratedFromForIn
          case Annotation.isAutoEnvClass => new IsAutoEnvClass
          case Annotation.isCapturedClassInCC => new IsCapturedClassInCC
          case Annotation.linkTypeInfo => new LinkTypeInfo
          case Annotation.skipCheck => new SkipCheck
          case Annotation.neverOverflowInfo => new NeverOverflowInfo
          case Annotation.enumCaseIndex => new EnumCaseIndex
          case Annotation.virMethodOffset => new VirMethodOffset
          case Annotation.wrappedRawMethod => new WrappedRawMethod
          case Annotation.overrideSrcFuncType => new OverrideSrcFuncType
        }

        annos.get(obj, i) match {
          case a: IsAutoEnvClass => IsAutoEnvClassImpl(a)
          case a: OverrideSrcFuncType => OverrideSrcFuncTypeImpl(a)
          case a: WrappedRawMethod => WrappedRawMethodImpl(a)
          case other => null // Fallback for unhandled objects (skips them safely)
        }
    }.filterNot(_ == null)
  }
}

final class IsAutoEnvClassImpl(i: IsAutoEnvClass) extends CHIR.IsAutoEnvClass {
  def value: Boolean = i.value
}

final class OverrideSrcFuncTypeImpl(o: OverrideSrcFuncType)(using provider: CHIRItemProvider) extends CHIR.OverrideSrcFuncType {
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](o.`type`).get
}

final class WrappedRawMethodImpl(w: WrappedRawMethod)(using provider: CHIRItemProvider) extends CHIR.WrappedRawMethod {
  def rawMethod: CHIR.Func = provider.getValue[CHIR.Func](w.rawMethod).get
}

trait HasAttributesImpl extends HasAttributes {
  def attrs: Long

  lazy val attributes: Seq[CHIR.Attribute] = {
    CHIR.Attribute.values.toIndexedSeq.filter { attr =>
      (attrs & (1L << attr.ordinal)) != 0L
    }
  }
}

trait HasDeclaringDefImpl extends HasDeclaringDef {
  def gv: GlobalValue
  implicit def provider: CHIRItemProvider

  def declaringDef: Option[CHIR.CustomTypeDef] = provider.getDef[CHIR.CustomTypeDef](gv.declaredParent)
}

final class InstanceVarImpl(m: MemberVarInfo)(using provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl {
  val attrs: Long = m.attributes
  def tpe: CHIR.Type = provider.getType[CHIR.Type](m.`type`).get
  def name: String = m.name
}

final class VTableImpl(v: VTableInType)(using provider: CHIRItemProvider) extends CHIR.VTable {
  def srcParentType: CHIR.ClassType = provider.getType[CHIR.ClassType](v.srcParentType).get
  def vMethods: Seq[CHIR.VMethod] = v.virtualMethodsVector.toSeq
}

final class VMethodImpl(v: VirtualMethodInfo)(using provider: CHIRItemProvider) extends CHIR.VMethod with HasAttributesImpl {
  val attrs: Long = v.attributes
  def name: String = v.funcName
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](v.sigType).get
  def instance: CHIR.Func = provider.getValue[CHIR.Func](v.instance).get
  def genericTypeParams = v.methodGenericTypeParamsVector.toTypeSeq[CHIR.Type]
  def originalType: CHIR.FuncType = provider.getType[CHIR.FuncType](v.originalType).get
  def parentType: CHIR.Type = provider.getType[CHIR.Type](v.parentType).get
  def returnType: CHIR.Type = provider.getType[CHIR.Type](v.returnType).get
}

final class EnumCtorImpl(e: EnumCtorInfo)(using provider: CHIRItemProvider) extends CHIR.EnumCtor {
  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](e.funcType).get
}
