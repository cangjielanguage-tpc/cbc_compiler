package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.{notImplemented, toSeq}
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRItemProvider, HasAttributesImpl}

trait HasAnnotationsImpl(b: Base)(using provider: CHIRItemProvider) extends HasAnnotations {
  override lazy val annotations: Seq[CHIR.Annotation] = {
    val annos = b.annosVector
    for (i <- 0 to annos.length) yield {
      val obj = b.annosType(i) match {
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
        case _ => notImplemented("CHIR annotation mapping", (obj, i))
      }
    }
  }
}

final class IsAutoEnvClassImpl(i: IsAutoEnvClass) extends CHIR.IsAutoEnvClass {
  override def value: Boolean = i.value
}

final class OverrideSrcFuncTypeImpl(o: OverrideSrcFuncType)(using provider: CHIRItemProvider) extends CHIR.OverrideSrcFuncType {
  override def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](o.`type`).get
}

final class WrappedRawMethodImpl(w: WrappedRawMethod)(using provider: CHIRItemProvider) extends CHIR.WrappedRawMethod {
  override def rawMethod: CHIR.Func = provider.getValue[CHIR.Func](w.rawMethod).get
}

trait HasAttributesImpl(attrs: Long) extends HasAttributes {
  override def attributes: Seq[CHIR.Attribute] = {
    CHIR.Attribute.values.toIndexedSeq.filter { attr =>
      (attrs & (1L << attr.ordinal)) != 0L
    }
  }
}

trait HasDeclaringDefImpl(gv: GlobalValue)(using provider: CHIRItemProvider) extends HasDeclaringDef {
  override def declaringDef: Option[CHIR.CustomTypeDef] = provider.getDef[CHIR.CustomTypeDef](gv.declaredParent)
}

final class InstanceVarImpl(m: MemberVarInfo)(using provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl(m.attributes) {
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](m.`type`).get
  override def name: String = m.name
}

final class VTableImpl(v: VTableInType)(using provider: CHIRItemProvider) extends CHIR.VTable {
  override def srcParentType: CHIR.ClassType = provider.getType[CHIR.ClassType](v.srcParentType).get
  override def vMethods: Seq[CHIR.VMethod] = {
    for (idx <- v.virtualMethodsVector.toSeq) yield {
      VMethodImpl(idx)
    }
  }
}

final class VMethodImpl(v: VirtualMethodInfo)(using provider: CHIRItemProvider) extends CHIR.VMethod with HasAttributesImpl(v.attributes) {
  override def name: String = v.funcName
  override def sig: CHIR.FuncType = provider.getType[CHIR.FuncType](v.sigType).get
  override def instance: CHIR.Func = provider.getValue[CHIR.Func](v.instance).get
  override def genericTypeParams: Seq[CHIR.Type] = {
    for (idx <- v.methodGenericTypeParamsVector.toSeq) yield {
      provider.getType[CHIR.Type](idx).get
    }
  }
  override def originalType: CHIR.FuncType = provider.getType[CHIR.FuncType](v.originalType).get
  override def parentType: CHIR.Type = provider.getType[CHIR.Type](v.parentType).get
  override def returnType: CHIR.Type = provider.getType[CHIR.Type](v.returnType).get
}

final class EnumCtorImpl(e: EnumCtorInfo)(using provider: CHIRItemProvider) extends CHIR.EnumCtor {
  override def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](e.funcType).get
}