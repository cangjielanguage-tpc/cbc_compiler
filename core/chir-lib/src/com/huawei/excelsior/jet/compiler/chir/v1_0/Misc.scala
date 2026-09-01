package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.v1_0.CHIRUtils.notImplemented
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.*

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
  override lazy val tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](o.`type`) 
}

final class WrappedRawMethodImpl(w: WrappedRawMethod)(using provider: CHIRItemProvider) extends CHIR.WrappedRawMethod {
  override lazy val rawMethod: CHIR.Func = provider.getValue[CHIR.Func](w.rawMethod).get
}

trait HasAttributesImpl(attrs: Long) extends HasAttributes {
  override lazy val attributes: Seq[CHIR.Attribute] = {
    CHIR.Attribute.values.toIndexedSeq.filter { attr =>
      (attrs & (1L << attr.ordinal)) != 0L
    }
  }
}

trait HasDeclaringDefImpl(gv: GlobalValue)(using provider: CHIRItemProvider) extends HasDeclaringDef {
  override lazy val declaringDef: Option[CHIR.CustomTypeDef] = provider.getDef[CHIR.CustomTypeDef](gv.declaredParent)
}

final class InstanceVarImpl(m: MemberVarInfo)(using provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl(m.attributes) {
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](m.`type`)
  override def name: String = m.name
}
