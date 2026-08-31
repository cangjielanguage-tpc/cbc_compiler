package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{HasAnnotations, HasAttributes, HasDeclaringDef}
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{Base, GlobalValue}

trait HasAnnotationsImpl(b: Base) extends HasAnnotations {
  override lazy val annotations: Seq[CHIR.Annotation] = ???
}

trait HasAttributesImpl(attrs: Long) extends HasAttributes {
  override lazy val attributes: Seq[CHIR.Attribute] = ???
}

trait HasDeclaringDefImpl(gv: GlobalValue) extends HasDeclaringDef {
  override lazy val declaringDef: Option[CHIR.CustomTypeDef] = ???
}