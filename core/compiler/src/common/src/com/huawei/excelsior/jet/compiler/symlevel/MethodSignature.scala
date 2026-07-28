/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

case class MethodSignature(returnType: SignatureType, parameterTypes: Seq[SignatureType]) extends Signature {
  def toJETSignature: String = s"(${parameterTypes.map(_.toJETSignature).mkString("_")})${returnType.toJETSignature}"
}

object MethodSignature {
  def apply(parameterTypes: SignatureType*)(returnType: SignatureType) = new MethodSignature(returnType, parameterTypes)
  
  def equalInstantiatedLegacy(instantiatedTypeParameters: Seq[SignatureType])(x: MethodSignature, y: MethodSignature): Boolean = {
    SignatureType.equalInstantiatedLegacy(instantiatedTypeParameters)(x.returnType, y.returnType) &&
      (x.parameterTypes zip y.parameterTypes forall SignatureType.equalInstantiatedLegacy(instantiatedTypeParameters))
  }

  def equalInstantiated(cparams: Seq[SignatureType], lparams: Seq[SignatureType])(x: MethodSignature, y: MethodSignature): Boolean = {
    SignatureType.equalInstantiated(cparams, lparams)(x.returnType, y.returnType) &&
      x.parameterTypes.size == y.parameterTypes.size &&
      (x.parameterTypes zip y.parameterTypes forall SignatureType.equalInstantiated(cparams, lparams))
  }

  val equalExact: (MethodSignature, MethodSignature) => Boolean = _ == _
}
