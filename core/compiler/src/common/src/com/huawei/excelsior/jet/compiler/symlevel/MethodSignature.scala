/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import scala.collection.mutable

case class MethodSignature(returnType: SignatureType, parameterTypes: Seq[SignatureType]) extends Signature {
  def toJETSignature: String = s"(${parameterTypes.map(_.toJETSignature).mkString("_")})${returnType.toJETSignature}"

  def instantiate(cparams: Seq[SignatureType], lparams: Seq[SignatureType]): MethodSignature = {
    if (cparams.nonEmpty || lparams.nonEmpty) instantiateImpl(cparams, lparams) else this
  }

  private[symlevel] def instantiateImpl(cparams: Seq[SignatureType], lparams: Seq[SignatureType]): MethodSignature = {
    def boxTypeVar(g: SignatureType, i: SignatureType): SignatureType = {
      if (g.isTypeVariable && !i.isTypeVariable && (!i.isReference || i.isInstanceOf[SignatureType.OptionLikeEnum])) {
        SignatureType.Box(i)
      } else {
        i
      }
    }

    MethodSignature(boxTypeVar(returnType, returnType.instantiateImpl(cparams, lparams)), parameterTypes.map(t => boxTypeVar(t, t.instantiateImpl(cparams, lparams))))
  }

  /** Matches this signature against the given instantiated one and returns the collected bindings
    * of its type variables, or None if the signatures do not match. Bindings are shared between
    * the return type and the parameter types, so a repeated variable must be bound consistently
    * in all of them. See SignatureType.matchInstantiatedImpl.
    */
  def matchInstantiatedImpl(instantiated: MethodSignature): Option[collection.Map[SignatureType.TypeVariable, SignatureType]] = {
    if (parameterTypes.size != instantiated.parameterTypes.size) {
      None
    } else {
      val bindings = mutable.Map[SignatureType.TypeVariable, SignatureType]()
      val matched = SignatureType.matchInstantiated(returnType, instantiated.returnType, bindings) &&
        (parameterTypes zip instantiated.parameterTypes).forall(SignatureType.matchInstantiated(_, _, bindings))
      Option.when(matched)(bindings)
    }
  }

  /** Returns the substitution (cparams, lparams) with `instantiate(cparams, lparams)` equal to the
    * given instantiated signature, or None if it does not match. See SignatureType.matchInstantiated.
    */
  def matchInstantiated(instantiated: MethodSignature): Option[(Seq[SignatureType], Seq[SignatureType])] =
    matchInstantiatedImpl(instantiated).map(SignatureType.instantiationParams)
}

object MethodSignature {
  def apply(parameterTypes: SignatureType*)(returnType: SignatureType) = new MethodSignature(returnType, parameterTypes)

  val equalExact: (MethodSignature, MethodSignature) => Boolean = _ == _
}
