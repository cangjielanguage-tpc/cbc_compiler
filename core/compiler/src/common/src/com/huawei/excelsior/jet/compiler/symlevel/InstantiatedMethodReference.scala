/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.compiler.types.CompiledType
import xscala.util.hash

/** A reference to instantiated method with universal generic context.
  *
  * Param [[instantiatedTypeParameters]] contains instantiations of type parameters of enclosing class followed by
  * instantiations of type parameters of the method itself.
  *
  * @author arab
  */
final class InstantiatedMethodReference(_methodType: MethodType,
                                        _accessKind: MethodReferenceAccessKind,
                                        _method: Method,
                                        _permanentMethod: PermanentMember = null,
                                        _refClass: CompiledType,
                                        val instantiatedTypeParameters: Seq[SignatureType],
                                        explicitVNum: Option[Int])
  extends MethodReference(_methodType, _accessKind, _method, _permanentMethod, _refClass, explicitVNum) with UniversalGenericMethodReference {

  def this(method: Method, accessKind: MethodReferenceAccessKind,
           instantiatedTypeParameters: Seq[SignatureType], refType: SignatureType, explicitVNum: Option[Int]) = {
    this(method.getMethodType, accessKind, method.ensuring(_.hasUniversalGenericContext), null, CompiledType(refType), instantiatedTypeParameters, explicitVNum)
  }

  override def toString: String = {
    super.toString + s" with UniversalGenericContext(refType: $refType, instantiatedTypeParameters: $instantiatedTypeParameters)"
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: InstantiatedMethodReference => super.equals(that) &&
      this.instantiatedTypeParameters == that.instantiatedTypeParameters &&
      this.refType == that.refType
    case _ => false
  }

  override def hashCode: Int = hash(super.hashCode, refType, instantiatedTypeParameters)

  override protected def copy(methodType: MethodType, method: Method, permanentMethod: PermanentMember, refClass: CompiledType, accessKind: MethodReferenceAccessKind, explicitVNum: Option[Int]): InstantiatedMethodReference =
    new InstantiatedMethodReference(methodType, accessKind, method, permanentMethod, refClass, instantiatedTypeParameters, explicitVNum)
}
