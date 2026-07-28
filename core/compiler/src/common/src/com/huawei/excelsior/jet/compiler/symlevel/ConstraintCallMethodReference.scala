/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.SPECIAL
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.LocalTypeVariable
import com.huawei.excelsior.jet.compiler.types.CompiledType
import xscala.util.hash

final class ConstraintCallMethodReference(val sourceSig: MethodSignature, _methodType: MethodType, override val methodName: XString, _refClass: CompiledType)
  // TODO consider making a separate mak 'CONSTRAINT'
  extends MethodReference(_methodType, SPECIAL, null, null, _refClass, None) with UniversalGenericMethodReference {

  def receiverType = _methodType.parameterType(_methodType.getReceiverArgIdx).asInstanceOf[LocalTypeVariable]

  def name = methodName.toString
  
  override def getPermanent = this

  override def hasVirtualMethodSlot = false

  override def virtualMethodSlot = shouldNotCallThis()

  override def isDirectCall = false

  override def toString = {
    s"ConstraintCallMethodReference($accessKind, $methodName)"
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: ConstraintCallMethodReference => super.equals(that) && methodName == that.methodName
    case _ => false
  }

  override def hashCode = hash(super.hashCode, methodName)
}
