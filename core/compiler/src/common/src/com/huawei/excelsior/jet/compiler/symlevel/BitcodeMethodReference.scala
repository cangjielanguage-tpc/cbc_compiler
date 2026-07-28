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
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{CONSTRUCTOR_NAME, isArraySliceConstructor}
import com.huawei.excelsior.jet.compiler.types.CompiledType
import xscala.util.hash

/** A method reference based on information from bitcode.
  *
  * See [[MethodReference]] for details.
  *
  * @author arab
  */
final class BitcodeMethodReference(_methodType: MethodType, val sourceMethodType: MethodType,
                                   _accessKind: MethodReferenceAccessKind,
                                   _refClass: CompiledType, override val methodName: XString, val linkageName: Option[XString])
  extends MethodReference(_methodType, _accessKind, null, null, _refClass, None) {

  def this(methodType: MethodType, sourceMethodType: MethodType,
           accessKind: MethodReferenceAccessKind, refClass: CompiledType, methodName: XString) =
    this(methodType, sourceMethodType, accessKind, refClass, methodName, None)

  assert(hasRefClass)
  assert(methodName != null)

  override protected def copy(methodType: MethodType, method: Method, permanentMethod: PermanentMember,
                              refClass: CompiledType, accessKind: MethodReferenceAccessKind, explicitVNum: Option[Int]) = {
    assert(method == null)
    assert(permanentMethod == null)
    assert(explicitVNum.isEmpty)
    new BitcodeMethodReference(methodType, sourceMethodType, accessKind, refClass, methodName, linkageName)
  }

  override def getPermanent = this


  override def hasVirtualMethodSlot = false

  override def virtualMethodSlot = shouldNotCallThis()

  override def isDirectCall = !(isInterfCall || isVirtualCall)

  override def isCangjieMut = BitcodeMethodReference.isCangjieMut(refType, methodName.toString)

  def getMutRecordType: SignatureType = {
    assert(isCangjieMut)
    if (refType.sigType.isRecord) {
      refType.sigType
    } else {
      assert(isArraySliceConstructor(methodName.toString)) // array slice constructor
      methodType.signature.parameterTypes.head ensuring (_.isArraySliceLike)
    }
  }

  override def toString = {
    val refClassName = refClass.getName
    val methodDescriptor = methodType.toMethodDescriptor.toJETSignature
    s"MethodReference($accessKind, refClass: $refClassName, $methodName, $methodDescriptor)"
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: BitcodeMethodReference =>
      super.equals(that) &&
        sourceMethodType == that.sourceMethodType &&
        methodName == that.methodName
    case _ => false
  }

  override def hashCode = hash(super.hashCode, sourceMethodType, methodName)
}

object BitcodeMethodReference {
  def isCangjieMut(refClass: CompiledType, methodName: String): Boolean =
    refClass.sigType.isRecord && methodName == CONSTRUCTOR_NAME || isArraySliceConstructor(methodName)
}