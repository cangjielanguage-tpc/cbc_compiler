/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.{RTConst, TypeProvider}
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, MethodAccessKind}
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.STATIC_VIRTUAL
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType

/** A method reference is an immutable structure containing original info used for method invocation,
  * [[MethodType]] of this method and an actual symlevel [[Method]] reference to generate an invocation to.
  *
  * Hence the only object needed to generate code sequence of invoke is MethodReference
  * which MethodType was adjusted according to real var.args types, if some.
  *
  * @author ijorch
  */
class MethodReference(val methodType: MethodType,
                      _accessKind: MethodReferenceAccessKind,
                      private val _method: Method = null,
                      private val _permanentMethod: PermanentMember = null,
                      _refClass: CompiledType = null,
                      private val _explicitVNum: Option[Int] = None)
  extends MethodTables.Ref with Symbol {

  private val (rawRefClass, rawAccessKind) = if (_method != null) {
    assert(_refClass != null)

    assert(!_method.isDeferred && !_refClass.symType.isDeferred,
      "deferred method reference should not have method")

    assert((_accessKind == MethodReferenceAccessKind.STATIC || _accessKind == MethodReferenceAccessKind.STATIC_VIRTUAL) == (_method.isStatic && !_method.isCangjieMut),
      "method is accessed as STATIC or STATIC_VIRTUAL iff it is static")

    assert(_accessKind != MethodReferenceAccessKind.INTERFACE || _refClass.symType.isInterface,
      "if method is accessed as INTERFACE then it is non-static and referenced from interface")

    assert(_accessKind != MethodReferenceAccessKind.VIRTUAL || (!_refClass.symType.isInterface || _method.getDeclaringClass.isJavaLangObject || isStandalone),
      "if method is accessed as VIRTUAL then it is non-static and referenced from non-interface (or declared in j.l.Object (JET-7343))")

    assert(!_method.isConstructor || _accessKind == MethodReferenceAccessKind.SPECIAL || _accessKind == MethodReferenceAccessKind.MUT,
      "constructor should be accessed as SPECIAL or MUT")

    assert(!_method.isCangjieMut || _accessKind == MethodReferenceAccessKind.MUT,
      "mut function should be accessed as MUT")

    assert(!_method.isClinit,
      "clinit should not be accessed at all")

    assert(_permanentMethod == null,
      "method reference should have either method or permanent method")

    assert(!_refClass.symType.isCangjieType || _refClass.sigType.isUniversalGeneric == asClassType(_refClass.symType).isUniversalGeneric,
      s"erased reference type ${_refClass.sigType.toJETSignature}")

    // Transform interface calls of java/lang/Object methods into virtual calls.
    // TODO: this is not correct, see JET-7343
    if (_accessKind == MethodReferenceAccessKind.INTERFACE && _method.getDeclaringClass.isJavaLangObject) {
      (ReferenceType(_method.getDeclaringClass), MethodReferenceAccessKind.VIRTUAL)
    } else {
      (_refClass, _accessKind)
    }
  } else {
    if (_refClass == null) {
      assert(_accessKind == MethodReferenceAccessKind.SPECIAL || _accessKind == MethodReferenceAccessKind.STATIC)
    }

    (_refClass, _accessKind)
  }

  private lazy val vmtSlot = if (hasMethod) {
    refClass.getMTLayout.vnum(this)
  } else {
    MethodTables.NO_VNUM
  }

  /** Creates a method reference to given method with given access kind and ref class.
    * All properties inferred form the method and ref class.
    */
  def this(method: Method, accessKind: MethodReferenceAccessKind, refClass: CompiledType, explicitVNum: Option[Int]) =
    this(method.getMethodType, accessKind, method, null, refClass, explicitVNum)

  /** Creates a method reference to given method with given access kind and ref class.
    * All properties inferred form the method and ref class.
    */
  def this(method: Method, accessKind: MethodReferenceAccessKind, refClass: CompiledType) =
    this(method, accessKind, refClass, None)

  /** Creates a method reference to given method with given access kind.
    * All properties inferred form the method and its host class itself.
    *
    * Should be used only for hand-crafted invocations when there is no ref class available.
    */
  def this(method: Method, accessKind: MethodReferenceAccessKind) =
    this(method, accessKind, CompiledType(method.getDeclaringClass))

  def this(method: Method, accessKind: MethodReferenceAccessKind, refClass: CompiledType, vnum: Int) =
    this(method, accessKind, refClass, Some(vnum))

  protected def copy(methodType: MethodType, method: Method, permanentMethod: PermanentMember, refClass: CompiledType, accessKind: MethodReferenceAccessKind, explicitVNum: Option[Int]) =
    new MethodReference(methodType, accessKind, method, permanentMethod, refClass, explicitVNum)

  final def withMethodType(methodType: MethodType) = if (methodType == this.methodType) {
    this
  } else {
    copy(methodType, _method, _permanentMethod, rawRefClass, rawAccessKind, _explicitVNum)
  }

  final def withAccessKind(accessKind: MethodReferenceAccessKind) = if (accessKind == this.rawAccessKind) {
    this
  } else {
    copy(methodType, _method, _permanentMethod, rawRefClass, accessKind, _explicitVNum)
  }

  final def withMethod(method: Method) = if (method == this._method) {
    this
  } else {
    copy(methodType, method, null, rawRefClass, rawAccessKind, _explicitVNum)
  }

  final def withPermanentMethod(permanentMethod: PermanentMember) = if (permanentMethod == this._permanentMethod) {
    this
  } else {
    copy(methodType, null, permanentMethod, rawRefClass, rawAccessKind, _explicitVNum)
  }

  final def withRefClassAndAccessKind(refClass: CompiledType, accessKind: MethodReferenceAccessKind) = if ((refClass == this.rawRefClass) && (accessKind == this.rawAccessKind)) {
    this
  } else {
    copy(methodType, _method, _permanentMethod, refClass, accessKind, _explicitVNum)
  }

  def toBytecodeMethodReference(isMemberNameInvoke: Boolean, cpIndex: Int) =
    new BytecodeMethodReference(methodType, MethodAccessKind.fromMethodRefernceAccessKind(rawAccessKind),
      _method, _permanentMethod, asClassType(rawRefClass.symType), isMemberNameInvoke, cpIndex)

  def toBitcodeMethodReference(sourceMT: MethodType, methodName: XString, linkageName: Option[XString]) = {
    assert(_method == null)
    assert(_permanentMethod == null)
    new BitcodeMethodReference(methodType, sourceMT, rawAccessKind, rawRefClass, methodName, linkageName)
  }

  def toConstraintCallMethodReference(sourceSig: MethodSignature, methodName: XString) = {
    assert(_method == null)
    assert(_permanentMethod == null)
    new ConstraintCallMethodReference(sourceSig, methodType, methodName, rawRefClass)
  }

  def toInstantiatedMethodReference(instantiatedTypeParameters: Seq[SignatureType], refType: SignatureType) =
    new InstantiatedMethodReference(_method, _accessKind, instantiatedTypeParameters, refType, _explicitVNum)

  /** Returns method type for this method with no special wrapper params. */
  def realMethodType: MethodType =
    if (hasMethod) methodType.dropFirstNParameters(method.getSpecialParamsCount) else methodType

  def hasMethod = _method != null || hasPermanentMethod

  override def method = {
    assert(hasMethod)
    if (_method != null) {
      _method
    } else {
      _permanentMethod.get.asInstanceOf[Method]
    }
  }

  def getReceiverArgIndex: Int = methodType.getReceiverArgIdx

  def getRetByValArgIndex: Int = methodType.getRetByValArgIdx

  def hasReceiverParameter: Boolean = methodType.hasReceiverParameter

  def isCangjieMut: Boolean = hasMethod && method.isCangjieMut

  def hasNonRecordReceiverParameter(implicit typeProvider: TypeProvider): Boolean =
    hasReceiverParameter && !methodType.parameterType(getReceiverArgIndex).isRecord

  def hasRefClass = rawRefClass != null

  @Deprecated
  override def refClass: ClassType = {
    assert(hasRefClass)
    asClassType(rawRefClass.symType)
  }

  def refType: CompiledType = {
    assert(hasRefClass)
    rawRefClass
  }

  def hasPermanentMethod = _permanentMethod != null

  def getPermanent: MethodReference = withPermanentMethod(method.getPermanent)

  def accessKind = rawAccessKind

  def isInterfCall = rawAccessKind == MethodReferenceAccessKind.INTERFACE

  def isVirtualCall = rawAccessKind == MethodReferenceAccessKind.VIRTUAL

  def explicitVNum: Option[Int] = _explicitVNum

  def hasVirtualMethodSlot = vmtSlot != MethodTables.NO_VNUM

  def virtualMethodSlot = {
    assert(hasVirtualMethodSlot)
    vmtSlot
  }

  /** Returns whether the call is direct, i.e. does not require VMT or IMT dispatching.
    * Please note that virtual call may be direct when the method does not have a virtual method slot (e.g. is final).
    */
  def isDirectCall = !((isInterfCall || isVirtualCall) && hasVirtualMethodSlot)

  def methodName = XString(if (hasMethod) method.getName else "<unknown>")

  /** Returns count of bytes that should be checked by caller before call of `this` method reference. */
  def getStackCheckByCallerBytes: Int = {
    if (hasMethod) {
      if (method.shouldStackCheckByCaller) {
        return method.getStackCheckByCallerBytes
      }
    }
    if (methodType.callConv.isManaged) {
      RTConst.StackOverflowHandling.STACK_RESERVE_FOR_MANAGED_METHOD.intValue
    } else {
      RTConst.StackOverflowHandling.STACK_RESERVE_FOR_RT_METHOD.intValue
    }
  }

  override def toString = {
    val refClassName = if (hasRefClass) refClass.getName else "<unknown>"
    val methodName = if (hasMethod) s"${method.getDeclaringClass.getName}.${method.getName}" else "<unknown>"
    val methodDescriptor = methodType.toMethodDescriptor.toJETSignature
    s"MethodReference($rawAccessKind, refClass: $refClassName, $methodName, $methodDescriptor)"
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: MethodReference if this.getClass == that.getClass =>
      methodType == that.methodType &&
        _method == that._method &&
        _permanentMethod == that._permanentMethod &&
        rawRefClass == that.rawRefClass &&
        rawAccessKind == that.rawAccessKind
    case _ => false
  }

  override def hashCode = (methodType, _method, _permanentMethod, rawRefClass, rawAccessKind).##
}
