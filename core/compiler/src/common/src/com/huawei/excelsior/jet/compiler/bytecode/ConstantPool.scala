/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.classfile.SignatureTraverser
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.{Access, ErrorAccess, ErrorAccessInfo}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPoolAccessResult.{DEFERRED, ERROR}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.indy.{MethodHandle, ReferenceKind}
import com.huawei.excelsior.jet.compiler.{RTSProc, TypeProvider}

import scala.PartialFunction.cond
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/** ConstantPool represents constant pool of class file of particular class.
  *
  * @author paul
  */
abstract class ConstantPool {

  /** Get class of this constant pool. */
  def getHost: ClassType

  /** Get type provider for this constant pool. */
  def getTypeProvider: TypeProvider

  /** Returns number of entries in this constant pool. */
  def getCount: Int

  /** Get the tag for the constant pool entry */
  def getTag(cpEntry: Int): Tag

  /** Get the value of constant `Integer` entry. */
  def getInt(cpEntry: Int): Int

  /** Get the value of constant `Float` entry. */
  def getFloat(cpEntry: Int): Float

  /** Get the value of constant 'Long' entry. */
  def getLong(cpEntry: Int): Long

  /** Get the value of constant 'Double' entry. */
  def getDouble(cpEntry: Int): Double

  /** Get the character sequence of `Utf8` entry. */
  def getUtf8(cpEntry: Int): XString // o2: BufferPtr

  /** Get the value of constant `String` entry. */
  final def getStringValue(cpEntry: Int) = getUtf8(getStringIndex(cpEntry))

  /** Get the name of constant `Class` entry. */
  def getClassNameValue(cpEntry: Int) = getUtf8(getClassNameIndex(cpEntry))

  /** Get code attribute for given method from this constant pool. */
  // FIXME: this method does not belong here
  def getMethodCodeAttribute(method: Method): Method.CodeAttribute

  /** Returns index of `Class` entry representing a class or interface type that has a field or method as a member. */
  def getClassIndex(cpEntry: Int): Int // o2: Index

  /** Returns index of `Utf8` entry that contains name of class. */
  def getClassNameIndex(cpEntry: Int): Int // o2: IndexName

  /** Returns index of `NameAndType` entry for a field or method. */
  def getNameAndTypeIndex(cpEntry: Int): Int // o2: IndexName

  /** Returns index of `Utf8` entry that contains name of class, field or method. */
  def getNameIndex(cpEntry: Int): Int // o2: IndexName

  /** Returns index of `Utf8` entry that contains field or method descriptor. */
  def getDescriptorIndex(cpEntry: Int): Int // o2: Index

  /** Returns index of `Utf8` entry representing characters of a constant string. */
  def getStringIndex(cpEntry: Int): Int // o2: Index

  /** Get the Method Handle reference kind value. */
  def getMethodHandleRefKind(cpMethodHandleEntry: Int): Int

  def getMemberIndex(cpMethodHandleEntry: Int): Int
  def getMethodTypeDescriptorIndex(cpMethodTypeEntry: Int): Int
  def getBootstrapMethodIndex(cpInvokeDynamicEntry: Int): Int
  def getBootstrapMethodArgsIndexes(cpInvokeDynamicEntry: Int): Array[Short]

  def getType(cpClassEntry: Int): Access[Type]
  def getClassType(cpClassEntry: Int): Access[ClassType]
  def getField(cpFieldEntry: Int, akind: FieldAccessKind): Access[Field]

  protected def getMethod(cpMethodEntry: Int, akind: MethodAccessKind): Access[Method]

  final def getMethodReference(cpMethodEntry: Int, akind: MethodAccessKind): Access[BytecodeMethodReference] = {
    val methodAccess = getMethod(cpMethodEntry, akind)
    if (methodAccess.getResult == ERROR) {
      return new ErrorAccess[BytecodeMethodReference](cpMethodEntry, methodAccess.asInstanceOf[ErrorAccessInfo])
    }
    val method = methodAccess.getObject

    val refClassAccess = getMethodRefClass(cpMethodEntry)
    assert(refClassAccess.getResult != ERROR)
    val refClass = refClassAccess.getObject

    if ((methodAccess.getResult == DEFERRED) || (refClassAccess.getResult == DEFERRED)) {
      // Explicitly check refClass access because:
      //   In AOT a class can be `excluded` from compilation by RTA or Singlecomp.
      //   Such class is not considered deferred by O2 symlevels,
      //   but in common symlevel interface it is (see Type.isDeferred and Type.isUnavailableForAOT).
      //   This leads to a situation where O2 symlevel successfully resolves method while refClass is deferred.
      new BytecodeMethodReference(method.getMethodType, akind, null, null, refClass, false, cpMethodEntry)
    } else {
      new BytecodeMethodReference(method, akind, refClass, cpMethodEntry)
    }
  }

  protected def getMethodRefClass(cpMethodEntry: Int) = {
    getClassType(getClassIndex(cpMethodEntry))
  }

  def getMethodHandle(cpMethodHandleEntry: Int): Access[MethodHandle] = {
    val refKind = ReferenceKind.fromBytecode(getMethodHandleRefKind(cpMethodHandleEntry))
    assert(refKind.isValid)
    val memberIndex = getMemberIndex(cpMethodHandleEntry)
    val memberAccess = if (refKind.isInvoke) {
      getMethod(memberIndex, refKind.asMethodAccessKind)
    } else {
      getField(memberIndex, refKind.asFieldAccessKind)
    }

    if (memberAccess.isError) {
      assert(memberAccess.isInstanceOf[ErrorAccessInfo])
      return new ErrorAccess[MethodHandle](cpMethodHandleEntry, memberAccess.asInstanceOf[ErrorAccessInfo])
    }
    new MethodHandle(refKind, memberAccess.getObject)
  }

  def getMethodType(cpMethodTypeEntry: Int) = {
    val descriptor = getUtf8(getMethodTypeDescriptorIndex(cpMethodTypeEntry))
    val sig = getTypeProvider.resolveMethodSignature(descriptor, getHost)
    MethodType(sig)
  }

  /** Returns `true` if given `MethodRef` constant pool entry references signature polymorphic method. */
  final def isMethodSignaturePolymorphic(cpMethodEntry: Int) = {
    getSignaturePolymorphicMethodID(cpMethodEntry) != SigPolyMethodID.NONE
  }

  /** Returns signature polymorphic method ID of the given `MethodRef` constant pool entry, or [[SigPolyMethodID.NONE]]
    * if method is not signature polymorphic.
    */
  def getSignaturePolymorphicMethodID(cpMethodEntry: Int): SigPolyMethodID

  final protected def isRef(index: Int) = cond(getTag(index)) {
    case Tag.FIELDREF | Tag.INTERFACE_METHODREF | Tag.METHODREF | Tag.INVOKE_DYNAMIC => true
  }

  /** Returns name of the given `FieldRef`, `MethodRef` or `InvokeDynamic` constant pool entry. */
  final def getRefName(index: Int) = {
    assert(isRef(index))
    getUtf8(getNameIndex(getNameAndTypeIndex(index)))
  }

  /** Returns signature of the given `FieldRef`, `MethodRef` or `InvokeDynamic` constant pool entry. */
  final def getRefSignature(index: Int) = {
    assert(isRef(index))
    getUtf8(getDescriptorIndex(getNameAndTypeIndex(index)))
  }

  def getRefSignatureTraverser(index: Int) = {
    SignatureTraverser.fromString(getRefSignature(index))
  }

  /** Returns resolved bootstrap method arguments or null if resolve of one of the arguments failed. */
  @nowarn("msg=match may not be exhaustive")
  def getBootstrapMethodArgs(cpInvokeDynamicEntry: Int): Array[Any] = {
    import Tag.*

    val argIndexes = getBootstrapMethodArgsIndexes(cpInvokeDynamicEntry)
    if (argIndexes == null) {
      return null
    }
    val args = new Array[Any](argIndexes.length)
    for ((index, i) <- argIndexes.zipWithIndex) {
      args(i) = getTag(index) match {
        case METHOD_HANDLE =>
          val methodHandleAccess = getMethodHandle(index)
          if (methodHandleAccess.isError) {
            return null
          }
          methodHandleAccess.getObject

        case CLASS =>
          val typeAccess = getClassType(index)
          if (typeAccess.isError) {
            return null
          }
          typeAccess.getObject

        case METHOD_TYPE => getMethodType(index)
        case STRING      => getStringValue(index)
        case INTEGER     => getInt(index)
        case LONG        => getLong(index)
        case FLOAT       => getFloat(index)
        case DOUBLE      => getDouble(index)
      }
    }
    args
  }

  /** Get the Symbol for constant string entry */
  def getConstString(index: Int): ConstString

  /** Get the type kind of field */
  def getFieldTypeKind(index: Int): BytecodeTypeKind

  def getFieldRefClass(index: Int): Type

  /** Ensures that entry from given `index` in `another` ConstantPool is accessible in runtime
    * through `this` ConstantPool by the index returned from this method.
    *
    * In JIT that preserves class files it will be the same as given `index`.
    * In AOT (or JIT without class files) it might be different, because only requested entries are preserved in AOT Constant Pool.
    */
  def getRuntimeIndex(another: ConstantPool, index: Int): Int

  /** Returns index of an AOT constant pool entry containing given `importIndex` of a class or interface. */
  def getAOTClassRefIndex(importIndex: Int): Int

  /** For given cpEntryIdx of InvokeDynamic entry returns the sequential number of this entry among all InvokeDynamic entries in Constant Pool. */
  def getInvokeDynamicEntryNumber(cpEntryIdx: Int): Int
}

object ConstantPool {

  trait ErrorAccessInfo {

    /** Get the exception-throwing procedure for the throwing exception object.
      *
      * TODO: replace by `getErrorKind` returning error kind
      */
    def getThrowProc: RTSProc

    /** Get the exception's message for the throwing exception object. */
    def getErrorMessage: XString
  }

  trait DeferredAccessInfo {
    // TODO: would be better to have ConstantPool cp() method to have more sound interface, but it's easier to restore CP from compilation context

    /** Index of entry from which deferred object was obtained. */
    def cpIndex: Int
  }

  /** Access is result of access action to symbolic reference in constant pool. */
  trait Access[T <: ConstantPoolObject] {

    /** Get result kind of access action. */
    def getResult: ConstantPoolAccessResult

    def getObject: T
    def getError: ErrorAccessInfo

    def isError = getResult == ERROR
    def isDeferred = getResult == DEFERRED

    /** Get info about DEFERRED result. */
    def getDeferredInfo: DeferredAccessInfo
  }

  /** Generic [[DEFERRED]] access implementation. */
  final class DeferredAccess[T <: ConstantPoolObject](val cpIndex: Int, private val obj: T)
    extends Access[T] with DeferredAccessInfo {

    override def getResult = DEFERRED
    override def getObject = obj
    override def getError = shouldNotCallThis()
    override def getDeferredInfo = this

    override def equals(that: Any) = that match {
      case that: AnyRef if this eq that => true
      case that: DeferredAccess[?] => cpIndex == that.cpIndex && obj == that.obj
      case _ => false
    }

    override def hashCode = (cpIndex, obj).##
  }

  /** Generic [[ERROR]] access implementation. */
  class ErrorAccess[T <: ConstantPoolObject](cpIndex: Int, rtsProc: RTSProc, errorMessage: XString)
    extends Access[T] with ErrorAccessInfo {

    def this(cpIndex: Int, cause: ErrorAccessInfo) = {
      this(cpIndex, cause.getThrowProc, cause.getErrorMessage)
    }

    override def getResult = ERROR
    override def getObject = shouldNotCallThis()
    override def getError = this
    override def getDeferredInfo = shouldNotCallThis()
    override def getThrowProc = rtsProc
    override def getErrorMessage = errorMessage
  }
}
