/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder

trait VerifiableType {
  /** Do NOT use this function!
    *
    * It is misleading and badly designed.
    * For example, it returns true for `@AJExtended` classes.
    */
  //@deprecated // TODO: rework isAJType both here and in o2 to exclude AJExtended classes from it
  def isAJType: Boolean

  /** Check if this class is runtime anonymous class (i.e. lambda class). */
  def isAnonymous: Boolean

  def isClass: Boolean

  def isClassOrInterface: Boolean

  /** If type is deferred it should be accessed via reflection stubs.
    *
    * Class is deferred if it has no class file info at compile time or it is unavailable for some AOT-specific reasons.
    * Array is deferred if and only if its base type is deferred.
    * Java-annotated classes from extension classloaders should always be treated as deferred,
    * because we can't reliably analyse them ahead-of-time (e.g. their bytecode can be instrumented at run-time).
    */
  def isDeferred: Boolean

  def isHierarchyRoot: Boolean

  def isInterface: Boolean

  /** Whether this and that types reside in the same package. */
  def isSamePackage(that: VerifiableType): Boolean

  /** Class will throw class definition error on loading, thus will never be loaded into JVM. */
  def isUnloadable: Boolean

  /** Exact analog of `Class.isAssignableFrom(Class)`.
    * Determines if this type is assignable from `type` according JLS.
    * Works both for primitive and reference types.
    *
    * Same as Scala's `type <: this`.
    */
  def isAssignableFrom(`type`: VerifiableType): Boolean

  /** Returns if this class or one of its superclasses contains a field with name and non-null signature and the field is protected. */
  def containsProtectedField(fieldName: XString, fieldSig: XString): Boolean

  /** Returns if this class or one of its superclasses contains a method with name and non-null signature and the method is protected. */
  def containsProtectedMethod(methodName: XString, methodSig: XString): Boolean

  /** Returns unavailable super class or null. */
  def getAbsentSuper: VerifiableType

  /** Returns iterator over all methods declared in class file.
    * Note: in o2-based symlevels the order is NOT the same as in class file.
    */
  def getDeclaredMethods: Iterator[VerifiableMethod]

  /** Returns declared super interfaces iterator. */
  def getDeclaredSuperInterfaces: Iterator[VerifiableType]

  /** Returns host class of anonymous class and null otherwise. */
  def getHostClass: VerifiableType

  /** Return super class if any.
    * Return `null` for [[Object]] and interfaces.
    */
  def getSuperClass: VerifiableType

  /** Constant pool of normal class. */
  def getClassConstantPool: ConstantPool

  def getClassInheritanceLevel: Int

  /** Returns [[AbstractVerifier.Info]] for this type or null if it is unavailable. */
  def getVerificationInfo: AbstractVerifier.Info

  /** Returns true if this class passed verification stage and has verification info associated with it.
    *
    * Passing verification stage results in one of the following states of this class:
    *   - successfully verified without errors (but may require additional verification pass at runtime)
    *   - will throw class definition error at runtime (see [[isUnloadable]])
    *   - will throw verification error at runtime (see [[isUnverifiable]])
    */
  def hasVerificationInfo: Boolean

  def getName: String = getXName.toString

  def getXName: XString

  def resolveTypeByName(tp: TypeProvider, name: XString): VerifiableType

  def addVerificationPair(builder: InfoBuilder, to: VerifiableType, context: VerifiableMethod): Unit

  /** Mark passed importedType as being loaded during verification of {@code this}. */
  def addVerificationImport(importedType: VerifiableType): Unit
}
