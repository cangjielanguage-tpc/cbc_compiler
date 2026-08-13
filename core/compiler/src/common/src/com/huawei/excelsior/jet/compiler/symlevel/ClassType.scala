/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{CodeUnit, TypeProvider}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRVTable, CangjieEnumInfo}
import com.huawei.excelsior.jet.compiler.layout.MethodTables
import com.huawei.excelsior.jet.compiler.symlevel.FindMethodImplResult.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.util.Numbering

import scala.annotation.tailrec
import scala.collection.mutable

/** Type needed to describe entities that are containers for fields and methods (e.g. Class and Interface for Java).
  *
  * Currently, we have [[com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType]] which is used
  * in some parts of Opt, but they represent different entities.
  * [[com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType]] represents type
  * which extends ReferenceType (java Class and Interface), but this ClassType represents container
  * for methods and fields, and include not only java Class and Interface, but also AJ Array, AJ Namespace etc.
  *
  * @author orangebyte256
  */
object ClassType {
  /** Returns `true` if `target` overrides package-private `original`. */
  private def doesOverridePackagePrivate(original: Method, target: Method)(implicit typeProvider: TypeProvider): Boolean = {
    val targetClass = target.getDeclaringClass
    val originalClass = original.getDeclaringClass

    if (targetClass.isCangjieType) {
      assert(target.getXName == original.getXName)
    } else {
      assert(target.overridesNameAndSig(original))
    }
    assert(!original.isPrivate && !original.isPublic && !original.isProtected)

    // if target class and original are in the same package then target always overrides original
    if (targetClass isSamePackage originalClass) return true

    // otherwise overriding is possible only if target overrides another method which elevates access of original
    // i.e. target class has a superclass which is in the same package as original class
    // and it has public or protected instance method with the same name and signature as original
    var superClass = asClassType(targetClass.getSuperClassSig) // TODO should propagate SignatureType?
    while (superClass != originalClass) {
      if (superClass.isSamePackage(originalClass)) {
        val m = superClass.findDeclaredOverridingMethodOrNull(original)
        if (m != null && !m.isStatic && (m.isPublic || m.isProtected)) return true
      }
      superClass = asClassType(superClass.getSuperClassSig)
    }

    false
  }
}

abstract class ClassType extends Type {
  def allSuperInterfacesSigs: Iterable[SignatureType] = allSuperInterfaces.map(SignatureType.fromSymType)
  /** Returns iterable containing all distinct super interfaces. */
  lazy val allSuperInterfaces: Iterable[ClassType] = {
    val interfs = mutable.LinkedHashSet.empty[ClassType]

    // Note that `superType.isInterface` doesn't work with deferred types (see JET-15701)
    if (getSuperClassSig != null) interfs ++= asClassType(getSuperClassSig).allSuperInterfaces
    for (interf <- getDeclaredSuperInterfacesSym) {
      interfs += interf
      interfs ++= interf.allSuperInterfaces
    }

    interfs
  }

  /** Returns this class clinit or null, if class has no clinit. */
  def getClinit: Method

  /** Whether this and that types reside in the same package. */
  def isSamePackage(that: ClassType): Boolean

  /** Constant pool of normal class. */
  def getClassConstantPool: ConstantPool

  def isInterpreterInternals: Boolean

  def isVerifiable: Boolean

  /** Returns thin type descriptor symbol. */
  def getThinTypeHandle: ThinTypeHandleSymbol

  def getThinInheritanceLevel: Int

  final def getIMTSlot(interf: Type) = {
    val idx = getSuperInterfaceIndex(interf)
    val inums = getMTLayout.inums
    if (0 <= idx && idx < inums.length) {
      inums(idx)
    } else {
      MethodTables.NO_INUM
    }
  }

  /** Returns method declared in this class or one of its superclasses by name and signature. */
  def findMethodOrNull(methodName: XString, methodSig: MethodSignature): Method =
    findMethodOrNull(methodName, methodSig, (_: Method) => true)

  /** Returns method declared in this class or one of its superclasses by name and signature filtered by supplied filter. */
  def findMethodOrNull(methodName: XString, methodSig: MethodSignature, filter: Method => Boolean): Method =
    findMethodOrNullWithSigEq(methodName, methodSig, MethodSignature.equalExact, filter)

  def findMethodOrNullWithSigEq(methodName: XString, methodSig: MethodSignature, sigEq: (MethodSignature, MethodSignature) => Boolean): Method =
    findMethodOrNullWithSigEq(methodName, methodSig, sigEq, (_: Method) => true)

  def findMethodOrNullWithSigEq(methodName: XString, methodSig: MethodSignature, sigEq: (MethodSignature, MethodSignature) => Boolean, filter: Method => Boolean): Method = {
    var clazz = this
    while (clazz != null) {
      val res = clazz.findDeclaredMethodOrNullWithSigEq(methodName, methodSig, sigEq)
      if (res != null && filter(res)) {
        return res
      }
      clazz = asClassType(clazz.getSuperClassSig)
    }

    for (interf <- allSuperInterfaces) {
      val res = interf.findDeclaredMethodOrNullWithSigEq(methodName, methodSig, sigEq)
      if (res != null && filter(res)) {
        return res
      }
    }

    null
  }

  /** Returns method declared in this class by name and signature or null if method not found. */
  def findDeclaredMethodOrNull(methodName: XString, methodSig: MethodSignature): Method =
    findDeclaredMethodOrNullWithSigEq(methodName, methodSig, MethodSignature.equalExact)

  def findDeclaredMethodOrNullWithSigEq(methodName: XString, methodSig: MethodSignature, sigEq: (MethodSignature, MethodSignature) => Boolean): Method = {
    for (m <- getDeclaredMethods) {
      if (m.getXName == methodName && (methodSig == null || sigEq(m.getSignature, methodSig))) {
        return m
      }
    }
    null
  }

  /** Returns method declared in this class by name and signature. */
  final def findDeclaredMethod(methodName: XString, methodSig: MethodSignature): Method = {
    val m = findDeclaredMethodOrNull(methodName, methodSig)
    if (m == null) {
      shouldNotReachHere(s"No method with name '$methodName' and signature '${methodSig.toJETSignature}' in class '$getName'")
    }
    m
  }

  /** Returns reference to method declared in this class by name and signature. */
  final def getMethodRefToLocal(methodName: XString, methodSig: MethodSignature, akind: MethodReferenceAccessKind) =
    new MethodReference(findDeclaredMethod(methodName, methodSig), akind)

  final def getMethodRefToLocalOrNull(methodName: XString, methodSig: MethodSignature, akind: MethodReferenceAccessKind): MethodReference =
    getMethodRefToLocalOrNullWithSigEq(methodName, methodSig, akind, MethodSignature.equalExact)

  final def getMethodRefToLocalOrNullWithSigEq(methodName: XString, methodSig: MethodSignature, akind: MethodReferenceAccessKind,
                                               sigEq: (MethodSignature, MethodSignature) => Boolean): MethodReference = {
    val method = findDeclaredMethodOrNullWithSigEq(methodName, methodSig, sigEq)
    if (method != null) {
      new MethodReference(method, akind)
    } else {
      null
    }
  }

  /** Returns reference to method declared in this class or in one of its supers by name and signature. */
  final def getMethodRefTo(methodName: XString, methodSig: MethodSignature, akind: MethodReferenceAccessKind): MethodReference = {
    val m = findMethodOrNull(methodName, methodSig)
    if (m == null) {
      shouldNotReachHere(s"No method with name '$methodName' and signature '${methodSig.toJETSignature}' in class '$getName' or its supers")
    }
    new MethodReference(m, akind)
  }

  /** Returns whether this type has declared super interfaces. */
  def hasDeclaredSuperInterfaces: Boolean

  /** Returns declared super interfaces iterator. */
  def getDeclaredSuperInterfaces: Iterator[RefInterfaceType]
  def getDeclaredSuperInterfacesSym: Iterator[ClassType] = getDeclaredSuperInterfaces.map(_.symType)
  def getDeclaredSuperInterfacesSig: Iterator[SignatureType] = getDeclaredSuperInterfaces.map(_.sigType)

  /** Returns iterator over declared super class and super interfaces. */
  final def getDeclaredSuperTypes: Iterator[ClassType] = { // TODO SUPER REFACTOR
    Option(asClassType(getSuperClassSig)).iterator ++ getDeclaredSuperInterfacesSym
  }

  def getSuperInterfaceIndex(interf: Type): Int = allSuperInterfaces.iterator.indexOf(interf) // TODO SUPER REFACTOR

  /** Returns iterator over all methods declared in class file in order of declaration. */
  def getDeclaredMethods: Iterator[Method]

  // TODO: it is workaround for JET-16660
  def dropDeclaredMethodsCache(): Unit

  /** Returns iterator over methods generated in resulting binary artifact. */
  def getGeneratedMethods: Iterator[Method]

  /** Returns index of given method in resulting binary artifact or -1 if method will not be generated. */
  def getGeneratedMethodIndex(method: Method): Int

  /** Find method which is called when calling original method reference in scope of this class.
    * Returns either target method or thrown exception.
    */
  def findMethodImplementation(originalRef: MethodTables.Ref): FindMethodImplResult =
    findMethodImplementation(originalRef.method, originalRef.refClass.isInterface)

  /** Find method which is called when calling original method in scope of this class.
    * Returns either target method or thrown exception.
    */
  def findMethodImplementation(original: Method, isInterfCall: Boolean): FindMethodImplResult = {
    assert(MethodTables.canBeInMethodTable(original))
    assert(original.getDeclaringClass.isAssignableFrom(this))

    val target = findMethodImplementationInClasses(original, isInterfCall)
    if (target != null) {
      if (isInterfCall && !target.isPublic && !this.isCangjieType) {
        return FindMethodImplResult.Error(MethodSearchError.ILLEGAL_ACCESS)
      } else if (target.isAbstract) {
        return FindMethodImplResult.Error(MethodSearchError.ABSTRACT_METHOD)
      } else {
        return FindMethodImplResult.Found(target)
      }
    }
    findMostSpecificDefaultMethod(original)
  }

  private def findMethodImplementationInClasses(original: Method, isInterfCall: Boolean): Method = {
    assert(!original.isStatic && !original.isPrivate && !original.isConstructor)

    if (this == original.getDeclaringClass) {
      // very important fast-path
      original

    } else {
      val isPackagePrivate = !original.isPublic && !original.isPrivate && !original.isProtected
      var clazz = this

      while (clazz != null) {
        val target = clazz.findDeclaredOverridingMethodOrNull(original)
        if (target != null && !target.isStatic) {
          if (isInterfCall || (!target.isPrivate &&
            (!isPackagePrivate || ClassType.doesOverridePackagePrivate(original, target)))) {

            return target
          }
        }
        clazz = asClassType(clazz.getSuperClassSig)
      }

      null
    }
  }

  def findMostSpecificDefaultMethod(original: Method): FindMethodImplResult = {
    val candidates = new mutable.HashSet[Method]

    def collectMostSpecificCandidates(clazz: ClassType): Unit = {
      for (interf <- clazz.getDeclaredSuperInterfacesSym) {
        val m = interf.findDeclaredOverridingMethodOrNull(original)
        if (m != null && !m.isStatic && !m.isPrivate) {
          candidates.add(m)
        } else {
          collectMostSpecificCandidates(interf)
        }
      }
    }

    if (isInterface) {
      collectMostSpecificCandidates(this)
    } else {
      var clazz = this
      while (clazz != null) {
        collectMostSpecificCandidates(clazz)
        clazz = asClassType(clazz.getSuperClassSig)
      }
    }

    if (candidates.isEmpty) {
      return FindMethodImplResult.Error(MethodSearchError.ABSTRACT_METHOD)
    }

    if (candidates.size == 1) {
      val m = candidates.head
      if (m.isAbstract) {
        return FindMethodImplResult.Error(MethodSearchError.ABSTRACT_METHOD)
      } else {
        return FindMethodImplResult.Found(m)
      }
    }

    val arr = candidates.toArray
    for (i <- 0 until arr.length) {
      val i1 = arr(i).getDeclaringClass
      for (j <- i + 1 until arr.length) {
        val i2 = arr(j).getDeclaringClass
        if (i1.doesImplement(i2)) {
          candidates.remove(arr(j))
        } else if (i2.doesImplement(i1)) {
          candidates.remove(arr(i))
        }
      }
    }

    val filtered = candidates.filter(!_.isAbstract)
    filtered.size match {
      case 1 => Found(filtered.head)
      case 0 => Error(ABSTRACT_METHOD)
      case _ => Error(INCOMPATIBLE_CLASS_CHANGE)
    }
  }

  /** Returns method declared in this class which overrides given original one or null if such method not found. */
  protected def findDeclaredOverridingMethodOrNull(original: Method): Method = {
    for (m <- getDeclaredMethods) {
      if (m.overridesNameAndSig(original)) {
        return m
      }
    }
    null
  }

  /** Returns [[MethodTables.Layout]] for this type. */
  def getMTLayout: MethodTables.Layout

  final def refMethodByVNum(vnum: Int): Method = {
    val mtLayout = getMTLayout
    mtLayout.refs.find(mtLayout.vnum(_) == vnum).map(_.method).orNull
  }

  def getVersionedMethods: Iterator[CodeUnit]

  /** Returns either [[CodeUnit]] of a given `method` version specialized for this type
    * or the [[CodeUnit.of]] given `method` if it has no specialized version for this type.
    * TODO: rework exterior method versioning so that [[findMethodImplementation]] will return proper code units.
    */
  def chooseMethodVersion(method: Method): CodeUnit

  def getRefFieldOffsets: Array[Int]

  def findDeclaredFieldOrNull(fieldIndex: Int): Field =
    getDeclaredFields.find(_.getFieldIndex == fieldIndex).orNull

  /** Returns iterator over all fields declared in class file.
    * Order is the same as it was in class file. Injected fields are located at the end.
    */
  def getDeclaredFields: Iterator[Field]

  /** Returns iterator over already parsed declared fields.
    * Note that data may be incomplete.
    * TODO: it is workaround for JET-16660
    */
  def getCurrentDeclaredFields: Iterator[Field]

  // TODO: it is workaround for JET-16660
  def dropDeclaredFieldsCache(): Unit

  /** Returns iterator over all fields declared in this class and all its superclasses. */
  final def getFields: Iterator[Field] = new Iterator[Field]() {
    private var superclass = asClassType(getSuperClassSig) // TODO should propagate SignatureType?
    private var current = getDeclaredFields

    override def hasNext: Boolean = {
      if (current.hasNext) return true
      while (superclass != null) {
        current = superclass.getDeclaredFields
        superclass = asClassType(superclass.getSuperClassSig)
        if (current.hasNext) return true
      }
      false
    }

    override def next() = {
      assert(hasNext)
      current.next()
    }
  }

  /** Finds field declared in class file by name.
    * Returns `null` if there is no such field.
    * Returns first declared field in case of ambiguity
    * (as `Class.getDeclaredField(String)` does).
    */
  def findDeclaredFieldOrNull(name: XString): Field =
    getDeclaredFields.find(_.getXName == name).orNull

  /** Returns field declared in this class by name and non-null signature or null if field is not found. */
  def findDeclaredFieldOrNull(fieldName: XString, fieldSig: SignatureType): Field = {
    for (f <- getDeclaredFields) {
      if (f.getXName == fieldName && (fieldSig == null || f.getType == fieldSig)) {
        return f
      }
    }
    null
  }

  /** Returns field declared in this class or one of its superclasses by name and nullable signature.
    * Returns null if field is not found.
    * Behaviour is unspecified if signature is null and name is ambiguous (may throw exception, return null or return some field).
    */
  def findFieldOrNull(fieldName: XString, fieldSig: SignatureType): Field = {
    var clazz = this
    while (clazz != null) {
      val res = clazz.findDeclaredFieldOrNull(fieldName, fieldSig)
      if (res != null) {
        return res
      }

      for (interf <- clazz.getDeclaredSuperInterfacesSym) {
        val res = interf.findFieldOrNull(fieldName, fieldSig)
        if (res != null) {
          return res
        }
      }

      clazz = asClassType(clazz.getSuperClassSig)
    }

    null
  }

  /** Returns field declared in this class or one of its superclasses by name.
    * Throws exception if field is not found.
    * Behaviour is unspecified if name is ambiguous (may throw exception, return null or return some field).
    *
    * TODO: specify behaviour in case of ambiguity
    */
  final def findField(fieldName: XString): Field = {
    val field = findFieldOrNull(fieldName, null)
    if (field == null) {
      shouldNotReachHere(s"No field by name '$fieldName' in class '$getName'")
    }
    field
  }

  /** Returns whether instance of this traceable type has any traceable fields.
    *
    * In runtime this property is sometimes referenced as "non-leaf" type.
    */
  @tailrec
  final def hasTraceableFields: Boolean = {
    if (isClass || isRecord) {
      getFields.exists { field => !field.isStatic && field.getType.isTraceableReference }
    } else {
      val elemType = getArrayElemType
      if (elemType.isRecord) {
        asClassType(elemType).hasTraceableFields
      } else if (isArray) {
        elemType.isTraceableReference
      } else {
        shouldNotReachHere(this)
      }
    }
  }

  final def isVariableSizeType: Boolean = {
    if (isRecord && isUniversalGeneric) {
      val fields = getFields.filterNot(_.isStatic)
      fields.exists { f =>
        f.getType.isVariableSizeType
      }
    } else {
      false
    }
  }

  def getCJAnnotationFactory: Method

  def isUniversalGeneric: Boolean
  
  def getGenericInfo: GenericInfo
  
  def getCHIRVTable: CHIRVTable

  def isCangjieEnum: Boolean

  def getCangjieEnumInfo: CangjieEnumInfo

}
