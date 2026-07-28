/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{ARRAY_SLICE_NAME, ARRAY_SLICE_PREFIX}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.{Env, Environment, RTConst, RTSProc, TypeProvider}
import com.huawei.excelsior.jet.compiler.debug.info.CompilationUnitInfo
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.layout.FieldsLayout
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.indy.LambdaInfo
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier
import xscala.util.MathUtils.alignUp

/** Some Java type
  *
  * @author cypok
  * @author paul
  */
object Type {
  def asClassType(tpe: Type): ClassType = {
    if (tpe == null) return null

    assert(tpe.isClassOrInterface || tpe.isRecord || tpe.isArray || tpe.isAJArray || tpe.isCangjieArray)
    tpe.asInstanceOf[ClassType]
  }

  def asClassType(tpe: SignatureType)(implicit typeProvider: TypeProvider): ClassType = {
    if (tpe == null) return null
    asClassType(tpe.symType)
  }

  @deprecated def asClassType(tpe: ClassType): Type = asClassType(tpe.asInstanceOf[Type])
}

abstract class Type extends ConstantPoolObject {
  protected implicit def provider: TypeProvider

  def getAccessFlags: Int

  def getCJModifiers: Modifiers

  def isAbstractClass: Boolean

  def isFinal: Boolean
  
  def isCHIRDef: Boolean

  /** If type is deferred it should be accessed via reflection stubs.
    *
    * Class is deferred if it has no class file info at compile time or it is unavailable for some AOT-specific reasons.
    * Array is deferred if and only if its base type is deferred.
    * Java-annotated classes from extension classloaders should always be treated as deferred,
    * because we can't reliably analyse them ahead-of-time (e.g. their bytecode can be instrumented at run-time).
    */
  def isDeferred: Boolean = {
    if (isClassOrInterface || isRecord) {
      isRealAbsentForAOT || isUnavailableForAOT || isJavaExtAnnotatedClass
    } else if (isJBCArray) {
      getArrayBase.isDeferred
    } else {
      false
    }
  }

  def hasDeferredSuper: Boolean

  /** If type is absent it should be accessed via reflection stubs.
    *
    * Class is really absent if its class file is unavailable at compile time.
    * TODO: move to AOTType if/when it will be introduced
    */
  def isRealAbsentForAOT: Boolean

  /** Returns true if type was explicitly excluded from compilation to be accessed only via reflection stubs
    * or has absent super.
    * TODO: move to AOTType if/when it will be introduced
    */
  protected def isUnavailableForAOT: Boolean

  /** Returns true if this class passed verification stage and has verification info associated with it.
    *
    * Passing verification stage results in one of the following states of this class:
    *   - successfully verified without errors (but may require additional verification pass at runtime)
    *   - will throw class definition error at runtime (see [[isUnloadable]])
    *   - will throw verification error at runtime (see [[isUnverifiable]])
    */
  def hasVerificationInfo = getVerificationInfo != null

  def isErroneousOrAbsent = !hasVerificationInfo || isErroneous

  /** Class is not verifiable or not loadable at runtime. */
  def isErroneous = isUnloadable || isUnverifiable

  /** Class will throw class definition error on loading, thus will never be loaded into JVM. */
  def isUnloadable: Boolean

  /** Class will throw verification error at runtime. */
  def isUnverifiable = {
    val verificationInfo = getVerificationInfo
    verificationInfo != null && verificationInfo.getVerifyError != null
  }

  /** If class is synthetic it does not actually exist and is never loaded into JVM. */
  def isSynthetic: Boolean

  /** Whether class is from user's current compilation set. */
  def isInCurrentCompilationSet: Boolean

  /** Whether we have access to bytecode of this class. */
  def isBytecodeAvailable: Boolean

  /** Returns true if this class is already clinited in any context (no clinit or it's empty),
    * clinit checks should not be generated.
    */
  def isPreClinited: Boolean

  /** Returns true if this class is "turbo" clinited, i.e. it is system class clinited during bootstrap
    * before loading of any non-system class.
    */
  def isTurboClinited: Boolean

  /** Returns true if this class is turbo clinited in given context. */
  final def isTurboClinitedIn(context: Method) = isTurboClinited && !context.getDeclaringClass.isSystemClass

  /** Type has a non-trivial finalizer. */
  def finalizable: Boolean

  def getKind: TypeKind

  /** Returns number of array dimensions */
  def getArrayDimnum: Int

  /** Unwinds array base type (removes all []) */
  def getArrayBase: Type

  def getArrayElemType: SignatureType

  def getVArrayLength: Long

  def getVArrayElemType: SignatureType

  def getArraySliceElemType: SignatureType

  def getCangjieBoxValueType: SignatureType

  /** Return super class if any.
    * Return `null` for [[Object]] and interfaces.
    */
  def getSuperClass: RefClassType
  def getSuperClassSym: ClassType = {
    val s = getSuperClass
    if (s == null) null else s.symType
  }
  def getSuperClassSig: SignatureType = {
    val s = getSuperClass
    if (s == null) null else s.sigType
  }

  /** Returns iterator over superclasses from immediate supper class to [[Object]] */
  final def getSuperClasses: Iterator[ClassType] = new Iterator[ClassType] { // TODO SUPER REFACTOR
    private var current = asClassType(getSuperClassSig)

    override def hasNext: Boolean = current != null

    override def next() = {
      if (!hasNext) throw new NoSuchElementException
      val result = current
      current = asClassType(current.getSuperClassSig)
      result
    }
  }

  def getCohenSupertype: ClassType

  final def getCohenSupertypes: Iterator[ClassType] = new Iterator[ClassType] {
    private var current = getCohenSupertype

    override def hasNext: Boolean = current != null

    override def next() = {
      if (!hasNext) throw new NoSuchElementException
      val result = current
      current = current.getCohenSupertype
      result
    }
  }

  /** Returns this type's level in Cohen display run-time structure. */
  def getCohenLevel: Int

  def getObjectHeaderSize: Int

  /** Size of heap object of this managed type. This size does not include allocator-specific additional required size. */
  def getHeapObjectSize: Int = alignUp(getUnalignedHeapObjectSize, RTConst.HeapObj.alignment)

  /** Computes the unaligned heap size of object. This size does not include allocator-specific additional required size. */
  def getUnalignedHeapObjectSize: Int = {
    assert(!isInterface)
    assert(!isAbstractClass)
    assert(!isUnverifiable)
    var size = getRawObjectSize
    if (finalizable) {
      size = alignUp(size, RTConst.ObjLink.alignment)
      size += RTConst.ObjLink.size
    }
    size
  }

  /** Returns size of an instance of this type without additional (heap) alignment at the end and
    * synthetic finalizer field. Works for managed classes and AJ Compounds.
    */
  def getRawObjectSize: Int

  /** Useful for AJ Struct to get info about needed alignment from javac. */
  def getObjectAlignment: Int

  /** Size of array object of this type with given length. */
  def getArrayObjectSize(length: Long, aligned: Boolean): Long = {
    assert(isArray)
    val elemType = getArrayElemType.symType
    val elemSize: Long = if (elemType.isRecord) {
      elemType.getRawObjectSize
    } else {
      elemType.size
    }

    val bodyOffs = getObjectHeaderSize
    val bodySize = bodyOffs + length * elemSize
    if (aligned) {
      alignUp(bodySize, getObjectAlignment)
    } else {
      bodySize
    }
  }

  def computeTSWordForArray(length: Long, isStackAlloc: Boolean): Int = {
    assert(isArray)
    val alignedSize = if (isStackAlloc) {
      0L
    } else {
      getArrayObjectSize(length, aligned = true)
    }
    val noRefFields = (length == 0) || hasNoRefFields
    computeTSWord(Math.toIntExact(alignedSize), hasFinalize = false, special = false, isArray = true, isStackAlloc, noRefFields, isGuest, hasCHA = false)
  }

  def hasNoRefFields: Boolean = {
    if (isArray) {
      val elemType = getArrayElemType
      elemType.isPrimitive || (elemType.isRecord && !elemType.symType.classHasRefFields)
    } else {
      !classHasRefFields
    }
  }

  def hasRefFields: Boolean = !hasNoRefFields

  def classHasRefFields: Boolean

  def computeTSWordForClass(isStackAlloc: Boolean, hasCHA: Boolean, special: Boolean): Int = {
    assert(!isArray)
    computeTSWord(getHeapObjectSize, finalizable, special, false, isStackAlloc,
      hasNoRefFields,
      isGuest, hasCHA
    )
  }

  // MUST be the same as in com.huawei.excelsior.jet.runtime.jobject.ObjTypeInfo#computeTSWord
  def computeTSWord(alignedSizeInBytes: Int, hasFinalize: Boolean, special: Boolean, isArray: Boolean, isStackAlloc: Boolean, noRefFields: Boolean, isGuest: Boolean, hasCHA: Boolean): Int

  /** Returns type handle symbol.
    * Should be used for comparison operations instead of other symbols.
    */
  def getTypeHandle: TypeHandleSymbol

  /** Returns instance descriptor symbol.
    * Requires this type to be prepared ahead of time.
    */
  def getInstanceDescriptor: InstanceDescriptorSymbol

  def getSingletonObject: SingletonObjectSymbol

  def isSingletonObject: Boolean

  /** Returns true if this class does not require preparation. */
  def isPrepared: Boolean

  def isClass = getKind == TypeKind.CLASS || getKind == TypeKind.THIN

  def isInterface = getKind == TypeKind.INTERFACE

  def isRecord = getKind == TypeKind.RECORD

  def isArraySlice = isRecord && getName.startsWith(ARRAY_SLICE_PREFIX)

  def isVArray: Boolean

  def isClassOrInterface = isClass || isInterface

  def isArray = getKind == TypeKind.ARRAY

  def isJavaArray = isArray && isJavaReference

  def isXScalaArray = isArray && isXScalaType

  def isJBCArray = isJavaArray || isXScalaArray

  def isAJArray: Boolean

  def isPrimitive = getKind.isPrimitive

  def isReference = getKind.isReference

  /** Exact analog of `Class.isAssignableFrom(Class)`.
    * Determines if this type is assignable from `type` according JLS.
    * Works both for primitive and reference types.
    *
    * Same as Scala's `type <: this`.
    */
  def isAssignableFrom(`type`: Type): Boolean

  def isAssignableFrom(`type`: SignatureType): Boolean = {
    isAssignableFrom(`type`.symType)
  }

  /** Returns `true` if this class either equals given `interfType` or implements it;
    * otherwise, returns `false` if `interfType` is `null` or this class doesn't implement it.
    */
  def doesImplement(interfType: ClassType): Boolean

  def doesImplement(interfType: SignatureType): Boolean = {
    doesImplement(asClassType(interfType))
  }

  def isJavaLangObject: Boolean

  def isHierarchyRoot: Boolean

  def isJavaLangCloneable: Boolean

  def isJavaIoSerializable: Boolean

  def isJavaLangSystem: Boolean

  def isJavaLangClassLoader: Boolean

  def isSunMiscUnsafe: Boolean

  def isXScalaAnyRef: Boolean

  def isAJLockable: Boolean = provider.getObjectType.isAssignableFrom(this) || provider.getLockableAJObjectType.isAssignableFrom(this)

  /** Check if this class is runtime anonymous class (i.e. lambda class). */
  def isAnonymous: Boolean

  def hasSequentialLayout: Boolean

  def getXName: XString

  final def getName = getXName.toString

  /** Returns name of a class prefixed with classloader id provider information, if any.
    * It is guarantied that provided name is unique for current project compilation.
    * Should be used for composing names for serialization of class-dependent information.
    */
  def getMangledName: String

  // Note: it is intentionally different from getName() to avoid confusion in future
  override final def toString = s"$kindString $getName"

  private def kindString: String = {
    if (isAJArray)        "AJ array"
    else if (isAJArray)   "Cangjie array"
    else if (isJavaArray) s"Java ${getArrayBase.kindString} array$getArrayDimnum"
    else if (isXScalaArray) s"XScala ${getArrayBase.kindString} array$getArrayDimnum"
    else if (isDeferred)  "deferred"
    else if (isInterface) "interface"
    else if (isThinClass) "thin"
    else if (isClass)     "class"
    else if (isPrimitive) "primitive"
    else                  "type"
  }

  /** Returns value of SourceFile attribute for classes/interfaces, or `null` if source file is not defined. */
  def getSourceFile: XString

  def getFileDescriptor: String = shouldNotCallThis()

  def setFileDescriptor(name: String): Unit = shouldNotCallThis()

  def hasFileDescriptor: Boolean = shouldNotCallThis()

  def setSourceFile(sourceFile: XString): Unit

  /** Returns source file full path when available or just [[getSourceFile]] otherwise. */
  def getSourceFilePath: XString = getSourceFile

  /** Get path to input file (i.e. bytecode). */
  def getInputFile: XString

  /** Returns [[AbstractVerifier.Info]] for this type or null if it is unavailable. */
  def getVerificationInfo: AbstractVerifier.Info

  def isJetRuntimeClass: Boolean

  def isSystemClass: Boolean

  def isJDKClass: Boolean

  def isOptimizedAggressively: Boolean

  /** Returns true iff `this` type has run-time type info. */
  def hasRunTimeTypeInfo: Boolean

  /** Returns true iff `this` type should be prepared. */
  def preparationRequired: Boolean

  def isBootstrapAnnotated: Boolean

  def isNonBootstrapAnnotated: Boolean

  def isCompilerInterface: Boolean

  /** Returns unique number of this type in global scope. */
  def getUniqueNumber: Int

  def getClassLoaderID: Int

  def getClassLoaderSID: String

  final def isTraceableReference = isReference && !isNamespace && !isValueClass && !isAJCompoundClass

  final def isJavaReference = isTraceableReference && !isAJManagedType && !isCangjieType && !isXScalaType

  def isLambdaClass: Boolean

  def isCangjieLambdaClass: Boolean

  def isEvacuatedType: Boolean

  def getLambdaInfo: LambdaInfo

  def isInfectedAJClass: Boolean

  /** Do NOT use this function!
    *
    * It is misleading and badly designed.
    * For example, it returns true for `@AJExtended` classes.
    */
  //@deprecated // TODO: rework isAJType both here and in o2 to exclude AJExtended classes from it
  final def isAJType = isAJCompoundClass || isValueClass || isNamespace || isAJManagedType || isAJExtendedType

  final def isAJCompoundClass = isStructClass || isThinClass || isAJCompoundBaseType

  protected def isAJCompoundBaseType: Boolean

  def isNamespace: Boolean

  def isValueClass: Boolean

  def isStructClass: Boolean

  def isThinClass: Boolean

  def isPolyThinClass: Boolean

  def isAJManagedType: Boolean

  def isAJExtendedType: Boolean

  def isGuest = !isAJManagedType && !isInfectedAJClass // TODO: consider changing to Java & Cangjie

  def isUltraThinClass = isThinClass && !isPolyThinClass

  def getClassBytes: Array[Byte]

  def size = getKind.size

  def alignment = getKind.alignment

  def log2Size = getKind.log2Size

  /** Cangjie package of the class. */
  def getCangjiePackage: ClassType

  final def isCangjiePackage = this == getCangjiePackage

  def isCangjieType: Boolean

  /** Returns whether this type is annotated with `@java['app']` or `@java['ext']` annotation in Cangjie. */
  def isJavaAnnotatedCangjieClass: Boolean

  /** Returns whether this type is annotated with `@java['ext']` annotation in Cangjie.
    *
    * Such classes can be loaded by extension classloaders and should not be referenced
    * from regular `@java['app']`-annotated classes and Cangjie code.
    *
    * TODO: use HLIR to distinguish @java['ext'] and @java classes
    */
  def isJavaExtAnnotatedClass: Boolean = isJavaAnnotatedCangjieClass

  /** Returns whether this type represents a synthetic helper class that contains Cangjie code for `@java`-annotated classes. */
  def isCangjieJavaHelper: Boolean

  def isCangjieArray: Boolean

  def isXScalaType: Boolean

  /** Get the Symbol for constant string */
  def getConstString(value: XString): ConstString

  def hasMain = false

  def getImportTable: collection.Map[ClassType, Integer]

  /** Returns debug type of `this` type if debug information exists. Otherwise, returns null. */
  def getDebugType: DebugType

  /** Set debug type for `this` type. */
  def setDebugType(debugType: DebugType): Unit

  /** Returns '/'-separated package name, or empty string if the class is not part of any package. */
  def getPackageName: String
}
