/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{CodeUnit, RTConst, TypeProvider}
import com.huawei.excelsior.jet.compiler.Env.{languagePack, targetArch}
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.ErrorAccessInfo
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.layout.{FieldsLayout, MethodTables, MethodTablesScala}
import com.huawei.excelsior.jet.compiler.o2lib.fe.*
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JBCPreprocessor
import com.huawei.excelsior.jet.compiler.o2lib.u.{ClassID, xiFilesModule}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{ExteriorVersioning, GenAOTReflectionInfo, GenDebug}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{fromSymType, Void as V}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.IntValue
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.TypeImpl.{TypeInfo, fromO2Type, typesInfo}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.VerifiableTypeImpl.asVerifiableTypeImpl
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.util.Names.AJ.mangleName
import com.huawei.excelsior.jet.compiler.util.Maps.Defaults.default
import com.huawei.excelsior.jet.compiler.util.CachedValue
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder
import com.huawei.excelsior.jet.compiler.verifier.{AbstractVerifier, VerifiableMethod, VerifiableType, VerificationError}
import com.huawei.excelsior.jet.util.Numbering
import xscala.collection.IdentityHashMap

import scala.collection.mutable

object VerifiableTypeImpl {
  def asVerifiableTypeImpl(cls: pcOModule.Class) = if (cls != null) new VerifiableTypeImpl(fromO2Type(cls)) else null
}

final class VerifiableTypeImpl(private val impl: TypeImpl) extends VerifiableType {
  override def isAJType = impl.isAJType

  override def isAnonymous = impl.isAnonymous

  override def isClass = impl.isClass

  override def isClassOrInterface = impl.isClassOrInterface

  override def isDeferred = impl.isDeferred

  override def isHierarchyRoot = impl.isHierarchyRoot

  override def isInterface = impl.isInterface

  override def isSamePackage(that: VerifiableType) = that match {
    case that: VerifiableTypeImpl => impl.isSamePackage(that.impl)
  }

  override def isUnloadable = impl.isUnloadable

  override def isAssignableFrom(`type`: VerifiableType): Boolean = `type` match {
    case typeImpl: VerifiableTypeImpl => impl.isAssignableFrom(typeImpl.impl)
  }

  def containsProtectedField(fieldName: XString, fieldSig: XString): Boolean = {
    require(fieldSig != null)
    assert(isClassOrInterface)
    assert(!isDeferred)
    val sig = env.parseSingleElementSignature(fieldSig)
    val f = impl.findFieldOrNull(fieldName, sig)
    f != null && f.isProtected
  }

  def containsProtectedMethod(methodName: XString, methodSig: XString): Boolean = {
    require(methodSig != null)
    assert(isClassOrInterface)
    assert(!isDeferred)
    val sig = env.parseMethodSignature(methodSig)
    val m = impl.findMethodOrNull(methodName, sig)
    m != null && m.isProtected
  }

  def getAbsentSuper: VerifiableType = {
    if (impl.isClassOrInterface && impl.asClass.hasAbsentSuper) {
      asVerifiableTypeImpl(impl.asClass.getAbsentSuper)
    }
    else {
      null
    }
  }

  override def getDeclaredMethods: Iterator[VerifiableMethod] =
    impl.getDeclaredMethods.map(method => new VerifiableMethodImpl(method.asInstanceOf[MethodImpl]))

  override def getDeclaredSuperInterfaces: Iterator[VerifiableType] =
    impl.asClass.getSuperInterfacesO2.map(intfc => asVerifiableTypeImpl(intfc).asInstanceOf[VerifiableType])

  override def getHostClass: VerifiableType = asVerifiableTypeImpl(impl.asClass.hostClass)

  override def getSuperClass: VerifiableType = {
    val s = impl.getSuperClass
    if (s != null) asVerifiableTypeImpl(typeToO2Class(s.symType)) else null
  }

  override def getClassConstantPool: ConstantPool = impl.getClassConstantPool

  override def getClassInheritanceLevel: Int = {
    assert(!impl.isThinClass)
    impl.asClass.getInheritanceLevel
  }

  override def getVerificationInfo: AbstractVerifier.Info = impl.getVerificationInfo

  override def hasVerificationInfo = impl.hasVerificationInfo

  override val getXName: XString = JBCPreprocessor.originalScalaClassName(impl.getXName)

  override def resolveTypeByName(tp: TypeProvider, name: XString): VerifiableType = {
    tp.resolveTypeByName(impl, name) match {
      case resolvedImpl: TypeImpl => new VerifiableTypeImpl(resolvedImpl)
    }
  }

  override def addVerificationPair(builder: InfoBuilder, to: VerifiableType, context: VerifiableMethod): Unit = to match {
    case to: VerifiableTypeImpl => builder.addVerificationPair(impl, to.impl, context)
  }

  override def addVerificationImport(importedType: VerifiableType): Unit = importedType match {
    case importedType: VerifiableTypeImpl => impl.asClass.addImport(importedType.impl.asClass)
  }

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: VerifiableTypeImpl => impl.equals(that.impl)
    case _ => false
  }

  override def hashCode() = impl.hashCode()
}

object TypeImpl {
  private val typesInfo = IdentityHashMap.empty[TypeImpl, TypeImpl.TypeInfo]

  private class TypeInfo (val `type`: TypeImpl) {
    val constantPool      = new CachedValue[ConstantPool](() => `type`.initConstantPool)
    val mtLayout          = new CachedValue[MethodTables.Layout](() => `type`.initMTLayout)
    val refFieldsOffsets  = new CachedValue[Array[Int]](() => FieldsLayout.getRefFieldOffsets(`type`)(LightweightEnvironment.getInstance))
    val declaredMethods   = new CachedValue[Array[Method]](() => `type`.initDeclaredMethods)
    val generatedMethods  = new CachedValue[Numbering[Method]](() => `type`.initGeneratedMethods)
    val declaredFields    = new CachedValue[Array[Field]](() => `type`.initDeclaredFields)
    val versionedMethods  = new CachedValue[mutable.LinkedHashMap[Method, CodeUnit]](() => `type`.initVersionedMethods)
  }

  private[light] def fromO2Type(o2type: pc.SymType): TypeImpl = o2type.symType

  private[light] def cleanTypeCacheDroppableData(): Unit = typesInfo.clear()

  private def o2TypeHashCode(t: pc.SymType): Int = t match {
    case t: pc.SymType.Primitive => t.hashCode
    case t: pcOModule.Class => t.mno
    case t: pc.SymType.Array => t.dim + 31 * o2TypeHashCode(t.arrayBaseType)
  }

  private def isArraySuper(arrayType: Type, tpe: Type) = {
    if (arrayType.isXScalaArray) {
      tpe.isXScalaAnyRef
    } else {
      assert(arrayType.isJavaArray)
      tpe.isJavaLangObject || tpe.isJavaLangCloneable || tpe.isJavaIoSerializable
    }
  }
}

final class TypeImpl private[light](val o2type: pc.SymType) extends ClassType with SymLevelObject with ConstantPool.Access[Type] with ErrorAccessInfo with AbstractVerifier.Info {

  protected implicit val provider: TypeProvider = LightweightEnvironment.getInstance

  private val kind = o2env.getTypeKind(o2type)

  private def getTypeInfo = typesInfo.getOrElseUpdate(this, new TypeInfo(this))

  private def initConstantPool = {
    assert(isClassOrInterface || isAJArray)
    assert(isBytecodeAvailable, "ConstantPool is unavailable for " + getName)
    new ConstantPoolImpl(this)
  }

  private def initMTLayout = MethodTablesScala.buildMTLayout(this, env)

  private def initVersionedMethods = {
    val versionedMethodsOfType = new mutable.LinkedHashMap[Method, CodeUnit]
    // TODO-DWARF not supported for original method that comes from .obj so CompileUnitInfo is not available for it
    if (env.enabled(ExteriorVersioning) && isClass && isVerifiable) { // TODO: JET-11849
      var idx = 0
      val superclass = asClassType(getSuperClassSig)
      for (i <- allSuperInterfaces) {
        if (superclass == null || !superclass.doesImplement(i)) {
          for (m <- i.getDeclaredMethods) {
            if (!m.isAbstract && m.shouldBeSerialized && MethodTables.canBeInMethodTable(m) &&
                (findMethodImplementation(m, true).contains(m) || findMethodImplementation(m, false).contains(m))
            ) {
              versionedMethodsOfType(m) = VersionedMethod(m, asClass, idx)
              idx += 1
            }
          }
        }
      }
    }
    versionedMethodsOfType
  }

  private def initDeclaredMethods = (asClass.declaredMethods map methodByO2Object).toArray ensuring { ms =>
    ms sameElements ms.sortBy(_.getMethodIndex)
  }

  private def initGeneratedMethods = Numbering(getDeclaredMethods filterNot (m => getO2Method(m).isUnstableForwarder))

  private def initDeclaredFields = (asClass.declaredFields map fieldByO2Object).toArray

  override def o2object: pc.SymType = o2type

  private def isPcOClass: Boolean = o2type.isInstanceOf[pcOModule.Class]

  private def ifPcOClass(predicate: pcOModule.Class => Boolean): Boolean = isPcOClass && predicate(asClass)

  private[light] def asClass = o2type.asInstanceOf[pcOModule.Class]

  private[light] def asArray = o2type.asInstanceOf[pc.SymType.Array]

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: TypeImpl => this.o2type == that.o2type
    case _ => false
  }

  override def hashCode = TypeImpl.o2TypeHashCode(o2type)

  override def getUniqueNumber = {
    assert(isClassOrInterface || isAJArray || isCangjieArray || isRecord)
    o2type.mno
  }

  override def getClassLoaderID = asClass.getClassloaderID

  override def getClassLoaderSID = {
    val clid = asClass.nameObj.getClassloaderID
    if (clid == null) null else clid.toString
  }

  override def isLambdaClass = isClass && asClass.isLambdaClass

  override def isCangjieLambdaClass: Boolean = (isClass || isInterface) && asClass.isCangjieLambdaBaseClass

  override def isEvacuatedType: Boolean = (isClass || isInterface) && asClass.isEvacuatedType

  override def getLambdaInfo = asClass.getLambdaInfo

  override def isInfectedAJClass = isClass && asClass.isInfectedAJClass

  override protected def isAJCompoundBaseType = isClass && O2TypeProvider.isAJCompoundType(asClass)

  override def isInCurrentCompilationSet = asClass.isInCompilationSet

  override def isBytecodeAvailable = o2env.isBytecodeAvailable(asClass)

  override def getClassConstantPool = getTypeInfo.constantPool.get()

  override def getAccessFlags = {
    assert(isClassOrInterface || isRecord || isAJArray || isCangjieArray)
    asClass.getAccessFlags.toInt
  }

  override def getCJModifiers = {
    assert(isCangjieType)
    Modifiers(asClass.getCJModifiers.toInt)
  }

  override def isAbstractClass = ifPcOClass(_.isAbstract)

  override def isFinal = isAJArray || isCangjieArray || ifPcOClass(_.isFinal)

  override def isCHIRDef = asClass.isCHIRDef

  override def hasDeferredSuper = if (isClassOrInterface) {
    asClass.hasDeferredSuper
  } else {
    false
  }

  override def isRealAbsentForAOT = if (isClassOrInterface || isRecord) {
    asClass.isAbsent || asClass.isBitcodeDeferred
  } else if (isJavaArray) {
    getArrayBase.isRealAbsentForAOT
  } else {
    false
  }

  override def isUnavailableForAOT = isClassOrInterface && asClass.hasAbsentSuper

  override def isUnloadable = isClassOrInterface && asClass.isClassDefinitionError

  override def isUnverifiable = isClassOrInterface && asClass.isNotVerifiedCode

  override def isSynthetic = isClassOrInterface && asClass.isSynthetic

  override def isPreClinited = asClass.isPreclinited

  override def isTurboClinited = asClass.isTurboClinited

  override def getClinit: Method = {
    val clinit = asClass.getClinit
    if (clinit == null) return null
    methodByO2Object(clinit)
  }

  override def finalizable: Boolean = {
    assert(!isInterface)

    if (isArray || !isVerifiable) {
      false

    } else if (isCangjieType) {
      // In Cangjie only final classes can have finalizer.
      getDeclaredMethods exists (_.isFinalize)

    } else if (isAJManagedType) {
      // managed AJ classes (successors of AJObject) do not have finalize methods
      this.allSuperInterfaces.iterator.contains(env.getFinalizableType)
    } else {
      val m = findMethodOrNull(XString("finalize"), MethodSignature()(V))
      m != null && !m.getDeclaringClass.isJavaLangObject && !m.getDeclaringClass.isXScalaAnyRef
    }
  }

  override def getKind = kind

  override def hasDeclaredSuperInterfaces = asClass.getSuperInterfacesCount > 0

  override def getDeclaredSuperInterfaces: Iterator[RefInterfaceType] = asClass.getSuperInterfaces

  override def getDeclaredMethods = getTypeInfo.declaredMethods.get().iterator

  override def dropDeclaredMethodsCache(): Unit = {
    getTypeInfo.declaredMethods.invalidate()
    getTypeInfo.generatedMethods.invalidate()
  }

  override def getGeneratedMethods = getTypeInfo.generatedMethods.get().order.iterator

  override def getGeneratedMethodIndex(method: Method) = {
    val generatedMethods = getTypeInfo.generatedMethods.get()
    if (generatedMethods contains method) generatedMethods.number(method) else -1
  }

  override def getDeclaredFields = getTypeInfo.declaredFields.get().iterator

  override def getCurrentDeclaredFields = initDeclaredFields.iterator

  override def dropDeclaredFieldsCache(): Unit = getTypeInfo.declaredFields.invalidate()

  override def getVersionedMethods = getTypeInfo.versionedMethods.get().values.iterator

  override def chooseMethodVersion(method: Method) = getTypeInfo.versionedMethods.get().getOrElse(method, CodeUnit.of(method))

  override def getArrayDimnum = {
    assert(!isAJArray && !isCangjieArray)
    asArray.dim
  }

  override def getArrayBase = if (isAJArray || isCangjieArray) {
    getArrayElemType.symType
  } else {
    typeByO2Object(asArray.arrayBaseType)
  }

  override def getArrayElemType: SignatureType = {
    if (isCangjieArray) {
      asClass.getCangjieArrayElementType

    } else if (isAJArray) {
      val kind = env.getAJArrayTypeKind(this)
      if (kind.isPrimitive) {
        SignatureType.Primitive(kind)
      } else {
        assert(kind eq TypeKind.CLASS)
        fromSymType(env.getAJObjectType)
      }

    } else {
      val dimnum = getArrayDimnum
      val base = getArrayBase
      if (dimnum == 1) {
        fromSymType(base)
      } else {
        fromSymType(env.getArrayType(base, dimnum - 1))
      }
    }
  }

  override def getVArrayLength: Long = {
    require(isVArray)
    val varrayName = getName
    varrayName.substring(1, varrayName.indexOf("$")).toLong
  }

  override def getVArrayElemType: SignatureType = {
    require(isVArray)
    asClass.getCangjieArrayElementType
  }

  override def getArraySliceElemType: SignatureType = {
    require(isArraySlice)
    asClass.getCangjieArrayElementType
  }

  override def getCangjieBoxValueType: SignatureType = {
    require(asClass.isCangjieBox)
    asClass.getCangjieBoxValueType
  }

  override def isSamePackage(that: ClassType) = asClass.isSamePackage(that.asInstanceOf[TypeImpl].asClass)

  override lazy val getSuperClass: RefClassType = {
    if (isThinClass && getThinInheritanceLevel <= -1) {
      // FIXME: introduce proper root for Thin hierarchy
      null
    } else if (isRecord) {
      null
    } else if (isNamespace) {
      null
    } else if (isValueClass) {
      val s = asClass.getSuperClass
      // In case of definition type value-classes, they will have abstract definition type superclass.
      // So we need to keep them all, but remove java.lang.Object
      if (s.symType.isJavaLangObject) null else s
    } else {
      asClass.getSuperClass
    }
  }

  override def getCohenSupertype = if (isJBCArray) {
    val baseType = getArrayBase
    val dimnum = getArrayDimnum
    def root = if (isXScalaArray) {
      env.getXScalaAnyRef
    } else {
      assert(isJavaArray)
      env.getObjectType
    }
    if (baseType.isPrimitive || baseType.isJavaLangObject || baseType.isXScalaAnyRef) {
      if (dimnum == 1) {
        root
      } else {
        env.getArrayType(root, dimnum - 1)
      }
    } else if (baseType.isInterface) {
      env.getArrayType(root, dimnum)
    } else {
      env.getArrayType(asClassType(baseType.getSuperClassSig), dimnum)
    }
  } else if (isCangjieArray) {
    env.getCangjieRefType
  } else if (isJavaLangObject) {
    if (languagePack.supports(JAVA)) {
      env.getJavaRefType
    } else {
      env.getLockableAJObjectType
    }
  } else {
    asClassType(getSuperClassSig)
  }

  override def getCohenLevel = if (isThinClass) {
    getThinInheritanceLevel
  } else if (isJBCArray || isCangjieArray) {
    getCohenSupertype.getCohenLevel + 1
  } else if (isAJManagedType || isCangjieType || isXScalaType) {
    asClass.getInheritanceLevel
  } else {
    assert(isJavaReference)
    assert(isClass)
    asClass.getInheritanceLevel + env.getJavaRefType.getCohenLevel + 1
  }

  override def getObjectHeaderSize = if (isClass) {
    asClass.getObjectHeaderSize
  } else if (isAJArray) {
    RTConst.AJArray.BODY_OFFS.intValue
  } else if (isCangjieArray) {
    RTConst.CangjieArray.BODY_OFFS.intValue
  } else if (isXScalaArray) {
    RTConst.ScalaArray.ARRAY_BODY_OFFS.intValue
  } else {
    assert(isJavaArray)
    RTConst.JavaArray.ARRAY_BODY_OFFS.intValue
  }

  // TODO: fight copy-paste with [[FieldsLayout#getRefFieldOffsets]]
  override def classHasRefFields: Boolean = {
    if (this.hasDeferredSuper) {
      return true // unknown, safely assume that it has
    }

    var c: ClassType = this
    while (c != null) {
      for (field <- c.getDeclaredFields) {
        if (!field.isStatic) {
          val fieldType = field.getType
          if (fieldType.isTraceableReference || fieldType.isRecord && asClassType(fieldType).classHasRefFields) {
            return true
          }
        }
      }
      c = asClassType(c.getSuperClassSig)
    }

    false
  }


  // MUST be the same as in com.huawei.excelsior.jet.runtime.jobject.ObjTypeInfo#computeTSWord
  override def computeTSWord(alignedSizeInBytes: Int, hasFinalize: Boolean, special: Boolean, isArray: Boolean, isStackAlloc: Boolean, noRefFields: Boolean, isGuest: Boolean, hasCHA: Boolean): Int = {

    def inclSize(alignedSizeInBytes: Long): Int = {
      val size: Int = Math.toIntExact(alignedSizeInBytes)
      assert((size & RTConst.HeapObj.TSWord.SIZE_MASK.intValue) == size)
      size
    }

    def inclIf(condition: Boolean, bitNum: Int): Int = if (condition) 1 << bitNum else 0

    var tags: Int = 0
    if (isStackAlloc) {
      // 'size' field = 0
      tags += RTConst.ObjTags.LOCATION_TYPE_OF_STACK_ALLOC_OBJECT.intValue
    } else if (alignedSizeInBytes <= RTConst.SmallAllocConfig.MAX_SMALL_OBJ_SIZE.intValue) {
      tags += inclSize(alignedSizeInBytes)
      tags += RTConst.ObjTags.LOCATION_TYPE_OF_SMALL_OBJECT.intValue
    } else if (alignedSizeInBytes < RTConst.AllocConfig.MIN_LARGE_OBJ_SIZE.intValue) {
      // 'size' field = 0
      tags += RTConst.ObjTags.LOCATION_TYPE_OF_NORMAL_OBJECT.intValue
    } else {
      // 'size' field = 0
      tags += RTConst.ObjTags.LOCATION_TYPE_OF_LARGE_OBJECT.intValue
    }

    tags += inclIf(special, RTConst.ObjTag.SPECIAL_OBJECT.intValue)
    tags += inclIf(noRefFields, RTConst.ObjTag.NO_TRACEABLE_FIELDS.intValue)
    tags += inclIf(isGuest, RTConst.ObjTag.GUEST.intValue)
    tags += inclIf(hasCHA, RTConst.ObjTag.CHA_BIT.intValue)
    tags += inclIf(isArray, RTConst.ObjTag.ARRAY.intValue)

    tags
  }

  override def getRawObjectSize = {
    // Size of an instance of this type. Works for Java classes and AJ Struct.
    // Returns size without additional (heap) alignment at the end and synthetic finalizer field.
    if (targetArch == CBC && !asClass.instanceLayoutIsNumerated) {
      0
    } else if (isRecord) {
      asClass.getInstanceSize0
    } else {
      // Hacks for RTConst resolve
      if (!asClass.instanceLayoutIsNumerated) {
        NumerateModule.processClass(asClass)
      }
      asClass.size
    }
  }

  override def getObjectAlignment = if (isArray) {
    RTConst.HeapObj.alignment
  } else {
    assert(isPcOClass)
    if (isRecord) {
      asClass.getInstanceAlignment0
    } else {
      // Hacks for RTConst resolve
      if (!asClass.instanceLayoutIsNumerated) {
        NumerateModule.processClass(asClass)
      }
      asClass.alignment
    }
  }

  override def getThinTypeHandle = {
    assert(!isArray)
    assert(!isDeferred)
    new ThinTypeHandleSymbolImpl(this)
  }

  override def getTypeHandle = new TypeHandleSymbolImpl(this)

  override def getInstanceDescriptor = new InstanceDescriptorSymbolImpl(this)

  override def getSingletonObject = new SingletonObjectSymbolImpl(this)

  override def isSingletonObject = ifPcOClass(_.isSingletonObject)

  override def isPrepared = false

  override def isVArray = ifPcOClass(_.isVArray)

  override def isAJArray = ifPcOClass(_.isAJArray)

  override def getXName = {
    // Note: NonNullType will have the same name as its base type, it is simpler this way.
    //       Can still distinguish them via signature (getDescriptor).
    o2name(o2type)
  }

  override def getMangledName: String = {
    if (isJBCArray) {
      val mangledName = new java.lang.StringBuilder(getArrayBase.getMangledName)
      for (_ <- 0 until getArrayDimnum) {
        mangledName.append("[]")
      }
      return mangledName.toString
    }
    assert(isClassOrInterface || isAJArray || isCangjieArray || isRecord)
    asClass.getMangledName.toString
  }

  override def doesImplement(interfType: ClassType) = {
    assert(interfType.isInterface)
    assert(this.isClassOrInterface)
    this.asClass.isInheritedFromInterface(interfType.asInstanceOf[TypeImpl].asClass)
  }

  override def isJavaLangObject = O2TypeProvider.isJavaLangObject(o2type)

  override def isHierarchyRoot = isClass && asClass.isHierarchyRoot

  override def isJavaLangCloneable = O2TypeProvider.isJavaLangCloneable(o2type)

  override def isJavaIoSerializable = O2TypeProvider.isJavaIoSerializable(o2type)

  override def isJavaLangSystem = O2TypeProvider.isJavaLangSystem(o2type)

  override def isJavaLangClassLoader = O2TypeProvider.isJavaLangClassLoader(o2type)

  override def isSunMiscUnsafe = O2TypeProvider.isSunMiscUnsafe(o2type)

  override def isXScalaAnyRef = O2TypeProvider.isXScalaAnyRef(o2type)

  override def isAnonymous = asClass.isAnonymous

  override def hasSequentialLayout = O2TypeProvider.isAJObject(o2type) || O2TypeProvider.isLockableAJObject(o2type) || isRecord

  // TODO: split TypeImpl and ErrorAccess
  override def getResult = if (o2env.isTypeErroneous(o2type)) {
    ConstantPoolAccessResult.ERROR
  } else if (o2env.isTypeDeferred(o2type)) {
    shouldNotReachHere()
  } else {
    ConstantPoolAccessResult.OK
  }

  // Scalac required explicit implementation because there are two methods with such name: one in `Type` and the other
  // in `ConstantPool.Access`.
  override def isDeferred = super[ClassType].isDeferred

  override def getObject = this

  override def getError = this

  override def getDeferredInfo = shouldNotCallThis()

  override def getThrowProc = o2env.getTypeThrowProc(o2type)

  override def getErrorMessage = o2env.getTypeThrowMessage(o2type)

  /** Returns value of SourceFile attribute for classes/interfaces, or {@code null} if source file is not defined. */
  override def getSourceFile = {
    assert(isClassOrInterface || isRecord)
    asClass.getBCSourceName
  }

  override def setSourceFile(sourceFile: XString): Unit = {
    assert(isClassOrInterface || isRecord || isCangjieArray)
    asClass.setBCSourceName(sourceFile)
  }

  override def getFileDescriptor = {
    val fd = asClass.fileDescriptor
    if (fd != null) fd.getName.toString else null
  }

  override def setFileDescriptor(name: String): Unit =
    asClass.fileDescriptor = xiFilesModule.sys.createFileDescriptor(XString(name))

  override def hasFileDescriptor: Boolean = asClass.isInCompilationSet

  override def getInputFile = {
    assert(isClassOrInterface)
    asClass.fileDescriptor.getName
  }

  override def getRefFieldOffsets = getTypeInfo.refFieldsOffsets.get()

  override def getMTLayout = getTypeInfo.mtLayout.get()

  override def getVerificationInfo = if (asClass.isUnavailable) null else this

  override def getVerifyError: VerificationError = {
    val o2VerifyError = asClass.getVerifyError
    if (o2VerifyError == null) {
      return null
    }
    new VerificationError(o2VerifyError.errmsg, VerificationError.ErrorKind.CLASSLOADING_ERROR, o2VerifyError.errcode)
  }

  override def isJetRuntimeClass = asClass.isJetRuntimeClass

  override def isSystemClass = asClass.isSystemClass

  override def isJDKClass = asClass.isJDKClass

  override def isOptimizedAggressively = asClass.isOptimizedAggressively

  override def hasRunTimeTypeInfo = isArray || isPrimitive || asClass.hasManagedMetaInformation

  override def preparationRequired = if (isJavaArray) {
    val baseType = getArrayBase
    val bootstrapPrepared = getArrayDimnum == 1 && (baseType.isPrimitive || baseType.isJavaLangObject)
    !bootstrapPrepared
  } else if (isXScalaArray) {
    true
  } else if (isAJArray || isCangjieArray) {
    !isBootstrapAnnotated
  } else if (isRecord) {
    true
  } else {
    isClassOrInterface && hasRunTimeTypeInfo
  }

  override def isBootstrapAnnotated = ifPcOClass(_.isBootstrap)

  override def isNonBootstrapAnnotated = ifPcOClass(_.isNonBootstrap)

  override def isCompilerInterface = O2TypeProvider.isCompilerInterface(o2type)

  override def isAssignableFrom(that: Type): Boolean = {
    assert(that != null)
    if (this eq that) return true
    if (that.isJBCArray) {
      if (this.isJBCArray) {
        val typeDim = that.getArrayDimnum
        val thisDim = this.getArrayDimnum
        val typeBase = that.getArrayBase
        val thisBase = this.getArrayBase
        if (typeDim == thisDim) {
          return thisBase.isAssignableFrom(typeBase)
        } else if (typeDim > thisDim) {
          return TypeImpl.isArraySuper(that, thisBase)
        } else {
          return false
        }
      } else {
        return TypeImpl.isArraySuper(that, this)
      }
    } else if (this.isJBCArray) {
      return false
    }
    if (this.isPrimitive || that.isPrimitive) {
      return false
    }
    that.asInstanceOf[TypeImpl].asClass.isSubType(this.asClass)
  }

  override def isNamespace = isClass && asClass.isNamespace

  override def isValueClass = isClass && asClass.isValueClass

  override def isStructClass = isClass && asClass.isStructClass

  override def isThinClass = isClass && asClass.isThinClass

  override def isPolyThinClass = isThinClass && asClass.isPolyThinClass

  override def isAJManagedType = ifPcOClass(_.isAJManagedType)

  override def isAJExtendedType = ifPcOClass(_.isAJExtended)

  override def getThinInheritanceLevel = {
    assert(isThinClass)
    asClass.getThinInheritanceLevel
  }

  override def getClassBytes = {
    val fileDescriptor = asClass.fileDescriptor
    if (fileDescriptor != null) fileDescriptor.getFileContents else null
  }

  override def isInterpreterInternals = asClass.isInterpreterInternals

  override def isVerifiable = asClass.isVerifiable

  override def getCangjiePackage: ClassType = {
    val m = asClass.cangjiePackage
    if (m != null) classByO2Object(m) else null
  }

  override def isCangjieType = ifPcOClass(_.isCangjieType)

  override def isJavaAnnotatedCangjieClass = ifPcOClass(_.isJavaAnnotatedCangjieClass)

  override def isCangjieJavaHelper = ifPcOClass(_.isCangjieJavaHelper)

  override def isCangjieArray = ifPcOClass(_.isCangjieArray)

  override def isXScalaType = {
    if (languagePack == LanguagePack.SCALA) {
      // Note: XScala is not compatible with Java
      // TODO: distinguish XScala arrays from Java arrays in o2 to allow Scala+Java language pack
      o2type match {
        case _: pc.SymType.Array => true
        case c: pcOModule.Class => c.isXScalaType
        case _ => false
      }
    } else {
      false
    }
  }

  override def getConstString(value: XString) = {
    val stringTable = asClass.getStringTable
    var strnum = stringTable.getIndexByStringIfPresent(value)
    if (strnum == -1) {
      strnum = stringTable.addString(value)
    }
    constStringByHostAndStringNumber(asClass, strnum)
  }

  override def hasMain = isClass && asClass.hasMain

  // FIXME: CBC-only
  private val importTable = mutable.LinkedHashMap.empty[ClassType, Integer]

  private[light] def getImportedClassIdx(importedClass: ClassType): Int = {
    assert(targetArch == CBC)
    val newIdx = importTable.size
    importTable.getOrElseUpdate(importedClass, newIdx)
  }

  override def getImportTable: collection.Map[ClassType, Integer] = {
    assert(targetArch == CBC || env.enabled(GenAOTReflectionInfo))
    importTable
  }

  override def getDebugType = asClass.getLLVMDebugType

  override def setDebugType(tpe: DebugType): Unit = asClass.setLLVMDebugType(tpe)

  override def getPackageName: String = asClass.getPackageName.toString

  override def getCJAnnotationFactory: Method = {
    val factory = asClass.getCJAnnotationFactory
    if (factory == null) {
      null
    } else {
      methodByO2Object(factory)
    }
  }

  override def isUniversalGeneric = asClass.isUniversalGeneric

  override def getGenericInfo = asClass.getGenericInfo

  override def getCHIRVTable = asClass.getCHIRVTable.get

}
