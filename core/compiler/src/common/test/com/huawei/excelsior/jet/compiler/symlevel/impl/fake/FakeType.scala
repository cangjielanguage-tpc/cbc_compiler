/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.classfile.NameAndSigComparable
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.{DeferredAccessInfo, ErrorAccessInfo}
import com.huawei.excelsior.jet.compiler.cangjie.{CHIRVTable, CangjieSymLevelMaker}
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.layout.{FieldsLayout, MethodTables, MethodTablesScala}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.verifier.{AbstractVerifier, VerificationError}
import com.huawei.excelsior.jet.compiler.{CodeUnit, TypeProvider}
import com.huawei.excelsior.jet.util.Numbering

import scala.collection.mutable

/** Class declaration.
  *
  * @author cypok
  */
object FakeType {
  private val cache = new mutable.HashMap[Class[?], FakeType]

  def getCreatedTypes: Seq[FakeType] = Seq.from(cache.values)

  def create(clazz: Class[?]): FakeType = {
    if (clazz == null) return null
    if (cache.contains(clazz)) return cache(clazz)

    val kind = if (clazz.isInterface) {
      TypeKind.INTERFACE
    } else if (clazz.isArray) {
      TypeKind.ARRAY
    } else if (clazz.isPrimitive) {
      // FIXME: This doesn't work correctly for Character.class and Integer.class.
      //        Moreover, the types here differ to the ones created by FakeEnvironment.getPrimitiveType,
      //        which might lead to problems with some unit-tests.
      TypeKind.valueOf(clazz.getName.toUpperCase)
    } else {
      TypeKind.CLASS
    }
    val `type` = new FakeType(clazz, clazz.getName.replace('.', '/'), kind, null)

    if (kind == TypeKind.ARRAY) {
      val arrayElemType = create(clazz.getComponentType)
      `type`.setArrayElemType(arrayElemType)
    }

    cache.put(clazz, `type`)

    `type`.setSuperClass(create(clazz.getSuperclass))
    `type`.setImplementedInterfaces(clazz.getInterfaces.map(create).toSeq*)

    // TODO: add fields and don't forget to sort them
    for (constr <- clazz.getDeclaredConstructors if !isSynthetic(constr)) { // TODO: sort constructors by sig
      `type`.addMethod(FakeMethod.apply(constr))
    }

    for (method <- clazz.getDeclaredMethods.sortBy(_.getName) if !isSynthetic(method)) {
      `type`.addMethod(FakeMethod.apply(method))
    }

    for (field <- clazz.getDeclaredFields.sortBy(_.getName) if !isSynthetic(field)) {
      `type`.addField(FakeField.apply(field))
    }

    `type`
  }

  private def isSynthetic(m: java.lang.reflect.Member) = {
    val SYNTHETIC = 0x00001000
    (m.getModifiers & SYNTHETIC) != 0
  }

  private var fakeCounter = 0
  private def fakeName(): String = {
    val newIdx = fakeCounter
    fakeCounter += 1
    s"$$type$newIdx"
  }

  private val fakeCache = new mutable.HashMap[String, FakeType]

  def apply(name: String, kind: TypeKind, superType: FakeType, implementedInterfaces: FakeType*): FakeType = {
    fakeCache.getOrElseUpdate(name, new FakeType(null, name, kind, superType, implementedInterfaces*))
      .ensuring(t => t.kind == kind, s"$name: $kind")
  }

  def apply(name: String = fakeName(), kind: TypeKind = TypeKind.CLASS): FakeType =
    apply(name, kind, null)

  def apply(kind: TypeKind): FakeType =
    apply(fakeName(), kind)
}

class FakeType private (val klass: Class[?], name: String, private val kind: TypeKind, superType: FakeType, implementedInterfaces: FakeType*)
  extends ClassType with ConstantPool.Access[Type] {

  protected implicit def provider: TypeProvider = new FakeEnvironment
  protected var typeKind: TypeKind = _
  private var typeInfo: FakeTypeInfo = _
  private var inheritanceLevel = 0
  protected var superClass: ClassType = _
  var implInterfs = Seq.empty[FakeType]

  setKind(kind)
  setTypeInfo(new FakeTypeInfo(name))
  if (isClass) setSuperClass(superType)
  if (isClassOrInterface) setImplementedInterfaces(implementedInterfaces*)

  override def getXName: XString = XString.ascii(name)

  override def getMangledName: String = name

  override def isUnloadable = false
  override def isSynthetic = false
  override def isInCurrentCompilationSet = true
  override def isBytecodeAvailable = false

  private def setImplementedInterfaces(implementedInterfaces: FakeType*): Unit = {
    implInterfs = implementedInterfaces
  }
  override def doesImplement(interfType: ClassType): Boolean = {
    assert(isClassOrInterface)
    this == interfType || implInterfs.exists(_.doesImplement(interfType)) ||
      (superClass != null) && superClass.doesImplement(interfType)
  }

  override def isJavaLangObject = name == "java/lang/Object"
  override def isHierarchyRoot = isJavaLangObject || name == "com/huawei/excelsior/aj/lang/AJObject" || name == "com/huawei/excelsior/aj/lang/ThinType"
  override def isJavaLangCloneable = name == "java/lang/Cloneable"
  override def isJavaIoSerializable = name == "java/io/Serializable"
  override def isJavaLangSystem = shouldNotCallThis()
  override def isJavaLangClassLoader = shouldNotCallThis()
  override def isSunMiscUnsafe = shouldNotCallThis()

  override def isXScalaAnyRef = false

  override def isAnonymous = false

  override def hasSequentialLayout = false

  private var constantPool: ConstantPool = _
  override def getClassConstantPool = constantPool
  def setClassConstantPool(constantPool: ConstantPool): FakeType = {
    this.constantPool = constantPool
    this
  }

  private var preClinited = true
  override def isPreClinited = preClinited
  def withClinit(hasClinit0: Boolean): FakeType = {
    this.preClinited = !hasClinit0
    this
  }

  private var turboClinited = false
  override def isTurboClinited = turboClinited
  def markTurboClinited(): FakeType = {
    withClinit(true)
    turboClinited = true
    this
  }

  override def getClinit: Method = null

  override def finalizable = false

  override def getKind = typeKind
  private def setKind(typeKind: TypeKind): Unit = {
    this.typeKind = typeKind
  }

  override def hasDeclaredSuperInterfaces = implInterfs.nonEmpty

  override def getDeclaredSuperInterfaces = implInterfs.iterator.map(t => RefInterfaceType(t.asInstanceOf[ClassType]))

  private val declaredMethods = mutable.LinkedHashMap.empty[NameAndSigComparable, FakeMethod]
  def clearMethods(): Unit = {
    declaredMethods.clear()
  }
  def addMethod(method: FakeMethod): Unit = {
    method.setDeclaringClass(this)
    method.setMethodIndex(declaredMethods.size)
    declaredMethods.put(NameAndSigComparable.of(method.getXName, XString.ascii(method.getSignature.toJETSignature)), method)
  }
  def method(name: String) = declaredMethods(NameAndSigComparable.of(XString.ascii(name), null))
  def hasMethod(name: String) = declaredMethods.contains(NameAndSigComparable.of(XString.ascii(name), null))

  private[fake] def declaredFakeMethods: Iterator[FakeMethod] = declaredMethods.valuesIterator
  override def getDeclaredMethods: Iterator[Method] = declaredFakeMethods.map(_.asInstanceOf[Method])
  def getMethodOrNull(methodIndex: Int): Method = declaredFakeMethods.drop(methodIndex).nextOption().orNull

  override def dropDeclaredMethodsCache(): Unit = shouldNotCallThis("dropDeclaredMethodsCache")

  override def getGeneratedMethods: Iterator[Method] = getDeclaredMethods

  override def getGeneratedMethodIndex(method: Method): Int = getDeclaredMethods.indexOf(method)

  private val declaredFields = mutable.LinkedHashMap.empty[XString, FakeField].withDefaultValue(null)
  def addField(field: FakeField): Unit = {
    field.setDeclaringClass(this)
    declaredFields.put(field.getXName, field)
  }

  override def getDeclaredFields: Iterator[Field] = declaredFields.valuesIterator.map(_.asInstanceOf[Field])

  override def getCurrentDeclaredFields: Iterator[Field] = shouldNotCallThis("getCurrentDeclaredField")

  override def dropDeclaredFieldsCache(): Unit = shouldNotCallThis("dropDeclaredFieldsCache")

  override def getVersionedMethods: Iterator[CodeUnit] = Iterator.empty

  override def chooseMethodVersion(method: Method): CodeUnit = CodeUnit.of(method)

  private var arrayElem: Type = _

  def setArrayElemType(arrayElem: Type): FakeType = {
    this.arrayElem = arrayElem
    this
  }

  override def getArrayDimnum =
    if (arrayElem == null) 0
    else if (arrayElem.isJavaArray) arrayElem.getArrayDimnum + 1
    else 1

  override def getArrayBase: Type =
    if (arrayElem == null) null
    else if (arrayElem.isJavaArray) arrayElem.getArrayBase
    else arrayElem

  override def getArrayElemType: SignatureType = SignatureType.fromSymType(arrayElem)

  override def getVArrayLength = shouldNotReachHere()

  override def getVArrayElemType = shouldNotReachHere()

  override def getArraySliceElemType = {
    JETSignatureParser.parse(getName.stripPrefix(CangjieSymLevelMaker.ARRAY_SLICE_PREFIX)).asInstanceOf[SignatureType]
  }

  override def getCangjieBoxValueType = shouldNotReachHere()

  override def isSamePackage(that: ClassType) = {
    val name1 = this.getName
    val name2 = that.getName
    val p1 = name1.lastIndexOf('/')
    val p2 = name2.lastIndexOf('/')
    (p1 == p2) && ((p1 == -1) || name1.startsWith(name2.substring(0, p1)))
  }

  override def getAccessFlags = 0

  override def getCJModifiers = Modifiers(0)

  private var abstractClass = false
  override def isAbstractClass = abstractClass
  def setAbstractClass(abstractClass: Boolean): FakeType = {
    this.abstractClass = abstractClass
    this
  }

  private var finalClass = false
  override def isFinal = finalClass
  def setFinal(finalClass: Boolean): FakeType = {
    this.finalClass = finalClass
    this
  }

  override def isCHIRDef = shouldNotCallThis()

  private var deferred = false
  override def isDeferred = deferred
  def setDeferred(): FakeType = {
    deferred = true
    inheritanceLevel = 0
    this
  }

  override def hasDeferredSuper = false

  override def isRealAbsentForAOT = shouldNotCallThis()
  override def isUnavailableForAOT = shouldNotCallThis()

  override def getSuperClass = if (superClass == null) null else RefClassType(superClass)
  override def getCohenSupertype: ClassType = superClass
  private def setSuperClass(superClass: FakeType): Unit = {
    this.superClass = superClass
    if (superClass != null) setClassInheritanceLevel(superClass.getClassInheritanceLevel + 1)
  }

  def getClassInheritanceLevel = inheritanceLevel
  private def setClassInheritanceLevel(inheritanceLevel: Int): Unit = {
    this.inheritanceLevel = inheritanceLevel
  }

  override def getCohenLevel = inheritanceLevel

  override def getObjectHeaderSize = shouldNotCallThis()

  private var rawObjectSize: Option[Int] = None
  override def getRawObjectSize = {
    rawObjectSize.getOrElse(shouldNotCallThis())
  }

  def setRawObjectSize(size: Int): FakeType = {
    rawObjectSize = Some(size)
    this
  }

  override def getObjectAlignment = shouldNotCallThis()

  override def classHasRefFields = shouldNotCallThis()
  override def computeTSWord(alignedSizeInBytes: Int, hasFinalize: Boolean, special: Boolean, isArray: Boolean, isStackAlloc: Boolean, noRefFields: Boolean, isGuest: Boolean, hasCHA: Boolean) = shouldNotCallThis()
  override def getThinTypeHandle = shouldNotCallThis()

  def getTypeInfo: FakeTypeInfo = typeInfo
  private def setTypeInfo(typeInfo: FakeTypeInfo): Unit = {
    this.typeInfo = typeInfo
  }

  override def getTypeHandle = getTypeInfo
  override def getInstanceDescriptor = getTypeInfo

  override def getSingletonObject = shouldNotCallThis()

  override def isSingletonObject = false

  override def isPrepared = false

  override def isVArray = false

  override def isAJArray = isArray && isAJManagedType

  override def getResult: ConstantPoolAccessResult = if (deferred) ConstantPoolAccessResult.DEFERRED else ConstantPoolAccessResult.OK

  override def getObject: ClassType = this
  override def getError: ErrorAccessInfo = shouldNotCallThis()
  override def getDeferredInfo: DeferredAccessInfo = shouldNotCallThis()

  override def getSourceFile: XString = null

  override def setSourceFile(sourceFile: XString): Unit = {}

  override def getInputFile: XString = null

  override lazy val getRefFieldOffsets =
    FieldsLayout.getRefFieldOffsets(this)(new FakeEnvironment)

  override lazy val getMTLayout: MethodTables.Layout =
    MethodTablesScala.buildMTLayout(this, new FakeEnvironment)

  override def getVerificationInfo: AbstractVerifier.Info = if (deferred) {
    null
  } else {
    new AbstractVerifier.Info() {
      override def getVerifyError: VerificationError = null
    }
  }

  override def isJetRuntimeClass = false
  override def isSystemClass = false
  override def isJDKClass = false
  override def isOptimizedAggressively = false
  override def hasRunTimeTypeInfo = true
  override def preparationRequired = true
  override def isBootstrapAnnotated = false
  override def isNonBootstrapAnnotated = false
  override def isCompilerInterface = false
  override def getUniqueNumber = 0
  override def getClassLoaderID = 0

  override def isAssignableFrom(`type`: Type): Boolean = {
    if (this == `type`) return true

    if (this.isPrimitive || `type`.isPrimitive) return false

    if (this.isJavaLangObject) return true

    if (`type`.isJavaArray) {
      assert(this.isClassOrInterface, "not implemented yet")

      // Note: java/lang/Object was checked earlier.
      return this.isJavaLangCloneable || this.isJavaIoSerializable

    } else if (this.isJavaArray) {
      return false
    }

    if (this.isInterface) {
      `type`.doesImplement(this)
    } else {
      assert(this.isClass)
      `type`.getSuperClasses.contains(this)
    }
  }

  override def getClassLoaderSID: String = null

  override def isLambdaClass = false
  override def isCangjieLambdaClass = false

  private var evacuatedType = false
  override def isEvacuatedType = evacuatedType

  def markAsEvacuatedType(): FakeType = {
    evacuatedType = true
    this
  }

  override def getLambdaInfo = null
  override def isInfectedAJClass = false
  override def isAJCompoundBaseType = false
  override def isNamespace = false
  override def isValueClass = false
  override def isStructClass = false

  private var thinClass = false
  override def isThinClass = thinClass
  def markAsThinClass(): FakeType = {
    thinClass = true
    this
  }

  private var polyThinClass = false
  override def isPolyThinClass = polyThinClass
  def markAsPolyThinClass(): FakeType = {
    polyThinClass = true
    this
  }

  private var ajManagedType = false
  override def isAJManagedType = ajManagedType
  def markAsAJManagedType(): FakeType = {
    ajManagedType = true
    this
  }

  private var ajExtendedType = false
  override def isAJExtendedType = ajExtendedType
  def markAsAJExtendedType(): FakeType = {
    ajExtendedType = true
    this
  }

  private var cangjieType = false
  override def isCangjieType = cangjieType
  def markAsCangjieType(): FakeType = {
    cangjieType = true
    this
  }
  def unmarkAsCangjieType(): FakeType = {
    cangjieType = false
    this
  }

  override def getThinInheritanceLevel = getClassInheritanceLevel

  override def getClassBytes: Array[Byte] = null
  override def isInterpreterInternals: Boolean = shouldNotCallThis()
  override def isVerifiable: Boolean = shouldNotCallThis()
  override def getCangjiePackage: ClassType = null
  override def isJavaAnnotatedCangjieClass = false
  override def isCangjieJavaHelper = false

  override def isCangjieArray = false
  override def isXScalaType = false

  override def getConstString(value: XString): ConstString = shouldNotCallThis()
  override def getImportTable = Map.empty[ClassType, Integer] // empty table is OK for now
  override def getDebugType: DebugType = null
  override def setDebugType(tpe: DebugType): Unit = shouldNotCallThis()
  override def getPackageName: String = {
    val name = this.getName
    val index = name.lastIndexOf('/')
    if (index == -1) {
      return ""
    }
    name.substring(0, index)
  }
  override def getCJAnnotationFactory: Method = null

  override def isUniversalGeneric: Boolean = false

  override def getGenericInfo: GenericInfo = null

  override def getCHIRVTable: CHIRVTable = null
}
