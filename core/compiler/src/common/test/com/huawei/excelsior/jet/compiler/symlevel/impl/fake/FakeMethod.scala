/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{ConstantPool, ConstantPoolAccessResult}
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool.{DeferredAccessInfo, ErrorAccessInfo}
import com.huawei.excelsior.jet.compiler.driver.CompilationMode
import com.huawei.excelsior.jet.compiler.driver.CompilationMode.O2
import com.huawei.excelsior.jet.compiler.intrinsics.{IntrinsicWithBody, IntrinsicWithoutBody}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.symlevel.*

import java.lang.reflect.{Constructor, Executable, Modifier}

/** Method with setters for all getters
  *
  * @author cypok
  */
object FakeMethod {
  def apply(method: Executable): FakeMethod = {
    val isConstructor = method.isInstanceOf[Constructor[?]]
    val methodName = if (isConstructor) "<init>" else method.getName
    new FakeMethod(methodName, FakeMethodType.create(method))
      .setJavaModifiers(method.getModifiers)
      .setConstructor(isConstructor)
  }
}

class FakeMethod(name: String = "fake", private var methodType: MethodType = FakeMethodType.create()) extends Method with ConstantPool.Access[Method] {
  def this(_methodType: MethodType) = this(methodType = _methodType)

  private var javaModifiers = 0

  setJavaModifier(isStaticInMT, Modifier.STATIC)
  setJavaModifier(value = true, Modifier.PUBLIC)

  override def getXName = XString.ascii(name)

  private var methodReference: FakeMethodReference = _

  override def isDeferred = super.isDeferred

  def getMethodReference = {
    if (methodReference == null) methodReference = new FakeMethodReference(this)
    methodReference
  }
  def setMethodReference(methodReference: FakeMethodReference): FakeMethod = {
    this.methodReference = methodReference
    this
  }

  private var constructor = false
  override def isConstructor = constructor
  def setConstructor(constructor: Boolean): FakeMethod = {
    this.constructor = constructor
    this
  }

  override def isThinConstructor = false

  override def isRecordConstructor = false

  override def isExternal = false

  override def isExported = false

  override def isClinit = false

  override def isPackageInit = false

  override def isPackageLiteralInit = false

  override def isGlobalInit = false

  override def isMutWrapper = false

  private def calcDeclaredMethodIndex: Int = {
    getDeclaringClass.getDeclaredMethods.indexOf(this)
  }

  private var methodIndex = -1
  override def getMethodIndex = {
    if (methodIndex == -1) methodIndex = calcDeclaredMethodIndex
    methodIndex
  }
  private[fake] def setMethodIndex(methodIndex: Int): FakeMethod = {
    this.methodIndex = methodIndex
    this
  }

  override def getJavaModifiersValue = javaModifiers
  def setJavaModifiers(javaModifiers: Int): FakeMethod = {
    this.javaModifiers = javaModifiers
    setStaticInMT((javaModifiers & Modifier.STATIC) != 0)
    this
  }

  private def setJavaModifier(value: Boolean, mask: Int): FakeMethod = {
    if (value) {
      javaModifiers |= mask
    } else {
      javaModifiers &= ~mask
    }
    this
  }

  override def getCJModifiers = Modifiers.EMPTY

  def setAbstract(value: Boolean): FakeMethod = setJavaModifier(value, Modifier.ABSTRACT)
  def setFinal(value: Boolean): FakeMethod = setJavaModifier(value, Modifier.FINAL)
  def setPublic(value: Boolean): FakeMethod = setJavaModifier(value, Modifier.PUBLIC)
  def setPrivate(value: Boolean): FakeMethod = setJavaModifier(value, Modifier.PRIVATE)

  override def isStatic = {
    val res = super.isStatic
    assert(res == isStaticInMT)
    res
  }

  def setStatic(value: Boolean): FakeMethod = {
    setJavaModifier(value, Modifier.STATIC)
    setStaticInMT(value)
    this
  }

  private def isStaticInMT = !getMethodType.hasReceiverParameter
  private def setStaticInMT(staticFlag: Boolean): Unit = {
    (methodType.hasReceiverParameter, staticFlag) match {
      case (true, true)   => methodType = methodType.dropReceiverParameter
      case (false, false) => methodType = methodType.insertReceiverType(SignatureType.fromSymType(getDeclaringClass), shouldHaveReceiver = true)
      case (true, false) | (false, true) =>
    }
  }

  override def getBytecodeSize = 0

  private var declaringClass = FakeType("Fake", TypeKind.CLASS)
  override def getDeclaringClass = declaringClass
  private[fake] def setDeclaringClass(declaringClass: FakeType): FakeMethod = {
    this.declaringClass = declaringClass
    this
  }

  private var deferred = false
  def setDeferred(): FakeMethod = {
    this.deferred = true
    this
  }

  private var ajCallKind = MethodAJCallKind.NORMAL
  override def getAJCallKind = ajCallKind
  def setAJCallKind(ajCallKind: MethodAJCallKind) = {
    this.ajCallKind = ajCallKind
    this
  }

  override def getPermanent: PermanentMember = shouldNotCallThis()

  def setCallConv(callConv: CallConv): FakeMethod = {
    methodType = methodType.changeCallConv(callConv)
    this
  }

  override def isStrictMemory = false

  override def isSkippedByCallStackIterator = false

  private var longSafe = false
  def isAJLongSafe = longSafe
  def setLongSafe(longSafe: Boolean): FakeMethod = {
    this.longSafe = longSafe
    this
  }

  override def shouldStackCheckByCaller = false
  override def getStackCheckByCallerBytes: Int = shouldNotCallThis()

  override def getExportedName: XString = null
  override def getExternalName: XString = null

  override def isInlineAllAndRemove = false
  override def isAJInlineForced = false
  override def isAJInline = false
  override def isNeverInline = false
  override def isUniversalGeneric = false

  override def isGenTableSwitch = false

  override def getAJInlineIfConstParams: Array[Int] = null

  private var jcaInlined = false
  def setJCAInlined(jcaInlined: Boolean): FakeMethod = {
    this.jcaInlined = jcaInlined
    this
  }
  override def isJCAInline = jcaInlined
  override def isJCAInlineWithContextPointTest = false
  override def isJCAUnrollLoops = false

  private var nativeIndex = 0
  override def getNativeMethodIndex = nativeIndex
  def setNativeMethodIndex(nativeIndex: Int): FakeMethod = {
    this.nativeIndex = nativeIndex
    this
  }

  private var nativeVMTSlot = 0
  def setNativeMethodVMTSlot(nativeSlot: Int): FakeMethod = {
    this.nativeVMTSlot = nativeSlot
    this
  }

  private var callOrderCounterOffset = 0
  def setCallOrderCounterOffset(callOrderCounterOffset: Int): FakeMethod = {
    this.callOrderCounterOffset = callOrderCounterOffset
    this
  }

  private val frameDescriptor = new FakeSymbol("FrameDescriptor")
  override def getFrameDescriptor: FakeSymbol = frameDescriptor

  override lazy val codeAttribute: Method.CodeAttribute = shouldNotCallThis()

  override def getMethodType: MethodType = methodType

  override def getSignature: MethodSignature = getMethodType.dropReceiverParameter.signature

  override def calcMethodType: MethodType = shouldNotCallThis()

  override def markAsContainingMonitorOperations(): Unit = {}

  override def containsMonitorOperations = false

  def addParamType(`type`: SignatureType): FakeMethod = {
    methodType = methodType.appendParameterType(`type`)
    this
  }

  def setParamTypes(params: SignatureType*): FakeMethod = {
    methodType = methodType.changeParameters(params)
    this
  }

  def setReturnType(retType: SignatureType): FakeMethod = {
    methodType = methodType.changeReturnType(retType)
    this
  }

  override def getUniqueNumberInClass = 0

  private var ajReplacement: Method = _
  override def getAJReplacement: Method = ajReplacement
  def setAJReplacement(ajReplacement: Method): FakeMethod = {
    this.ajReplacement = ajReplacement
    this
  }

  override def getInlineMarker: Type = null

  override def isAJReplaced = ajReplacement != null

  override def isHookInvoker = false

  override def getUncheckedCallTarget: Method = null

  override def getUncheckedNewTarget: Method = null

  override def getIntrinsicType: IntrinsicWithoutBody = null
  override def getIntrinsicWithBodyType: IntrinsicWithBody = null

  override def getCallToManagedTarget: Method = null

  override def getResult: ConstantPoolAccessResult = if (deferred) ConstantPoolAccessResult.DEFERRED else ConstantPoolAccessResult.OK

  override def getObject: Method = this
  override def getError: ErrorAccessInfo = shouldNotCallThis()
  override def getDeferredInfo: DeferredAccessInfo = shouldNotCallThis()

  override def isDirtyForClassGC = false

  private var _isVersionedContext = false
  override def isVersionedContext = _isVersionedContext
  def setVersionedContext(isVersionedContext: Boolean): FakeMethod = {
    this._isVersionedContext = isVersionedContext
    this
  }

  override def isAJRTAllocator = false

  override def isNoLocalGCPoints = false

  override def isNoTracedRegsOnEntry = false

  override def isAjNoReturn = false

  override def isNoCodeGen = false

  override def shouldContainGCPointBeforeResultTransfer: Boolean = false

  override def getJCAParamsEscapeInfo: Int = Method.JCA_PARAMS_ESCAPE_NO_INFO

  override def isInterpretationLoop = false

  override def isNonThrowing = false

  override def getSourceFullName = XString("fake")

  override def getSourceFile: XString = null

  override def setSourceFile(sourceFile: XString): Unit = {}

  override def setSourceFullName(sourceFullName: XString): Unit = {}

  override def getLLVMIndex = -1

  override def getCHIRDef = None

  override def isCAnnotated = false

  override def getCFuncWrapperIndex = -1

  override def isFinalize = false

  override def isMain = false

  override def canAssertTypePreparation(target: Type) = true

  override def shouldBeGenerated: Boolean = false

  override def isOverloaded: Boolean = false

  override def isCJAnnotationFactory: Boolean = false

  override def getCJAnnotationFactory: Method = null

  override def getCJAnnotationFactoriesForParameters: Array[Method] = null

  override def initialCompilationMode: CompilationMode = O2

  override def shouldBeSerialized: Boolean = true

  override def isAJRecordInitializer = false

  override def getGenericInfo: GenericInfo = null

  override def isMethodInfoFrameDescriptorGetter: Boolean = false

  override def isAJDelayedIntrinsic: Boolean = false

  override def getAJDelayedIntrinsicName: XString = shouldNotCallThis()

  override def getAJDelayedIntrinsicClassName: XString = shouldNotCallThis()

  override protected def jcaOptionEnabled(name: String): Boolean = false

  override protected def jcaOptionDisabled(name: String): Boolean = false
}
