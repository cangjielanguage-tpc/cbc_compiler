/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute
import com.huawei.excelsior.jet.compiler.debug.info.DebugType
import com.huawei.excelsior.jet.compiler.driver.CompilationMode
import com.huawei.excelsior.jet.compiler.intrinsics.{Intrinsic, IntrinsicWithBody, IntrinsicWithoutBody}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, MethodID, ReplacementLibrary}
import com.huawei.excelsior.jet.compiler.options.BoolOption.SmartRecordZeroing
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption, StrOption}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Method.{JCA_PARAMS_ESCAPE_NO_ESCAPE, JCA_PARAMS_ESCAPE_NO_INFO, JCA_PARAMS_ESCAPE_RET_ESCAPE_BASE}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, asVerifiableMethodType}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.VOID
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerifiableMethodType, VerifiableType}

import scala.collection.immutable.ArraySeq

class VerifiableMethodImpl(impl: MethodImpl) extends VerifiableMethod {
  override def getDomain: Domain = impl.getDomain

  override def getDeclaringClass: VerifiableType = impl.getDeclaringClass match {
    case cls: TypeImpl => new VerifiableTypeImpl(cls)
  }

  override def getName: String = impl.getName

  override def getFullName: String = impl.getFullName

  override def getXSignature: XString = XString(JBCSignature(impl.getSignature))

  override def codeAttribute: MethodCodeAttribute = impl.codeAttribute

  override def isConstructor: Boolean = impl.isConstructor

  override def isStatic: Boolean = impl.isStatic

  override def markAsContainingMonitorOperations(): Unit = impl.markAsContainingMonitorOperations()

  override def canBeVerified: Boolean = !impl.isNative && !impl.isAbstract && !impl.isAJReplaced
}

class MethodImpl private[light](val o2m: pcOModule.Method) extends Method with SymLevelObject { self =>
  private val host = classByO2Object(o2m.getDeclaringClass)
  private var cachedFD: FrameDescSymbolImpl = _
  private var hasCachedIntrinsicType = false
  private var cachedIntrinsicType: Intrinsic = _

  override def getMethodIndex = o2m.getNumberInClassFile

  private def callConv = o2m.getCallConv

  private def statik = o2m.isStatic

  override def o2object: pcOModule.Method = o2m

  override def getJavaModifiersValue = o2m.getJavaModifiers.toInt

  override def getCJModifiers = {
    assert(getDeclaringClass.isCangjieType)
    Modifiers(o2m.getCJModifiers.toInt)
  }

  override def getAJCallKind = o2env.getAJCallKind(o2m)

  override def isStrictMemory = o2m.isAjStrictMemory

  override def isSkippedByCallStackIterator = CacheAPIModule.isThisMethod(o2m, MethodID.invoke) || o2m.getDeclaringClass.isMethodAccessorImplSubclass

  private def isAJLongSafe = o2m.isAjLongSafe

  override def isVersionedContext = o2m.isAjVersionedContext

  override def isDirtyForClassGC = o2m.isDirtyForClassGC

  override def getExportedName = if (o2m.isExported) o2m.getExportedName else null

  override def getExternalName = if (o2m.isExternal) o2m.getExternalName else null

  override def shouldContainGCPointBeforeResultTransfer: Boolean = {
    env.enabled(SmartRecordZeroing) && returnsRecord && getReturnType.hasRefFields(env.getTypeProvider)
  }

  override def getJCAParamsEscapeInfo = {
    val info = o2m.getJcaKnownSafeInfo
    if (info == pcOModule.JCA_NO_KNOWN_SAFE_INFO) {
      JCA_PARAMS_ESCAPE_NO_INFO
    } else if (info == -1) {
      JCA_PARAMS_ESCAPE_NO_ESCAPE
    } else {
      assert(info >= 0)
      JCA_PARAMS_ESCAPE_RET_ESCAPE_BASE + info
    }
  }

  override def shouldStackCheckByCaller = o2m.hasDefinedStackCheckByCallerByteCount

  override def getStackCheckByCallerBytes = o2m.getStackCheckByCallerByteCount

  override def isExternal = o2m.isExternal

  override def isExported = o2m.isExported

  override def isConstructor = o2m.isConstructor

  override def isThinConstructor = o2m.isThinConstructor

  override def isRecordConstructor = o2m.isRecordConstructor

  override def isClinit = o2m.isClinit

  override def isPackageInit = o2m.isPackageInit

  override def isPackageLiteralInit = o2m.isPackageLiteralInit

  override def isGlobalInit = o2m.isGlobalInit

  override def isAJRTAllocator = o2m.isAjRTAllocator

  override protected def isNoLocalGCPoints = o2m.isNoLocalGCPoints

  override def isNoTracedRegsOnEntry = o2m.isNoTracedRegsOnEntry

  override def isAjNoReturn = o2m.isAjNoReturn

  override def isNoCodeGen = o2m.isNoCodeGen

  private[light] def isReallyVirtual = !isStatic && o2m.isVirtual

  override def isAJReplaced = o2m.isAjReplaced

  override def isHookInvoker = o2m.isAjHookInvoker

  override def getAJReplacement = ReplacementLibrary.getReplacement(o2m).map(methodByO2Object).orNull

  override def isAJDelayedIntrinsic = o2m.isAJDelayedIntrinsic

  override def getAJDelayedIntrinsicClassName = o2m.getAJDelayedIntrinsicClassName

  override def getAJDelayedIntrinsicName = o2m.getAJDelayedIntrinsicName

  override def getInlineMarker = if (CacheAPIModule.isThisMethod(o2m, MethodID.invoke)) {
    typeByO2Object(pc.SymType.JBC.Primitive(VOID))
  } else {
    null
  }

  override def isInlineAllAndRemove = o2m.isInlineAllAndRemove

  override def isAJInlineForced = o2m.isAJInlineForced

  override def isAJInline = o2m.isAJInline

  override def isJCAInline = o2m.isJCAInline

  override def isJCAInlineWithContextPointTest = o2m.isJCAInlineWithContextPointTest

  override def isJCAUnrollLoops = o2m.isJCAUnrollLoops

  override def isNeverInline = o2m.isNeverInline

  override def isUniversalGeneric = o2m.isUniversalGeneric

  override def isGenTableSwitch: Boolean = o2m.isGenTableSwitch

  override def getAJInlineIfConstParams = if (o2m.isAJInlineIfConstParams) {
    o2m.getAJInlineIfConstParamsIndices
  } else {
    null
  }

  override def getBytecodeSize = o2m.getBytecodeSize

  override def getDeclaringClass = host

  override def getIntrinsicType: IntrinsicWithoutBody = {
    fillIntrinsicCache()
    cachedIntrinsicType match {
      case body: IntrinsicWithoutBody => body
      case _ => null
    }
  }

  override def getIntrinsicWithBodyType: IntrinsicWithBody = {
    fillIntrinsicCache()
    cachedIntrinsicType match {
      case body: IntrinsicWithBody => body
      case _ => null
    }
  }

  private def fillIntrinsicCache(): Unit = if (!hasCachedIntrinsicType) {
    cachedIntrinsicType = env.findIntrinsicType(this)
    hasCachedIntrinsicType = true
  }

  override protected def getCallToManagedTarget = methodByO2Object(o2m.getAjCallToManagedTarget)

  override protected def getUncheckedCallTarget = methodByO2Object(o2m.getAjUncheckedCallTarget)

  override protected def getUncheckedNewTarget = methodByO2Object(o2m.getAjUncheckedNewTarget)

  override def getNativeMethodIndex: Int = {
    var nativeNum = 0
    for (method <- getDeclaringClass.getDeclaredMethods) {
      if (methodToO2Method(method).isDeclaredNative && !method.isAJReplaced) { // TODO: skip intrinsics here
        if (method == this) {
          return nativeNum
        }
        nativeNum += 1
      }
    }
    shouldNotReachHere(s"Method \"$getFullName\" doesn't have native method index")
  }

  override def getFrameDescriptor = {
    if (cachedFD == null) {
      cachedFD = new FrameDescSymbolImpl(o2m)
    }
    cachedFD
  }


  /** Returns `@CallConv.Head(inLimit)` value. */
  private def headInLimit: Int = o2m.getCallConvHeadInLimit

  /** Returns `@CallConv.Head(outLimit)` value. */
  private def headOutLimit: Int = o2m.getCallConvHeadOutLimit

  /** Return bitset of parameters which were annotated with `@CallConv.Preserved` */
  private def preservedParameters: Int = o2m.getPreservedParamsSet

  /** Return bitset of parameters which were annotated with `@CallConv.AltLocation` */
  private def altLocationParameters: Int = o2m.getAltLocationParamsSet

  /** Returns `true` when method itself is annotated with `@CallConv.AltLocation` */
  private def hasAltLocationResult: Boolean = o2m.hasAltLocationResult

  override def isMethodInfoFrameDescriptorGetter: Boolean = o2m.isMethodInfoFrameDescriptorGetter

  override def getSignature: MethodSignature = o2m.getSignature

  override protected def calcMethodType = {
    val ck = if (isAJLongSafe) {
      CallKind.AJ_LONG_SAFE
    } else if (isCangjieForeign) {
      CallKind.CJ_FOREIGN
    } else {
      CallKind.NORMAL
    }

    // For Unmanaged, CCall and etc. varargs we are using
    // the same flag (ACC_VARARGS == TRANSIENT) as varargs for managed methods.
    val isVarArgs = o2m.isVarArgs && !callConv.isManaged

    MethodType(
      o2m.getABISignature,
      callConv, ck, o2m.getSpecialParamSet,
      isVarArgs, MethodType.UNINITIALIZED_FIRST_VAR_ARG,
      headInLimit, headOutLimit, preservedParameters, MethodType.AltLocationInfo(hasAltLocationResult, altLocationParameters)
    )
  }

  override def markAsContainingMonitorOperations(): Unit = o2m.markAsContainingMonitorOperations()

  override def containsMonitorOperations = o2m.containsMonitorOperations()

  override def getUniqueNumberInClass = o2m.lref

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: MethodImpl => memberEquals(this.o2m, that.o2m)
    case _ => false
  }

  override def hashCode = memberHashCode(o2m)

  override def getXName = o2name(o2m)

  def getObject = this

  override protected def getExplicitDomain = o2m.getDomain

  override def isInterpretationLoop = o2m.isInterpretationLoop

  override def isNonThrowing = o2m.isNonThrowing

  override def ownsSegment = o2m.ownsSegment

  override def getSourceFullName = o2m.getSourceFullName

  // TODO unify setSourceFullName and setSourceFile
  override def setSourceFullName(sourceFullName: XString): Unit =
    o2m.setSourceFullName(sourceFullName)

  override def getLLVMIndex = o2m.getLLVMIndex

  override def getCHIRDef = o2m.getCHIRDef

  override def getCPPLinkageName  = o2m.cppLinkageName
  override def getSourceName      = o2m.sourceName
  override def getSourceFile      = o2m.sourceFile
  override def getSourceLine      = o2m.sourceLine
  override def getDebugType       = o2m.debugType

  override def setCPPLinkageName(name: XString)   : Unit = o2m.cppLinkageName = name
  override def setSourceName(name: XString)       : Unit = o2m.sourceName = name
  override def setSourceFile(file: XString)       : Unit = o2m.sourceFile = file
  override def setSourceLine(line: Int)           : Unit = o2m.sourceLine = line
  override def setDebugType(debugType: DebugType) : Unit = o2m.debugType = debugType

  override def getPermanent: PermanentMember = new PermanentMemberImpl(o2m.getRef) {
    override def get: Method = new MethodImpl(ref.getMethod)
  }

  override def isCAnnotated = o2m.isCAnnotated

  override def getCFuncWrapperIndex = o2m.getCFuncWrapperIndex

  override def isFinalize = o2m.isFinalize

  override def isMain = o2m.isMainMethod

  override def isOverloaded = o2m.isOverloaded

  override def canAssertTypePreparation(target: Type) = !o2m.isAjNoPreparationCheck

  /** Returns true when there is a compiler-generated binary object (code or static data) for this member. */
  override def shouldBeGenerated: Boolean = o2m.shouldBeGenerated

  override def isCJAnnotationFactory = o2m.isCangjieAnnotationFactory

  override def getCJAnnotationFactory: Method = {
    val factory = o2m.getCJAnnotationFactory
    if (factory == null) {
      null
    } else {
      methodByO2Object(factory)
    }
  }

  override def getCJAnnotationFactoriesForParameters: Array[Method] = {
    val factories = o2m.getCJAnnotationFactoriesForParameters
    if (factories == null) {
      null
    } else {
      factories.map { factory =>
        if (factory == null) {
          null
        } else {
          methodByO2Object(factory)
        }
      }
    }
  }

  override def initialCompilationMode: CompilationMode = o2m.initialCompilationMode

  override def shouldBeSerialized: Boolean = o2m.shouldBeSerialized

  override def isAJRecordInitializer = o2m.isAJRecordInitializer

  override def getGenericInfo = o2m.getGenericInfo

  override protected def jcaOptionEnabled(name: String): Boolean = o2m.jcaOptionEnabled(name)

  override protected def jcaOptionDisabled(name: String): Boolean = o2m.jcaOptionDisabled(name)
}
