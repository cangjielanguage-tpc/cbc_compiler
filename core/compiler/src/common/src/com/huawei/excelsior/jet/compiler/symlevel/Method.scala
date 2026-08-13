/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.FrameProperties
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute
import com.huawei.excelsior.jet.compiler.driver.CompilationMode
import com.huawei.excelsior.jet.compiler.intrinsics.{IntrinsicWithBody, IntrinsicWithoutBody}
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.ir.{LineNumber, Modifiers}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{XCheckArrStore, XCheckIndex, XCheckNull}
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.{CCALL, GCAWARE, MANUAL, RTCALL}
import com.huawei.excelsior.jet.compiler.symlevel.Method.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.JBCReference
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.{Domain, Environment}

/** Some Java method
  *
  * @author cypok
  */
object Method {

  trait LocalVariablesTable {
    def localCount(): Int
    def localName(i: Int): XString
    def localSignature(i: Int): XString
  }

  /** Method's 'Code' attribute as recorded in class file.
    *
    * TODO-DECAF: make ExceptionTableTraverser (and its ArrayImpl) inner trait of CodeAttribute
    */
  trait CodeAttribute extends MethodCodeAttribute {
    def exceptionTableLength: Int

    /** Looks for line number information by given bytecode offset.
      *
      * @return best matching line number, or [[LineNumber.UNKNOWN]] if line number information is not available.
      */
    def findLineNumber(bytecodeOffset: Int): Int

    def firstLineNumber: Int = notImplemented("feel free to implement")

    def getLocalVariablesTable(): Option[LocalVariablesTable] = None

    /** Content of 'LocalVariableTable' attribute of this Code attribute.
      *
      * Note: currently unused. Some day we will restore verification of LVT.
      */
    def localVariableTable: Array[Byte]
  }

  val JCA_PARAMS_ESCAPE_NO_INFO = Integer.MIN_VALUE
  val JCA_PARAMS_ESCAPE_NO_ESCAPE = -1
  val JCA_PARAMS_ESCAPE_RET_ESCAPE_BASE = 0

  /** Returns [[AccessKind]] of cross-component calls. Currently they are always generated using
    * import tables which are directly accessible.
    */
  def accessKind = AccessKind.DIRECT
}

abstract class Method extends Symbol with Member with ConstantPoolObject with FrameProperties {

  /** Method's `CodeAttribute` or `null` if it's not available (i.e. native). */
  lazy val codeAttribute: CodeAttribute = if (isAJReplaced || isNative || isAbstract) null else {
    getDeclaringClass.getClassConstantPool.getMethodCodeAttribute(this)
  }

  /** Returns method descriptor as written in class file. */
  //public abstract XString getDescriptor(); //TODO: move to Member

  /** JVM spec:
    * public, private, protected, static, final,
    * synchronized, bridge, varargs, native, abstract, strict, synthetic
    */
  //protected abstract int getAccessFlags(); //TODO: move to Member

  //public abstract XString _getName(); //TODO: move to Member

  def isSynchronized = getJavaModifiers contains SYNCHRONIZED
  def isNative       = getJavaModifiers contains NATIVE
  def isAbstract     = getJavaModifiers contains ABSTRACT
  def isJavaVarArgs  = getJavaModifiers contains VARARGS

  /** Returns index of this method in class file of declaring class. */
  def getMethodIndex: Int

  /** Returns index of this method in resulting binary artifact or -1 if method will not be generated. */
  def getHostedIndex: Int = getDeclaringClass.getGeneratedMethodIndex(this)

  override def getMemberIndex = getMethodIndex

  /** Returns raw method type (varargs as array and with special wrapper params at the start). */
  private lazy val _getMethodType = calcMethodType
  def getMethodType = _getMethodType

  /** Returns method signature as defined in source.
    *
    * Note: receiver parameter type is not included!
    */
  def getSignature: MethodSignature

  /** Creates raw method type (varargs as array and with special wrapper params at the start). */
  protected def calcMethodType: MethodType

  override def getRealMethodType(varArgs: Iterable[SignatureType]): MethodType =
    getMethodType.dropFirstNParameters(getSpecialParamsCount).appendVarArgs(varArgs)

  /** Returns method type for real native procedure which corresponds to this native wrapper. */
  def getNativeProcedureMethodType(env: Environment): MethodType = {
    assert(isNative)
    var result = getMethodType.changeCallConv(CCALL)

    // add class object parameter
    val tp = env.getTypeProvider
    if (isStatic) {
      // TODO: consider making it non-nullable
      result = result.prependParameterType(JBCReference(tp.getObjectType))
    }

    // add EE parameter
    result.prependParameterType(SignatureType.Address)
  }

  /** Mark `this` method as containing `monitorEnter`/`monitorExit` bytecodes.
    * NOTE: can be called either on AOT parsing stage, or during JIT.
    */
  def markAsContainingMonitorOperations(): Unit

  /** Returns whether `this` method contains `monitorEnter`/`monitorExit` bytecodes. */
  def containsMonitorOperations: Boolean

  /** Returns unique number of this method in scope of host class. */
  def getUniqueNumberInClass: Int

  /** Returns unique number of this method in global scope. */
  def getUniqueNumber: Long = (getDeclaringClass.getUniqueNumber.toLong << 32) + getUniqueNumberInClass

  def isDeferred = getDeclaringClass.isDeferred

  def isExternal: Boolean

  def isExported: Boolean

  def isConstructor: Boolean

  /** Whether this method is a translated Thin constructor annotated with `@ThinConstructor`. */
  def isThinConstructor: Boolean

  def isRecordConstructor: Boolean

  def isClinit: Boolean

  def isPackageInit: Boolean

  def isPackageLiteralInit: Boolean

  def isGlobalInit: Boolean

  def isAJRTAllocator: Boolean

  /** True if method is marked with AJ annotation @NoLocalGCPoints or JCA directive NO_LOCAL_GC_POINTS. */
  protected def isNoLocalGCPoints: Boolean

  def isNoTracedRegsOnEntry: Boolean

  def isAjNoReturn: Boolean

  def isNoCodeGen: Boolean

  override def isVarArgs = getMethodType.isVarArgs
  def isCVarArgs = getMethodType.isCVarArgs
  def isJETVarArgs = getMethodType.isJETVarArgs

  /** Whether this method could be safely inlined with no limits. */
  def isInlineAllAndRemove: Boolean

  /** Whether this method has AJ annotation `@Inline(forced = true)`. */
  def isAJInlineForced: Boolean

  /** Whether this method has AJ annotation `@Inline`. */
  def isAJInline: Boolean

  /** Whether this method is specified in JCA file as `ALWAYS_INLINE`. */
  def isJCAInline: Boolean

  /** Whether this method is specified in JCA file as `INLINE_WITH_CONTEXT_POINT_TEST`. */
  def isJCAInlineWithContextPointTest: Boolean

  /** Whether this method is specified in JCA file as `UNROLL_LOOPS`. */
  def isJCAUnrollLoops: Boolean

  /** Whether this method has AJ annotation `@NoInline`
    * or this method is specified in JCA file as `NOTINLINE`.
    */
  def isNeverInline: Boolean

  def isGenTableSwitch: Boolean

  def isUniversalGeneric: Boolean

  def hasUniversalGenericContext: Boolean =
    isUniversalGeneric || getDeclaringClass.isUniversalGeneric

  def getMutRecordType: SignatureType = {
    assert(hasMutRecordParameter)
    if (getDeclaringClass.isRecord) {
      SignatureType.fromSymType(getDeclaringClass)
    } else {
      assert(isStatic) // array slice constructor
      getSignature.parameterTypes.head ensuring (_.isArraySliceLike)
    }
  }

  def startSpecialParamsCount: Int = getMethodType.startSpecialParamsCount

  def hasReceiverParameter: Boolean = getMethodType.hasReceiverParameter

  def hasReferenceReceiver: Boolean = getMethodType.hasReferenceReceiver

  def hasRecordReceiver: Boolean = getMethodType.hasRecordReceiver

  def getReceiverArgIdx: Int = getMethodType.getReceiverArgIdx

  def hasMutRecordParameter: Boolean = getMethodType.hasMutRecordParameter

  def getMutRecordArgIdx: Int = getMethodType.getMutRecordArgIdx

  def hasMutObjectParameter = getMethodType.hasMutObjectParameter

  def getMutObjectArgIdx: Int = getMethodType.getMutObjectArgIdx

  def hasThisTypeInfoParameter: Boolean = getMethodType.hasThisTypeInfoParameter

  def getThisTypeInfoArgIdx: Int = getMethodType.getThisTypeInfoArgIdx

  def hasRetByValParameter: Boolean = getMethodType.hasRetByValParameter

  def hasOuterTypeInfoParameter: Boolean = getMethodType.hasOuterTypeInfoParameter

  def getRetByValArgIdx: Int = getMethodType.getRetByValArgIdx

  def hasCFuncRetByValParameter: Boolean = getMethodType.hasCFuncRetByValParameter
  
  /** Returns array of param indices if the method is annotated with `@InlineIfConstParams`.
    * Otherwise returns `null`.
    */
  def getAJInlineIfConstParams: Array[Int]

  /** Returns type of return value of the method. */
  def getReturnType: SignatureType = getMethodType.returnType

  /** Returns number of params. */
  def getParamsCount = getMethodType.parameterCount

  /** Returns type of i-th param of the method. */
  def getParamType(paramIdx: Int): SignatureType = getMethodType.parameterType(paramIdx)

  def getCallConv: CallConv = getMethodType.callConv

  /** Returns size of method in bytecode bytes. */
  def getBytecodeSize: Int

  /** Get nullable intrinsic type for method. */
  def getIntrinsicType: IntrinsicWithoutBody

  /** Get nullable intrinsic type for method, which is intrinsic with body. */
  def getIntrinsicWithBodyType: IntrinsicWithBody

  /** Get target method of CallToManaged method. */
  protected def getCallToManagedTarget: Method

  /** Get target method of CallToManaged method. */
  final def getCallToManagedTargetRef: MethodReference = {
    val target = getCallToManagedTarget

    val akind = if (target.isStatic) {
      assert(!target.isClinit)
      MethodReferenceAccessKind.STATIC
    } else {
      assert(!target.isConstructor)
      MethodReferenceAccessKind.SPECIAL
    }

    new MethodReference(target, akind)
  }

  def isAJReplaced: Boolean

  /** Get replacement method (or `null` if it is not active or not available). */
  def getAJReplacement: Method

  def isAJDelayedIntrinsic: Boolean

  def getAJDelayedIntrinsicName: XString

  def getAJDelayedIntrinsicClassName: XString

  def getInlineMarker: Type

  protected def getUncheckedCallTarget: Method

  final def getUncheckedCallTargetRef: MethodReference = {
    val target = getUncheckedCallTarget
    assert(!target.isVarArgs)
    assert(getParamsCount == target.getParamsCount)

    val akind = if (target.isStatic) {
      MethodReferenceAccessKind.STATIC
    } else if (target.getDeclaringClass.isInterface) {
      MethodReferenceAccessKind.INTERFACE
    } else if (target.isPrivate) {
      MethodReferenceAccessKind.SPECIAL
    } else {
      MethodReferenceAccessKind.VIRTUAL
    }

    new MethodReference(target, akind)
  }

  protected def getUncheckedNewTarget: Method

  final def getUncheckedNewTargetRef: MethodReference = {
    val target = getUncheckedNewTarget
    assert(target.isConstructor)
    assert(!target.isVarArgs)
    assert(1 + getParamsCount == target.getParamsCount, "target method should have one extra param - this")

    val targetClass = target.getDeclaringClass
    assert(!targetClass.isDeferred)
    assert(!targetClass.isAbstractClass)

    new MethodReference(target, MethodReferenceAccessKind.SPECIAL, CompiledType(targetClass))
  }

  /** Returns native method's index in the native methods table. */
  def getNativeMethodIndex: Int

  override def hasFrameDescriptor = isManagedFrame

  /** Get kind of this AJ method. */
  def getAJCallKind: MethodAJCallKind

  // Note: it is intentionally different from getFullName() to avoid confusion in future
  override final def toString = s"method $getFullName"

  override def getFullName: String = {
    val fullNameNoClassloaderSID = s"${getDeclaringClass.getName}.$getName${getSignature.toJETSignature}"

    val classloaderSID = getDeclaringClass.getClassLoaderSID
    if (classloaderSID == null) {
      fullNameNoClassloaderSID
    } else {
      s"$classloaderSID%$fullNameNoClassloaderSID"
    }
  }

  def isMethodInfoFrameDescriptorGetter: Boolean

  override def isManaged = getCallConv.isManaged

  def hasManagedExecEnv = getCallConv.hasManagedExecEnv

  def isGCAware = getCallConv == GCAWARE

  def isManual = getCallConv == MANUAL

  def isIntrinsicCall = getAJCallKind == MethodAJCallKind.INTRINSIC_CALL

  def isIntrinsicWithBodyCall = getAJCallKind == MethodAJCallKind.INTRINSIC_WITH_BODY_CALL

  def isIndirectCall = getAJCallKind == MethodAJCallKind.INDIRECT_CALL

  def isThinUncheckedCast = getAJCallKind == MethodAJCallKind.THIN_UNCHECKED_CAST

  def isGetFlatThinIntrinsic = getAJCallKind == MethodAJCallKind.GET_FLAT_THIN_INTRINSIC

  def getSpecialParamsCount = if (isIndirectCall) 1 else 0

  def isCallToManaged = getAJCallKind == MethodAJCallKind.CALL_TO_MANAGED

  /** Whether this method has AJ annotation `@StrictMemory`. */
  def isStrictMemory: Boolean

  /** Returns true iff given method is skipped by call stack iterator.
    * Method is skipped if:
    * it belongs to JET runtime
    * it is java.lang.reflect.Method.invoke()
    * it is declared in a subclass of sun.reflect.MethodAccessorImpl
    */
  def isSkippedByCallStackIterator: Boolean

  /** Returns true iff this method is annotated with @VersionedContext. */
  def isVersionedContext: Boolean

  /** Returns true if method is annotated with @DirtyForClassGC. */
  def isDirtyForClassGC: Boolean

  override def shouldContainGCPoints = {
    val res = hasManagedExecEnv && !isManual && !isNoLocalGCPoints
    if (isAJRTAllocator) {
      // This condition is necessary for correct GC-points insertion.
      // If is fails, try to convince runtime-engineers, that they are wrong, or
      // remove assert and replace
      //   foo.shouldContainGCPoints() to
      //   foo.shouldContainGCPoints OR foo.isAjRTAllocator()
      // in
      //   opCodeOpt::HasActualGCPoint
      //   GCPointsInserting::makeGCPointsExpectanceMap
      //
      // TODO: simplify GC-points!
      assert(res)
    }
    res
  }

  override def shouldContainGCPointInEpilogue =
    shouldContainGCPoints && !isAJRTAllocator && !isConstructor && !isHookInvoker && !shouldContainGCPointBeforeResultTransfer

  override def shouldContainGCPointInEpilogueBeforeFrameDrop = shouldContainGCPointInEpilogue && {
    val mt = getMethodType

    // In case we have a @Preserved reference parameter, it will be pushed in prologue and restored in epilogue,
    // so GC will adjust it in stack slot if it is alive outside. However, we must avoid placing a GC-point
    // after restoring those references onto their registers, as we cannot properly tell GC to adjust these registers.
    val hasPreservedReferenceParameters =
      mt.hasPreservedParameters && mt.parameterTypes.zipWithIndex.exists(_.isReference && mt.isPreservedParameter(_))

    // We need a register to generate epilogue GC-point.
    val noRegsAvailableInEpilogue = targetArch match {
      case ARM64 => false // On ARM we always have volatile IP to use.
      case CBC   => false // CBC bytecode can be lowered for all architectures, but it never represents an RTCall method.
      case AMD64 => getCallConv == RTCALL // TODO: check if we have a non-preserved register-passed parameter.
    }

    hasPreservedReferenceParameters || noRegsAvailableInEpilogue
  }

  override def shouldContainGCPointInEpilogueAfterFrameDrop =
    shouldContainGCPointInEpilogue && !shouldContainGCPointInEpilogueBeforeFrameDrop

  def shouldContainGCPointBeforeResultTransfer: Boolean

  /** Returns true if this method does not require clinit check in prologue.
    * It means that the class must be already clinited before calling this method.
    */
  final def isPreClinited = getDeclaringClass.isPreClinited || !isStatic || isPrivate || isClinit

  /** Obtains JCA "KNOWN_SAFE" information about escape of parameters. Returns:
    *
    *   - [[JCA_PARAMS_ESCAPE_NO_INFO]] if there is no information about this method.
    *   - [[JCA_PARAMS_ESCAPE_NO_ESCAPE]] if all parameters do not escape from this method.
    *   - ([[JCA_PARAMS_ESCAPE_RET_ESCAPE_BASE]] + N) if all parameters do not escape
    *     except the N-th parameter which may be a return value.
    */
  def getJCAParamsEscapeInfo: Int

  override def isStackCheckDisabled = false

  override def isManagedFrame = hasManagedExecEnv || isCallToManaged

  /** Determines the domain of the method.
    *
    * NOTE: `@AJExtended` and `@java` classes
    * (and their managed methods) are considered as having Java domain.
    * All unmanaged methods are considered AJ.
    */
  def getDomain: Domain = {
    val explicitDomain = getExplicitDomain
    if (explicitDomain != null) {
      return explicitDomain
    }

    if (isManaged) {
      if (getDeclaringClass.isJavaReference) {
        Domain.JAVA
      } else if (getDeclaringClass.isCangjieType) {
        Domain.CANGJIE
      } else if (getDeclaringClass.isXScalaType) {
        Domain.SCALA
      } else {
        Domain.AJ
      }
    } else {
      // Unmanaged -> AJ
      Domain.AJ
    }
  }

  protected def getExplicitDomain: Domain = null

  def isInterpretationLoop: Boolean

  def isNonThrowing: Boolean

  /** Cangjie foreign method. */
  final def isCangjieForeign = getDeclaringClass.isCangjieType && (getCJModifiers contains CJ_FOREIGN)

  /** Cangjie `mut` function with proper ABI. */
  final def isCangjieMut = isCangjieMutMarked && hasMutObjectParameter // TODO: introduce and use "hasMutPair"

  /** Cangjie function marked with `mut` modifier. TODO: use isCangjieMut everywhere */
  final def isCangjieMutMarked = getDeclaringClass.isCangjieType && (getCJModifiers contains CJ_MUT)

  def getSourceFullName: XString

  def setSourceFullName(sourceFullName: XString): Unit

  final def hasSourceFullName = getSourceFullName != null

  def getLLVMIndex: Int

  /** Returns true if Cangjie method is annotated with `@c`.
    * Addresses of such methods can be represented as CFunc function pointer via wrapper obtained from [[getCFuncWrapperIndex]].
    */
  def isCAnnotated: Boolean

  /** An index at which a CFunc wrapper of a [[isCAnnotated]] method is located inside RTTI of declaring type. */
  def getCFuncWrapperIndex: Int

  def isFinalize: Boolean

  def isMain: Boolean

  def canAssertTypePreparation(target: Type): Boolean

  def returnsRecord: Boolean = getReturnType.isRecord

  def isCJAnnotationFactory: Boolean

  def getCJAnnotationFactory: Method

  def getCJAnnotationFactoriesForParameters: Array[Method]

  /** Returns true if `that` method has the same name and signature. */
  def sameNameAndSig(that: Method): Boolean =
    this.getXName == that.getXName && this.getSignature == that.getSignature

  /** Returns true if `that` method can override or be overridden by this method based on name and signature. */
  def overridesNameAndSig(that: Method): Boolean = {
    val clazz = getDeclaringClass
    this.getXName == that.getXName && {
      if (clazz.isCangjieType) {
        // In cangjie methods can override with more precise return type,
        // so for overriding we need to check parameter types equality and return type subtyping separately.
        // TODO: actually check subtyping of return type, do not ignore it!
        this.getSignature.parameterTypes == that.getSignature.parameterTypes
      } else {
        // In Java bytecode overriding is defined based on full signature equality,
        // so javac inserts special bridge methods when overriding with more precise return type.
        // Thus, we can use full signature equality here.
        this.getSignature == that.getSignature
      }
    }
  }

  /** Returns [[CompilationMode]] initial for this method. It may be changed during compilation. */
  def initialCompilationMode: CompilationMode

  /** Returns true iff `this` method IR should be serialized with extra info. */
  def shouldBeSerialized: Boolean

  /** Returns `true` if method has `@RecordInitializer` annotation. */
  def isAJRecordInitializer: Boolean

  /** Returns `true` if current method has `@RecordInitializer` annotation or is replaced by such method. */
  final def isRecordInitializer: Boolean =
    (isAJReplaced && getAJReplacement.isAJRecordInitializer) || this.isAJRecordInitializer

  def getGenericInfo: GenericInfo

  protected def jcaOptionEnabled(name: String): Boolean

  protected def jcaOptionDisabled(name: String): Boolean

  def noNullCheck       (env: Environment) : Boolean = !env.enabled(XCheckNull)     || jcaOptionDisabled("CHECKNULL")
  def noArrayIndexCheck (env: Environment) : Boolean = !env.enabled(XCheckIndex)    || jcaOptionDisabled("CHECKINDEX")
  def noArrayStoreCheck (env: Environment) : Boolean = !env.enabled(XCheckArrStore) || jcaOptionDisabled("CHECKSTORE")

  def isJCAPGOHost: Boolean = jcaOptionEnabled("PGOHOST")
}
