/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg}
import com.huawei.excelsior.jet.assembler.{AsmType, Location}
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, ParamsQueue, TailSlot, makeRegsBitMap}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{CangjieEnumWrapper, Primitive, TypeVariable, fromSymType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.{Env, RTConst, TypeProvider}
import xscala.util.MathUtils.{alignUp, isAligned, setBit}

import scala.collection.mutable
import scala.reflect.ClassTag

object ABI {
  /** Given the map from savable registers to their positions in bit map and list of registers to be saved,
    * produces bit map in which set bit at position `indexMap(r)` indicates that register `r` will be saved.
    *
    * For example, for `r1 -> 3, r2 -> 2, r3 -> 1, r4 -> 0 , {r4, r1, r2}` it will produce `0b1101`
    */
  private def makeRegsBitMap[R <: AnyReg](indices: Map[R, Int], savedRegs: Iterable[R]): Int = {
    if (savedRegs == null) return 0

    var bitmap = 0
    val mapSize = indices.size
    assert(mapSize <= 32)

    for (r <- savedRegs) {
      val idx = indices(r)
      assert(idx >= 0)
      assert(idx < mapSize)
      bitmap |= (1 << idx)
    }
    bitmap
  }

  private[abi] class ParamsQueue[R <: AnyReg](arguments: Array[R]) {
    private val used = new mutable.HashSet[R]
    var nextIdx = 0

    def hasNext: Boolean = nextIdx < arguments.length

    def next(): R = {
      assert(hasNext)
      val r = arguments(nextIdx)
      used += r
      nextIdx += 1
      r
    }

    def skip(): Unit = {
      assert(hasNext)
      nextIdx += 1
    }

    def close(): ActualParams[R] = new ActualParams[R](used, arguments drop nextIdx)
  }

  private[abi] case class ActualParams[R <: AnyReg](used: collection.Set[R], remaining: Array[R])

  case class TailSlot(offset: Int, tpe: AsmType) extends Location.Other {
    override def width = tpe.width
  }

  case class AltLocation(slot: Int) extends Location.Other {
    require(0 <= slot && slot <= 1)

    def width = shouldNotCallThis()
  }

  object AltLocation {
    val Result = AltLocation(slot = 0)
  }

  enum RetType {
    case I, L, F, D, Void
  }

  object RetType {
    def apply(methodType: MethodType) = methodType.returnType match {
      case Primitive(TypeKind.VOID) => Void
      case Primitive(TypeKind.FLOAT) => F
      case Primitive(TypeKind.DOUBLE) => D
      case Primitive(TypeKind.LONG) => L
      case CangjieEnumWrapper(Primitive(TypeKind.LONG), _) => L
      case _ => I
    }
  }

  def makeABISigType(sig: SignatureType)(implicit typeProvider: TypeProvider): SignatureType = {
    if (sig.isVariableSizeType) SignatureType.Box(sig) else sig
  }

  /** Creates ABI signature from source one. It may include hidden params and/or receiver.
    * Note: Has runtime implementation as [[com.huawei.excelsior.jet.runtime.jit.cbc.file.MethodRef.getABISignature]] method.
    */
  def makeABISignature(sig: MethodSignature, receiver: Option[SignatureType] = None,
                       hasUGDesc: Boolean = false, hasMutParameter: Boolean = false, hasThisTypeInfoParameter: Boolean = false,
                       isCFunc: Boolean = false, hasOuterTypeInfo: Boolean = false, genericFuncParamsCount: Int = 0)
                      (implicit typeProvider: TypeProvider): (MethodSignature, SpecialParamSet) = {

    def convertToABI(t: SignatureType): SignatureType = makeABISigType(t)

    def makeABISigWithSpecialParams(specialParamsMap: collection.Map[SpecialParameter, SignatureType]): MethodSignature = {
      def obtainSpecialParams(params: Iterable[SpecialParameter]) = params.flatMap(specialParamsMap.get)

      val startElements = obtainSpecialParams(SpecialParamSet.completeListOfStartSpecialParameters)
      val endElements = obtainSpecialParams(SpecialParamSet.completeListOfEndSpecialParameters)
      // TODO: make it more systematic
      val genericFuncParams = specialParamsMap.get(GenericFuncParams) match {
        case Some(t) => assert(genericFuncParamsCount > 0); Seq.fill(genericFuncParamsCount)(t)
        case None => assert(genericFuncParamsCount == 0); Seq.empty
      }
      val abiParamTypes = startElements ++ sig.parameterTypes.map(makeABISigType) ++ genericFuncParams ++ endElements
      val abiReturnType = makeABISigType(sig.returnType)
      sig.copy(parameterTypes = abiParamTypes.toSeq, returnType = abiReturnType)
    }

    val specialParamMap = mutable.LinkedHashMap.empty[SpecialParameter, SignatureType]

    for (rcv <- receiver) {
      assert(!hasMutParameter)
      specialParamMap(Receiver) = rcv
    }

    if (hasMutParameter) {
      if (Env.isStandalone) {
        // Split mut record param into pair (AddrUInt, RefType)
        assert(receiver.isEmpty)
        specialParamMap(SMutRecord) = SignatureType.Address
        specialParamMap(SMutObject) = ReferenceType.cangjieStdCoreObject.sigType

      } else {
        // Split mut record param into pair (AddrUInt, RefType)
        assert(receiver.isEmpty)
        specialParamMap(MutRecord) = SignatureType.Address
        // TODO: in theory, it can be `std.core.Any` on cangjie lp
        specialParamMap(MutObject) = SignatureType.fromSymType(typeProvider.getAJObjectType) // AJObject to allow permanent local/global objects
      }
    }

    if (hasUGDesc) {
      specialParamMap(UGDesc) = SignatureType.Address
    }

    if (hasThisTypeInfoParameter) {
      specialParamMap(ThisTypeInfo) = if (Env.isStandalone) SignatureType.Address else SignatureType.ThisTypeInfo
    }

    if (hasOuterTypeInfo) {
      specialParamMap(OuterTypeInfo) = SignatureType.Address
    }

    if (genericFuncParamsCount > 0) {
      specialParamMap(GenericFuncParams) = SignatureType.Address
    }

    val sigReturnType = sig.returnType
    
    if (!isCFunc) {
      sigReturnType match {
        case rt: TypeVariable                                  => specialParamMap(RetByVal) = if (isStandalone) SignatureType.Address else rt
        case rt if rt.isVariableSizeType                       => specialParamMap(RetByVal) = if (isStandalone) SignatureType.Box(rt) else SignatureType.Address // TODO: allow `rt` here instead
        case rt @ (SignatureType.Unit | SignatureType.Nothing) => specialParamMap(RetByVal) = rt
        case rt if rt.isRecord                                 => specialParamMap(RetByVal) = rt
        case _ =>
      }
    } else if (sigReturnType.isRecord) {
      specialParamMap(CFuncRetByVal) = sigReturnType
    }

    val abiSig = makeABISigWithSpecialParams(specialParamMap)

    val specialParameters = SpecialParamSet(specialParamMap.keys)

    (abiSig, specialParameters)
  }
}

abstract class ABI[IR <: IReg : ClassTag, FR <: FReg : ClassTag] protected(val methodType: MethodType, protected val cc: CallingConvention[IR, FR]) {
  require(methodType.callConv == cc.sourceCC)
  if (methodType.isVarArgs) {
    assert(methodType.areVarArgsInitialized)
  }

  protected def resultRegsImpl: Array[AnyReg]

  private def resultRegs: Array[AnyReg] = if (hasAltLocationResult) Array.empty[AnyReg] else resultRegsImpl

  /** Returns array of param locations and size of used Tail, if any exists. */
  protected def initLocations(iRegsQueue: ParamsQueue[IR], fRegsQueue: ParamsQueue[FR], limit: Int): (Array[Location], Option[Int])

  /** Creates TailSlot of `kind` at current `tailSize` offset from TR. Returns pair of this slot and new Tail size. */
  protected def makeTailSlot(paramIdx: Int, kind: TypeKind, tailSize: Int) = {
    assert(!isPreservedParameter(paramIdx), "@Preserved makes no sense on Tail parameter")
    assert(!isAltLocationParameter(paramIdx), "@AltLocation makes no sense on Tail parameter")
    val asmType = kind.toBytecodeApproximation match {
      case TypeKind.VOID => AsmType.U8
      case kind => kind.toAsm
    }
    val occupiedSize = if asmType == PTR then addressSize else asmType.sizeInBytes // TODO-NEW-ABI: consider making alignment method of AsmType or ABI
    assert(occupiedSize <= stackSlotSize) // Otherwise we should align Tail allocation address to `occupiedSize`
    assert(isAligned(tailSize, stackSlotSize))
    (TailSlot(tailSize, asmType), alignUp(tailSize + occupiedSize, stackSlotSize))
  }

  // independent from actual used registers for given parameters
  final def allArgumentIRegs: Array[IR] = cc.baseIRegs.headArea
  final def allArgumentFRegs: Array[FR] = cc.baseFRegs.headArea

  /** Each method may use some size on caller frame. We determine `sizeOnCallerFrame`, including the following:
    *  1. Caller's frame descriptor
    *  1. Platform-specific space (e.g. shadow space on windows amd64)
    *  1. Tail size
    * It does not include return address slot even if one exists.
    */
  val (paramLocations, sizeOnCallerFrameInBytes, iRegArgs, fRegArgs, tailSize) = {
    val iRegStream = new ParamsQueue(allArgumentIRegs)
    val fRegStream = new ParamsQueue(allArgumentFRegs)
    val nonVariadicParams = if isJETVarArgs then methodType.firstVarArg else parameterCount
    val (paramLocations, tailSize) = initLocations(iRegStream, fRegStream, nonVariadicParams min methodType.headInLimit)
    (paramLocations, tailSize.getOrElse(0) + stackParamsStartOffset, iRegStream.close(), fRegStream.close(), tailSize)
  }

  private val (preservedParamRegs, volatileParamRegs) = if (methodType.hasPreservedParameters) {
    val (preserved, volatile) = paramLocations.zipWithIndex filter (_._1.isReg) partitionMap { case (loc, idx) =>
      if isPreservedParameter(idx) then Left(loc.asReg) else Right(loc.asReg)
    }
    (preserved.toSet, volatile)
  } else {
    (Set.empty[AnyReg], paramLocations collect { case x if x.isReg => x.asReg })
  }

  assert(!resultRegs.exists(preservedParamRegs),
    "@Preserved is not allowed on parameters passed on registers used to return value")

  protected val (iRegs, fRegs) = if (cc.ecoFriendly) {
    val volatilesAnyway = resultRegs ++ volatileParamRegs ++ cc.alwaysVolatile
    (cc.baseIRegs.withAllNonVolatile(except = volatilesAnyway), cc.baseFRegs.withAllNonVolatile(except = volatilesAnyway))
  } else {
    (cc.baseIRegs.withExtraNonVolatile(preservedParamRegs), cc.baseFRegs.withExtraNonVolatile(preservedParamRegs))
  }

  /** Returns true iff `r` is volatile (caller-saved, not preserved through call). */
  final def isVolatile(r: AnyReg): Boolean = r match {
    case r: IR => iRegs.volatilesSet(r)
    case r: FR => fRegs.volatilesSet(r)
  }

  /** Returns true iff `r` is non-volatile (callee-saved, preserved through call). */
  final def isNonVolatile(r: AnyReg) = !isVolatile(r)

  assert(volatileParamRegs forall isVolatile)
  assert(preservedParamRegs forall isNonVolatile)

  final def availableIRegs    : Array[IR] = iRegs.available
  final def volatileIRegs     : Array[IR] = iRegs.volatiles
  final def usedArgumentIRegs : collection.Set[IR] = iRegArgs.used

  def savedIRegsOrder         : Iterator[IR]

  final def availableFRegs    : Array[FR] = fRegs.available
  final def volatileFRegs     : Array[FR] = fRegs.volatiles
  final def usedArgumentFRegs : collection.Set[FR] = fRegArgs.used

  def savedFRegsOrder         : Iterator[FR]

  final def shouldBeSavedInPrologue(r: AnyReg): Boolean =
    (r == linkRegister) || isNonVolatile(r ensuring (_ != stackPointer))

  /** Returns true iff reg is volatile or used for parameter passing (directly or as TR). */
  final def isTouched(r: AnyReg): Boolean =
    isVolatile(r) || (hasRealTail && (r == tailRegister)) || (preservedParamRegs contains r)

  /** Returns true iff short integral values are passed as-is in parameters and return value, without conversion to 32-bit int. */
  def allowShortIntegers: Boolean = false // most common implementation

  /** Returns offset of callee stack parameters (excluding callers frame descriptor if exists one) from SP of caller,
    * calculated before call instruction (without return address if exists one).
    */
  def stackParamsStartOffset: Int =
    if (cc.hasFrameDescriptorSlotParam) stackSlotSize else 0 // most common implementation

  protected def callerFrameTopMayBeUsed: Boolean =
    isCVarArgs || // Frame top may contains C var-args stack parameter
    (sizeOnCallerFrameInBytes > 0 && (stackParamsStartOffset == 0)) // Frame top guaranteed contains first stack parameter

  /** Returns true iff `caller` frame slot [SP] contains it's descriptor and method in this CC can spoil it. */
  final def spoilsCallerFrameDescriptor(caller: MethodType): Boolean =
    caller.callConv.hasManagedExecEnv && callerFrameTopMayBeUsed

  final def sizeOnCallerFrameInSlots = sizeOnCallerFrameInBytes / stackSlotSize

  final def isVarArgs                               = methodType.isVarArgs
  final def isCVarArgs                              = methodType.isCVarArgs
  final def isJETVarArgs                            = methodType.isJETVarArgs
  final def parameterCount                          = methodType.parameterCount
  final def parameterType(paramIdx: Int)            = methodType.parameterType(paramIdx)
  final def parameterTypes: Iterator[SignatureType] = methodType.parameterTypes
  final def isVarArgParam(paramIdx: Int)            = methodType.isVarArgParam(paramIdx)
  final def isPreservedParameter(paramIdx: Int)     = methodType.isPreservedParameter(paramIdx)
  final def isAltLocationParameter(paramIdx: Int)   = methodType.isAltLocationParameter(paramIdx)
  final def hasAltLocationResult                    = methodType.altLocationInfo.methodHasAltLocationResult
  final def hasAltLocationParametersOrResult        = methodType.altLocationInfo.nonEmpty
  final def returnType                              = methodType.returnType

  final def resultLocation: Location = resultRegs.length match {
    case 0 => if (hasAltLocationResult) AltLocation.Result else null
    case 1 => resultRegs.head
  }

  /** Returns true iff stack slots used to pass arguments to `this` ABI are volatile.
    * Default behavior is `false` except the case when @CCall method is called from managed context.
    * See also [[MachineDescription.nodeOnReadOnlyResource]]. */
  final def argumentSlotsAreVolatile(caller: MethodType): Boolean = spoilsCallerFrameDescriptor(caller)

  /** Update bitmap of registers which are scanned by GC at safe point. */
  def updateRegMaskForGCMap(regMaskPar: Int, r: IR) = {
    var regMask = regMaskPar
    val bit = iRegs.savedIndex.getOrElse(r, -1)
    if (bit >= 0) {
      regMask = setBit(regMask, bit)
    }
    regMask
  }

  /** Bitmap of integer registers which are saved in prolog. */
  def getSavedIRegsBitMap(savedIRegs: Iterable[IR]) = makeRegsBitMap(iRegs.savedIndex, savedIRegs)

  /** Bitmap of floating-point registers which are saved in prolog. */
  final def getSavedFRegsBitMap(savedFRegs: Iterable[FR]) = makeRegsBitMap(fRegs.savedIndex, savedFRegs)

  /** Returns index of `r` in `arguments` array. Fails iff `r` not contains in `arguments`. */
  def getParamIdxByReg(r: AnyReg): Int = r match {
    case r: IR => iRegs.headAreaIndex(r)
    case r: FR => fRegs.headAreaIndex(r)
  }

  final def hasTail = tailSize.isDefined

  /** Returns true iff this ABI is JET calling convention and has Tail. In this case Tail will be supported the following way:
    *  1. Caller: Tail params may be passed anywhere in memory
    *  1. Caller: Invocation should set TR to Tail start
    *     - In CBC caller sets TR even for non-JET CC calls so those also said to have a real tail,
    *       which is used by JIT & interpreter to pass arguments in a platform-dependent way.
    */
  final def hasRealTail = hasTail && (cc.isJET || targetArch == CBC)

  /** Returns true iff this ABI is not JET calling convention and has Tail. In this case Tail will be supported the following way:
    *  1. Caller: Tail params should be passed according to calling convention requirements (on fixed offset from SP)
    *  1. Caller: Invocation may not set TR to [SP + fixed offset]
    *  1. Callee: In prologue we will emulate Tail (set TR to [SP + fixed offset])
    */
  final def hasEmulatedTail = hasTail && !cc.isJET

  final def jetVarArgsOffset: Int = {
    assert(isJETVarArgs)
    tailSize.get
  }

  final def epilogueGCPointTrapOffset(typeProvider: TypeProvider) = if (methodType.returnType.isTraceableReference(typeProvider)) {
    RTConst.GCPoints.epilogueReturningRefTrapOffset.intValue
  } else {
    RTConst.GCPoints.epilogueNotReturningRefTrapOffset.intValue
  }
}
