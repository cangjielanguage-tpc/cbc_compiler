/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.Width.{W32, W64, WPTR}
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Location, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.{STORE_LOAD, STORE_STORE}
import com.huawei.excelsior.jet.codeemitter.BranchOp.{EQ, LT, TESTNZ, ULT}
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.RTSProc.*
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, TailSlot}
import com.huawei.excelsior.jet.compiler.abi.{ABI, DAIGenerator, Frame, SlotBase}
import com.huawei.excelsior.jet.compiler.bytecode.*
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.SourceCodeLabel
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator.{EnrichGenerationMode, isFastTypeCheck}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.*
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.NodeType.{ADDR, TREF}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.IntValue
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType

import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.chaining.scalaUtilChainingOps

/** Class for generation of machine code using IR nodes and their locations information. */
object Generator {

  /** Every consumer of rich EOP also can accept plain one. Baseline supports following modes of enrichment: */
  enum EnrichGenerationMode {
    case NOP           // provide plain EOP
    case WITHOUT_CACHE // try to enrich EOP (compact and not performance critical way)
    case WITH_CACHE    // try to enrich EOP (performance critical way)
  }

  trait XSiteCreator {
    def xinfo: XInfo

    protected def addXSiteImpl(gen: Generator, kind: XSiteKind, site: Label, handler: Label, bytecodeOffset: Int, lineNumber: Int,
                 inlineContext: InlineContext, calledMethodRef: MethodReference, softExceptionID: Int, domain: Domain,
                 additionalAliveLocations: Seq[(Node, Location)]): Unit = {
      if (kind.needGCMap) {
        gen.gatherGCMap(xinfo, additionalAliveLocations)
      } else {
        assert(additionalAliveLocations.isEmpty)
      }
      xinfo.addXSite(site, handler, kind, 0, bytecodeOffset, lineNumber, inlineContext, calledMethodRef, softExceptionID, domain)
    }

    def addXSite(gen: Generator, kind: XSiteKind, site: Label, bytecodeOffset: Int, lineNumber: Int,
                 inlineContext: InlineContext, calledMethodRef: MethodReference, softExceptionID: Int,
                 domain: Domain, additionalAliveLocations: Seq[(Node, Location)]): Unit
  }

  class XSitesWithoutHandler(override val xinfo: XInfo) extends Generator.XSiteCreator {
    def this() = this(new XInfo())

    override def addXSite(gen: Generator, kind: XSiteKind, site: Label, bytecodeOffset: Int,
                          lineNumber: Int, inlineContext: InlineContext, calledMethodRef: MethodReference,
                          softExceptionID: Int, domain: Domain, additionalAliveLocations: Seq[(Node, Location)]): Unit = {
      addXSiteImpl(gen, kind, site, null, bytecodeOffset, lineNumber, inlineContext, calledMethodRef, softExceptionID, domain, additionalAliveLocations)
    }
  }

  // casts to hierarchy root are rare and we don't want to make type checks even more complicated because of them
  private def isFastTypeCheck(`type`: Type) =
    (`type`.isClass && !`type`.isHierarchyRoot) ||
      (`type`.isArray && !`type`.getArrayBase.isInterface)
}

@nowarn("msg=match may not be exhaustive")
abstract class Generator protected(
  protected val env: Environment,
  protected val symbolLinker: SymbolLinker,
  protected val context: GenerationContext,

  protected val emit: CodeEmitter,
  protected val globalLocations: GlobalLocations,

  val locations: Locations,
  val nodes: Nodes,

  xSites: Generator.XSiteCreator,
  enableOptimizedEnrichGeneration: Boolean
) {
  protected implicit val typeProvider: TypeProvider = env.getTypeProvider
  protected val frame = globalLocations.frame

  private val enrichGenerationMode = {
    if (!hasEops || env.enabled(NoEnrichInBaseline)) {
      EnrichGenerationMode.NOP
    } else if (enableOptimizedEnrichGeneration) {
      EnrichGenerationMode.WITH_CACHE
    } else {
      EnrichGenerationMode.WITHOUT_CACHE
    }
  }

  private val genDebug = env.enabled(GenDebug)

  private var currentLineNumber = LineNumber.UNKNOWN
  private var currentBC = BytecodeOffset.INVALID
  private var lastPosition: SourceCodeLabel = _

  private def savePosition(): Unit = if (genDebug && LineNumber.isKnown(currentLineNumber)) {
    val newPosition = SourceCodeLabel(context.inlineContext, currentLineNumber, 0, null)
    if (lastPosition == null || lastPosition != newPosition) {
      emit.bind(newPosition)
      lastPosition = newPosition
    }
  }

  /** @param lineNumber line number for stack trace generation, may be obtained by
    *                   [[com.huawei.excelsior.jet.compiler.symlevel.Method.CodeAttribute.findLineNumber]]
    *                   or be equal to [[LineNumber.UNKNOWN]]
    */
  def setCurrentLineNumber(lineNumber: Int): Unit = {
    currentLineNumber = lineNumber ensuring LineNumber.isValid _
    savePosition()
  }

  def setCurrentBytecodeOffset(offset: Int): Unit =
    currentBC = offset ensuring BytecodeOffset.isValid _

  private def genBranchIfClassIsPrepared(`type`: Type, target: Label): Unit = {
    val flags = mem(I16, `type`.getTypeHandle, RTConst.TypeHandle.flags.offset)
    emit.branchIf(flags, TESTNZ, RTConst.TypeHandle.Flags.PREPARED.intValue, target)
  }

  private def genBranchIfClassIsInitialized(typeHandle: Symbol, target: Label): Unit = {
    val initializedFlag = mem(PTR, typeHandle, RTConst.HostingTypeHandle.initialized.offset)
    emit.branchIfNotNull(initializedFlag, target)
  }

  // TODO-SYMLEVEL change Type -> ClassType after refactoring
  private def genPreparationCheck(`type`: Type, method: Method, kind: PreparationKind): Unit = {
    assert(!isJIT)

    def canAssertTypePreparation: Boolean = method.canAssertTypePreparation(`type`)

    def genLazyCheck(failCase: => Unit): Unit = {
      val skip = emit.newLabel
      genBranchIfClassIsPrepared(`type`, skip)

      nodes.withSavedState {
        failCase
      }

      emit.bind(skip)
    }

    if (kind.bootstrap && !`type`.isNonBootstrapAnnotated) {
      env.markForBootstrapPreparation(`type`)
    } else if (!kind.`lazy`) {
      env.markForPreparation(`type`)
    }

    if (kind.`lazy` && !kind.assertionOnly) {
      genLazyCheck {
        rtsCall(JR_PrepareType)(`type`.getTypeHandle)
      }
    } else if ((kind.assertionOnly || env.enabled(PreparationAsserts)) && canAssertTypePreparation) {
      genLazyCheck {
        genFatalError(s"type ${`type`.getName} should be prepared")
      }
    }
  }

  // TODO: collapse overloads when MethodBytecodeGenerator is translated to Scala
  def ensurePrepared(tpe: Type): Unit = {
    ensurePrepared(tpe, context.inlineContext.method, null)
  }

  def ensurePrepared(tpe: Type, context: Method): Unit = {
    ensurePrepared(tpe, context, null)
  }

  def ensurePrepared(tpe: Type, context: Method, kind: PreparationKind): Unit = {
    if (tpe != null && !(kind == null && tpe == this.context.hostingClass) && !tpe.isPrepared) {
      val checkKind = if (kind != null) kind else PreparationKind(context.isManaged, env)
      genPreparationCheck(tpe, context, checkKind)
    }
  }

  private def withPreparation[T](t: Type)(action: Type => T): T = {
    ensurePrepared(PreparationRequired.forType(t))
    action(t)
  }

  def acquireInstanceDescriptor(t: Type) = withPreparation(t)(_.getInstanceDescriptor)

  def copyWithoutRelease(dst: Node, src: Node): Unit = {
    assert(nodes.sizeOf(dst) == nodes.sizeOf(src))
    // Currently implemented only for address-like types.
    val srcLoc = nodes.loadToIReg(src)
    val dstLoc = nodes.bindToAnyFreeIRegWithPreferred(dst, srcLoc)
    emit.mov(dstLoc, srcLoc)
  }

  def copyAndRelease(dst: Node, src: Node): Unit = {
    assert(nodes.sizeOf(dst) == nodes.sizeOf(src))
    val srcLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(src)
    val dstLoc = nodes.bindToAnyFreeIRegWithPreferred(dst, srcLoc)
    emit.mov(dstLoc, srcLoc)
  }

  /** Copy reference to new frame slot which should not be tracked by reg.alloc and would be marked explicitly as traced. */
  def copyRefValueToNewTracedFrameSlot(node: Node): MemLocal = {
    assert(node.`type` == TREF)
    val loc = globalLocations.allocateOnStackTraced(node.asmType)
    emit.copyAny(loc, nodes.getLoc(node))
    loc
  }

  def gatherGCMap(xinfo: XInfo, additionalAliveLocations: Seq[(Node, Location)]): Unit = {
    xinfo.startGCMap()

    val aliveLocations = nodes.locationsMapping.toSeq ++ additionalAliveLocations
    var regsMask = 0
    for ((node, loc) <- aliveLocations if node.`type` == TREF) {
      if (loc.isIReg) {
        val abi = frame.abi.asInstanceOf[ABI[IReg, ?]]
        regsMask = abi.updateRegMaskForGCMap(regsMask, loc.asIReg)

      } else if (loc.isMem) {
        val refSize = locations.sizeOf(TREF)

        val wholeSlot = loc match {
          case local: MemLocal =>
            Some(globalLocations.slotByLoc(local))
          case MemBased(tpe, base, offset) => if (base == stackPointer) {
            Some(frame.newSlot(locations.sizeOf(tpe.width), SlotBase.SP, offset))
          } else {
            None
            // no-op, we do not collect Tail params for gc Map
          }
        }

        for (slot <- wholeSlot) {
          val refSlot = if (slot.size > refSize) {
            // Sometimes we bind references to memory slots of larger size.
            // It can happen on 32-bit platforms in case of special handler slots
            // (which are 8 bytes) when we bind 4 bytes references to their lower part.
            assert(slot.isBound)
            frame.newSlot(refSize, slot.base, slot.offset)
          } else {
            assert(slot.size == refSize)
            slot
          }
          // TODO: possibly, if gathering is performed for Call, then `allowedForDeltaList` should be set to false for additional param location
          xinfo.addTracedSlot(refSlot)
        }

      } else {
        assert(Locations.isInvalid(loc))
      }
    }

    for (x <- globalLocations.tracedStackAllocSlots) {
      assert(x.size >= addressSize)
      xinfo.addTracedSlot(x)
    }
    xinfo.setRegistersMask(regsMask)
    xinfo.setUnmovableRegisters(0)
  }

  private def addXSite(kind: XSiteKind, site: Label, calledMethodRef: MethodReference,
                       softExceptionID: Int, additionalAliveLocations: Seq[(Node, Location)]): Unit = {
    if (context.shouldAddXSite) {
      val (xSiteLineNum, xSiteBytecodePos, xSiteInlineContext) = if (context.isManaged) {
        (currentLineNumber, currentBC, context.inlineContext)
      } else {
        (LineNumber.UNKNOWN, BytecodeOffset.INVALID, null)
      }
      val domainOwner = context.inlineContext.method
      xSites.addXSite(this, kind, site, xSiteBytecodePos, xSiteLineNum, xSiteInlineContext, calledMethodRef, softExceptionID, domainOwner.getDomain, additionalAliveLocations)
    }
  }

  private def addXSite(kind: XSiteKind, site: Label, calledMethodRef: MethodReference, paramLocations: Seq[(Node, Location)]): Unit =
    addXSite(kind, site, calledMethodRef, RTConst.XTable.State.Initial.SOFT_EXCEPTION_ID.intValue, paramLocations)

  private def addXSite(kind: XSiteKind, softExceptionID: Int): Unit = {
    assert(!kind.isCall)
    val site = emit.newBoundLabel
    addXSite(kind, site, null, softExceptionID, Seq.empty)
  }

  protected def addXSite(kind: XSiteKind): Unit =
    addXSite(kind, RTConst.XTable.State.Initial.SOFT_EXCEPTION_ID.intValue)

  protected def addSoftExceptionXSite(softExceptionID: Int): Unit =
    addXSite(XSiteKind.SOFT_EXCEPTION, softExceptionID)

  protected def addPreCallXSite(paramLocations: Seq[(Node, Location)]): Unit = {
    val site = emit.newBoundLabel // note that it will be adjusted during xSites preparation
    addXSite(XSiteKind.PRE_CALL, site, null, RTConst.XTable.State.Initial.SOFT_EXCEPTION_ID.intValue, paramLocations)
  }

  protected def addCallXSite(calledMethodRef: MethodReference, isDeferred: Boolean, paramLocations: Seq[(Node, Location)]): Unit = {
    val site = emit.newBoundLabel // note that it will be adjusted during xSites preparation
    val kind = if (isDeferred) XSiteKind.DEFERRED_CALL else XSiteKind.CALL
    addXSite(kind, site, calledMethodRef,
      paramLocations.filter {
        case (_, MemBased(_, base, _)) if base == stackPointer =>
          true // parameters passed on stack stay alive during the call

        case (_, loc) if loc.isReg =>
          false // parameters passed on registers should be tracked by callee gcmaps

        case (n, MemBased(_, base, _)) if base == execEnvRegister =>
          assert(n.`type` != TREF) // only primitive parameters are currently passed on alt-locations
          false // even when references are supported on alt-locs, they will be scanned via callee gcmaps

        // no other parameters locations are expected, MatchError is intended
      }
    )
  }

  protected def needPreCallXSite(targetRef: MethodReference, isDeferred: Boolean): Boolean = {
    import MethodReferenceAccessKind.*
    targetRef match {
      case ref: BytecodeMethodReference if ref.isMemberNameInvoke => true
      case ref => ref.accessKind match {
        case VIRTUAL | INTERFACE => true
        case STATIC | SPECIAL => isDeferred
        case STATIC_VIRTUAL | MUT => shouldNotReachHere(ref)
      }
    }
  }

  /** Generates invocation of `proc` passing `params` to it and binds `result` if exists one. Always releases all
    * temporary nodes passed in `params`, also releases bytecode param nodes if `releaseBCParams` is true.
    *
    * @see [[genInvokeMethod]]
    */
  def rtsCall(proc: RTSProc, result: Node = null, releaseBCParams: Boolean = false)(params: Any*): Unit = {
    val target = env.getRTSProc(proc)
    assert(target.isStatic && !target.isVarArgs)

    val targetRef = new MethodReference(target, MethodReferenceAccessKind.STATIC)

    val paramNodes = {
      // TODO rename to enable general use of this methods without call
      def nodeOnRegForRTSCallParam(`type`: NodeType): Node =
        Node.newTemporary(`type`) tap nodes.bindToAnyFreeIReg

      def rtsCallParamSymbol(symbol: Symbol): Node = {
        val param = nodeOnRegForRTSCallParam(ADDR)
        emit.lea(nodes.loadToIReg(param), symbol)
        param
      }

      def rtsCallParamInt(number: Int): Node = {
        val param = nodeOnRegForRTSCallParam(NodeType.INT)
        emit.mov32(nodes.loadToIReg(param), number)
        param
      }

      params.map {
        case node: Node => node
        case symbol: Symbol => rtsCallParamSymbol(symbol)
        case number: Int => rtsCallParamInt(number)
      }
    }

    genInvokeNormal(targetRef, paramNodes, releaseBCParams, result)

    // duplicate current line number to avoid multiple calls between 2 line-num infos
    if (context.isRootInlineLevel) {
      savePosition()
    } else {
      // TODO deal with inlining
    }
  }

  def genFatalError(err: String): Unit = {
    val fullMsg = s"Fatal Error: $err (${context.fullName})"
    val msgSymbol = symbolLinker.makeConstStringData(XString.ascii(fullMsg), bstr = true)
    rtsCall(JR_FatalError)(msgSymbol)
  }

  private def genGetIMTForInvoke(`object`: Node, interfaceType: Type, result: Node): Unit = {
    assert(!interfaceType.isDeferred)
    assert(interfaceType.isInterface)
    assert(`object`.`type` == TREF)

    val objectForCall = if (`object`.isTemporary) {
      // Current implementation of call releases all temporary nodes. So we create a copy.
      val objectForCall = Node.newTemporary(TREF)
      emit.copyAny(nodes.bindToAnyFreeIReg(objectForCall), nodes.getLoc(`object`), objectForCall.asmType)
      objectForCall
    } else {
      `object`
    }

    val cache = symbolLinker.makeUninitializedData(RTConst.InterfCast1LCache.size)
    rtsCall(JR_FindIMTOrThrow, result)(objectForCall, interfaceType.getTypeHandle, cache)
  }

  protected def hasEops = RTConst.Eop.ENABLED.boolValue

  /** Returns new enriched node. */
  protected final def genEnrichedCopy(obj: Node, interfSigType: SignatureType) = {
    assert(interfSigType.isInterface)
    val interf = interfSigType.symType(env.getTypeProvider)

    val objCopy = Node.newTemporary(TREF /*interface reference*/)
    copyWithoutRelease(objCopy, obj) // copy obj node to prevent releasing

    if (enrichGenerationMode == EnrichGenerationMode.NOP || env.getTypeProvider.isManagedEopUnderlyingType(interfSigType)) {
      objCopy

    } else {
      val resultBits = Node.newTemporary(ADDR /*Eop.Plain*/)
      assert(hasEops)
      if (enrichGenerationMode == EnrichGenerationMode.WITH_CACHE) {
        val cache = symbolLinker.makeUninitializedData(RTConst.InterfCast1LCache.size)
        rtsCall(IFaceOps_castAndEnrich, resultBits)(objCopy, interf.getTypeHandle, cache)
      } else {
        assert(enrichGenerationMode == EnrichGenerationMode.WITHOUT_CACHE)
        rtsCall(IFaceOps_castAndEnrichNoCache, resultBits)(objCopy, interf.getTypeHandle)
      }
      val result = Node.newTemporary(TREF /*interface reference*/)
      copyAndRelease(result, resultBits)
      result
    }
  }

  protected final def genEnrichedCopyAndReleaseIfNotUsedLater(obj: Node, interf: SignatureType): Node = {
    val enrichedCopy = genEnrichedCopy(obj, interf)
    nodes.releaseLocIfNotUsedLater(obj)
    enrichedCopy
  }

  /** Deprives given node. */
  final def genDeprive(eop: Node): Unit = depriveEOP(nodes.getLoc(eop))

  /** Deprives given node if it may be rich. */
  final def genDepriveIfNeeded(eop: Node, `type`: SignatureType): Unit = {
    if ((`type`.isDeferred || `type`.isInterface) && !env.getTypeProvider.isManagedEopUnderlyingType(`type`)) {
      // we must deprive absent types (because they may be interfaces)
      genDeprive(eop)
    }
  }

  protected final def instanceFieldAddr(obj: IReg, `type`: AsmType, fieldOffset: Int) =
    mem(`type`, obj, fieldOffset)

  protected final def arrayElemAddr(array: IReg, index: IReg, elemTKind: BytecodeTypeKind) = {
    val bodyOffs = if (languagePack == LanguagePack.SCALA) {
      RTConst.ScalaArray.ARRAY_BODY_OFFS.intValue
    } else {
      RTConst.JavaArray.ARRAY_BODY_OFFS.intValue
    }
    mem(elemTKind.toAsm, array, scaled(index, elemTKind.width), bodyOffs)
  }

  final def genReturnValue(returnValue: Node): Unit = {
    assert(context.isRootInlineLevel)

    val `type` = context.rootMethodType.returnType
    val enrichedReturnValue = if (`type`.isInterface) {
      genEnrichedCopyAndReleaseIfNotUsedLater(returnValue, `type`)
    } else {
      returnValue
    }

    val dst = frame.abi.resultLocation match {
      case altLoc: AltLocation => convertAltLocation(altLoc)
      case loc => loc
    }
    val src = nodes.getLoc(enrichedReturnValue)

    movReturnValue(enrichedReturnValue.asmType, dst, src)
  }

  protected final def movReturnValue(tpe: AsmType, dst: Location, src: Location): Unit =
    emit.copyAny(dst, src, tpe)

  final def genTDBarrier(obj: IReg, mayBeNull: Boolean): Unit = {
    val done = emit.newLabel
    if (mayBeNull) {
      emit.branchIfNull(obj, done)
    }
    assert(context.shouldAddXSite)
    addSoftExceptionXSite(RTConst.SoftExceptions.Kind.TD_BARRIER.intValue)
    emit.borrowScratch { tmp =>
      val start = emit.newBoundLabel
      emit.load(tmp, mem(PTR, obj, RTConst.HeapObj.TYPEDESC_OFFSET.intValue))
      clearIdescHigh16BitsIfNeeded(tmp)
      val end = emit.newBoundLabel
      emit.load(tmp, mem(PTR, tmp, end.position - start.position))
    }
    emit.bind(done)
  }

  final def genTDBarrier(obj: Node, mayBeNull: Boolean): Unit =
    genTDBarrier(nodes.loadToIReg(obj), mayBeNull)

  def readStaticField(sigType: SignatureType, isAtomic: Boolean, field: Symbol, result: Node): Unit

  def writeStaticField(sigType: SignatureType, isAtomic: Boolean, field: Symbol, value: Node): Unit

  def readInstanceField(sigType: SignatureType, isAtomic: Boolean, obj: Node, fieldOffset: Int, result: Node, releaseObject: Boolean = true): Unit

  def writeInstanceField(sigType: SignatureType, isAtomic: Boolean, obj: Node, fieldOffset: Int, value: Node): Unit

  protected final def arrayLenAddr(array: IReg) = {
    val lenOffs = if (languagePack == LanguagePack.SCALA) {
      RTConst.ScalaArray.length.offset
    } else {
      RTConst.JavaArray.length.offset
    }
    instanceFieldAddr(array, I32, lenOffs)
  }

  def readArrayElem(kind: BytecodeTypeKind, array: Node, index: Node, result: Node): Unit

  def writeArrayElem(kind: BytecodeTypeKind, array: Node, index: Node, value: Node): Unit

  final def readArrayLength(array: Node, result: Node): Unit = {
    val arrayReg = nodes.loadToIRegAndReleaseIfNotUsedLater(array)
    val resultReg = nodes.bindToAnyFreeIReg(result)
    emit.load(resultReg, arrayLenAddr(arrayReg))
  }

  def genConstString(result: Node, str: ConstString): Unit = {
    val resultLoc = nodes.bindToAnyFreeIReg(result)
    assert(context.isManaged) // In unmanaged context constant strings are allowed only for AJ intrinsics implementation
    assert(!str.getHost.isInfectedAJClass) // Constant strings are not allowed in infected AJ classes
    ensurePrepared(PreparationRequired.forConstString(str))
    val stringTable = str.getStringTable
    emit.load(resultLoc, mem(PTR, stringTable, stringTable.dataOffset + str.getStringNumber * addressSize))
  }

  final def genIntConst(result: Node, value: Int): Unit =
    emit.mov32(nodes.bindToAnyFreeIReg(result), value)

  final def genLongConst(result: Node, value: Long): Unit =
    emit.mov64(nodes.bindToAnyFreeIReg(result), value)

  final def genFloatConst(result: Node, value: Float): Unit =
    emit.fmov32(nodes.bindToAnyFreeFReg(result), value)

  final def genDoubleConst(result: Node, value: Double): Unit =
    emit.fmov64(nodes.bindToAnyFreeFReg(result), value)

  /** Conversion from fp-type or to fp-type. */
  protected def genConvertFloat(op: ConvertOp, arg: Node, result: Node): Unit

  /** Conversion from long to int or from int to long. */
  protected def genConvertLong(op: ConvertOp, arg: Node, result: Node): Unit

  final def genConvert(op: ConvertOp, arg: Node, result: Node): Unit = {
    import ConvertOp.*
    op match {
      case I2B | I2C | I2S =>
        val argLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(arg)
        val resultLoc = nodes.bindToAnyFreeIReg(result)
        signExtendShortIntegralToInt(op.dstKind, resultLoc, argLoc)

      case I2L | L2I =>
        genConvertLong(op, arg, result)

      case I2F | I2D | L2F | L2D | F2I | F2L | D2I | D2L | F2D | D2F =>
        genConvertFloat(op, arg, result)
    }
  }

  def genBinaryArithOp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit

  def genNeg(tkind: TypeKind, arg: Node, result: Node): Unit

  /** Generate clinit check if `refClass` has clinit. */
  def genClinit(refClass: ClassType): Unit = {
    if (!context.isClinited(refClass)) {
      assert(context.isManaged)
      val skip = emit.newLabel
      val typeHandle = refClass.getTypeHandle
      genBranchIfClassIsInitialized(typeHandle, skip)

      nodes.withSavedState {
        if (isJIT) {
          rtsCall(X2J_INIT_CLASS)(refClass.getTypeHandle)
        } else {
          rtsCall(JR_Clinit)(env.getImportedClassIdx(refClass, null))
        }
      }

      emit.bind(skip)
    }
  }

  def genCheckNull(node: Node, refType: Type = null): Unit = {
    assert(refType == null || refType.isReference)

    if (refType != null && refType.isThinClass) {
      assert(!isJIT) // may be called by CallToManagedGenerator
      genTrapCheck(null, nodes.loadToIReg(node))
      return
    }

    if (context.isManaged) {
      // Note that during bootstrapping baseline for new platform this case may be implemented as a software null check
      // with explicit call to JR_ThrowNullPointerException
      genTrapCheck(XSiteKind.NULLCHECK, nodes.loadToIReg(node))

    } else if (isWorkMode) {
      val ok = emit.newLabel
      emit.branchIfNotNull(nodes.loadToIReg(node), ok)

      nodes.withSavedState {
        genFatalError("NullPointerException")
      }

      emit.bind(ok)
    }
  }

  protected def branchIf(reg: IReg, op: BranchOp, mem: Mem, target: Label): Unit = emit.borrowScratch { tmp =>
    emit.load(tmp, mem)
    emit.branchIf(op, reg, tmp, mem.width, target)
  }

  final def genCheckIndex(array: Node, index: Node): Unit = {
    assert(context.isManaged)
    val ok = emit.newLabel
    branchIf(nodes.loadToIReg(index), ULT, arrayLenAddr(nodes.loadToIReg(array)), ok)

    nodes.withSavedState {
      if (languagePack == LanguagePack.SCALA) {
        rtsCall(JR_ThrowScalaArrayIndexOutOfBoundsException)()
      } else {
        rtsCall(JR_ThrowArrayIndexOutOfBoundsException)()
      }
    }

    emit.bind(ok)
  }

  def genClassObject(result: Node, `type`: Type): Unit = {
    assert(context.isManaged)
    val (typeSymbol, dimNum) = if (`type`.isJavaArray) {
      (`type`.getArrayBase.getTypeHandle, `type`.getArrayDimnum)
    } else {
      (`type`.getTypeHandle, 0)
    }
    assert(!`type`.isDeferred)
    rtsCall(JR_GetClassObject, result)(typeSymbol, dimNum)
  }

  protected final def genInvokeTargetIndirect(target: Node, appendix: Node) = {
    passAppendixArgumentOfHookInvoker(appendix, frame.EER)
    assert(!target.`type`.isFP)
    val loc = nodes.getLoc(target)
    if (loc.isIReg) {
      emit.call(loc.asIReg)
    } else {
      assert(loc.isMem)
      emit.callIndirect(loc.asMem)
    }
    nodes.releaseLocIfNotUsedLater(target)
  }

  /** Generate method invocation: save target method volatile regs, pass parameters, and call target method.
    *
    * @param targetRef           method reference for target method
    * @param target              node with address of target method in case of indirect call
    *                            or symbol of target method in case of direct call
    * @param ctmwFrameDescriptor frame descriptor slot of call to managed wrapper for call-to-managed calls, `null` otherwise
    * @param appendix            extra implicit parameter passed on special fixed location, `null` otherwise
    * @param params              params for target method
    * @param result              node where result will be stored, if `result != null`
    * @param callToManaged       this is a call to managed from unmanaged
    * @param callToNative        this is a call to real native code from native wrapper
    * @param forceAddXSite       indicates whether xSite should be added for this call even if it couldn't throw an exception
    * @param releaseBCParams     indicates whether params should be released
    */
  protected def genCall(targetRef: MethodReference, target: AnyRef,
                        ctmwFrameDescriptor: Frame.Slot, appendix: Node,
                        params: collection.Seq[Node], result: Node,
                        callToManaged: Boolean, callToNative: Boolean, forceAddXSite: Boolean, releaseBCParams: Boolean): Unit

  protected def placeParam(abi: ABI[_, _], dst: Location, param: Node, paramIdx: Int): Seq[(Node, Location)] = {
    emit.copyAny(dst, nodes.getLoc(param), param.asmType)
    Seq((param, dst))
  }

  protected def passMethodParams(abi: ABI[_, _], targetParams: collection.Seq[Node], releaseBCParams: Boolean): Seq[(Node, Location)] = {
    val paramLocations = mutable.ListBuffer.empty[(Node, Location)]
    val targetParamsIter = targetParams.iterator

    for (i <- 0 until abi.parameterCount) {
      val param = targetParamsIter.next()
      val pkind = abi.parameterType(i).jbcKind
      assert(checkNodeType(param, pkind))

      val dst = abi.paramLocations(i) match {
        case slot: TailSlot => convertTailSlotForParamPassing(slot, abi)
        case altLoc: AltLocation => convertAltLocation(altLoc)
        case loc => loc
      }

      assert(!dst.isMem || dst.width == param.asmType.width)
      assert(!dst.isIReg || !emit.canSpoil(dst.asIReg),
        "target placement should not be spoiled while repush of stack params")

      paramLocations ++= placeParam(abi, dst, param, i)
    }
    assert(!targetParamsIter.hasNext)

    releaseMethodParams(targetParams, releaseBCParams)
    paramLocations.toSeq
  }

  /** Implements `invocation` which will make call to `callee`. If `callee` has Tail params sets up TR register for it.
    * If root method also have Tail saves it before `invocation` and restores after. */
  protected final def withTRSetupForCall(callee: ABI[?, ?])(invocation: => Unit): Unit = {
    if (!callee.hasRealTail) {
      invocation

    } else {
      val node = if (frame.abi.hasTail) {
        // Guaranteed by [[Locations.iRegs]] definition. In this case we will set TR for callee
        // and restore it from spill slot after call.
        assert(!locations.isAllocatable(tailRegister))
        val node = Node.newTemporary(ADDR)
        val slot = nodes.bindToAnyFreeMem(node)
        emit.store(slot, tailRegister)
        Some(node)

      } else {
        // Guaranteed by [[Nodes.rescueAndSpoilRegs]] method used [[ABI.isTouched]] predicate
        // which includes `tailRegister` for ABI with tail params.
        assert(locations.isFree(tailRegister))
        None
      }

      frame.registerUsedReg(tailRegister)
      emit.lea(tailRegister, mem(stackPointer, callee.stackParamsStartOffset))

      invocation

      node match {
        case Some(node) =>
          emit.load(tailRegister, nodes.getMemLoc(node))
          nodes.releaseLocIfNotUsedLater(node)
        case None =>
      }
    }
  }

  protected final def convertTailSlotForParamPassing(slot: TailSlot, callee: ABI[?, ?]): MemBased =
    mem(slot.tpe, stackPointer, callee.stackParamsStartOffset + slot.offset)

  private def convertAltLocation(altLoc: AltLocation): MemBased =
    mem(PTR, frame.EER, RTConst.ExecEnv.Offsets.altLocation(altLoc.slot).intValue)

  protected def dropFakeArea(fakeAreaSize: Int): Unit =
    emit.addPtr(stackPointer, stackPointer, fakeAreaSize)

  protected final def prepareRegs(targetRef: MethodReference): Unit = {
    if (targetRef.hasMethod && targetRef.method.isNoTracedRegsOnEntry) {
      nodes.ensureNoAliveRefsOnRegs(_.`type` == TREF)
    }
  }

  protected final def checkNoRefsOnRegs(targetRef: MethodReference): Unit = {
    if (targetRef.hasMethod && targetRef.method.isNoTracedRegsOnEntry) {
      nodes.checkNoAliveRefsOnRegs(_.`type` == TREF)
    }
  }

  private def genInvokeMethod(targetRef: MethodReference, rawParams: collection.Seq[Node], result: Node,
                              releaseBCParams: Boolean, callToManaged: Boolean, ctmwFrameDescriptor: Frame.Slot): Unit = {
    // Getting array of real parameters for target method without special params (e.g. first param of indirect call).
    // Note: such special params should be released exclusively.
    val realMethodType = targetRef.realMethodType

    assert(targetRef.method.getSpecialParamsCount == 0, "baseline does not support AJ indirect calls")
    assert(realMethodType.parameterCount == rawParams.size)

    val targetParams = preprocessInterfaceParams(realMethodType, rawParams, releaseBCParams)

    val callTarget = prepareAndMakeCallTarget(targetRef, rawParams)

    invokeNormalPreAction(targetRef)

    val forceAddXSite = context.rootHasFrameDescriptor
    genCall(targetRef, callTarget, ctmwFrameDescriptor, null, targetParams, result, callToManaged, callToNative = false, forceAddXSite, releaseBCParams)
    invokeNormalPostAction(targetRef)

    if (result != null && nodes.hasLoc(result)) {
      genDepriveIfNeeded(result, realMethodType.returnType)
    }
  }

  def genInvokeNormal(targetRef: MethodReference, params: collection.Seq[Node], releaseBCParams: Boolean, result: Node): Unit = { // TODO-DECAF
    assert(!targetRef.methodType.callConv.isManaged || context.isManaged)
    ensurePrepared(PreparationRequired.forInvoke(targetRef))
    genInvokeMethod(targetRef, params, result, releaseBCParams, callToManaged = false, null)
  }

  def genInvokeManaged(targetRef: MethodReference, params: collection.Seq[Node], result: Node, ctmwFrameDescriptor: Frame.Slot): Unit = { // TODO-DECAF
    assert(targetRef.methodType.callConv.isManaged && !context.isManaged)
    assert(!targetRef.method.isVarArgs)
    genInvokeMethod(targetRef, params, result, releaseBCParams = false, callToManaged = true, ctmwFrameDescriptor)
  }

  def genNativeCall(targetRef: MethodReference, nativeAddr: Node, params: collection.Seq[Node], result: Node): Unit = {
    // no enrichment or deprivation needed, params and result of native call are plain
    assert(!targetRef.methodType.callConv.isManaged && context.isManaged)
    genCall(targetRef, nativeAddr, null, null, params, result, callToManaged = false, callToNative = true, forceAddXSite = false, releaseBCParams = true)
  }

  def genInvokeSigPolyIntrinsic(targetType: MethodType, targetAddr: Node, memberName: Node, params: collection.Seq[Node], result: Node): Unit = {
    // "Everybody lies" - it's better to never enrich params and always deprive result of such dynamic calls.
    assert(context.isManaged && targetType.callConv.isManaged)
    val targetRef = new BytecodeMethodReference(targetType, MethodAccessKind.STATIC, isMemberNameInvoke = true)
    genCall(targetRef, targetAddr, null, memberName, params, result, callToManaged = false, callToNative = false, forceAddXSite = false, releaseBCParams = true)

    val returnType = targetType.returnType
    if (returnType.isClass || returnType.isInterface) {
      assert(!env.getTypeProvider.isManagedEopUnderlyingType(returnType))
      genDeprive(result)
    }
  }

  def genInvokeViaDAI(targetRef: MethodReference, dai: DAIGenerator.DAITarget, params: collection.Seq[Node], result: Node): Unit = {
    assert(context.isManaged && targetRef.methodType.callConv.isManaged)
    genCall(targetRef, dai, null, null, params, result, callToManaged = false, callToNative = false, forceAddXSite = false, releaseBCParams = true)
    frame.registerStackCheckForDAICall()
    if (result != null) {
      genDepriveIfNeeded(result, targetRef.methodType.returnType)
    }
  }

  protected def passAppendixArgumentOfHookInvoker(appendix: Node, eeReg: IReg): Unit = if (appendix != null) {
    assert(appendix.`type` == TREF)
    val offs = RTConst.ExecEnv.appendixArgumentOfHookInvoker.offset
    emit.copyAny(mem(appendix.asmType, eeReg, offs), nodes.getLoc(appendix))
    nodes.releaseLocIfNotUsedLater(appendix)
  }

  protected def putFrameDescriptorForCallToManaged(ctmwFrameDescriptor: Frame.Slot): Unit = emit.borrowScratch { tmp =>
    emit.lea(tmp, ctmwFrameDescriptor)
    emit.store(mem(PTR, stackPointer), tmp)
  }

  private def invokeNormalPreAction(targetRef: MethodReference): Unit = {
    if (context.rootHasManagedExecEnv && !context.rootManual && targetRef.methodType.isAJLongSafe) {
      enterGCSafeRegion(RTConst.ExecEnv.safeSectionEntranceFrameAddr.offset)
    }
    if (targetRef.methodType.isAJLongSafe) {
      gcSafeStateAssert()
    }
  }

  private def invokeNormalPostAction(targetRef: MethodReference): Unit = {
    if (context.rootHasManagedExecEnv && !context.rootManual && targetRef.methodType.isAJLongSafe) {
      leaveGCSafeRegion(RTConst.ExecEnv.safeSectionEntranceFrameAddr.offset)
    }
  }

  private def hasEnrichableInterfaceParams(mt: MethodType): Boolean = {
    assert(hasEops)
    for (i <- 0 until mt.parameterCount) {
      if (!mt.isReceiverParameter(i) && mt.parameterType(i).isInterface) {
        return true
      }
    }
    false
  }

  private def preprocessInterfaceParams(target: MethodType, targetParams: collection.Seq[Node], releaseBCParams: Boolean): collection.Seq[Node] = {
    if (!hasEops || !hasEnrichableInterfaceParams(target)) {
      // fast path
      return targetParams
    }

    val targetParamsIter = targetParams.iterator
    val enrichedParams = new ArrayBuffer[Node](targetParams.size)

    val paramsToRelease = mutable.LinkedHashSet.empty[Node]

    for (i <- 0 until target.parameterCount) {
      val param = targetParamsIter.next()
      val `type` = target.parameterType(i)

      enrichedParams += {
        if (!target.isReceiverParameter(i) && `type`.isInterface) {
          val enrichedParam = genEnrichedCopy(param, `type`)

          // Plain version should be released manually. This code is similar to [[releaseMethodParams]].
          if (releaseBCParams || param.isTemporary) {
            paramsToRelease += param
          }
          enrichedParam

        } else {
          param
        }
      }
    }

    // We should correctly handle plain versions of enriched params which are passed as plain.
    paramsToRelease --= enrichedParams
    nodes.releaseLocIfNotUsedLater(paramsToRelease)

    enrichedParams
  }

  protected final def releaseMethodParams(params: collection.Seq[Node], releaseBCParams: Boolean): Unit = {
    for (n <- params.distinct if releaseBCParams || n.isTemporary) {
      nodes.releaseLocIfNotUsedLater(n)
    }
  }

  /** Resolve call target and generate null check. Note: the order of calls and checks is important and defined
    * by specification. Returns node with address of target method or symbol of target method.
    */
  private def prepareAndMakeCallTarget(targetRef: MethodReference, rawParams: collection.Seq[Node]): Object = {
    val akind = targetRef.accessKind
    val target = targetRef.method

    assert(!target.isDeferred)
    assert(!target.isThinConstructor)
    assert(!target.isIndirectCall)

    if (targetRef.hasReceiverParameter) {
      genCheckNull(rawParams(targetRef.getReceiverArgIndex), target.getDeclaringClass)
    }

    if (targetRef.isDirectCall) {
      target
    } else {
      val callTarget = Node.newTemporary(ADDR)
      val thisParam = rawParams(targetRef.getReceiverArgIndex)

      val methodTableOffset = if (akind == MethodReferenceAccessKind.INTERFACE) {
        genGetIMTForInvoke(thisParam, targetRef.refClass, callTarget)
        0 // callTargetReg already contains addr of interface method table
      } else {
        val thisReg = nodes.loadToIReg(thisParam)
        val callTargetReg = nodes.bindToAnyFreeIReg(callTarget)
        assert(!target.getDeclaringClass.isThinClass)
        readObjectTD(thisReg, callTargetReg, mayBeNull = false)
        if (target.getDeclaringClass.isAJManagedType) {
          RTConst.ManagedInstanceDescriptor.VMT_OFFSET.intValue
        } else if (target.getDeclaringClass.isXScalaType) {
          RTConst.ScalaInstanceDescriptor.VMT_OFFSET.intValue
        } else {
          RTConst.JavaInstanceDescriptor.VMT_OFFSET.intValue
        }
        // callTargetReg contains addr of type descriptor
      }

      val offset = methodTableOffset + addressSize * targetRef.virtualMethodSlot

      val callTargetReg = nodes.loadToIReg(callTarget)
      emit.load(callTargetReg, mem(PTR, callTargetReg, offset))
      callTarget
    }
  }

  protected def checkNodeType(node: Node, expectedType: BytecodeTypeKind): Boolean = {
    val expectedNodeType = NodeType.by(expectedType)
    var nodeType = node.`type`
    if (nodeType == ADDR) {
      // ADDR is a local type which does not present in symlevel.Type system and should be lowered to int/long.
      nodeType = addressSize match {
        case 4 => NodeType.INT
        case 8 => NodeType.LONG
      }
    }
    nodeType == expectedNodeType
  }

  /** Moves type descriptor of object (assumed to be on `obj` register) to `result` register.
    * `obj` may be equal to `result`. */
  // FIXME: hide low-level Location operations
  final def readObjectTD(obj: IReg, result: IReg, mayBeNull: Boolean): Unit = {
    if (env.enabled(GenTDBarriers) && context.isManaged && context.shouldAddXSite) {
      assert(result != obj)
      emit.withScratch(result) { genTDBarrier(obj, mayBeNull) }
    }
    emit.load(result, instanceFieldAddr(obj, PTR, RTConst.HeapObj.TYPEDESC_OFFSET.intValue))
    clearIdescHigh16BitsIfNeeded(result)
  }

  private def clearIdescHigh16BitsIfNeeded(dst: IReg): Unit = {
    if (env.enabled(IdescHigh16BitsCleaning)) {
      depriveEOP(dst)
    }
  }

  def genNew(refClass: ClassType, result: Node): Unit = {
    assert(!refClass.isDeferred)
    if (refClass.isAbstractClass) {
      assert(!refClass.isAJManagedType)
      if (isJIT) {
        rtsCall(JRS_ThrowInstantiationError)(refClass.getTypeHandle)
      } else {
        rtsCall(JR_ThrowInstantiationError)(env.getImportedClassIdx(refClass, null))
      }
      // instruction has result, so we must bind result node to some location anyway
      nodes.bindToAnyFreeIReg(result)
    } else {
      val desc = acquireInstanceDescriptor(refClass)
      val size = refClass.getHeapObjectSize
      val proc = if (!refClass.finalizable && size <= RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue) {
        specializedAllocatorProc(size)
      } else {
        JR_NEW
      }
      rtsCall(proc, result)(desc)
    }
  }

  private def specializedAllocatorProc(size: Int): RTSProc = {
    assert(8 <= RTConst.Allocator.MIN_SIZE_OF_SPECIALIZED_OBJECT.intValue)
    assert(96 == RTConst.Allocator.MAX_SIZE_OF_SPECIALIZED_OBJECT.intValue)
    size match {
      case 8  => JR_NEW8
      case 16 => JR_NEW16
      case 24 => JR_NEW24
      case 32 => JR_NEW32
      case 40 => JR_NEW40
      case 48 => JR_NEW48
      case 56 => JR_NEW56
      case 64 => JR_NEW64
      case 72 => JR_NEW72
      case 80 => JR_NEW80
      case 88 => JR_NEW88
      case 96 => JR_NEW96
    }
  }

  def genNewArray(arrayType: Type, length: Node, result: Node): Unit = {
    val baseType = arrayType.getArrayBase
    assert(!baseType.isDeferred)
    val desc = acquireInstanceDescriptor(arrayType)

    if (baseType.isPrimitive && arrayType.getArrayDimnum == 1) {
      val elementLog2Size = baseType.log2Size
      rtsCall(JR_NEW_PRIMARRAY, result, releaseBCParams = true)(elementLog2Size, desc, length)
    } else {
      rtsCall(JR_NEW_REFARRAY, result, releaseBCParams = true)(0, desc, length)
    }
  }

  def genNewMultidimArray(arrayType: Type, dimArray: Array[Node], result: Node): Unit = {
    assert(arrayType.getArrayDimnum > 1)
    assert(arrayType.getArrayDimnum >= dimArray.length)
    val desc = acquireInstanceDescriptor(arrayType)
    val dimArrayOnStack = storeIntValuesToStackAllocatedArray(dimArray)
    rtsCall(JR_NEW_ARRAY_MD, result)(desc, dimArray.length, dimArrayOnStack)
  }

  /** Stores sequence of int values to new array allocated on stack. Returns RTSCall param node, that points to start of the array. */
  def storeIntValuesToStackAllocatedArray(values: Array[Node]): Node = {
    val result = Node.newTemporary(ADDR)
    val count = values.length
    val elemType = I32
    val elemSize = elemType.width.nbytes

    genRawStackAlloc(elemSize * count, elemSize, result)

    val uniqueValues = mutable.LinkedHashSet.empty[Node]
    for (i <- 1 to count) {
      val value = values(i - 1)
      uniqueValues += value
      val arrayElemLoc = mem(elemType, nodes.loadToIReg(result), elemSize * (count - i))
      emit.store(arrayElemLoc, nodes.loadToIReg(value))
    }
    nodes.releaseLocIfNotUsedLater(uniqueValues)

    result
  }

  private def genFastInstanceOf(`type`: Type, `object`: Node, result: Node): Unit =
    genFastTypeCheck(isInstanceOf = true, `type`, `object`, result)

  private def genFastCheckCast(`type`: Type, `object`: Node): Unit =
    genFastTypeCheck(isInstanceOf = false, `type`, `object`, null)

  private def genFastTypeCheck(isInstanceOf: Boolean, `type`: Type, objectNode: Node, resultNode: Node): Unit = {
    val obj = nodes.loadToIReg(objectNode)

    val tdNode = Node.newTemporary(ADDR)
    val td = nodes.bindToAnyFreeIReg(tdNode)

    val expectedTDNode = Node.newTemporary(ADDR)
    val expectedTD = nodes.bindToAnyFreeIReg(expectedTDNode)

    val result = if (isInstanceOf) nodes.bindToAnyFreeIReg(resultNode) else null

    assert(obj == nodes.getLoc(objectNode)) // ensure that we have enough free registers for all these nodes

    /////////////////////////////////////////////////////////////////////////////////////////////////
    // Tricky CFG is generated here, don't touch reg.alloc. below this line (i.e. don't add extra nodes.***() calls).

    val stateChanged = nodes.withSavedState {
      val pass = emit.newLabel
      val failure = emit.newLabel
      val end = emit.newLabel

      emit.branchIfNull(obj, if (isInstanceOf) failure else pass)

      readObjectTD(obj, td, mayBeNull = false)

      val isFinalClass = `type`.isClass && `type`.isFinal
      val isArrayOfFinal = `type`.isArray && (`type`.getArrayBase.isPrimitive || `type`.getArrayBase.isFinal)

      if (isFinalClass || isArrayOfFinal) {
        // our td is td to compare

      } else if (`type`.getCohenLevel <= RTConst.CohenDisplay.INLINED_SIZE.intValue) {
        emit.load(td, mem(PTR, td,
          RTConst.InstanceDescriptor.cohenDisplay.offset +
            RTConst.CohenDisplay.inlined.offset +
            (`type`.getCohenLevel - 1) * addressSize))
        andAddr(td, ~RTConst.CohenDisplay.CC_BIT.intValue)

        // td from inlined cohen display is td to compare

      } else {
        // It's rare case, don't acquire temporal node.
        emit.borrowScratch { actualLevel =>
          emit.load(actualLevel, mem(I32, td, RTConst.InstanceDescriptor.cohenDisplay.offset + RTConst.CohenDisplay.level.offset))
          andAddr(actualLevel, ~RTConst.CohenDisplay.LEVEL_BIT.intValue)
          emit.branchIf(LT, actualLevel, `type`.getCohenLevel, W32, failure)
        }

        val outlinedIdx = `type`.getCohenLevel - RTConst.CohenDisplay.INLINED_SIZE.intValue - 1
        emit.load(td, mem(PTR, td, RTConst.InstanceDescriptor.cohenDisplay.offset + RTConst.CohenDisplay.outlined.offset))
        emit.load(td, mem(PTR, td, outlinedIdx * addressSize))
        andAddr(td, ~RTConst.CohenDisplay.CC_BIT.intValue)

        // td from outlined cohen display is td to compare
      }

      emit.lea(expectedTD, acquireInstanceDescriptor(`type`))
      emit.branchIf(EQ, td, expectedTD, WPTR, pass)

      emit.bind(failure)
      if (isInstanceOf) {
        emit.mov(result, 0, W32)
        emit.jump(end)
      } else {
        nodes.withSavedState {
          if (`type`.isAJType) {
            rtsCall(JR_ThrowAJClassCastException)()
          } else {
            rtsCall(JR_ThrowClassCastExceptionByObj)(objectNode)
          }
        }
      }

      emit.bind(pass)
      if (isInstanceOf) {
        emit.mov(result, 1, W32)
      }
      // fall through to the end

      emit.bind(end)
    }
    assert(!stateChanged)

    // Tricky CFG is generated here, don't touch regalloc above this line (i.e. don't add extra nodes.***() calls).
    /////////////////////////////////////////////////////////////////////////////////////////////////

    nodes.releaseLoc(tdNode)
    nodes.releaseLoc(expectedTDNode)
    nodes.releaseLocIfNotUsedLater(objectNode)
  }

  def genInstanceOf(`type`: Type, `object`: Node, result: Node): Unit = {
    assert(!`type`.isDeferred)
    assert(!`type`.isThinClass)

    if (isFastTypeCheck(`type`)) {
      genFastInstanceOf(`type`, `object`, result)

    } else if (`type`.isInterface) {
      val cache = symbolLinker.makeUninitializedData(RTConst.InterfInstanceofCacheEx.size)
      rtsCall(JR_InterfaceIsWithCache, result, releaseBCParams = true)(`object`, `type`.getTypeHandle, cache)

    } else {
      val proc = if (`type`.isArray) JR_ArrayIs else JR_ClassIs
      rtsCall(proc, result, releaseBCParams = true)(`object`, `type`.getTypeHandle)
    }
  }

  def genCheckCast(`type`: Type, `object`: Node): Unit = {
    assert(!`type`.isDeferred)
    assert(!`type`.isThinClass)
    assert(context.isManaged)

    if (isFastTypeCheck(`type`)) {
      genFastCheckCast(`type`, `object`)
    } else {
      import TypeKind.*
      val proc = `type`.getKind match {
        case CLASS      => JR_ClassCast
        case INTERFACE  => JR_InterfaceCheckCast
        case ARRAY      => JR_ArrayCast
      }
      rtsCall(proc, releaseBCParams = true)(`object`, `type`.getTypeHandle)
    }
  }

  final def genRawStackAlloc(size: Int, align: Int, result: Node): Unit = {
    val slot = globalLocations.allocateSlot(size, align, traced = false)
    val resultReg = nodes.bindToAnyFreeIReg(result)
    emit.lea(resultReg, slot)
  }

  def genTrapCheckInstruction(trapLoc: IReg, scratch: IReg, offset: Int, isGCPoint: Boolean): Unit

  final def genTrapCheck(xSiteKind: XSiteKind, trapLoc: IReg): Unit =
    emit.borrowScratch { tmp => genTrapCheck(xSiteKind, trapLoc, tmp, 0) }

  /** Generate trap check with XSite. XSite is not generated if `xSiteKind` is `null`. */
  def genTrapCheck(xSiteKind: XSiteKind, trapLoc: IReg, scratch: IReg, offset: Int): Unit = {
    if (xSiteKind != null) {
      addXSite(xSiteKind)
    }
    genTrapCheckInstruction(trapLoc, scratch, offset, xSiteKind == XSiteKind.GCPOINT)
  }

  private def genGCPointImpl(trapOffset: Int, needXSite: Boolean): Unit = {
    assert(context.rootHasManagedExecEnv)

    val offset = RTConst.ExecEnv.memoryManagerData.offset +
      RTConst.ThreadLocalMMData.gcPointsTLD.offset +
      RTConst.GCPoints.ThreadLocalData.gcPointTrapAddressUnion.offset

    emit.borrowScratch { trapLoc =>
      emit.load(trapLoc, mem(PTR, frame.EER, offset))
      genTrapCheck(if (needXSite) XSiteKind.GCPOINT else null, trapLoc, trapLoc, trapOffset)
    }
  }

  final def genGCPoint(): Unit =
    genGCPointImpl(RTConst.GCPoints.usualTrapOffset.intValue, needXSite = true)

  private def genFastGCPoint(atEnter: Boolean): Unit =
    genGCPointImpl(if (atEnter) RTConst.GCPoints.fastNoInspectionTrapOffset.intValue else RTConst.GCPoints.fastWithInspectionTrapOffset.intValue, needXSite = atEnter)

  private def setGCSafetyState(value: Int): Unit = {
    val offset = RTConst.ExecEnv.memoryManagerData.offset +
      RTConst.ThreadLocalMMData.gcPointsTLD.offset +
      RTConst.GCPoints.ThreadLocalData.gcSafetyState.offset
    emit.store(mem(I8, frame.EER, offset), value)
  }

  def enterGCSafeRegion(savedFrameAddrOffset: Int): Unit = {
    frame.markAsFrameWithGCSafeCallSite()

    val gcPointLabel = emit.newLabel

    // Save continuation point
    // TODO: we can implement this in one instruction on amd64. should we?
    emit.borrowScratch { tmp =>
      emit.loadLabelPosition(tmp, gcPointLabel)
      emit.store(mem(I32, frame.EER, RTConst.ExecEnv.safeRegionEntranceOffset.offset), tmp)
    }

    emit.store(mem(PTR, frame.EER, savedFrameAddrOffset), stackPointer)

    // GC can start scanning of the thread while it is executing native code.
    // That's why we need to ensure that no alive traceable reference values would be passed to a native method
    // on any of the registers.
    nodes.ensureNoAliveRefsOnRegs(_.`type` == TREF) // FIXME except arguments

    // vvv ATTENTION: NO REGALLOC USAGE HERE vvv
    // Preserve all alive nodes locations except ee.
    // TODO: allow gc point with explicit gc map

    emit.memBarrier(STORE_STORE)
    setGCSafetyState(RTConst.GCSafetyState.SAFE.intValue)

    // ^^^ ATTENTION: NO REGALLOC USAGE HERE ^^^
    emit.memBarrier(STORE_LOAD)

    genFastGCPoint(true)
    emit.bind(gcPointLabel)
  }

  def leaveGCSafeRegion(savedFrameAddrOffset: Int): Unit = {
    setGCSafetyState(RTConst.GCSafetyState.UNSAFE.intValue)

    // to prevent reordering: read trap addr, set unsafe state, dereference trap addr.
    // TODO: try setGCSafetyState with release semantics
    emit.memBarrier(STORE_LOAD)
    genFastGCPoint(false)

    emit.storeNull(mem(PTR, frame.EER, savedFrameAddrOffset))
  }

  def gcSafeStateAssert(): Unit = {
    if (env.enabled(GCSafetyChecks)) {
      rtsCall(JR_GCSafeStateAssert)()
    }
  }

  def loadCurrentClassObject(classObject: Node): Unit = genClassObject(classObject, context.fromClass)

  def depriveEOP(loc: Location): Unit

  final def receiveParameter(paramNum: Int): Location = {
    val loc = frame.abi.paramLocations(paramNum) match {
      case TailSlot(offset, tpe) => mem(tpe, tailRegister, offset)
      case altLoc: AltLocation => convertAltLocation(altLoc)
      case loc => loc
    }
    if (!frame.abi.methodType.isReceiverParameter(paramNum)) {
      // `this` is always plain
      val parameterType = frame.abi.methodType.parameterType(paramNum)
      if ((parameterType.isDeferred || parameterType.isInterface) && !env.getTypeProvider.isManagedEopUnderlyingType(parameterType)) {
        depriveEOP(loc)
      }
    }
    loc
  }

  /** Binds all root method parameters to temporary nodes. Designed to be used from wrapper/thunk generators. */
  def receiveAllParameters(): mutable.Buffer[Node] = {
    val parameters = new ListBuffer[Node]
    val mt = context.rootMethodType
    for (i <- 0 until mt.parameterCount) {
      val n = Node.newTemporary(NodeType.by(mt.parameterType(i).jbcKind))
      nodes.bind(n, receiveParameter(i))
      parameters += n
    }
    parameters
  }

  final def signExtendShortIntegralToInt(`type`: BytecodeTypeKind, dst: IReg, src: IReg): Unit =
    signExtendShortIntegralToInt(`type`.width, `type` != BytecodeTypeKind.CHAR, dst, src)

  protected def signExtendShortIntegralToInt(width: Width, signed: Boolean, dst: IReg, src: IReg): Unit

  def andAddr(mem: Mem, value: Int): Unit

  def andAddr(iReg: IReg, value: Int): Unit


  protected def genDivisionByZeroCheck(divisor: IReg, width: Width): Unit = {
    if (context.isManaged) {
      val ok = new Label
      emit.branchIf(BranchOp.NE, divisor, 0, width, ok)

      nodes.withSavedState {
        val rtsProc = context.inlineContext.method.getDomain match {
          case Domain.AJ    => RTSProc.JR_ThrowAJArithmeticException
          case Domain.SCALA => RTSProc.JR_ThrowScalaArithmeticException
          case Domain.JAVA  => RTSProc.JR_ThrowArithmeticException
        }
        rtsCall(rtsProc)()
      }

      emit.bind(ok)
    }
  }
}
