/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone, targetArch}
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.abi.{Frame, SlotBase}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.ir.*
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.{FrameSlotCBC, TypedFrameSlotCBC}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption.SmartRecordZeroing
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, MethodReferenceAccessKind}
import com.huawei.excelsior.jet.compiler.{Domain, RTConst}

import scala.annotation.tailrec
import scala.collection.mutable

/** Collection of utilities around XSites and GC maps in them.
  *
  * @author conwor
  */
trait XSitesToolbox extends RecordSlotsLiveness { self: Universe with BackEnd with CodeGenerator =>

  private val isCBC = targetArch == CBC

  /** Returns true iff `node` should be generated with XSite. */
  def needXSite(node: Node): Boolean = {
    @tailrec
    def needXSiteImpl(node: Node): Boolean = {
      node match {
        case _: CheckCast | _: BitcodeDeferred.CheckCast => true
        case call: Call => assert(!call.hasImplicitCheck); call.hasXSite || call.gcActions.generateGCSafeRegion
        case _: PreCall => true // intentionally above `WithImplicitCheck(check)` case
        case _: GCPoint => rootMethod.hasManagedExecEnv
        case x: NullCheck => x.canThrow
        case x: DivisorCheck => x.canThrow
        case x: CheckedOp => x.canThrow
        case x: ArrayIndexCheck => x.canThrow
        case x: ArrayStoreCheck => x.canThrow
        case _: LoadMemory.Soft => true
        case _: TDBarrier => true
        case _: PackageInit => true
        case _: PackageInitCheck => !isStandalone
        case WithImplicitCheck(check) => needXSiteImpl(check)
        case x: SpinalNode => assert(!x.hasXSite, x); false
        case _ => false
      }
    }

    assert(node.isGroupRoot)
    needXSiteImpl(node)
  }

  /** Returns spinal node associated with XSite for `node`. This spinal node used to get information about
    * bytecode position and exception handler. */
  private def xSiteSpinalPoint(node: Node): SpinalNode = node match {
    case WithImplicitCheck(check) => check
    case node: SpinalNode => node
  }

  def xSiteKind(node: Node): XSiteKind = node match {
    case _: CheckCast | _: BitcodeDeferred.CheckCast => XSiteKind.CALL
    case DAICall(_) => XSiteKind.DEFERRED_CALL
    case _: Call => XSiteKind.CALL
    case preCall: PreCall => preCall match { // intentionally above `WithImplicitCheck(_: NullCheck)` case
      case WithImplicitCheck(_: NullCheck) => XSiteKind.PRE_CALL_WITH_NULLCHECK
      case _ => assert(!preCall.hasImplicitCheck); XSiteKind.PRE_CALL
    }
    case _: DivisorCheck => XSiteKind.CALL
    case _: CheckedOp => XSiteKind.CALL
    case _: ArrayIndexCheck => XSiteKind.CALL
    case _: ArrayStoreCheck => XSiteKind.CALL
    case _: NullCheck | WithImplicitCheck(_: NullCheck) => XSiteKind.NULLCHECK
    case WithImplicitCheck(_: DivisorCheck) => XSiteKind.DIV
    case _: GCPoint => XSiteKind.GCPOINT
    case _: LoadMemory.Soft => XSiteKind.SOFT_EXCEPTION
    case _: TDBarrier => XSiteKind.SOFT_EXCEPTION
    case _: PackageInit | _: PackageInitCheck => XSiteKind.CALL
  }

  private def xSiteAccessOffset(node: Node): Int = node match {
    case rma @ LoadStoreMemoryAccess.Disposed(_, offset) if rma.hasImplicitCheck => offset
    case _ => 0
  }

  protected def xSiteTargetRef(node: Node): MethodReference = node match {
    case call: Call => call.targetRef
    case check: DivisorCheck => new MethodReference(env.getRTSProc(check.throwProc), MethodReferenceAccessKind.STATIC)
    case check: CheckedOp => new MethodReference(env.getRTSProc(check.throwProc), MethodReferenceAccessKind.STATIC)
    case check: ArrayIndexCheck => new MethodReference(env.getRTSProc(check.throwProc), MethodReferenceAccessKind.STATIC)
    case check: ArrayStoreCheck => new MethodReference(env.getRTSProc(check.throwProc), MethodReferenceAccessKind.STATIC)
    case _: PackageInit | _: PackageInitCheck => null
    case _: CheckCast | _: BitcodeDeferred.CheckCast => null
    case _ => assert(!xSiteKind(node).isCall); null
  }

  /** Returns true iff `node` has XSite with GC map in it.
    *
    * TODO: there is a problem with this method - it will return different values for DivisorCheck before and after
    *  it becomes implicit. So we will make BGCM and regalloc, based on assumption that DivisorCheck has GCMap
    *  (intra pointer lea will be rematerialized, frame slots could not be recolored to FReg) but later it will not
    *  have GCMap. This problem will be eliminated when implicit checks optimization will be done in BGCM.
    */
  def needGCMap(node: Node): Boolean = needXSite(node) && xSiteKind(node).needGCMap

  /** Returns true iff local unmovable analysis may associate this `node` with the set of live nodes unmovable at
    * this very point.
    *
    * @see [[com.huawei.excelsior.jet.compiler.opt.middle.UnmovableAnalysis UnmovableAnalysis]]
    * @see [[com.huawei.excelsior.jet.compiler.opt.backend.preparation.SpecialSteps.localUnmovableAt SpecialSteps#localUnmovableAt]]
    */
  def couldGatherLocalUnmovableAt(node: ControlNode): Boolean = needGCMap(node)

  /** Returns true iff `node` could invalidate [[FragilePointerType]]-d nodes. */
  def couldInvalidateFragilePointers(node: Node): Boolean = node match {
    case _: XBlock | _: WriteBarrierMarker =>
      true

    case _: ExecEnvInvalidationPoint =>
      // Actually [[ExecEnvInvalidationPoint]] invalidates only [[ExecEnvType]]-based pointers nodes, but it is simpler to
      // invalidate all fragile pointers. Feel free to refactor it, if you want to.
      true

    case _ =>
      needGCMap(node)
  }

  /** Returns true iff `node` resource will be collected in GC map at XSites which it live through. */
  def willBeCollectedInGCMap(n: Node): Boolean = (valueOf(n).producer match {
    case _: Constant => false // TODO: expand this list with compile-known non-GC nodes, like ExtractLongBits
    case _ => mayBeTraceableReference(n)
  }) && !n.resource.isInstanceOf[TailSlot] // tail slots are presented in caller's gcmaps, where they live as frame slots


  /////////////////////////////////////////////////////////////////////////////

  /** Part of [[CodeGeneratorImpl]], responsible for XSites & GC maps collection during code generation. */
  trait XSitesGenerator { self: CodeGeneratorImpl =>

    /** Current [[XInfo]] - collection of XSites & GC maps. */
    protected var xInfo: XInfo = _

    /** Sets up current [[XInfo]] and executes `action`. */
    private[codegen] final def withXInfo[T](xInfo: XInfo)(action: => T): T = {
      val oldXInfo = this.xInfo
      this.xInfo = xInfo
      val result = action
      this.xInfo = oldXInfo
      result
    }

    protected def gatherGCMap(node: Node): Unit = {
      def getSubSlot(fs: FrameSlot, size: Int, offset: Int): Slot = {
        assert(size >= addressSize)
        val slot = frame.newSlot(size, addressSize, fs.tracedByHeader)
        slot.bind(SlotBase.SP, fs.offsetFromSP + offset)
        slot
      }

      def getSlot(fs: FrameSlot) = getSubSlot(fs, fs.size, 0)

      val slots = mutable.LinkedHashSet.empty[FrameSlot] // TODO gcMaps(node) is unstable Set, making this set also unstable: JET-17378

      def addTraced(fs: FrameSlot): Unit = fs match {
        case slot: TypedFrameSlotCBC =>
          assert(isCBC)
          shouldNotReachHere(s"$slot") // TODO [preciseGC]: support record fields liveness tracking
        case slot: FrameComponentCBC.FrameSlotCBC =>
          assert(isCBC)
          xInfo.addTracedSlot(frame.asInstanceOf[FrameCBC].newSlot(slot.local))
          slots += fs
        case fs => fs.kind match {
          case FrameSlot.Typed(allocType) if allocType.isRecord =>
            assert(!isCBC)
            for (offs <- asClassType(allocType).getRefFieldOffsets) {
              xInfo.addTracedSlot(getSubSlot(fs, addressSize, offs))
            }
          case _ =>
            xInfo.addTracedSlot(getSlot(fs))
        }
      }

      def addUnmovableTraced(fs: FrameSlot): Unit = {
        assert(!isCBC)
        val slot = getSlot(fs)
        xInfo.addTracedSlot(slot)
        xInfo.addUnmovableSlot(slot)
      }

      xInfo.startGCMap()

      if (env.enabled(SmartRecordZeroing) && rootDeclaringClass.isCangjieType && containsRecordSlots) {
        assert(!isCBC) // TODO: support smart zeroing for CBC
        for (subSlots <- recordSlotsAliveAt.get(node); (slot, offset) <- subSlots) {
          xInfo.addTracedSlot(getSubSlot(slot, addressSize, offset))
        }
      }
      tracedStackAllocSlots foreach addTraced

      val unmovable = node match {
        case block: Block => localUnmovableAt(block)
        case _ => localUnmovableAt(xSiteSpinalPoint(node))
      }

      var mask: Int = 0
      var unmovableMask: Int = 0
      for (n <- gcMaps(node)) {
        n.resource match {
          case ireg: IReg =>
            mask = frame.abi.updateRegMaskForGCMap(mask, ireg.asInstanceOf[IREG])
            if (unmovable(valueOf(n).producer)) {
              unmovableMask = frame.abi.updateRegMaskForGCMap(unmovableMask, ireg.asInstanceOf[IREG])
            }
          case fs: FrameSlot =>
            if (unmovable(valueOf(n).producer)) {
              addUnmovableTraced(fs)
            } else {
              addTraced(fs)
            }
        }
      }

      xInfo.setRegistersMask(mask)
      xInfo.setUnmovableRegisters(unmovableMask)

      afterGatherGCMap(node, slots, mask, unmovableMask)
    }

    protected def afterGatherGCMap(node: Node, slots: Iterable[FrameSlot], mask: Int, unmovableMask: Int): Unit = {}

    private def addXSiteWithContext(node: Node, kind: XSiteKind, accessOffset: Int, targetRef: MethodReference) = {
      val (bytecodePos, lineNumber, inlineContext) = xSiteSpinalPoint(node).pos match {
        case pos: BytecodePosition if pos.inlineContext.method.isManaged =>
          // save up space in meta-data by dropping unneeded bcPos
          val savedBCPos = if (isRegionMarker(node)) pos.offset else BytecodeOffset.INVALID
          (savedBCPos, pos.lineNumber, pos.inlineContext)
        case _ =>
          // If flag forced is specified the XSite should be inserted even for node which operation doesn't throw an exception.
          // In this case inlineContext could not be correctly gathered, so, we consider it null.
          (BytecodeOffset.INVALID, LineNumber.UNKNOWN, null)
      }

      val handlerLabel = xSiteSpinalPoint(node).xHandlerOption.map(startOf).orNull

      val domain = node match {
        case n: NullCheck => n.domain
        case WithImplicitCheck(n: NullCheck) => n.domain
        case _ => getDomain(inlineContext)
      }

      xInfo.addXSite(asm.newBoundLabel, handlerLabel, kind, accessOffset, bytecodePos, lineNumber, inlineContext, targetRef, RTConst.XTable.State.Initial.SOFT_EXCEPTION_ID.intValue, domain)
    }

    protected def addXSite(node: Node): Unit = {
      if (needXSite(node)) {
        val kind = xSiteKind(node)

        if (kind == XSiteKind.SOFT_EXCEPTION) {
          assert(!kind.needGCMap)

          val softKind: Int = node match {
            case n: LoadMemory.Soft => n.kind
            case _: TDBarrier => RTConst.SoftExceptions.Kind.TD_BARRIER.intValue
          }

          xInfo.addXSite(asm.newBoundLabel, null, kind, xSiteAccessOffset(node),
            BytecodeOffset.INVALID, LineNumber.UNKNOWN, null, null, softKind, getDomain(currentInlineContext))

        } else {
          if (kind.needGCMap && !isCBC) gatherGCMap(node)

          val accessOffset = xSiteAccessOffset(node)
          val targetRef = xSiteTargetRef(node)

          addXSiteWithContext(node, kind, accessOffset, targetRef)
        }
      }
    }

    private def getDomain(inlineContext: InlineContext): Domain = {
      val domainOwner = if (inlineContext == null) rootMethod else inlineContext.method
      domainOwner.getDomain
    }

    protected def addGCSafeRegionXSite(call: Call): Unit = {
      gatherGCMap(call)
      addXSiteWithContext(call, XSiteKind.GCPOINT, 0, null)
    }
  }
}
