/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.Arch.AMD64
import com.huawei.excelsior.jet.assembler.{AsmType, Location}
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isWorkMode, stackSlotSize, targetArch}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot.{AnyNewOnStack, NewOnStack}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.SmartRecordZeroing

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Frame utilities - params locations, stack alloc, spoiled resources, reserved space and temporal slots.
  *
  * @author conwor
  */
trait FrameComponent { self: Universe with BackEnd =>

  /** Returns location where `param` is placed. */
  def paramLocation(param: Param): Resource = frame.abi.paramLocations(param.num)


  ////////////////////////////////////////////////////////////////////////////////////////////////
  // Frame properties, calculated during code ordering and resources allocation, used later in
  // frame layout calculation, prologue/epilogue code generation and assembler code generation.

  /** True iff frame should be full for some reason (call, throw, e.t.c.). */
  private var _frameShouldBeFull = false

  /** True iff this frame must not only be full, but contain valid frame descriptor. */
  private[backend] var needFrameDescriptor = false

  /** Map from offset from SP to FrameSlot with address [SP + offset] used to pass arguments in callee methods. */
  private[backend] val slotsForArguments = new mutable.LinkedHashMap[Int, FrameSlot]

  /** Temporal frame slots shared between all nodes. Each node may use all of them. */
  private val temporalSlots = new ArrayBuffer[FrameSlot]
  private var maxTemporalSlots = 0

  /** Cache of spill slots unique for values. */
  private val spillSlots = new mutable.LinkedHashMap[Value, Resource]

  /** Ensures that frame is full. */
  protected def ensureFullFrame(): Unit = {
    if (frame.hasLayout) {
      assert(frame.isFull)
    } else {
      _frameShouldBeFull = true
    }
  }

  /** Returns true iff frame should be full. */
  private[backend] final def frameShouldBeFull(slots: collection.Seq[FrameSlot]): Boolean =
    _frameShouldBeFull || needFrameDescriptor || slots.nonEmpty

  // This code is based on ugly fragile odious implicit correspondence of opt's & Frame's slots by their indices.
  // Kill it with megatonns of nuclear fire ASAP!
  /** Assigns real address for given `slot`. */
  protected def calculateAddressForSlot(slot: FrameSlot, index: Int): Unit =
    slot._offsetFromSP = frame.deprecatedSlotOffsetFromSPByIndex(index)

  /** Returns new arch-specific frame slot with given kind. */
  protected def newFrameSlot(kind: FrameSlot.Kind): FrameSlot = new FrameSlot(kind)

  protected def newFrameSlotForStackAlloc(kind: FrameSlot.Kind): FrameSlot = newFrameSlot(kind)

  /** Returns new frame slot using to hold `value`. For more details see JET-15742. */
  private[backend] final def newSpillSlotUsedAsWorkaroundFor15742(value: Value): Resource = {
    assert(typeSize(value.producer.tpe) <= stackSlotSize)
    newFrameSlot(FrameSlot.Raw(stackSlotSize, stackSlotSize))
  }

  /** Returns unique frame slot using to hold `value`. For more details see JET-15742. */
  private final def spillSlotDoNotUseUntil15742IsFixed(value: Value): Resource =
    spillSlots.getOrElseUpdate(value, { newSpillSlotUsedAsWorkaroundFor15742(value) })

  /** Assigns real addresses for `slots`. */
  private[backend] final def calculateAddressesForSlots(slots: collection.Seq[FrameSlot]): Unit = {
    for ((slot, index) <- slots.zipWithIndex) {
      calculateAddressForSlot(slot, index)
    }
  }

  /** Creates new slot for argument with `offset` from caller SP. */
  protected def newSlotForArg(spOffsetInBytes: Int): FrameSlot =
    new FrameSlot(FrameSlot.CallParam, spOffsetInBytes)

  /** Returns slot for argument with `offset` from caller SP. */
  private[backend] final def slotForArg(tpe: Type, offset: Int): FrameSlot = {
    assert(tpe == VoidType || tpe.asInstanceOf[TypeWithSize].size <= stackSlotSize)
    slotsForArguments.getOrElseUpdate(offset, { newSlotForArg(offset) })
  }

  /** Returns temporal slot by its `index`. */
  private[backend] final def temporalSlot(index: Int) = temporalSlots(index)

  /** Register nodes that require some free stack space below current frame. */
  private[backend] final def registerStackChecks(): Unit = {
    all[Call] foreach { call => frame.registerStackCheckForCall(call.targetRef) }
    if (all[DAICallTarget].nonEmpty) frame.registerStackCheckForDAICall()
    if (all[SpinalNode] exists (_.hasXHandler)) frame.registerStackCheckForExceptionHandling()
  }

  /** Register frame properties changed because of `node` generation. */
  protected def registerNodeInFrame(node: Node): Unit = {
    def registerCall(abi: ABI): Unit = {
      ensureFullFrame()
      frame.reserveSpaceForCall(abi)
    }

    // 1. Full frame ensuring
    node match {
      case _: GCPoint => ensureFullFrame()
      case _: TrapCheck => ensureFullFrame(); if (rootMethod.hasFrameDescriptor) needFrameDescriptor = true
        // TrapCheck can be used in different contexts: if it is used in a context with ManagedExecEnv it means that we
        // can start iteration of callstack from it, just like it usual gc-point => so, we need a full frame and fd;
        // Otherwise, when it is used in unmanaged context, callstack will be iterated somehow differently,
        // so, no fd needed (and no fd can be generated actually).
      case _: Throw => ensureFullFrame(); needFrameDescriptor = true
      case _: Halt if isWorkMode => ensureFullFrame()
      case sp: SpinalNode if sp.hasXSite => ensureFullFrame()
      case ValueConvert(AsmType.F16, _, _) | ValueConvert(_, AsmType.F16, _) =>
      case ValueConvert(srcT, dstT, _) if srcT.isFloatingPoint && !dstT.isFloatingPoint => ensureFullFrame() // contain calls
      case _: LoadMemory.Soft => ensureFullFrame()
      case BitCount(BitCount.Kind.BIT_COUNT) if targetArch == AMD64 => ensureFullFrame() // contain call on slow path
      case _: CheckedOp => ensureFullFrame()
      case _: TDBarrier => ensureFullFrame()
      case _: FrameHeader => ensureFullFrame()
      case _ =>
    }

    if (needXSite(node)) {
      needFrameDescriptor = true
    }

    // 2. Reserve additional temporal slots for node
    maxTemporalSlots = maxTemporalSlots max temporalSlotsCount(node)

    // 3. Different special actions by node kind
    node match {
      case call: Call => registerCall(call.abi)

      case _: FrameHeader => needFrameDescriptor = true

      case sa: StackAlloc =>
        val kind = sa.kind
        assert(!kind.traced || (kind.size >= addressSize && kind.alignment >= addressSize) || kind.isInstanceOf[NewOnStack] || targetArch == Arch.CBC)

        val slot = newFrameSlotForStackAlloc(kind)

        val shouldTraceCurrentSlot = kind.traced && targetArch != Arch.CBC && (kind match {
          case _: AnyNewOnStack => true
          case _ => !(rootDeclaringClass.isCangjieType && env.enabled(SmartRecordZeroing))
        })
        if (shouldTraceCurrentSlot) {
          tracedStackAllocSlots += slot
        }

        sa.slot = slot

      case _ =>
    }
  }

  /** Clean caches which should not be used after registers allocation for extra safety. */
  protected def cleanupCachesAfterRegAlloc(): Unit = {
    // Spill slots will be recolored by [[FrameSlotsColoringComponent]], there is no reason to use this cache
    spillSlots.clear()
  }


  ///////////////////////////////////////////////////////////////////////////
  // Frame layout

  protected def makeFrameLayout(spoiledRegs: collection.Seq[Location.AnyReg], frameSlots: collection.Seq[FrameSlot]): Unit

  private def preProcessSlots(frameSlots: ArrayBuffer[FrameSlot]): Unit = {
    // The most of architectures have the problems with addressing huge offsets. When we create load/store instructions,
    // we do not know future offsets of spill slots, so we do not know whether we need an additional spoiled register
    // or not. To avoid greedy allocation of this spoiled, we do not allocate it and hope that the offsets will be small.
    //
    // This workaround based on assumption, that huge offsets in frame slots appear due to stack alloc with huge
    // sizes. If we sort frame slots by size, small slots (spill) will be allocated near stack top, so offsets in
    // load/store instructions will be small.
    //
    // Note that all spill slots are not zeroed and will stay properly sorted after this sort.
    frameSlots sortInPlaceBy (-_.size)

    // All zeroed frame slots should be near each other.
    if ((frameSlots count (_.zeroed)) > 1) {
      // In this case place them all at the beginning of frameSlots.
      frameSlots sortInPlaceWith { (x, y) => x.zeroed && !y.zeroed }
    }
  }

  private[backend] final def calculateUsedResourcesAndMakeFrame(): Unit = {
    val touched = emptyMSet()
    for (node <- allNodes) {
      node match {
        case _: Param | _: TailPointer =>
          // Param resources are not touched by us, so they are not needed to be saved/restored, if they
          // have allocated only to param nodes in the whole method.

        case _ =>
          if (node.mayHaveResource) touched += node.resource
          if (node.mayHaveSpoiled) touched ++= node.spoiled

          node match {
            case sa: StackAlloc =>
              touched += sa.slot

            case node: SpinalNode if !node.hasXHandler =>
              // `node` spoiled set contains all spoiled resources (volatile and temporal) which affects method code.
              // But if `node is a spinal without exception handler which have volatile resources on exceptional exit
              // only (e.g. [[DivisorCheck]] on arm64), they are not allocated for `node` and will not be in `node`
              // spoiled set. As we should register them in frame anyway we take them into account here.
              touched |= volatileResources(node, ExitKind.TRAP)

            case _ =>
          }
      }
    }

    for (_ <- 0 until maxTemporalSlots) {
      temporalSlots += newFrameSlot(FrameSlot.Raw(stackSlotSize, stackSlotSize))
    }
    touched ++= temporalSlots

    touched -= frame.EER

    val frameSlots = ArrayBuffer.from(touched.iterator collect {
      case slot: FrameSlot if !slot.kind.isInstanceOf[FrameSlot.Param] => slot
    })
    preProcessSlots(frameSlots)

    if (all[Call].exists(_.gcActions.generateGCSafeRegion)) {
      // should save non vol regs if there are gc safe regions in the method
      frame.markAsFrameWithGCSafeCallSite()
    }

    val spoiledRegs = ArrayBuffer.from(touched.iterator collect { case r: Location.AnyReg => r })
    spoiledRegs foreach frame.registerUsedReg
    makeFrameLayout(spoiledRegs, frameSlots)

    val zeroed = frameSlots filter { _.zeroed }

    if (zeroed.nonEmpty) {
      configureMassiveStackZeroing(zeroed)
    }

    if (frame.hasStackCheck) {
      needFrameDescriptor = true
    }
  }

  protected def configureMassiveStackZeroing(zeroed: ArrayBuffer[FrameSlot]): Unit = {
    val massiveStackZeroingBottom =
      zeroed.last ensuring { bottom => zeroed forall { s => s.offsetFromSP >= bottom.offsetFromSP } }

    val massiveStackZeroingSize =
      zeroed.head.size + zeroed.head.offsetFromSP - massiveStackZeroingBottom.offsetFromSP

    StackZeroing.Massive.setProperties(massiveStackZeroingBottom, massiveStackZeroingSize)
  }
}
