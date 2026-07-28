/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.frame

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg, MemBased, mem}
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.*
import com.huawei.excelsior.jet.compiler.abi.Frame.Mode.{FULL, SPECIAL_FOR_THUNK}
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.SlotBase.TR
import com.huawei.excelsior.jet.compiler.options.BoolOption.XCheckStack
import com.huawei.excelsior.jet.compiler.{Env, RTConst}
import xscala.util.MathUtils.{alignDown, alignUp, isAligned}

/** Part of [[Frame]] encapsulates layout calculation.
  *
  * Layout calculated ones after all [[FrameElements]] are determined.
  *
  * @author conwor
  * @author paul
  */
trait FrameLayout[IR >: Null <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] { self: Frame[IR, FR, XABI] =>

  /////////////////////////////////////////////////////////////////////////////
  // Layout calculation and checks

  /** Frame mode. Defined once when layout calculated. */
  protected var mode: Frame.Mode = _

  /** Frame layout. Calculated once after all [[FrameElements]] are ready. */
  protected var layout: Layout = _

  /** Returns true iff layout is already calculated. */
  final def hasLayout = layout != null

  /** Calculate layout with `mode`. */
  final def makeLayout(mode: Frame.Mode): Unit = {
    assert(!hasLayout)
    this.mode = mode
    layout = new Layout
  }

  protected final class Layout {
    // 1. Register common used elements
    if (hasFrameDescriptorSlot)             reserveSpaceAboveSP(stackSlotSize)
    if (useFMRAddressing && slots.nonEmpty) registerUsedReg(FMR)
    if ((linkRegister != null) && isFull)   registerUsedReg(linkRegister) // In full frame return address should be pushed on stack
    if (abi.hasEmulatedTail)                registerUsedReg(tailRegister)

    if (useFramePointer) {
      registerUsedReg(FP)
      if (linkRegister != null) {
        // Return address is a part of structure pointed by FP
        registerUsedReg(linkRegister)
      }
    }

    if (properties.isHookInvoker) {
      // Preserve arguments on registers of intercepted method while execution of hook invoker.
      // Force mark all argument registers as saved.
      // Note that @Hook.Invoker calling convention must be compatible with the intercepted method CC.
      savedRegs ++= abi.allArgumentIRegs
      savedRegs ++= abi.allArgumentFRegs
    }

    // 2. Check that compiler did not use some restricted elements in lightweight or empty frames
    if (!isFull) {
      assert(slots.isEmpty,                       s"non-empty stack slots in $mode")
      assert(requiredStackCheckSizeInBytes == 0,  s"$requiredStackCheckSizeInBytes stack check size required in $mode")
      assert(paramPassingAreaSize == 0,           s"$paramPassingAreaSize stack size above SP reserved in $mode")

      if (mode == SPECIAL_FOR_THUNK) {
        assert(!abi.hasEmulatedTail,              s"ABI has emulated tail in $mode")
        assert(savedRegs.isEmpty,                 s"non-empty savedRegs in $mode: {${savedRegs mkString ", "}}")
      }
    }

    // 3. Collect saved registers in ABI defined order.
    val savedIRegs = (abi.savedIRegsOrder filter savedRegs).toIndexedSeq
    val savedFRegs = (abi.savedFRegsOrder filter savedRegs).toIndexedSeq

    // 4. Determine frame alignment and check that slots may be aligned.
    val frameAligned = isFull || forceFrameAlignment
    assert(slots forall (_.alignment <= frameAlignment))

    // 5. Calculate saved registers sizes - pushable and non-pushable. All IRegs are pushable on currently supported platforms.
    val (savedPushableRegsSize, savedNonPushableRegsSize) = {
      var pushableSize = savedIRegs.length * stackSlotSize
      var nonPushableSize = 0
      if (savedFRegs.nonEmpty) {
        val fRegSize = savedFRegs.head.width.nbytes
        if (fRegsArePushable) {
          pushableSize += savedFRegs.length * fRegSize // TODO: align savedFRegsArea to fRegSize
        } else {
          val addend = preHeaderSize + pushableSize
          nonPushableSize += alignUp(savedFRegs.length * fRegSize + addend, fRegSize) - addend
        }
      }
      (pushableSize, nonPushableSize)
    }

    // 6. Calculate stack alloc, and extra alloc (param passing area + alignment) sizes.
    val (stackAllocSize, extraAllocSize) = {
      val callerSPOffsetBeforeStackAlloc = preHeaderSize + savedPushableRegsSize + savedNonPushableRegsSize

      val stackAllocSize = if (slots.isEmpty) {
        0

      } else if (useFMRAddressing) {
        // 1) FMR should be aligned to maximum alignment of frame slots
        // 2) slots should be allocated from down to top (new-baseline specific)
        var fmrOffset = 0
        for (slot <- slots) {
          // Slots has already calculated offsets, and they should be in consistent with calculations in this function
          assert(slot.offset == alignUp(fmrOffset, slot.alignment))
          fmrOffset = slot.offset + slot.size
        }
        val maxAlignment = slots.iterator.map(_.alignment).max
        alignUp(callerSPOffsetBeforeStackAlloc + fmrOffset, maxAlignment) - callerSPOffsetBeforeStackAlloc

      } else {
        var spOffset = callerSPOffsetBeforeStackAlloc
        for (slot <- slots) {
          spOffset = alignUp(spOffset + slot.size, slot.alignment)
        }
        spOffset - callerSPOffsetBeforeStackAlloc
      }

      val callerSPOffsetAfterStackAlloc = callerSPOffsetBeforeStackAlloc + stackAllocSize
      val alignmentSize = if (frameAligned) {
        val unalignedOffsetFromCallerSP = callerSPOffsetAfterStackAlloc + paramPassingAreaSize
        alignUp(unalignedOffsetFromCallerSP, frameAlignment) - unalignedOffsetFromCallerSP
      } else {
        0
      }

      (stackAllocSize, paramPassingAreaSize + alignmentSize)
    }

    // 7. Calculate last sizes - header, body and the whole frame.
    val headerSize = preHeaderSize + savedPushableRegsSize
    val bodySize = savedNonPushableRegsSize + stackAllocSize + extraAllocSize
    val frameSize = headerSize + bodySize

    if (frameAligned) assert(isAligned(frameSize, frameAlignment),
      s"frameSize = $frameSize, frameAlignment = $frameAlignment")

    if (mode == SPECIAL_FOR_THUNK) assert(savedPushableRegsSize == 0 && bodySize == 0,
      s"savedPushableRegsSize = $savedPushableRegsSize, savedRegs = {${savedRegs.mkString(", ")}}, bodySize = $bodySize in empty frame")

    // 8. Allocate stack slots if SP addressing is used (not FMR).
    if (!useFMRAddressing) {
      val start = extraAllocSize + stackAllocSize
      var spOffset = start
      for (slot <- slots) {
        spOffset = alignDown(spOffset - slot.size, slot.alignment)
        slot.bind(SlotBase.SP, spOffset)
      }
      assert((start - spOffset) == stackAllocSize)
    }

    // 9. Calculate stack check parameters (presence and size)
    val (hasStackCheck, additionalStackCheckSize) = {
      if (!env.enabled(XCheckStack) || properties.isStackCheckDisabled) {
        (false, 0)

      } else if (!properties.isManaged) {
        assert(!properties.shouldStackCheckByCaller)
        (false, 0)

      } else if (properties.shouldStackCheckByCaller) {
        val checkedValue = properties.getStackCheckByCallerBytes
        assert(frameSize + requiredStackCheckSizeInBytes <= checkedValue,
          s"Inconsistent @StackCheckByCaller in method ${properties.getFullName}: frameSize = $frameSize, " +
            s"requiredStackCheckSizeInBytes = $requiredStackCheckSizeInBytes, @StackCheckByCaller value = $checkedValue")
        (false, 0)

      } else {
        val additionalStackCheckSize = requiredStackCheckSizeInBytes
        val hasStackCheck = if (RTConst.StackOverflowHandling.ADVANCED.boolValue) {
          additionalStackCheckSize > 0
        } else {
          (frameSize + additionalStackCheckSize) > RTConst.StackOverflowHandling.STACK_RESERVE_FOR_MANAGED_METHOD.intValue
        }
        (hasStackCheck, additionalStackCheckSize)
      }
    }

    assert(isFull || !hasStackCheck)
    assert(additionalStackCheckSize >= 0)


    /////////////////////////////////////////////////////////////////////////////
    // Layout-dependent offsets and locations

    /** Returns offset from SP of slot with given `slotIndex`. Fails with assert if FMR addressing is used.
      * TODO: deprecate it or better remove because using indices is not reliable.
      */
    def deprecatedSlotOffsetFromSPByIndex(slotIndex: Int) = {
      assert(!useFMRAddressing)
      slotByIndex(slotIndex).offset
    }

    /** Returns offset from SP of `slot`. Works both for FMR and SP addressing. TODO: deprecate it or better remove. */
    def offsetFromSP(slot: Slot) = slot.base match {
      case SlotBase.SP => slot.offset
      case SlotBase.FMR => slot.offset + layout.extraAllocSize
      case SlotBase.TR => shouldNotReachHere()
    }

    /** Calculate offset used to encode `slot` in [[GCMapsGenerator]]. */
    def gcMapsOffset(slot: Slot) = slot.base match {
      case SlotBase.SP => slot.offset
      case SlotBase.FMR => offsetFromSP(slot)
      case SlotBase.TR => shouldNotReachHere()
    }

    /** Returns true iff [[FrameDescriptor]] (or junk) should be on top of frame ([SP]).
      * TODO: actually, in unmanaged methods we have frame descriptor SLOT, but without real frame descriptor.
      */
    def hasFrameDescriptorSlot = isFull && properties.isManagedFrame

    def getSavedIRegsBitMap = abi.getSavedIRegsBitMap(savedIRegs)

    def getSavedFRegsBitMap = abi.getSavedFRegsBitMap(savedFRegs)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Layout access methods outside [[Frame]]
  // TODO: consider create [[Frame]] object with layout

  final def deprecatedSlotOffsetFromSPByIndex(slotIndex: Int) = layout.deprecatedSlotOffsetFromSPByIndex(slotIndex)

  final def gcMapsOffset(slot: Slot) = layout.gcMapsOffset(slot)

  def hasStackCheck = layout.hasStackCheck

  def frameSize: Int = layout.frameSize

  final def isFull = { assert(mode != null); mode == FULL }

  /** Returns [[MemBased]] location where [[FrameDescriptor]] located. */
  final def frameDescriptorLoc: MemBased = mem(PTR, stackPointer)

  def getSavedIRegsBitMap = layout.getSavedIRegsBitMap

  def getSavedFRegsBitMap = layout.getSavedFRegsBitMap

  /** Returns location of caller frame descriptor. */
  final def callerFrameDescriptor: MemBased = {
    assert(useFramePointer)
    mem(PTR, FP, 2 * stackSlotSize)
  }

  /** Returns location of return address. */
  final def returnAddress: MemBased = {
    assert(useFramePointer)
    mem(PTR, FP, 1 * stackSlotSize)
  }
}
