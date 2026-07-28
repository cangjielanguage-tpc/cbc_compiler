/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.frame

import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg}
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame, SlotBase}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReference
import xscala.util.MathUtils.alignUp

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Part of [[Frame]] collecting all elements on which [[FrameLayout]] depends - used registers, stack slots,
  * param passing area requirements and stack check requirements.
  *
  * @author conwor
  * @author paul
  */
trait FrameElements[IR >: Null <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] { self: Frame[IR, FR, XABI] =>

  /////////////////////////////////////////////////////////////////////////////
  // Registers: special, available for compiler and saved in prologue

  /** Frame pointer used for access to caller frame info (return address, frame descriptor, frame pointer). */
  val FP: IR = if (useFramePointer) framePointer.asInstanceOf[IR] else null

  /** Frame middle register used in baseline compiler to access spill slots with compile-time known offsets before frame layout calculated. */
  val FMR: IR = if (useFMRAddressing) frameMiddleRegister.asInstanceOf[IR] else null

  /** ExecEnv register points to managed ExecEnv structure for current thread. */
  val EER: IR = if (properties.isManagedFrame) execEnvRegister.asInstanceOf[IR] else null

  /** Ordered sequence of all IRegs available for compiler. Volatile registers come before non-volatile ones; FP, FMR, EER are filtered out. */
  final def availableIRegs: Iterator[IR] = abi.availableIRegs.iterator.filter(r => (r != FMR) && (r != FP) && (r != EER))

  /** Ordered sequence of all FRegs available for compiler. Volatile registers come before non-volatile ones. */
  final def availableFRegs: Iterator[FR] = abi.availableFRegs.iterator

  /** Set of non-volatile registers which used by compiler. Should be saved in prologue and restored in epilogue. */
  protected val savedRegs = new mutable.HashSet[AnyReg]

  /** Register that `reg` was used by compiler. */
  final def registerUsedReg(reg: AnyReg) = {
    assert(!hasLayout)
    if (abi.shouldBeSavedInPrologue(reg)) savedRegs += reg
  }

  /** Marks this frame as one of method with GCSafe call-site. In this case all non-volatile registers which can
    * contain traceable reference should be saved/restored in prologue/epilogue even if they are not used in this
    * method. Note that Tail register is non-volatile and thus will also be handled here.
    */
  final def markAsFrameWithGCSafeCallSite(): Unit = {
    for (r <- abi.availableIRegs if abi.isNonVolatile(r) && r != EER) registerUsedReg(r)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Stack slots - used for spill or stack alloc

  /** All slots collection. */
  private val _slots = new ArrayBuffer[Slot]
  def slots = _slots.iterator

  /** TODO: remove it */
  protected def slotByIndex(index: Int) = _slots(index)

  /** Appends `slot` to this frame slots collection. */
  final def addSlot(slot: Slot) = {
    assert(!hasLayout)
    _slots += slot
  }

  /** Creates new [[Slot]] with `size` and required `alignment`. Iff `tracedByHeader` is true, only header of this slot will be in GC maps. */
  final def newSlot(size: Int, alignment: Int, tracedByHeader: Boolean = false): Slot = new Frame.Slot(size, alignment, tracedByHeader) {
    override protected def baseRegister() = base match {
      case SlotBase.SP => stackPointer
      case SlotBase.FMR => frameMiddleRegister
      case SlotBase.TR => tailRegister
    }
  }

  /** Creates new [[Slot]] with `size` and fixed `base` and `offset`. TODO: deprecate it or better remove. */
  final def newSlot(size: Int, base: SlotBase, offset: Int): Slot = {
    val slot = newSlot(size, size)
    slot.bind(base, offset)
    slot
  }


  /////////////////////////////////////////////////////////////////////////////
  // Param passing area (also used for frame descriptor)

  private var _paramPassingAreaSize = 0

  /** Reserve non-unique `size` space above SP. */
  protected final def reserveSpaceAboveSP(size: Int): Unit = {
    assert(!hasLayout)
    _paramPassingAreaSize = Math.max(size, _paramPassingAreaSize)
  }

  /** Returns size of param passing area. */
  final def paramPassingAreaSize = _paramPassingAreaSize

  /** Reserve non-unique space above SP required to pass params in `abi`. */
  final def reserveSpaceForCall(callee: XABI): Unit = {
    // TODO: refactor all complicated calculations of [[sizeOnCallerFrameInBytes]] and [[stackParamsStartOffset]]
    val extraSize = if (callee.spoilsCallerFrameDescriptor(caller = abi.methodType)) stackSlotSize else 0
    reserveSpaceAboveSP(callee.sizeOnCallerFrameInBytes + extraSize)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Stack check requirements

  /** Size of required stack check for any reasons of method code. */
  protected var requiredStackCheckSizeInBytes = 0

  private def updateRequiredStackCheckSize(stackCheckSize: Int) = {
    assert(!hasLayout)
    requiredStackCheckSizeInBytes = Math.max(requiredStackCheckSizeInBytes, stackCheckSize)
    requiredStackCheckSizeInBytes
  }

  /** Require stack check for call of `targetRef`. */
  final def registerStackCheckForCall(targetRef: MethodReference) = updateRequiredStackCheckSize(targetRef.getStackCheckByCallerBytes)

  /** Require stack check for arbitrary DAI call. */
  final def registerStackCheckForDAICall() = updateRequiredStackCheckSize(RTConst.StackOverflowHandling.STACK_RESERVE_FOR_RT_SUPPORT.intValue)

  /** Require stack check for exception handling. */
  final def registerStackCheckForExceptionHandling() = updateRequiredStackCheckSize(RTConst.StackOverflowHandling.STACK_RESERVE_FOR_RT_SUPPORT.intValue)


  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Synonyms for access from code without XABI type (newbaseline)
  // TODO: remove these crutches
  final def nb_reserveSpaceForCall(abi: Object): Unit = reserveSpaceForCall(abi.asInstanceOf[XABI])
}
