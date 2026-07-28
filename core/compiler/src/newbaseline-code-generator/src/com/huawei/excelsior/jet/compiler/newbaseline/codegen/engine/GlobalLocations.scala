/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.codeemitter.ScratchPool
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame, SlotBase}
import xscala.util.MathUtils.{alignUp, isPowerOf2}

import scala.collection.mutable.ArrayBuffer

abstract class GlobalLocations(
  // Here we are breaking bad by omitting generic types and so cheating javac.
  // FIXME: add generic types and fix numerous compilation errors
  val frame: Frame[? <: IReg, ? <: FReg, ? <: ABI[? <: IReg, ? <: FReg]]) {

  private var totalStackAlloc = 0
  private val _tracedStackAllocSlots = new ArrayBuffer[Frame.Slot]

  def allocate(slot: Frame.Slot): Unit = {
    val size = slot.size
    val alignment = slot.alignment
    assert(alignment >= stackSlotSize)

    if (!(size >= 0 && alignment >= 1 && isPowerOf2(alignment))) {
      shouldNotReachHere(s"invalid stack alloc arguments: size $size, alignment $alignment")
    }

    val offset = alignUp(totalStackAlloc, alignment)
    totalStackAlloc = offset + size

    slot.bind(SlotBase.FMR, offset)
    frame.addSlot(slot)
  }

  /** Allocates memory on the frame and returns location for accessing it. */
  private def allocateOnStack(`type`: AsmType, traced: Boolean): MemLocal = {
    val size = `type`.width.nbytes
    allocateSlot(size, size, traced) as `type`
  }

  /** Allocates traced memory on the frame and returns location for accessing it. */
  def allocateOnStackTraced(`type`: AsmType): MemLocal = allocateOnStack(`type`, traced = true)

  /** Allocates untraced memory on the frame and returns location for accessing it. */
  def allocateOnStackUntraced(`type`: AsmType): MemLocal = allocateOnStack(`type`, traced = false)

  /** For location created by `allocateOnStack` or `onStackParamLocation` returns slot associated with it. */
  def slotByLoc(loc: Mem): Frame.Slot =
    loc.asInstanceOf[MemLocal].slot.asInstanceOf[Frame.Slot]

  /** Allocate memory on the frame and return slot associated with it.
    *
    * @param size      non-negative size in bytes
    * @param alignment positive alignment which should be power of 2
    * @param traced    indicates whether the allocated buffer should be traced by GC
    */
  def allocateSlot(size: Int, alignment: Int, traced: Boolean): Frame.Slot = {
    // ignore tiny alignments for better runtime performance
    val slot = frame.newSlot(size, alignment max stackSlotSize)
    allocate(slot)
    if (traced && size >= stackSlotSize) {
      _tracedStackAllocSlots += slot
    }
    slot
  }

  def tracedStackAllocSlots: Iterable[Frame.Slot]  = _tracedStackAllocSlots

  /** General scratch register. It should be writable while passing params. It is desirable that it is volatile for most calling conventions. */
  def scratches: Array[_ <: IReg]

  def scratchProvider: ScratchPool = ScratchPool.apply(scratches) withOnAcquire frame.registerUsedReg
}
