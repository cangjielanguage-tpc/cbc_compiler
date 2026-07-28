/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.frame

import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.Env.{frameAlignment, stackPointer, stackSlotSize, tailRegister}
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}
import com.huawei.excelsior.jet.compiler.RTConst
import xscala.util.MathUtils.alignUp

/** Part of [[Frame]] encapsulates code generation - prologue & epilogue.
  *
  * @author conwor
  * @author paul
  */
trait FrameCodeGen[IR >: Null <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] { self: Frame[IR, FR, XABI] =>

  /** This [[CodeEmitter]] has scratches available only for prologue/epilogue code. Thus, it should be used very
    * carefully and only for parts of prologue/epilogue code written outside of [[Frame]] class.
    */
  val emit: CodeEmitter

  /** Generates frame build code and adjust params for calling conventions which are allowed to have short integer parameters. */
  def genBuildAndAdjustParams(needFrameDescriptor: Boolean): Unit = {
    if (needFrameDescriptor) {
      assert(isFull && properties.hasFrameDescriptor)
    } else {
      assert(!layout.hasStackCheck)
    }

    buildHeader()
    allocateFrameAndCheckStack() // Note that on some platforms last stack check is done in `finishFrameBuild`
    saveNonPushableRegs()
    finishFrameBuild(needFrameDescriptor)

    if (abi.allowShortIntegers) {
      for (i <- 0 until abi.parameterCount) {
        adjustParameter(i)
      }
    }
  }

  private def allocateFrameAndCheckStack(): Unit = {
    val lastStackAccess = allocateFrameBody(layout.bodySize)
    checkStackBelowSPIfNeeded(lastStackAccess) // Reserve additional stack size by memory accesses.
  }

  protected def checkStackBelowSPIfNeeded(lastTouchedOffset: Int): Unit = {
    if (!layout.hasStackCheck) {
      return
    }

    val pageSize = RTConst.VirtualMemory.MIN_PAGE_SIZE.intValue
    val requiredLastStackAccess = -layout.additionalStackCheckSize
    var accessPoint = lastTouchedOffset - pageSize

    while (accessPoint > requiredLastStackAccess) {
      touchMemory(accessPoint)
      accessPoint -= pageSize
    }
    touchMemory(requiredLastStackAccess)
  }

  /** SP gets shifted, but memory at SP may remain untouched. Returns the last touched offset from SP
    * (to be passed for future call of checkStackBelowSPIfNeeded).
    */
  protected def allocateFrameBody(bytesToAllocate: Int) = {
    val pageSize = RTConst.VirtualMemory.MIN_PAGE_SIZE.intValue

    // Offset of last stack access from SP value after all frame allocated.
    var lastStackAccess = bytesToAllocate

    while (lastStackAccess >= pageSize) {
      allocateAndTouchOnePage(pageSize)
      lastStackAccess -= pageSize
    }

    // Subtract remaining frame space from SP
    assert(lastStackAccess >= 0)
    addStackPointer(-lastStackAccess)
    lastStackAccess
  }

  private def finishFrameBuild(needFrameDescriptor: Boolean): Unit = {
    if (isFull) {
      if (layout.hasFrameDescriptorSlot) {
        assert(layout.extraAllocSize >= stackSlotSize) // Check that there is enough space for frame descriptor or junk slot
      }

      if (needFrameDescriptor) {
        val fd = properties.getFrameDescriptor
        assert(layout.hasFrameDescriptorSlot)
        assert((fd != null) && symbolLinker.isDirectAccess(fd))
        storeFrameDescriptor(fd) // Might be the SOE-provoking stack access on some platforms
      }
    }

    ///////////////////////////////////////////////////////////
    // Any code generated above this line                    //
    // might be subject to run-time prologue interpretation. //
    // Take care modifying it.                               //
    ///////////////////////////////////////////////////////////
    setupFramePointers()
  }

  protected def setupFramePointers(): Unit = {
    if (useFMRAddressing && slots.nonEmpty) {
      assert(layout.savedIRegs.contains(FMR))
      emit.addPtr(FMR, stackPointer, layout.extraAllocSize)
    }

    if (useFramePointer) {
      assert(layout.savedIRegs.contains(FP))
      emit.addPtr(FP, stackPointer, layout.frameSize - preHeaderSize - framePointerSetupOffset)
    }

    if (abi.hasEmulatedTail) {
      emit.addPtr(tailRegister, stackPointer, layout.frameSize + abi.stackParamsStartOffset)
    }
  }

  /** Generates frame destroy code. */
  def genDestroy(shouldReturn: Boolean): Unit = {
    epilogueBeginLabel()
    restoreNonPushableRegs()
    addStackPointer(layout.bodySize)
    destroyHeaderAndReturn(shouldReturn)
  }

  protected def addStackPointer(value: Int): Unit = if (value != 0) {
    emit.addPtr(stackPointer, stackPointer, value)
    updateCallerFrameInfo(-value)
  }

  def allocateFakeParamsArea(abi: XABI) = {
    val fakeParamsAreaSize = alignUp(abi.sizeOnCallerFrameInBytes, frameAlignment)
    addStackPointer(-fakeParamsAreaSize)
    // SOE-Note: stores to the newly allocated stack space cannot provoke SOE
    //           because this space is allocated for the call of unmanaged method
    //           and hence stack check has been generated in prologue.
    fakeParamsAreaSize
  }

  def loadCVarArgsAddrTo(reg: IR, registerSlot: Slot => Unit): Unit


  //////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected def buildHeader(): Unit
  protected def destroyHeaderAndReturn(shouldReturn: Boolean): Unit
  protected def touchMemory(stackPointerOffset: Int): Unit
  protected def storeFrameDescriptor(fd: Symbol): Unit
  protected def allocateAndTouchOnePage(pageSize: Int): Unit
  protected def saveNonPushableRegs(): Unit
  protected def restoreNonPushableRegs(): Unit
  protected def adjustParameter(paramIdx: Int): Unit


  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Synonyms for access from code without XABI type (newbaseline)
  // TODO: remove these crutches

  def nb_allocateFakeParamsArea(abi: Object): Int = allocateFakeParamsArea(abi.asInstanceOf[XABI])
}
