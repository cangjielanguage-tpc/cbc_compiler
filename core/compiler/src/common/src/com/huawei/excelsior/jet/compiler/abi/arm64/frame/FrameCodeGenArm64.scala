/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64.frame

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType.{I32, PTR}
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.Width.{W128, WPTR}
import com.huawei.excelsior.jet.assembler.arm64.Arg.M
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{IP0, IP1, LR, SP}
import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode.PRE_IDX
import com.huawei.excelsior.jet.assembler.arm64.{IRegister, Register, VFPRegister}
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol}
import com.huawei.excelsior.jet.codeemitter.ScratchPool
import com.huawei.excelsior.jet.codeemitter.arm64.CodeEmitterArm64
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{isJIT, stackSlotSize}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.arm64.FrameArm64.{FP_VARARGS_ALIGNMENT, FP_VARARG_SIZE}
import com.huawei.excelsior.jet.compiler.abi.arm64.frame.FrameCodeGenArm64.CALL_SCRATCH
import com.huawei.excelsior.jet.compiler.abi.arm64.{ABIArm64, FrameArm64}
import com.huawei.excelsior.jet.compiler.abi.frame.FrameCodeGen
import xscala.util.MathUtils.{alignUp, isAligned}

object FrameCodeGenArm64 {
  // Volatile register in all calling conventions.
  // Not RA so that it would not be needed to be restored during prologue interpreting.
  val CALL_SCRATCH = IP0
}

trait FrameCodeGenArm64 extends FrameCodeGen[IRegister.X, VFPRegister.D, ABIArm64] { self: FrameArm64 =>

  override val emit: CodeEmitterArm64 = new CodeEmitterArm64(asm, ScratchPool(CALL_SCRATCH), symbolLinker, isJIT)

  // The slot contains only va_list structure, the va-occupied regs are spilled along with savedRegs in prologue
  private var vaListSlot: Slot = _

  def initCVarArgs(): Unit = {
    assert(abi.isCVarArgs)
    addSlot(makeVaListSlot())
  }

  private def makeVaListSlot(): Slot = {
    assert(vaListSlot == null)
    vaListSlot = newSlot(env.getTypeProvider.getCVarArgListDescType.getRawObjectSize, stackSlotSize)
    vaListSlot
  }

  override def loadCVarArgsAddrTo(reg: IRegister.X, registerSlot: Slot => Unit): Unit = {
    assert(abi.isCVarArgs)

    if (hasLayout) {
      assert(vaListSlot != null)
      emit.addPtr(reg, SP, layout.offsetFromSP(vaListSlot))
    } else {
      if (vaListSlot == null) {
        registerSlot(makeVaListSlot())
      }
      // There is baseline compiler, which defines slot offset during register procedure
      assert(vaListSlot.isBound)
      emit.addPtr(reg, FMR, vaListSlot.offset)
    }
  }

  override def genBuildAndAdjustParams(needFrameDescriptor: Boolean): Unit = {
    if (needFrameDescriptor) {
      assert(isFull && properties.hasFrameDescriptor)
    } else {
      assert(!layout.hasStackCheck)
    }

    {
      // region Code emitter not allowed
      // See JET-15587 for reasoning and PrologueInterpreter for allowed asm instructions
      // Generated instructions may produce SOE

      initCallerFrameInfo()

      // this is the future feature turned off
      /*
      // adr MUST be the first prologue instruction (when needFrameDescriptor)
      if (needFrameDescriptor) {
        assert(abi.getCallingConvention.isVolatileReg(METADATA_BASE, abi.methodType))
        asm.adr(METADATA_BASE, 0)
      }
      */

      var spIs8BytesLower = false
      spIs8BytesLower = saveRegsByPair(vaOccupiedIRegs, spIs8BytesLower = false)

      // FP varargs should be spilled as quads
      spIs8BytesLower = saveRegsByPair(vaOccupiedFRegs.map(_.asV), spIs8BytesLower = false) // ignore free slot

      initCallerFrameRA(if (spIs8BytesLower) 0 else -stackSlotSize)

      spIs8BytesLower = saveRegsByPair(layout.savedIRegs, spIs8BytesLower)
      spIs8BytesLower = saveRegsByPair(layout.savedFRegs, spIs8BytesLower)

      assert(layout.frameAligned)
      val remainToAllocate = if (spIs8BytesLower) layout.bodySize - 8 else layout.bodySize
      assert(remainToAllocate >= 0, "when spilled regs num is odd, body must compensate the total frame alignment")
      assert(isAligned(remainToAllocate, 16), "saveRegsByPair should leave SP quad-aligned")
      val lastTouchedOffset = allocateFrameBody(remainToAllocate)

      if (needFrameDescriptor) {
        // this is the future feature turned off
        // asm.str(metadataBase, SP, 0) // store (not "push") here as sp is already shifted by allocateFrameBody

        // old style fd
        val fd = properties.getFrameDescriptor
        assert(properties.isManagedFrame)
        assert((fd != null) && symbolLinker.isDirectAccess(fd))

        if (isJIT) {
          asm.ldr(CALL_SCRATCH, asm.literal(fd, 0))
        } else {
          asm.adr(CALL_SCRATCH, fd)
        }
        asm.str(WPTR, CALL_SCRATCH, M(SP))
      }

      checkStackBelowSPIfNeeded(lastTouchedOffset)
      // endregion
      // Past this point of prologue no SOE in method can be thrown
    }

    setupFramePointers()

    generateVaList()

    prologueEndLabel()
  }

  override def genDestroy(shouldReturn: Boolean): Unit = {
    val bodyIsQuadAligned = isAligned(layout.bodySize, 16)
    addStackPointer(if (bodyIsQuadAligned) layout.bodySize else layout.bodySize - 8    )
    // SP is quad-aligned and optionally 8 bytes lower than saved regs starts
    var spIs8BytesLower = !bodyIsQuadAligned
    spIs8BytesLower = restoreRegsByPair(layout.savedFRegs, spIs8BytesLower)
    spIs8BytesLower = restoreRegsByPair(layout.savedIRegs, spIs8BytesLower)

    if (vaOccupiedIRegs.nonEmpty || vaOccupiedFRegs.nonEmpty) {
      // Drop extra saved slots for var args accounting 8 bytes optionally overstepped by loadRegsByPair
      val shift = preHeaderSize + (if (spIs8BytesLower) 8 else 0)
      addStackPointer(shift)
    }

    genEpilogueGCPoint()

    if (shouldReturn) {
      asm.ret(LR)
    }
  }

  /** @param regs            - registers to be spilled
    * @param spIs8BytesLower - if free slot is available at [SP + 0] before the call
    * @return true if free slot is available at [SP + 0]
    */
  private def saveRegsByPair(regs: IndexedSeq[_ <: Register], spIs8BytesLower: Boolean): Boolean = {
    // JET-15587
    // region Code emitter not allowed
    if (regs.isEmpty) {
      return spIs8BytesLower
    }
    val quadRegs = regs.head.width == W128
    var idx = 0
    if (spIs8BytesLower) {
      assert(!quadRegs, "pass spIs8BytesLower == 'false' when call for fp varargs")
      asm.str(regs(idx), M(SP, 0)) // store to free slot
      idx += 1
    }
    while (idx < regs.size - 1) {
      asm.stp(regs(idx + 1), regs(idx), M(PRE_IDX, SP, -2 * regs(idx + 1).width.nbytes))
      updateCallerFrameInfo(2 * stackSlotSize)
      idx += 2
    }
    if (idx == regs.size - 1) {
      if (quadRegs) {
        asm.str(regs(idx), M(PRE_IDX, SP, -16))
        return false
      } else {
        addStackPointer(-16)
        asm.str(regs(idx), M(SP, 8)) // store to upper slot, lower one is left free
        return true
      }
    }
    false
    // endregion
  }

  /** @param regs            - registers to be filled - are in the order they were spilled, so load them in reverse order
    * @param spIs8BytesLower - if the first fill should be done from [SP + 8] (SP must always be 16-bytes aligned)
    * @return true if the next fill should be done from [SP + 8], i.e. SP is shifted 8 bytes less than needed
    */
  private def restoreRegsByPair(regs: IndexedSeq[_ <: Register], spIs8BytesLower: Boolean): Boolean = {
    if (regs.isEmpty) {
      return spIs8BytesLower
    }
    var idx = regs.size - 1
    if (spIs8BytesLower) {
      asm.ldr(regs(idx), M(SP, 8))
      idx -= 1
      addStackPointer(16)
    }
    while (idx > 0) {
      emit.popPair(regs(idx), regs(idx - 1))
      updateCallerFrameInfo(-2 * stackSlotSize)
      idx -= 2
    }
    if (idx == 0) {
      asm.ldr(regs(idx), M(SP, 0))
      return true
    }
    false
  }

  private def genEpilogueGCPoint(): Unit = if (properties.shouldContainGCPointInEpilogueAfterFrameDrop) {
    emit.borrowScratch(tmp => {
      emit.load(tmp, mem(PTR, EER, RTConst.ExecEnv.gcPointTrapAddressUnionOffset))
      emit.load(tmp, mem(PTR, tmp, abi.epilogueGCPointTrapOffset(typeProvider)))
    })
  }

  /** this overrides METADATA_BASE (that resides in scratch-reg), so ensure FD is already stored. */
  private def generateVaList(): Unit = {
    if (vaListSlot == null) {
      // This method does not use var arguments.
      return
    }

    val gpArgsSize = vaOccupiedIRegs.length * stackSlotSize

    val fpArgsSize = vaOccupiedFRegs.length * FP_VARARG_SIZE

    val argsOnStackOffset = layout.frameSize
    val gpArgsOffset = argsOnStackOffset
    val fpArgsOffset = gpArgsOffset - alignUp(gpArgsSize, FP_VARARGS_ALIGNMENT)

    // fill va_list structure
    val vaListType = env.getTypeProvider.getCVarArgListDescType
    val vaListBase = layout.offsetFromSP(vaListSlot)

    def field(`type`: AsmType, name: String) =
      mem(`type`, SP, vaListBase + vaListType.findField(XString.ascii(name)).getInstanceFieldOffset)

    // TODO: consider tmp to be a FP/SIMD 128-bit register when it gets supported
    //       with V128 we can build va_list with only two stores: <stack|gr_top> for the first,
    //       and <vr_top|gr_offs|vr_offs> for the second one
    val tmp = IP1 ensuring (_ != CALL_SCRATCH)

    emit.addPtr(tmp, SP, argsOnStackOffset)
    emit.store(field(PTR, "stack"), tmp)

    emit.addPtr(tmp, SP, gpArgsOffset)
    emit.store(field(PTR, "gr_top"), tmp)

    emit.addPtr(tmp, SP, fpArgsOffset)
    emit.store(field(PTR, "vr_top"), tmp)

    emit.withScratch(tmp) {
      emit.store(field(I32, "gr_offs"), -gpArgsSize)
      emit.store(field(I32, "vr_offs"), -fpArgsSize)
    }
  }

  override protected def touchMemory(stackPointerOffset: Int): Unit = {
    assert(isFull)
    assert(stackPointerOffset <= 0)

    // JET-15587
    // region Code emitter not allowed
    if (stackPointerOffset == 0) {
      asm.ldr(false, PTR.width, CALL_SCRATCH, M(SP))
    } else {
      asm.movn(CALL_SCRATCH, ~stackPointerOffset, 0)
      asm.ldr(false, PTR.width, CALL_SCRATCH, M(SP, CALL_SCRATCH))
    }
    // endregion
  }

  override protected def allocateAndTouchOnePage(pageSize: Int): Unit = {
    // JET-15587
    // region Code emitter not allowed
    addStackPointer(-pageSize)
    asm.str(PTR.width, CALL_SCRATCH, M(SP, 0))
    // endregion
  }

  override protected def addStackPointer(value: Int): Unit = {
    // TODO: define different methods for addStackPointer, one for prologue and one for epilogue,
    //  since addStackPointer method is called from places with conflicting requirements for code-generation.
    if (value < 0) {
      // prologue
      // JET-15587
      // region Code emitter not allowed
      asm.sub(SP, SP, -value)
      updateCallerFrameInfo(-value)
      // endregion
    } else {
      // epilogue, super method complies with asm requirements
      // Example: com.huawei.excelsior.jet.runtime.excepts.arm64.FrameCrawlerImpl
      super.addStackPointer(value)
    }
  }

  override protected def buildHeader(): Unit = shouldNotReachHere()

  override protected def storeFrameDescriptor(fd: Symbol): Unit = shouldNotReachHere()

  override protected def destroyHeaderAndReturn(shouldReturn: Boolean): Unit = shouldNotReachHere()

  override protected def saveNonPushableRegs(): Unit = {} // Nothing to do

  override protected def restoreNonPushableRegs(): Unit = {} // Nothing to do

  override protected def adjustParameter(paramIdx: Int): Unit = {} // TODO: adjust params
}
