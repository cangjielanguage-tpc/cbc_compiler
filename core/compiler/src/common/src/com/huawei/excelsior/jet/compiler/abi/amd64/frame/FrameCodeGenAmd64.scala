/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64.frame

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType.{I32, PTR}
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.M
import com.huawei.excelsior.jet.assembler.amd64.GPR.{RAX, RSP}
import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol}
import com.huawei.excelsior.jet.codeemitter.ScratchPool
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{stackSlotSize, tailRegister, targetOS}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.amd64.CallingConventionAmd64.unixRSASize
import com.huawei.excelsior.jet.compiler.abi.amd64.frame.FrameCodeGenAmd64.INTERNAL_SCRATCH
import com.huawei.excelsior.jet.compiler.abi.amd64.{ABIAmd64, FrameAmd64}
import com.huawei.excelsior.jet.compiler.abi.frame.FrameCodeGen
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind
import xscala.properties.OS.{LINUX, WINDOWS}
import xscala.util.MathUtils.isAligned

object FrameCodeGenAmd64 {
  // This scratch could be used in all prologues and epilogues, except eco-friendly methods
  val INTERNAL_SCRATCH = RAX
}

trait FrameCodeGenAmd64 extends FrameCodeGen[GPR, XMM, ABIAmd64] { self: FrameAmd64 =>

  override val emit = {
    assert(!abi.usedArgumentIRegs(INTERNAL_SCRATCH))
    registerUsedReg(INTERNAL_SCRATCH)
    // `ScratchPool.of(INTERNAL_SCRATCH).withOnAcquire(registerUsedReg)` looks like good solution but it could not
    // be used because scratch may be acquired first time during prologue generation when layout is already calculated
    // and it is too late to call `registerUsedReg`.
    new CodeEmitterAmd64(asm, ScratchPool(INTERNAL_SCRATCH), symbolLinker)
  }

  private var unixVarArgsSlot: Slot = _

  private def makeUnixVarArgsSlot(): Slot = {
    assert(unixVarArgsSlot == null)
    // XMMs should be loaded to 16-aligned slots (movaps instructions used)
    unixVarArgsSlot = newSlot(env.getTypeProvider.getCVarArgListDescType.getRawObjectSize + unixRSASize, XMM.SIZE)
    unixVarArgsSlot
  }

  override protected def buildHeader(): Unit = {
    initCallerFrameInfo()
    for (reg <- layout.savedIRegs) {
      asm.push(reg)
      updateCallerFrameInfo(stackSlotSize)
    }
  }

  override protected def destroyHeaderAndReturn(shouldReturn: Boolean): Unit = {
    if (abi.resultLocation == INTERNAL_SCRATCH) {
      /** Generate epilogue without [[INTERNAL_SCRATCH]] because it holds return value. */
      emit.acquireScratch() ensuring (_ == INTERNAL_SCRATCH)
    }

    for (reg <- layout.savedIRegs.reverseIterator) {
      asm.pop(reg)
      updateCallerFrameInfo(-stackSlotSize)
    }

    if (properties.shouldContainGCPointInEpilogueAfterFrameDrop) {
      emit.borrowScratch { tmp =>
        emit.load(tmp, mem(PTR, EER, RTConst.ExecEnv.gcPointTrapAddressUnionOffset))
        emit.load(tmp, mem(I32, tmp, abi.epilogueGCPointTrapOffset(typeProvider)))
      }
    }

    if (shouldReturn) {
      asm.ret(0)
    }

    if (abi.resultLocation == INTERNAL_SCRATCH) {
      emit.releaseScratch(INTERNAL_SCRATCH)
    }
  }

  private def savedXMMOffsetFromSP(xmmIdx: Int, xmmCount: Int) = {
    // first XMM has the highest address, last XMM has the lowest address
    val offset = layout.bodySize - layout.savedNonPushableRegsSize + (xmmCount - xmmIdx - 1) * XMM.SIZE
    offset ensuring (_ >= 0)
  }

  override protected def saveNonPushableRegs(): Unit = {
    val xmmCount = layout.savedFRegs.length
    for (i <- 0 until xmmCount) {
      asm.sse.movaps(M(RSP, savedXMMOffsetFromSP(i, xmmCount)), layout.savedFRegs(i))
    }
  }

  override protected def restoreNonPushableRegs(): Unit = {
    val xmmCount = layout.savedFRegs.length
    for (i <- 0 until xmmCount) {
      asm.sse.movaps(layout.savedFRegs(i), M(RSP, savedXMMOffsetFromSP(i, xmmCount)))
    }
  }

  override protected def allocateAndTouchOnePage(pageSize: Int): Unit = {
    addStackPointer(-(pageSize - stackSlotSize))
    asm.push(RAX) // TODO: investigate the benefits of replacing "push" to "pop"
  }

  def initUnixVarArgs(): Unit = {
    assert(abi.isCVarArgs && targetOS.isLinux)
    addSlot(makeUnixVarArgsSlot())
  }

  override def loadCVarArgsAddrTo(reg: GPR, registerSlot: Slot => Unit): Unit = {
    assert(abi.isCVarArgs)

    val addr = if (hasLayout) {
      if (targetOS.isLinux) {
        assert(unixVarArgsSlot != null)
        mem(RSP, layout.offsetFromSP(unixVarArgsSlot) + unixRSASize)
      } else {
        mem(RSP, layout.frameSize + abi.methodType.parameterCount * stackSlotSize)
      }
    } else {
      // There is baseline compiler.
      if (targetOS.isLinux) {
        if (unixVarArgsSlot == null) {
          registerSlot(makeUnixVarArgsSlot())
        }
        // Baseline defines slot offset during register procedure.
        assert(unixVarArgsSlot.isBound)
        mem(FMR, unixVarArgsSlot.offset + unixRSASize)
      } else {
        // Baseline fix TR in prologue and not reuse it for register allocation so we could address var args from it.
        assert(abi.hasTail)

        //                                              |   var args   |
        //                                              |--------------| <-- TR + (args.size * stackSlotSize - shadow space size)
        //                                              |  fixed args  |
        //                                              |   on stack   |
        //             TR --> / |--------------|        |--------------|        |--------------|
        //                    | |              |        | size of args |        |   var args   |
        //       shadow space | |              |        |  on regs in  |        |--------------| <-- TR + (args.size * stackSlotSize - shadow space size)
        //                    | |              |        | shadow space |        |   reg args   |
        // SP before call --> \ |--------------|        |--------------|        |--------------|
        //
        //          Fig. 1, position of                Fig. 2, fixed args      Fig. 3, fixed args
        //             emulated TR                      on regs and stack         on regs only
        //
        mem(tailRegister, abi.methodType.parameterCount * stackSlotSize - abi.shadowSpaceSize)
      }
    }

    emit.lea(reg, addr)
  }

  override def genBuildAndAdjustParams(needFrameDescriptor: Boolean): Unit = {
    super.genBuildAndAdjustParams(needFrameDescriptor)
    if (abi.isCVarArgs) {
      targetOS match {
        case WINDOWS  => receiveVarArgsWindows()
        case LINUX    => receiveVarArgsSystemV()
      }
    }
    prologueEndLabel()
  }

  override protected def storeFrameDescriptor(fd: Symbol): Unit = {
    emit.borrowScratch(tmp => {
      emit.lea(tmp, mem(fd))
      emit.store(frameDescriptorLoc, tmp)
    })
  }

  override protected def touchMemory(stackPointerOffset: Int): Unit =
    emit.borrowScratch(tmp => emit.load(tmp, mem(I32, RSP, stackPointerOffset)))

  private def receiveVarArgsWindows(): Unit = {
    // Spill all varargs passed on registers into shadow space.
    assert(abi.shadowSpaceSize > 0)

    val fixedParamsCount = abi.methodType.parameterCount
    val paramRegs = abi.allArgumentIRegs

    for (i <- fixedParamsCount until paramRegs.length) {
      val offset = (layout.frameSize + i * stackSlotSize) ensuring (_ > 0)
      emit.store(mem(PTR, RSP, offset), paramRegs(i))
    }
  }

  private def receiveVarArgsSystemV(): Unit = {
    if (unixVarArgsSlot == null) {
      // This method does not use var arguments.
      return
    }

    // Calculate how many regular arguments passed on XMMs, GPRs and stack
    var gprArgsNumber = 0
    var xmmArgsNumber = 0
    var stackArgsNumber = 0
    for (loc <- abi.paramLocations) {
      if (loc.isIReg) {
        gprArgsNumber += 1
      } else if (loc.isFReg) {
        xmmArgsNumber += 1
      } else {
        assert(loc.isMem)
        stackArgsNumber += 1
      }
    }

    //  |                   |
    //  |-------------------|
    //  | va_list structure |
    //  |-------------------|
    //  | RSA               |
    //  |   XMMs            |
    //  |   GPRs            |
    //  |-------------------|
    //  |                   |

    // Fill RSA from down to top.
    val rsaOffsetFromSP = layout.offsetFromSP(unixVarArgsSlot)
    var offsetFromSP = rsaOffsetFromSP

    // 1. Skip GPRs used to pass regular arguments
    offsetFromSP += gprArgsNumber * stackSlotSize

    // 2. Copy var-args GPRs into RSA
    val argumentGPRs = abi.allArgumentIRegs
    val gprsOffsetFromRSA = offsetFromSP - rsaOffsetFromSP
    for (i <- gprArgsNumber until argumentGPRs.length) {
      emit.store(mem(PTR, RSP, offsetFromSP), argumentGPRs(i))
      offsetFromSP += stackSlotSize
    }

    // 3. Skip XMMs used to pass regular arguments
    offsetFromSP += xmmArgsNumber * XMM.SIZE

    // 4. Copy var-args XMMs into RSA
    // TODO: UNIX_VARARGS_XMMS_COUNT_REG may be used to optimize this code,
    //       but is it really necessary for var-args methods?
    //       Note that current implementation of choosing tmp reg knows that we do not use this reg,
    //       modify it if situation changes.
    val argumentXMMs = abi.allArgumentFRegs
    val xmmsOffsetFromRSA = offsetFromSP - rsaOffsetFromSP
    assert(isAligned(offsetFromSP, XMM.SIZE))
    for (i <- xmmArgsNumber until argumentXMMs.length) {
      asm.sse.movaps(M(RSP, offsetFromSP), argumentXMMs(i))
      offsetFromSP += XMM.SIZE
    }

    assert((offsetFromSP - rsaOffsetFromSP) == unixRSASize)

    // Fill va_list structure
    val vaListType = env.getTypeProvider.getCVarArgListDescType
    val vaListBase = offsetFromSP

    def field(`type`: AsmType, name: String) =
      mem(`type`, RSP, vaListBase + vaListType.findField(XString.ascii(name)).getInstanceFieldOffset)

    emit.borrowScratch { tmp =>
      // INTERNAL_SCRATCH == RAX, and it was used to pass number of used XMMs, reuse it.
      assert(tmp == ABIAmd64.UNIX_VARARG_XMMS_COUNT_REG.asGPR)
      // RAX cannot be non-volatile because VMCall with var-args is prohibited in javac.
      assert(abi.isVolatile(tmp))

      emit.addPtr(tmp, RSP, rsaOffsetFromSP)
      emit.store(field(PTR, "regSaveArea"), tmp)

      emit.addPtr(tmp, RSP, layout.frameSize + stackArgsNumber * stackSlotSize)
      emit.store(field(PTR, "overflowArea"), tmp)

      emit.store(field(I32, "fpOffs"), xmmsOffsetFromRSA)
      emit.store(field(I32, "gpOffs"), gprsOffsetFromRSA)
    }
  }

  override protected def adjustParameter(paramIdx: Int): Unit = {
    import TypeKind.*

    val pkind = abi.parameterType(paramIdx).symKindErased
    if (!pkind.isShortIntegral && (pkind != INT)) {
      return
    }

    val loc = abi.paramLocations(paramIdx)
    if (loc.isInstanceOf[TailSlot]) {
      return // TODO: adjust tail params
    }

    assert(loc.isIReg)
    val r32 = loc.asInstanceOf[GPR].asReg32

    pkind match {
      case BOOLEAN | BYTE | SHORT => asm.movsx(r32, r32.as(pkind.width))
      case CHAR => asm.movzx(r32, r32.asReg16)
      case INT => asm.mov(r32, r32)
      case _ => shouldNotReachHere()
    }
  }
}
