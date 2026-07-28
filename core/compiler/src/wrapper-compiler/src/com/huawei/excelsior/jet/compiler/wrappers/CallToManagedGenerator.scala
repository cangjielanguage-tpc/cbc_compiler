/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.jet.assembler.AsmType.{I16, I32, PTR}
import com.huawei.excelsior.jet.assembler.Location.{Mem, mem}
import com.huawei.excelsior.jet.codeemitter.BranchOp.TESTNZ
import com.huawei.excelsior.jet.compiler.Env.isWorkMode
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.RTSProc.JR_PrepareType
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Node, NodeType}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.VOID
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, MethodReferenceAccessKind}

class CallToManagedGenerator(ctx: GeneratorContext) extends GeneratorBase(ctx) {
  def genBody(): Unit = {
    val target = wrapper.getCallToManagedTargetRef

    val params = receiveParameters()

    val execEnv = params.remove(0)

    if (isWorkMode) {
      val eeIsValid = emit.newLabel
      val eeFrameAddr = mem(PTR, nodes.loadToIReg(execEnv), RTConst.ExecEnv.nativeWrapperFrameAddr.offset)
      emit.branchIfNotNull(eeFrameAddr, eeIsValid)

      nodes.withSavedState {
        gen.genFatalError("@CallToManaged method was called from context with invalid EE")
      }

      emit.bind(eeIsValid)
    }

    emit.mov(frame.EER, nodes.loadToIReg(execEnv))
    // we do not clear EE reg because it will be restored in epilogue

    val ctmwFDSlot = globalLocations.allocateSlot(RTConst.CallToManagedWrapperFrameDescriptor.size, RTConst.CallToManagedWrapperFrameDescriptor.alignment, traced = false)

    initCtmwFrameDescriptorWithThreadContext(ctmwFDSlot)

    // check if target's host is prepared
    locally {
      // The code below is similar to Generator#genPreparationCheck.
      // However here we are in unmanaged context so it's not easy to eliminate duplication.
      val hostTD = Node.newTemporary(NodeType.ADDR)
      nodes.bindToAnyFreeIReg(hostTD)
      emit.lea(nodes.loadToIReg(hostTD), target.method.getDeclaringClass.getTypeHandle)

      val alreadyPrepared = emit.newLabel

      val flagsAddr = mem(I16, nodes.loadToIReg(hostTD), RTConst.TypeHandle.flags.offset)
      emit.branchIf(flagsAddr, TESTNZ, RTConst.TypeHandle.Flags.PREPARED.intValue, alreadyPrepared)

      nodes.withSavedState {
        // prepare target's host
        if (!target.method.getDeclaringClass.isAJManagedType) {
          // Note that for Java classes reference to RawTypeHandle is different from reference to TypeHandle,
          // so we must reload the type handle here.
          emit.lea(nodes.loadToIReg(hostTD), target.method.getDeclaringClass.getTypeHandle)
        }
        val jrPrepareRef = new MethodReference(env.getRTSProc(JR_PrepareType), MethodReferenceAccessKind.STATIC)
        gen.genInvokeManaged(jrPrepareRef, Seq(hostTD), null, ctmwFDSlot)
      }

      emit.bind(alreadyPrepared)
    }

    val returnValue = if (!wrapper.getReturnType.isZST) {
      Node.newTemporary(NodeType.by(wrapper.getReturnType.jbcKind))
    } else {
      null
    }
    gen.genInvokeManaged(target, params, returnValue, ctmwFDSlot)
    // Async exception will be checked by caller of this wrapper.
    // Note, that the pending hardware exception code cannot remain set, because during fold-up of the target frame
    // it got materialized either into a proper exception object corresponding to the hardware exception code
    // or into SOE object (with or without full stack-trace). In any case, async exception check in caller is enough.

    restoreThreadContextInEE(ctmwFDSlot)

    nodes.releaseLocIfNotUsedLater(execEnv)
    // any node should not be used later!

    if (frame.EER != null) {
      frame.registerUsedReg(frame.EER)
    }

    handleReturnValue(returnValue)

    ctx.finishGenerationWithReturnAndSendCode()
  }

  private def initCtmwFrameDescriptorWithThreadContext(ctmwFDSlot: Frame.Slot): Unit = {
    val eeFrameAddr = mem(PTR, frame.EER, RTConst.ExecEnv.nativeWrapperFrameAddr.offset)
    val ctmwFDCode = ctmwFDSlot.field(PTR, RTConst.FrameDescriptor.code.offset)

    emit.store(ctmwFDCode, RTConst.FrameDescriptor.Code.CTMW_FD.addrValue)
    saveOrRestoreThreadContext(save = true, ctmwFDSlot)

    emit.storeNull(eeFrameAddr)
  }

  private def restoreThreadContextInEE(ctmwFDSlot: Frame.Slot): Unit = {
    saveOrRestoreThreadContext(save = false, ctmwFDSlot)
  }

  private def saveOrRestoreThreadContext(save: Boolean, ctmwFDSlot: Frame.Slot): Unit = {
    val eeFrameAddr = mem(PTR, frame.EER, RTConst.ExecEnv.nativeWrapperFrameAddr.offset)
    val eeSafeRegionEntranceOffset = mem(I32, frame.EER, RTConst.ExecEnv.safeRegionEntranceOffset.offset)

    val tcOffset = RTConst.CallToManagedWrapperFrameDescriptor.savedThreadContext.offset

    val ctmwFDFrameAddr = ctmwFDSlot.field(PTR, RTConst.CallToManagedWrapperFrameDescriptor.nativeWrapperFrameAddr.offset)
    val savedEESafeRegionEntranceOffset = ctmwFDSlot.field(I32, tcOffset + RTConst.SavedThreadContext.savedSafeRegionEntranceOffset.offset)

    saveOrRestore(save, ctmwFDFrameAddr, eeFrameAddr)
    saveOrRestore(save, savedEESafeRegionEntranceOffset, eeSafeRegionEntranceOffset)
  }

  private def saveOrRestore(save: Boolean, savedLoc: Mem, globalLoc: Mem): Unit = {
    emit.copyAny(if (save) savedLoc else globalLoc, if (save) globalLoc else savedLoc)
  }
}

