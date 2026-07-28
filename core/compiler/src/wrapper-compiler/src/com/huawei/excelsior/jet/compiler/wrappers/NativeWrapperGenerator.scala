/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.common.{Language, LanguagePack}
import com.huawei.excelsior.jet.assembler.AsmType.{I32, PTR}
import com.huawei.excelsior.jet.assembler.Location.{IReg, Mem, mem}
import com.huawei.excelsior.jet.assembler.Width.WPTR
import com.huawei.excelsior.jet.codeemitter.BranchOp.TESTZ
import com.huawei.excelsior.jet.compiler.Env.{addressSize, languagePack}
import com.huawei.excelsior.jet.compiler.{Env, RTConst, RTSProc}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Node, NodeType}
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenerateWriteBarriers
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{CLASS, VOID}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, MethodReferenceAccessKind}

class NativeWrapperGenerator(ctx: GeneratorContext) extends GeneratorBase(ctx) {
  def genBody(): Unit = {
    val params = receiveParameters()

    if (wrapper.isStatic) {
      val classObjNode = Node.newTemporary(NodeType.TREF)
      gen.loadCurrentClassObject(classObjNode)
      params.prepend(classObjNode)
    }

    locally {
      val execEnvParam = Node.newTemporary(NodeType.ADDR)
      val eeParamReg = nodes.bindToAnyFreeIReg(execEnvParam)
      emit.mov(eeParamReg, frame.EER)
      params.prepend(execEnvParam)
    }

    if (wrapper.isStatic) {
      gen.genClinit(wrapper.getDeclaringClass)
    }

    if (Env.languagePack.supports(Language.CANGJIE)) {
      gen.rtsCall(RTSProc.JR_PinFiberToCarrier)()
    }

    val monitorLoc = if (wrapper.isSynchronized) {
      // Native wrapper has complex control flow and it's hard to store monitor on node.
      // (Because Generator is created for generation of linear code.)
      // So we store monitor on frame location which should not be spoiled.
      // Similar solution is implemented for generation of normal synchronized methods:
      // see MethodBytecodeGenerator.monitorLocForSynchronized.
      val monitor = params(1) // params[1] = this or classobject
      val loc = gen.copyRefValueToNewTracedFrameSlot(monitor)
      genMonitorAction(loc, RTSProc.JR_MonitorEnter)
      loc
    } else {
      null
    }

    // NOTE: there should be no exception throwing between monitor enter & exit.

    // wrapper declaring class is surely prepared
    val hostTypeHandle = wrapper.getDeclaringClass.getTypeHandle
    val rttiAddress = Node.newTemporary(NodeType.ADDR)

    nodes.bindToAnyFreeIReg(rttiAddress)
    val rttiAddressReg = nodes.loadToIReg(rttiAddress)

    emit.load(rttiAddressReg, mem(PTR, hostTypeHandle, RTConst.TypeHandle.td.offset))

    // Load address of native method
    val nativeAddr = Node.newTemporary(NodeType.ADDR)
    nodes.bindToAnyFreeIReg(nativeAddr)

    val nativeMethodIndex = wrapper.getNativeMethodIndex

    val nativeAddrReg = nodes.loadToIReg(nativeAddr)
    emit.load(nativeAddrReg, mem(PTR, rttiAddressReg, RTConst.HostingRunTimeTypeInfo.nativeMethodTable.offset + nativeMethodIndex * addressSize))

    val alreadyLinked = emit.newLabel
    emit.branchIfNotNull(nativeAddrReg, alreadyLinked)

    val rethrowException = emit.newLabel

    nodes.withSavedState {
      nodes.releaseLoc(nativeAddr)

      assert(languagePack != LanguagePack.SCALA, s"Not replaced native method $wrapper")
      gen.rtsCall(RTSProc.JR_LinkNative, nativeAddr)(rttiAddress, nativeMethodIndex)

      // JR_LinkNative is a managed method but in case of any exception (e.g. UnsatisfiedLinkError)
      // it sets pending exception in EE. Any pending hardware exception will be instantiated by its explicit `catch`.
      //
      // This is done to allow us to "catch" exception, unlock monitor object and then rethrow it.
      //
      // Alternatively we could surround code between monitor enter and monitor exit with try-catch block
      // but this is hard to implement right now.

      val tmp = Node.newTemporary(NodeType.ADDR)
      val iReg = nodes.bindToAnyFreeIReg(tmp)
      emit.branchIfNotNull(getPendingExceptionLoc(iReg), rethrowException)

      nodes.releaseLoc(tmp)
    }

    emit.bind(alreadyLinked)

    pushRefArgsInEE(params)

    gen.enterGCSafeRegion(RTConst.ExecEnv.nativeWrapperFrameAddr.offset)

    val savedLrefIdx = Node.newTemporary(NodeType.INT)
    nodes.bindToAnyFreeIReg(savedLrefIdx)
    saveLrefIdx(savedLrefIdx)

    val targetType = wrapper.getNativeProcedureMethodType(env)
    val targetRef = new MethodReference(targetType, MethodReferenceAccessKind.STATIC, null, null, null)

    val returnValue = if (!wrapper.getReturnType.jbcKind.isVoid) {
      Node.newTemporary(NodeType.by(wrapper.getReturnType.jbcKind))
    } else {
      null
    }
    gen.genNativeCall(targetRef, nativeAddr, params, returnValue)

    gen.leaveGCSafeRegion(RTConst.ExecEnv.nativeWrapperFrameAddr.offset)

    popRefArgsFromEE()

    restoreLrefIdx(savedLrefIdx)
    nodes.releaseLocIfNotUsedLater(savedLrefIdx)

    locally {
      val tmp = Node.newTemporary(NodeType.ADDR)
      val iReg = nodes.bindToAnyFreeIReg(tmp)

      emit.branchIfNotNull(getPendingExceptionLoc(iReg), rethrowException)
      // No need to check for pending hardware exception, assertion in JNIEntryWrapper.leave should not allow them here.

      nodes.releaseLoc(tmp)
    }

    if (returnValue != null) {
      prepareReturnValue(returnValue)
    }

    if (wrapper.isSynchronized) {
      assert(monitorLoc != null)
      genMonitorAction(monitorLoc, RTSProc.JR_MonitorExit)
    }

    val normalFinalization = emit.newLabel
    emit.jump(normalFinalization)

    emit.bind(rethrowException)

    nodes.withSavedState {
      val xobj = Node.newTemporary(NodeType.TREF)
      val xobjReg = nodes.bindToAnyFreeIReg(xobj)

      val tmp = Node.newTemporary(NodeType.ADDR)
      val tmpReg = nodes.bindToAnyFreeIReg(tmp)

      genGetAndClearPendingException(xobjReg, tmpReg)

      nodes.releaseLoc(tmp)

      if (wrapper.isSynchronized) {
        assert(monitorLoc != null)
        genMonitorAction(monitorLoc, RTSProc.JR_MonitorExit)
      }

      gen.rtsCall(RTSProc.JR_Throw)(xobj)
    }

    emit.bind(normalFinalization)

    handleReturnValue(returnValue)

    ctx.finishGenerationWithReturnAndSendCode()
  }

  private def genMonitorAction(monitorLoc: Mem, proc: RTSProc): Unit = {
    val monitor = Node.newTemporary(NodeType.TREF)
    nodes.bindToAnyFreeIReg(monitor)
    emit.load(nodes.loadToIReg(monitor), monitorLoc)
    gen.rtsCall(proc)(monitor)
  }

  private def prepareReturnValue(returnValue: Node): Unit = {
    val retKind = wrapper.getReturnType.jbcKind
    if (retKind.isShortIntegral) {
      val resReg = nodes.loadToIReg(returnValue)
      gen.signExtendShortIntegralToInt(retKind, resReg, resReg)
    } else if (retKind.isReference) {
      val wrapperFlag = RTConst.JNIReference.WRAPPER_FLAG.intValue

      val resReg = nodes.loadToIReg(returnValue)

      val noWrap = emit.newLabel
      emit.branchIf(TESTZ, resReg, wrapperFlag, WPTR, noWrap)

      val srcMem = mem(CLASS.toAsm, resReg, -wrapperFlag)
      emit.load(resReg, srcMem)

      emit.bind(noWrap)
    }
  }

  private def getLrefLoc: Mem =
    mem(I32, frame.EER, RTConst.JavaExecEnv.localRefsPool.offset + RTConst.LocalRefsPool.index.offset)

  private def saveLrefIdx(savedLrefIdx: Node): Unit =
    emit.load(nodes.loadToIReg(savedLrefIdx), getLrefLoc)

  private def restoreLrefIdx(savedLrefIdx: Node): Unit =
    emit.store(getLrefLoc, nodes.loadToIReg(savedLrefIdx))

  private def pushRefArgsInEE(args: Iterable[Node]): Unit = {
    // NativeCallRefArgs
    val ncra = Node.newTemporary(NodeType.ADDR)
    val refArgsNum = args count (_.`type` == NodeType.TREF)
    val ncraSlot = globalLocations.allocateSlot(
      RTConst.NativeCallRefArgs.size + addressSize * refArgsNum,
      RTConst.NativeCallRefArgs.alignment,
      traced = false)
    emit.lea(nodes.bindToAnyFreeIReg(ncra), ncraSlot)

    var argNum = 0
    for (arg <- args.iterator if arg.`type` == NodeType.TREF) {
      if (env.enabled(GenerateWriteBarriers)) {
        // currently we conservatively globalize all reference parameters passed to JNI code
        // TODO: rework this and make JNI.LocalRefs "non-global by default"
        val argForBarrier = Node.newTemporary(NodeType.TREF)
        gen.copyWithoutRelease(argForBarrier, arg)
        gen.rtsCall(RTSProc.WriteBarriers_writeBarrier_static_baseline)(argForBarrier)
        assert(!nodes.hasLoc(argForBarrier))
      }

      val arrayOffs = RTConst.NativeCallRefArgs.elems.offset
      val argReg = nodes.loadToIReg(arg)
      emit.store(mem(PTR, nodes.loadToIReg(ncra), arrayOffs + argNum * addressSize), argReg)

      argNum += 1
    }
    emit.store(mem(I32, nodes.loadToIReg(ncra), RTConst.NativeCallRefArgs.elemNum.offset), argNum)

    val ncraReg = nodes.loadToIReg(ncra)
    val outerNcra = Node.newTemporary(NodeType.ADDR)
    val outerNcraReg = nodes.bindToAnyFreeIReg(outerNcra)
    assert(ncraReg != outerNcraReg)
    emit.load(outerNcraReg, getRefArgsLoc)

    emit.store(getOuterCallArgsLoc(ncraReg), outerNcraReg)
    emit.store(getRefArgsLoc, ncraReg)

    nodes.releaseLoc(outerNcra)
    nodes.releaseLoc(ncra)
  }

  private def popRefArgsFromEE(): Unit = {
    val ncra = Node.newTemporary(NodeType.ADDR)
    val ncraReg = nodes.bindToAnyFreeIReg(ncra)
    emit.load(ncraReg, getRefArgsLoc)

    val outerNcra = Node.newTemporary(NodeType.ADDR)
    val outerNcraReg = nodes.bindToAnyFreeIReg(outerNcra)
    emit.load(outerNcraReg, getOuterCallArgsLoc(ncraReg))

    emit.store(getRefArgsLoc, outerNcraReg)
    nodes.releaseLoc(outerNcra)
    nodes.releaseLoc(ncra)
  }

  private def getOuterCallArgsLoc(ncraReg: IReg): Mem = mem(PTR, ncraReg, RTConst.NativeCallRefArgs.outerCallArgs.offset)

  private def getRefArgsLoc: Mem = mem(PTR, frame.EER, RTConst.JavaExecEnv.nativeCallRefArgsStack.offset)
}

