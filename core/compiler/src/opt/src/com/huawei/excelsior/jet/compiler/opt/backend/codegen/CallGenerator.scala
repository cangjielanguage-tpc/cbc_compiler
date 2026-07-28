/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.compiler.Env.{stackPointer, tailRegister, targetArch, targetPlatform}
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.Field
import com.huawei.excelsior.jet.compiler.{RTConst, RTSProc}

/** Part of [[CodeGenerator]], responsible for call nodes generation.
  *
  * @author conwor
  * @author ikireev
  */
trait CallGenerator { self: Universe with BackEnd =>

  trait CallGeneratorImpl { self: XSitesGenerator with CodeGeneratorImpl =>
    import BarrierKind.*

    def fieldAddr(base: IReg, field: Field): Mem =
      mem(field.getType.toAsm, base, field.getInstanceFieldOffset)

    private def setGCSafetyState(gcPointsTLD: IReg, state: Int): Unit = {
      emit.store(fieldAddr(gcPointsTLD, RT.GCPointsThreadLocalData.gcSafetyState), state)
    }

    private def fastGCPoint(gcPointsTLD: IReg, call: Call, regionEnter: Boolean): Unit = {
      emit.load(gcPointsTLD, fieldAddr(gcPointsTLD, RT.GCPointsThreadLocalData.gcPointTrapAddress))
      if (regionEnter) {
        assert(rootMethod.hasManagedExecEnv)
        assert(!call.hasXSite)
        addGCSafeRegionXSite(call)
      }
      emit.load(gcPointsTLD, mem(PTR, gcPointsTLD, if (regionEnter) RTConst.GCPoints.fastNoInspectionTrapOffset.intValue else RTConst.GCPoints.fastWithInspectionTrapOffset.intValue))
    }

    /** AJ runtime method com.excelsior.jet.runtime.memory.gc.sections.GCSafe.onGCSafeEnterWithSavingContext implementation. */
    private def enterGCSafeRegion(call: Call, savedFrameAddrField: Field): Unit = {
      val (tmp1, tmp2) = {
        // On ARM64 we need two temporal registers.
        val available = (call.spoiled ++ call.allResultResources).filter(r => r.isIReg && !call.isArgumentResource(r))
        assert(available.size >= 2, s"not enough available spoiled registers to generate gc safe region for call: $call")
        // TODO: support methods with `CC.hasExtraNonVolatiles` (JET-14414)
        (available(0).asIReg, available(1).asIReg)
      }

      val asmLabel = new Label()
      emit.loadLabelPosition(tmp1, asmLabel)
      emit.store(fieldAddr(frame.EER, RT.ExecEnv.safeRegionEntranceOffset), tmp1)

      assert(rootMethod.hasFrameDescriptor)
      ensureFullFrame()
      emit.withScratch(tmp1) {
        emit.store(fieldAddr(frame.EER, savedFrameAddrField), stackPointer)
      }

      emit.memBarrier(STORE_STORE)

      emit.lea(tmp1, mem(frame.EER, RT.ExecEnv.memoryManagerData.getOffset + RT.ThreadLocalMMData.gcPointsTLD.getOffset))
      emit.withScratch(tmp2) {
        setGCSafetyState(tmp1, RTConst.GCSafetyState.SAFE.intValue)
      }

      emit.memBarrier(STORE_LOAD)

      fastGCPoint(tmp1, call, regionEnter = true)
      asm.bind(asmLabel)
    }

    /** AJ runtime method com.excelsior.jet.runtime.memory.gc.sections.GCSafe.onGCSafeLeave implementation. */
    private def leaveGCSafeRegion(call: Call, savedFrameAddrField: Field): Unit = {
      val (tmp1, tmp2) = {
        // On ARM64 we need two temporal registers.
        val available = call.spoiled.filter(_.isIReg)
        assert(available.size >= 2, s"not enough available spoiled registers to generate gc safe region for call: $call")
        // TODO: support methods with `CC.hasExtraNonVolatiles` (JET-14414)
        (available(0).asIReg, available(1).asIReg)
      }

      emit.lea(tmp1, mem(frame.EER, RT.ExecEnv.memoryManagerData.getOffset + RT.ThreadLocalMMData.gcPointsTLD.getOffset))
      emit.withScratch(tmp2) {
        setGCSafetyState(tmp1, RTConst.GCSafetyState.UNSAFE.intValue)
      }

      // to prevent reordering: read trap addr, set unsafe state, dereference trap addr.
      // TODO: try setGCSafetyState with release semantics
      emit.memBarrier(STORE_LOAD)

      fastGCPoint(tmp1, call, regionEnter = false)

      // if gc point is triggered it is guaranteed that PutField is not executed.
      emit.storeNull(fieldAddr(frame.EER, savedFrameAddrField))
    }

    private def gcSafeStateAssert(call: Call): Unit = {
      val assertMethod = env.getRTSProc(RTSProc.JR_GCSafeStateAssert)
      assert(assertMethod.getCallConv == VMCALL && assertMethod.isStatic && assertMethod.getReturnType.isZST)
      call match {
        case DirectCall(_) | DAICall(_) =>
        case _ =>
          // Guaranteed by NodesDescription.indirectCallTargetSet
          assert(targetPlatform.abi(assertMethod).isNonVolatile(iReg(call.target)))
      }
      emit.call(assertMethod)
    }

    protected def beforeCallActions(call: Call): Unit
    protected def genCallImpl(call: Call): Unit
    protected def afterCallActions(call: Call): Unit

    private[codegen] final def genCall(call: Call): Unit = {
      val targetRef = call.targetRef

      if (call.methodType.callConv.hasManagedExecEnv) {
        assert(rootMethod.hasManagedExecEnv, s"calling managed $targetRef from unmanaged context")
      }

      if (!env.enabled(NeverInline) && targetRef.hasMethod) {
        assert(!targetRef.method.isInlineAllAndRemove)
      }

      val gcActions = call.gcActions

      if (gcActions.generateGCSafeRegion && (targetArch != CBC)) { // lowering-jit & interpreter takes all needed actions
        enterGCSafeRegion(call, gcActions.savedFrameAddrField)
      }

      if (gcActions.checkGCSafeState && (targetArch != CBC)) {
        gcSafeStateAssert(call)
      }

      beforeCallActions(call)
      if (call.abi.hasRealTail) {
        assert(call spoils tailRegister)
        initTailRegister(call)
      }

      genCallImpl(call)
      if (call.hasXSite) {
        assert(!gcActions.generateGCSafeRegion || (targetArch == CBC))
        addXSite(call)
      }
      afterCallActions(call)

      if (gcActions.generateGCSafeRegion && (targetArch != CBC)) {
        leaveGCSafeRegion(call, gcActions.savedFrameAddrField)
      }
    }

    protected def initTailRegister(call: Call): Unit = { // most common implementation
      emit.lea(tailRegister, mem(stackPointer, call.abi.stackParamsStartOffset))
    }

    private[codegen] def genPreCall(node: PreCall): Unit = {
      assert(CodeOrder.next(node).isInstanceOf[Call])
      addXSite(node)
    }
  }
}
