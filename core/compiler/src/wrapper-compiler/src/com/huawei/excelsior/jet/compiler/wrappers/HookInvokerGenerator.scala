/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.wrappers

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.{F64, PTR}
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, Mem}
import com.huawei.excelsior.jet.assembler.Width.WPTR
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, tailRegister}
import com.huawei.excelsior.jet.compiler.{RTConst, RTSProc}
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{Node, NodeType}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, MethodReferenceAccessKind}

/** Generates body of `@Hook.Invoker`-annotated method.
  *
  * Such method must be declared in an effectively-final subclass of `Hook.Base` abstract class.
  * By effectively-final class we mean that it must not have any subclasses apart from definition-type implementations.
  *
  * Its body consists of the following important parts:
  *
  *  1. Save all arguments-passing registers on stack;
  *
  *  1. Allocate instance of host class on stack;
  *
  *  1. Fill its "magically-filled" fields defined in `Hook.Base`:
  *
  *    - return address,
  *    - caller's FrameDescriptor,
  *    - execEnv,
  *    - parameter-passing registers in the calling-convention order,
  *
  *  1. Allocate thread-local FrameDescriptor for the method-being-generated
  * and construct it from the compiler-generated base using dedicated runtime procedure;
  *
  *  1. Invoke implementation of `unmanagedPart`: abstract ''unmanaged'' method
  * (which must be present in host class or its super-classes), providing allocated instance;
  *
  *  1. Invoke implementation of `hookBody` abstract ''managed'' method
  * (which must be present in host class or its super-classes), providing allocated instance;
  *
  *  1. Clear frame and pass the execution to the address returned by `hookBody` invocation.
  *
  * The purpose of such non-trivial artificial construction is to execute arbitrary managed code (`hookBody`)
  * "on the edge" from the call instruction of some ''Java method'' to its body,
  * preserving all parameters passed into it and providing access to them from `hookBody`.
  * Magically-filled fields are needed as supplementary values for the `unmanagedPart` call and/or `hookBody`.
  *
  * @author ijorch
  */
class HookInvokerGenerator(ctx: GeneratorContext) extends GeneratorBase(ctx) {

  /** Declaring class of the method-being-generated. */
  private def host = wrapper.getDeclaringClass

  def genBody(): Unit = {
    val hostInstance = allocateAndFillHostInstance()

    prepareFrame(hostInstance)

    val continuation = invokeHookMethods(hostInstance)

    ctx.finishGenerationExceptFrameDrop()

    locally {
      val emit = frame.emit

      // for continuation holder we need a register which certainly would not be altered during frame drop
      emit.borrowScratch { continuationReg =>
        emit.copyAny(continuationReg, nodes.getLoc(continuation), continuation.asmType)
        nodes.releaseLoc(continuation)
        ctx.frame.genDestroy(false)
        emit.jump(continuationReg)
      }
    }

    ctx.tearDownAndSendCode()
  }

  private def allocateAndFillHostInstance() = {
    val hostInstanceSlot = globalLocations.allocateSlot(host.getRawObjectSize, host.getObjectAlignment, traced = false)

    val returnAddress         = hostInstanceSlot.field(PTR, getOurFieldOffset("returnAddress"))
    val callerFrameDescriptor = hostInstanceSlot.field(PTR, getOurFieldOffset("callerFrameDescriptor"))
    val execEnv               = hostInstanceSlot.field(PTR, getOurFieldOffset("execEnv"))
    val parameters            = hostInstanceSlot.field(PTR, getOurFieldOffset("parameters"))
    val appendixArgument      = hostInstanceSlot.field(PTR, getOurFieldOffset("appendixArgument"))

    receiveParameters(parameters)
    emit.copyAny(returnAddress, frame.returnAddress)
    emit.copyAny(callerFrameDescriptor, frame.callerFrameDescriptor)
    emit.store(execEnv, frame.EER)
    emit.store(appendixArgument, 0)
    gen.andAddr(callerFrameDescriptor, ~RTConst.MarkableFrameDescriptor.PROFILER_MARK_BITS.intValue)

    val hostInstanceNode = Node.newTemporary(NodeType.ADDR)
    emit.lea(nodes.bindToAnyFreeIReg(hostInstanceNode), hostInstanceSlot)
    hostInstanceNode
  }

  private def receiveParameters(parameters: Mem): Unit = {
    assert(wrapper.getParamsCount == 0)

    val locsType = env.getTypeProvider.getParameterPassingLocationsType

    val locsSlot = globalLocations.allocateSlot(locsType.getRawObjectSize, locsType.getObjectAlignment, traced = false)

    val iregs = locsSlot.field(PTR, getFieldOffset(locsType, "iregs"))
    val fregs = locsSlot.field(PTR, getFieldOffset(locsType, "fregs"))
    val stack = locsSlot.field(PTR, getFieldOffset(locsType, "stack"))
    val tail = locsSlot.field(PTR, getFieldOffset(locsType, "tail"))

    // Scala has issues inferring the full types of allArgument*Regs
    fillRegs(PTR, iregs, frame.abi.allArgumentIRegs.asInstanceOf[Array[AnyReg]])
    fillRegs(F64, fregs, frame.abi.allArgumentFRegs.asInstanceOf[Array[AnyReg]])
    putAddressOfMem(stack, frame.callerFrameDescriptor)
    emit.store(tail, tailRegister)

    putAddressOfSlot(parameters, locsSlot)
  }

  private def fillRegs[R <: AnyReg](`type`: AsmType, field: Mem, regs: collection.Seq[R]): Unit = {
    val size = `type`.width.nbytes
    // both FRegs and IRegs arrays must be allocated with alignment of long/double
    // to ensure aligned read (see JET-11923)
    val alignment = 8
    assert(size <= alignment)
    // `traced=false` because GC handles these references precisely using additional maps from caller frame
    val slot = globalLocations.allocateSlot(regs.length * size, alignment, traced = false)

    // TODO: this code pattern can be optimized for arm64 and maybe other platforms. Should we move it to CodeEmitter?
    for ((reg, i) <- regs.zipWithIndex) {
      emit.store(slot.field(`type`, i * size), reg)
    }

    putAddressOfSlot(field, slot)
  }

  private def putAddressOfMem(dst: Mem, mem: Mem): Unit = {
    val memAddr = Node.newTemporary(NodeType.ADDR)
    val memAddrReg = nodes.bindToAnyFreeIReg(memAddr)
    emit.lea(memAddrReg, mem)
    emit.store(dst, memAddrReg)
    nodes.releaseLoc(memAddr)
  }

  private def putAddressOfSlot(dst: Mem, slot: Frame.Slot): Unit = {
    val slotAddr = Node.newTemporary(NodeType.ADDR)
    val slotAddrReg = nodes.bindToAnyFreeIReg(slotAddr)
    emit.lea(slotAddrReg, slot)
    emit.store(dst, slotAddrReg)
    nodes.releaseLoc(slotAddr)
  }

  private def prepareFrame(hostInstance: Node): Unit = {
    // 1. Allocate stack space for the local copy of FrameDescriptor.
    val localFD = globalLocations.allocateSlot(RTConst.HookInvokerFrameDescriptor.size, RTConst.HookInvokerFrameDescriptor.alignment, traced = false)

    // 2. Initialize local FD using data from the common one.
    locally {
      val fdNode = Node.newTemporary(NodeType.ADDR)
      val fdReg = nodes.bindToAnyFreeIReg(fdNode)
      emit.lea(fdReg, wrapper.getFrameDescriptor)

      val localMIFDNode = Node.newTemporary(NodeType.ADDR)
      val localMIFDReg = nodes.bindToAnyFreeIReg(localMIFDNode)
      emit.lea(localMIFDReg, localFD)

      // Current implementation of call generation releases all temporary nodes.
      // But we want `hostInstance` to live longer, so we need to create a copy here.
      val hostInstanceCopy = Node.newTemporary(hostInstance.`type`)
      emit.copyAny(nodes.bindToAnyFreeIReg(hostInstanceCopy), nodes.getLoc(hostInstance), hostInstanceCopy.asmType)
      gen.rtsCall(RTSProc.HookInvokerFrameDescriptor_init)(localMIFDNode, fdNode, hostInstanceCopy)
    }

    // 3. Replace value in our FrameDescriptor slot with local (fixed) one.
    locally {
      val localMIFDNode = Node.newTemporary(NodeType.ADDR)
      val localMIFDReg = nodes.bindToAnyFreeIReg(localMIFDNode)
      emit.lea(localMIFDReg, localFD)

      emit.store(frame.frameDescriptorLoc, localMIFDReg)

      nodes.releaseLoc(localMIFDNode)
    }
  }

  private def invokeHookMethods(hostInstance: Node) = {
    // 1. Invoke `unmanagedPart`.
    locally {
      val unmanagedPart = getOurMethodRef("unmanagedPart")

      // Current implementation of call generation releases all temporary nodes.
      // But we want `hostInstance` to live longer, so we need to create a copy here.
      val hostInstanceCopy = Node.newTemporary(hostInstance.`type`)
      emit.copyAny(nodes.bindToAnyFreeIReg(hostInstanceCopy), nodes.getLoc(hostInstance), hostInstanceCopy.asmType)

      gen.genInvokeNormal(unmanagedPart, Seq(hostInstanceCopy), releaseBCParams = false, null)
    }

    // 2. Invoke `hookBody`.
    locally {
      val result = Node.newTemporary(NodeType.ADDR)
      val hookBody = getOurMethodRef("hookBody")

      gen.genInvokeNormal(hookBody, Seq(hostInstance), releaseBCParams = false, result)

      result
    }
  }


  // utility methods

  private def getOurFieldOffset(name: String) = getFieldOffset(host, name)

  private def getFieldOffset(host: ClassType, name: String) = host.findField(XString.ascii(name)).getInstanceFieldOffset

  private def getOurMethodRef(name: String) =
    host.getMethodRefTo(XString.ascii(name), null, MethodReferenceAccessKind.STATIC)
}

