/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.stubs

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.{IReg, Mem, MemBased, mem}
import com.huawei.excelsior.jet.assembler.{Location, Segment, Symbol}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.{LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.Env.{addressSize, tailRegister, targetPlatform}
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.abi.{DAIGenerator, Frame, FrameProperties}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.*
import com.huawei.excelsior.jet.compiler.symlevel.{Field, MethodType, SignatureType, Type, TypeKind}
import com.huawei.excelsior.jet.compiler.{Environment, RTConst, RTSProc, TypeProvider}

/** This class contains methods to generate various stubs and thunks.
  *
  * We define `thunk` as a piece of code that ends with tail-jump
  * and doesn't construct its own managed stack-frame for execution of its body.
  *
  * On the other hand, `stub` is a piece of code that returns to its original caller.
  * Therefore, if it contains calls to managed code in its body, it must construct proper frame.
  *
  * All generated stubs and thunks are documented in their own javadocs below.
  *
  * @author afilatov
  * @author ijorch
  */
object ThunkGenerator {
  def framePropertiesFor(thunkMethodType: MethodType, frameDescriptor: Symbol) = new FrameProperties() {
    override def hasFrameDescriptor = frameDescriptor != null
    override def getFrameDescriptor = frameDescriptor ensuring (_ != null)

    override def isStackCheckDisabled = true
    override def shouldStackCheckByCaller = shouldNotReachHere()
    override def getStackCheckByCallerBytes = shouldNotReachHere()

    override def shouldContainGCPoints = false
    override def shouldContainGCPointInEpilogue = false
    override def shouldContainGCPointInEpilogueBeforeFrameDrop = false
    override def shouldContainGCPointInEpilogueAfterFrameDrop = false

    override def getFullName = s"thunk: ${thunkMethodType.toMethodDescriptor.toJETSignature}"
    override def getRealMethodType(varArgs: Iterable[SignatureType]) = {
      assert(varArgs == null)
      assert(!isVarArgs)
      thunkMethodType
    }
    override def isVarArgs = thunkMethodType.isVarArgs
    override def isHookInvoker = false
    override def isManagedFrame = true
    override def isManaged = true
  }
}

abstract class ThunkGenerator protected(protected val env: Environment, globalLocations: GlobalLocations) {
  protected implicit val typeProvider: TypeProvider = env.getTypeProvider
  protected val frame = globalLocations.frame
  private lazy val generator: Generator = {
    val locations = new Locations(globalLocations, emit)
    val nodes = new Nodes(locations, emit, frame)
    locations.nodes = nodes
    createGeneratorForThunk(locations, nodes)
  }
  private var preheader: Segment = _
  private var body: Segment = _

  protected def emit: CodeEmitter

  /** Generates thunk containing nullcheck that must be done before jumping to `target`. */
  final def genNonVirtualForwarder(target: Symbol, receiverNullCheck: Boolean): Segment = {
    skipPreheaderAndStartBody()

    if (receiverNullCheck) {
      genNullCheck(getThisParam)
    }

    emit.jump(target)

    finishGeneration(true)
  }

  protected def saveParamPassingRegs(param1: IReg, param2: IReg): Unit

  protected def restoreParamPassingRegs(param1: IReg, param2: IReg): Unit

  /** Calls `@VMCall JR_FindIMTForThunk` manually, preserving all registers and stack locations.
    * It would be more convenient to use [[Generator.genInterfaceCallWithResult]]
    * but it inevitably touches stack locations and contradicts with
    * [[com.huawei.excelsior.jet.compiler.abi.Frame.Mode.SPECIAL_FOR_THUNK]] requirements for thunks.
    *
    * After the call, result of IMT search is used to call the method or throw ICCE.
    */
  private def genInterfaceJump(refClass: Type, thisParam: Location, vnum: Int): Unit = {
    val findIMT = env.getRTSProc(RTSProc.JR_FindIMTForThunk)
    val abi = targetPlatform.abi(findIMT.getMethodType)

    val param1 = abi.paramLocations(0).asIReg
    val param2 = abi.paramLocations(1).asIReg
    val result = abi.resultLocation.asIReg

    // Save param-passing register and align stack for findIMT call
    // SOE-Note: this push cannot provoke SOE because it can only appear in thunk generated for DAI call
    //           and every method with a DAI call generates explicit stack check to reserve some stack below its frame.
    assert(result == param1)
    assert(result != param2)
    saveParamPassingRegs(param1, param2)

    // Parameter placement
    assert(thisParam == param1)
    emit.lea(param2, refClass.getTypeHandle)

    emit.call(findIMT)

    emit.borrowScratch { tmp =>
      assert(tmp != param1)
      assert(tmp != param2)

      // Save result and remove alignment
      emit.mov(tmp, result)

      restoreParamPassingRegs(param1, param2)

      // Check IMT search result
      val icce = emit.newLabel
      emit.branchIfNull(tmp, icce)

      // IMT search returned non-null, meaning that the interface cast succeeded
      emit.jumpIndirect(mem(PTR, tmp, addressSize * vnum))

      emit.bind(icce) // IMT search returned null, throw ICCE
      emit.lea(tmp, env.getRTSProc(RTSProc.JR_ThrowIncompatibleClassChangeError0))
      emit.jump(tmp)
    }
  }

  protected def createGeneratorForThunk(locations: Locations, nodes: Nodes): Generator

  /** Generates thunk containing jump by virtual/interface table. Also, it might contain nullcheck, if needed. */
  final def genVirtualForwarder(refClass: Type, vnum: Int, isInvokeInterface: Boolean, receiverNullCheck: Boolean): Segment = {
    skipPreheaderAndStartBody()

    val thisParam = getThisParam
    if (isInvokeInterface) {
      if (receiverNullCheck) {
        genNullCheck(thisParam)
      }
      genInterfaceJump(refClass, thisParam, vnum)
    } else {
      emit.borrowScratch { tmp =>
        emit.copyAny(tmp, thisParam)
        generator.readObjectTD(tmp, tmp, mayBeNull = true) // implicit null-check inside

        assert(refClass.isJavaReference)
        emit.jumpIndirect(mem(PTR, tmp, RTConst.JavaInstanceDescriptor.VMT_OFFSET.intValue + addressSize * vnum))
      }
    }

    finishGeneration(true)
  }

  /** Generates unmanaged stub that performs some field operation.
    *
    * It might contain unmanaged call to runtime procedure if it performs writing of interface field.
    * The only exception that can be thrown from this stub is NPE and to throw it without creating proper managed frame
    * we perform nullcheck before frame construction (as in thunks).
    */
  final def genFieldOperation(field: Field, isWrite: Boolean, receiverNullCheck: Boolean): Segment = {
    val methodType = frame.abi.methodType

    assert(!field.getDeclaringClass.isDeferred)
    assert(methodType.toMethodDescriptor == DAIGenerator.methodTypeForDeferredFieldAccess(env.getTypeProvider, field, isWrite).toMethodDescriptor)

    val isStatic = field.isStatic
    val isVolatile = field.isVolatile
    val fieldType = field.getType

    startPreheader()
    // nullcheck (as well as implicit NPE handling in runtime) expect empty frame, but for fields we generate full frame
    // therefore preheader code must be generated BEFORE any other instructions (e.g. register saving in Frame.build or receiving of parameters)

    if (receiverNullCheck) {
      assert(!isStatic)
      genNullCheck(fieldOpReceiverParam(isWrite))
    }

    finishPreheaderAndStartBody()

    val parameters = receiveParameters()
    val obj = DAIGenerator.FieldAccessParametersOrdering.getObject(parameters, isWrite, isStatic)
    val value = DAIGenerator.FieldAccessParametersOrdering.getFieldValue(parameters, isWrite)

    if (isWrite) {
      if (isVolatile) {
        emit.memBarrier(STORE_STORE, LOAD_STORE)
      }

      if (isStatic) {
        generator.writeStaticField(fieldType, isVolatile, field.getStaticFieldSymbol, value)
      } else {
        assert(field.getDeclaringClass.isJavaReference)
        generator.writeInstanceField(fieldType, isVolatile, obj, field.getInstanceFieldOffset, value)
      }

      if (isVolatile) {
        emit.memBarrier(STORE_STORE, STORE_LOAD)
      }
    } else {
      val result = Node.newTemporary(NodeType.by(fieldType.jbcKind))

      if (isStatic) {
        generator.readStaticField(fieldType, isVolatile, field.getStaticFieldSymbol, result)
      } else {
        generator.readInstanceField(fieldType, isVolatile, obj, field.getInstanceFieldOffset, result)
      }

      if (isVolatile) {
        emit.memBarrier(LOAD_STORE, LOAD_LOAD)
      }

      generator.genReturnValue(result)
    }

    finishGeneration(false)
  }

  /** Generates thunk which places object from `appendix` field of given `dai`
    * into proper parameter location and then jumps to `target`.
    *
    * It is needed to invoke entry point of jsr292-call avoiding the race condition.
    */
  final def genJSR292AppendixPlacer(dai: Symbol, target: Symbol): Segment = {
    skipPreheaderAndStartBody()

    val abi = frame.abi

    val appendixMem = mem(TypeKind.CLASS.toAsm, dai, RTConst.JavaDAI.JSR292Appendix.appendix.offset)
    val lastParamLoc = abi.paramLocations(abi.parameterCount - 1)

    if (lastParamLoc.isIReg) {
      assert(abi.isVolatile(lastParamLoc.asIReg), "we don't want it to be saved/restored")
      emit.load(lastParamLoc.asIReg, appendixMem)

    } else {
      val lastParamSlot = lastParamLoc.asInstanceOf[TailSlot] // appendix is an object so it cannot be on FReg

      emit.borrowScratch { appendixReg =>
        // Forall with comparison used to avoid type incompatibility errors.
        assert(abi.allArgumentIRegs forall (_ != appendixReg), "we must not alter any arguments unintentionally")

        emit.load(appendixReg, appendixMem)

        // note that emitter does not have scratch reg, but it won't be a problem
        // until lastParamLoc is mem with too big offset, which should not happen for on-stack parameter.
        emit.store(mem(lastParamSlot.tpe, tailRegister, lastParamSlot.offset), appendixReg)
      }
    }

    emit.jump(target)

    finishGeneration(true)
  }

  private def startPreheader(): Unit = {
    assert(preheader == null)
    emit.setUp()
  }

  private def finishPreheaderAndStartBody(): Unit = {
    assert(preheader == null)
    preheader = emit.tearDown()
    assert(body == null)
    emit.setUp()
  }

  private def skipPreheaderAndStartBody(): Unit = {
    assert(preheader == null)
    assert(body == null)
    emit.setUp()
  }

  private def finishGeneration(tailJump: Boolean): Segment = {
    assert(body == null)
    body = emit.tearDown()
    emit.setUp()

    if (preheader != null) {
      emit.appendCode(preheader)
    }

    if (tailJump) {
      frame.makeLayout(Frame.Mode.SPECIAL_FOR_THUNK)
    } else {
      frame.makeLayout(Frame.Mode.FULL)
    }
    frame.genBuildAndAdjustParams(false)

    emit.appendCode(body)

    if (!tailJump) {
      frame.genDestroy(true)
    }

    val alignment = RTConst.MethodInfoFrameDescriptor.CODE_ALIGNMENT.intValue
    emit.alignCode(alignment)
    emit.alignStart(alignment)

    val result = emit.freeze().tearDown()

    if (tailJump) {
      // after tearDown no code can be generated (NPE will be thrown).
      // layoutMode == EMPTY_FRAME guarantees that genFrameDropWithoutReturn does nothing
      frame.genDestroy(false)
    }

    result
  }

  private def getThisParam: Location = {
    rawParamLocation(frame.abi.methodType.getReceiverArgIdx)
  }

  private def fieldOpReceiverParam(isWrite: Boolean): Location = {
    val receiverLocationIdx = DAIGenerator.FieldAccessParametersOrdering.getObject[Integer](Seq(0, 1), isWrite, isStatic = false)
    rawParamLocation(receiverLocationIdx)
  }

  private def rawParamLocation(parameterIdx: Int): Location = {
    // Other locations could not occur now. Feel free to implement it in the future if you need to.
    frame.abi.paramLocations(parameterIdx).asIReg
  }

  private def receiveParameters(): collection.Seq[Node] = generator.receiveAllParameters()

  private def genNullCheck(receiver: Location): Unit = {
    emit.borrowScratch { scratchReg =>
      assert(receiver.isReg && (receiver != scratchReg))
      generator.genTrapCheckInstruction(receiver.asIReg, scratchReg, 0, false)
    }
  }
}
