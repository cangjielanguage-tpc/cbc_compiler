/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64

import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg, Mem, mem}
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{IP0, LR, X0, XZR}
import com.huawei.excelsior.jet.assembler.arm64.*
import com.huawei.excelsior.jet.assembler.arm64.immediates.BitMaskImm
import com.huawei.excelsior.jet.assembler.{AsmError, Label, Location, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter}
import com.huawei.excelsior.jet.codeemitter.arm64.CodeEmitterArm64
import com.huawei.excelsior.jet.compiler.Domain.SCALA
import com.huawei.excelsior.jet.compiler.Env.{frameAlignment, targetPlatform}
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator.DAITarget
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.{ABI, DAIGenerator}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, ConvertOp}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator.XSiteCreator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arch64.GeneratorArch64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arm64.GeneratorArm64.{r, rW, rX}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{GlobalLocations, Locations, Node, Nodes}
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.fromBytecode
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, TypeKind}
import com.huawei.excelsior.jet.compiler.{Env, Environment, RTConst, RTSProc, SymbolLinker}
import xscala.util.MathUtils

import scala.annotation.nowarn
import scala.collection.mutable

@nowarn("msg=match may not be exhaustive")
object GeneratorArm64 {
  def createAssembler() = new Assembler

  def rW(loc: IReg): IRegister.W = rX(loc).asW
  def rX(loc: IReg): IRegister.X = loc.asInstanceOf[IRegister.X]

  private def rS(loc: FReg): VFPRegister.S = rD(loc).asS
  private def rD(loc: FReg): VFPRegister.D = loc.asInstanceOf[VFPRegister.D]

  def r(loc: IReg, tkind: TypeKind): IRegister = tkind.width match {
    case W64 | WPTR => rX(loc)
    case width if width <= W32 => rW(loc)
  }

  def r(loc: FReg, tkind: TypeKind): VFPRegister = tkind.width match {
    case W32 => rS(loc)
    case W64 => rD(loc)
  }
}

@nowarn("msg=match may not be exhaustive")
final class GeneratorArm64(_env: Environment, _symbolLinker: SymbolLinker, _generationContext: GenerationContext,
                           _emit: CodeEmitter, _globalLocations: GlobalLocations, _locations: Locations, _nodes: Nodes,
                           _xSites: XSiteCreator, _enableOptimizedEnrichGeneration: Boolean)
  extends GeneratorArch64(_env, _symbolLinker, _generationContext, _emit, _globalLocations, _locations, _nodes, _xSites, _enableOptimizedEnrichGeneration) {

  private def tmEmit = emit.asInstanceOf[CodeEmitterArm64] // TODO: remove after Generator will be translated
  private val asm = tmEmit.asm

  override protected def genConvertFloat(op: ConvertOp, arg: Node, result: Node): Unit = {
    import com.huawei.excelsior.jet.compiler.bytecode.ConvertOp.*

    val srcType = fromBytecode(op.srcKind)
    val dstType = fromBytecode(op.dstKind)
    op match {
      case F2D | D2F =>
        val src = r(nodes.loadToFRegAndReleaseIfNotUsedLater(arg), srcType)
        val dst = r(nodes.bindToAnyFreeFReg(result), dstType)
        asm.fcvt(dst, src)

      case I2F | I2D | L2D | L2F =>
        val src = r(nodes.loadToIRegAndReleaseIfNotUsedLater(arg), srcType)
        val dst = r(nodes.bindToAnyFreeFReg(result), dstType)
        asm.scvtf(dst, src)

      case F2I | D2I | F2L | D2L =>
        val src = r(nodes.loadToFRegAndReleaseIfNotUsedLater(arg), srcType)
        val dst = r(nodes.bindToAnyFreeIReg(result), dstType)
        asm.fcvtzs(dst, src)
    }
  }

  private def compareSet(dst: IRegister, src1: Register, src2: Register, op: ArithOp): Unit = {
    val lessCC = (src1, src2) match {
      case (src1: IRegister, src2: IRegister) =>
        assert(op == CMP)
        asm.cmp(src1, src2)
        CC.LT

      case (src1: VFPRegister, src2: VFPRegister) =>
        asm.fcmp(src1, src2)
        op match {
          case CMPL => CC.LT
          case CMPG => CC.LO
        }
    }
    asm.cset(dst, CC.NE)
    asm.cneg(dst, dst, lessCC)
  }

  override def genBinaryArithOp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    if (tkind.isFloatingPoint) {
      if (op == REM) {
        val proc = if (tkind == TypeKind.FLOAT) RTSProc.JR_frem else RTSProc.JR_drem
        rtsCall(proc, result, releaseBCParams = true)(arg1, arg2)
      } else {
        val argReg1 = nodes.loadToFReg(arg1)
        val argReg2 = nodes.loadToFReg(arg2)
        nodes.releaseLocIfNotUsedLater(arg1, arg2)

        def resultFReg = nodes.bindToAnyFreeFReg(result)

        op match {
          case CMPG | CMPL =>
            compareSet(rW(nodes.bindToAnyFreeIReg(result)), r(argReg1, tkind), r(argReg2, tkind), op)

          case ADD => emit.fadd(resultFReg, argReg1, argReg2, tkind.width)
          case SUB => emit.fsub(resultFReg, argReg1, argReg2, tkind.width)
          case MUL => emit.fmul(resultFReg, argReg1, argReg2, tkind.width)
          case DIV => emit.fdiv(resultFReg, argReg1, argReg2, tkind.width)
        }
      }
    } else {
      val left = nodes.loadToIReg(arg1)
      val right = nodes.loadToIReg(arg2)
      nodes.releaseLocIfNotUsedLater(arg1, arg2)

      val res = nodes.bindToAnyFreeIReg(result)
      val width = tkind.width

      op match {
        case ADD => emit.add(r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)
        case SUB => emit.sub(r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)
        case AND => emit.and(r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)
        case OR  => emit.or (r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)
        case XOR => emit.xor(r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)
        case MUL => emit.mul(r(res, tkind).asX, r(left, tkind).asX, r(right, tkind).asX, width)

        case DIV =>
          genDivisionByZeroCheck(right, width)
          emit.div(res, left, right, width)

        case REM =>
          genDivisionByZeroCheck(right, width)
          emit.rem(res, left, right, width)

        case LSL => asm.lsl(r(res, tkind), r(left, tkind), r(right, tkind))
        case ASR => asm.asr(r(res, tkind), r(left, tkind), r(right, tkind))
        case LSR => asm.lsr(r(res, tkind), r(left, tkind), r(right, tkind))

        case CMP  => compareSet(r(res, tkind).asW, r(left, tkind), r(right, tkind), CMP)
      }
    }
  }

  override def genNeg(tkind: TypeKind, arg: Node, result: Node): Unit = {
    if (tkind.isFloatingPoint) {
      val argLoc = r(nodes.loadToFRegAndReleaseIfNotUsedLater(arg), tkind)
      val resultLoc = r(nodes.bindToAnyFreeFReg(result), tkind)
      asm.fneg(resultLoc, argLoc)
    } else {
      val argLoc = r(nodes.loadToIRegAndReleaseIfNotUsedLater(arg), tkind)
      val resultLoc = r(nodes.bindToAnyFreeIReg(result), tkind)
      asm.neg(resultLoc, argLoc)
    }
  }

  private def genInvokeTargetViaDAI(target: DAITarget): Unit = {
    // Note: this instruction sequence is decoded at runtime (see com.huawei.excelsior.jet.runtime.classload.resolve.deferred.dai.DAILocator),
    //       so any changes to this code MUST be also reflected there.
    asm.ldr(IP0, asm.literal(target.symbol))
    asm.ldr(IP0, Arg.M(IP0))
    asm.blr(IP0)
  }

  override protected def genCall(targetRef: MethodReference, target: AnyRef, ctmwFrameDescriptor: Slot,
                                 appendix: Node, params: collection.Seq[Node], result: Node, callToManaged: Boolean,
                                 callToNative: Boolean, forceAddXSite: Boolean, releaseBCParams: Boolean): Unit = {
    val abi = targetPlatform.abi(targetRef)
    var fakeAreaSize = 0

    val retAddrRef: Label = if (callToNative) {
      val raf = new Label
      fakeAreaSize += pushFakeRetAddr(raf)
      raf
    } else {
      null
    }

    if (abi.spoilsCallerFrameDescriptor(context.rootMethodType)) {
      fakeAreaSize += frame.nb_allocateFakeParamsArea(abi)
    } else {
      frame.nb_reserveSpaceForCall(abi)
    }

    val availableStackSizeBelowCurrentFrame = frame.registerStackCheckForCall(targetRef)
    assert(availableStackSizeBelowCurrentFrame >= fakeAreaSize)

    prepareRegs(targetRef)

    if (targetRef.hasMethod && targetRef.method.isNoTracedRegsOnEntry) {
      // Rescue and push all registers on stack, including TR register, to ensure that ThreadLocalRootsProvider correctly unwinds the stack.
      // It is possible to save less registers, but such an optimization does not seem to worth it.
      nodes.rescueAndSpoilRegs(_ => true)
    } else {
      nodes.rescueAndSpoilRegs(r => abi.isTouched(r))
    }

    val paramLocations = passMethodParams(abi, params, releaseBCParams)

    if (callToManaged) {
      putFrameDescriptorForCallToManaged(ctmwFrameDescriptor)
    }

    checkNoRefsOnRegs(targetRef)

    assert(target.isInstanceOf[Node] || appendix == null) // just not needed and thus not implemented

    withTRSetupForCall(abi) {
      val isDeferred = target.isInstanceOf[DAITarget]
      if (needPreCallXSite(targetRef, isDeferred)) {
        addPreCallXSite(paramLocations)
      }

      target match {
        case node: Node     => genInvokeTargetIndirect(node, appendix)
        case dai: DAITarget => genInvokeTargetViaDAI(dai)
        case s: Symbol      => emit.call(s)
      }

      if (retAddrRef != null) {
        emit.bind(retAddrRef)
      }

      val targetMethodType = targetRef.methodType
      if (targetMethodType.callConv.hasManagedExecEnv || forceAddXSite) {
        addCallXSite(targetRef, isDeferred, paramLocations)
      }
    }

    dropFakeArea(fakeAreaSize)
    bindInvokeResult(abi, result)
  }

  private def pushFakeRetAddr(retAddrRef: Label) = {
    emit.borrowScratch { scratch =>
      val retAddrValue = rX(scratch)

      emit.lea(retAddrValue, retAddrRef)

      // SOE-Note: this push cannot provoke SOE because it is present only in the body of Native Wrapper
      //           which contains stack check reserving space to call an unmanaged (native) method.
      tmEmit.pushPair(retAddrValue, X0) // X0 added for alignment
    }

    val fakeSize = 2 * Env.stackSlotSize
    assert(MathUtils.isAligned(fakeSize, frameAlignment))
    fakeSize
  }

  private def bindInvokeResult(abi: ABI[_, _], result: Node): Unit = {
    if (result != null) {
      val resultKind = abi.returnType.jbcKind
      assert(checkNodeType(result, resultKind))
      nodes.bind(result, abi.resultLocation)
    }
  }

  override def genTrapCheckInstruction(trapLoc: IReg, scratch: IReg, offset: Int, isGCPoint: Boolean): Unit = {
    // triggered gc-point can lead to execution of managed handler that will use LR to return to the current execution
    if (isGCPoint) nodes.rescueAndSpoilIRegs(LR)
    emit.load(scratch, mem(PTR, rX(trapLoc), offset))
  }

  override protected def signExtendShortIntegralToInt(width: Width, signed: Boolean, dst: IReg, src: IReg): Unit = {
    val srcReg = rW(src)
    val dstReg = rW(dst)
    width match {
      case W8  => if (signed) asm.sxtb(dstReg, srcReg) else asm.uxtb(dstReg, srcReg)
      case W16 => if (signed) asm.sxth(dstReg, srcReg) else asm.uxth(dstReg, srcReg)
    }
  }

  override def signExtendIntToLong(dst: IReg, src: IReg): Unit = asm.sxtw(rX(dst), rW(src))

  override def andAddr(mem: Mem, value: Int): Unit = {
    assert(mem.`type` == PTR)

    if (value == 0) {
      emit.store(mem, XZR)
    } else {
      emit.borrowScratch { tmp =>
        emit.load(tmp, mem)
        andAddr(tmp, value)
        emit.store(mem, tmp)
      }
    }
  }

  override def andAddr(iReg: IReg, value: Int): Unit = {
    // TODO: feel free to use extra scratch, if exists one, or implement and by patterns
    AsmError.require(BitMaskImm.canEncode(value, WPTR), s"unsupported value $value for AND")
    val reg = rX(iReg)
    asm.and(reg, reg, value)
  }

  def depriveEOP(loc: Location): Unit = {
    val enrichmentMask = RTConst.Eop.ENRICHMENT_MASK.longValue
    if (loc.isIReg) {
      val reg = rX(loc.asIReg)
      asm.and(reg, reg, ~enrichmentMask)
    } else {
      val mem = loc.asMem
      emit.borrowScratch { tmp =>
        emit.load(tmp, mem)
        depriveEOP(tmp)
        emit.store(mem, tmp)
      }
    }
  }
}
