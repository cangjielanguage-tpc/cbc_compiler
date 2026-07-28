/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64

import com.huawei.excelsior.jet.assembler.AsmType.NONE
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg, Mem, mem}
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.M
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.*
import com.huawei.excelsior.jet.assembler.{Label, Location, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter}
import com.huawei.excelsior.jet.compiler.Env.{targetOS, targetPlatform}
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator.DAITarget
import com.huawei.excelsior.jet.compiler.abi.amd64.ABIAmd64
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.bytecode.ConvertOp.*
import com.huawei.excelsior.jet.compiler.bytecode.{ArithOp, ConvertOp}
import com.huawei.excelsior.jet.compiler.ir.XSiteKind
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.GenerationContext
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.Generator.XSiteCreator
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.amd64.GeneratorAmd64.{r, r32, r8}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.arch64.GeneratorArch64
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.{GlobalLocations, Locations, Node, NodeType, Nodes}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, TypeKind}
import com.huawei.excelsior.jet.compiler.{Environment, RTConst, RTSGlobal, RTSProc, SymbolLinker}
import com.huawei.excelsior.jet.assembler.Width.{W32, W64}

import scala.annotation.nowarn
import scala.collection.mutable

object GeneratorAmd64 {
  def createAssembler() = new Assembler()

  def r(loc: IReg): GPR = loc.asInstanceOf[GPR]
  def r(loc: FReg): XMM = loc.asInstanceOf[XMM]

  def r8 (loc: IReg): Register8  = r(loc).asReg8
  def r16(loc: IReg): Register16 = r(loc).asReg16
  def r32(loc: IReg): Register32 = r(loc).asReg32

  @nowarn("msg=match may not be exhaustive")
  def r(loc: IReg, width: Width): Register = {
    import com.huawei.excelsior.jet.assembler.Width.*
    width match {
      case W8  => r8(loc)
      case W16 => r16(loc)
      case W32 => r32(loc)
      case W64 | WPTR => r(loc)
    }
  }

  def r(loc: IReg, tkind: TypeKind): Register = r(loc, tkind.width)
}

@nowarn("msg=match may not be exhaustive")
final class GeneratorAmd64(_env: Environment, _symbolLinker: SymbolLinker, _generationContext: GenerationContext,
                           _emit: CodeEmitter, _globalLocations: GlobalLocations, _locations: Locations, _nodes: Nodes,
                           _xSites: XSiteCreator, _enableOptimizedEnrichGeneration: Boolean)
  extends GeneratorArch64(_env, _symbolLinker, _generationContext, _emit, _globalLocations, _locations, _nodes, _xSites, _enableOptimizedEnrichGeneration) {

  private def tmEmit = emit.asInstanceOf[CodeEmitterAmd64] // TODO: remove after Generator will be translated
  private val asm = emit.asInstanceOf[CodeEmitterAmd64].asm

  override protected def branchIf(reg: IReg, op: BranchOp, mem: Mem, target: Label): Unit = tmEmit.branchIf(reg, op, mem, target)

  private def genIntegralShifts(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    val shiftReg = RCX // fixed register for shift operations
    nodes.rescueAndSpoilIRegs(shiftReg)
    nodes.transfer(arg2, shiftReg)

    val arg1Loc = nodes.loadToIReg(arg1)
    val resultLoc = nodes.bindToAnyFreeIReg(result)
    nodes.releaseLocIfNotUsedLater(arg1, arg2)

    emit.mov(resultLoc, arg1Loc)

    val resReg = r(resultLoc, tkind)
    val shiftReg8 = r8(shiftReg)

    op match {
      case LSL => asm.shl(resReg, shiftReg8)
      case ASR => asm.sar(resReg, shiftReg8)
      case LSR => asm.shr(resReg, shiftReg8)
    }
  }

  private def genIntegralSub(tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    val arg1Loc = nodes.loadToIReg(arg1)
    val arg2Loc = nodes.loadToIReg(arg2)
    val resultLoc = nodes.bindToAnyFreeIReg(result)
    emit.sub(resultLoc, arg1Loc, arg2Loc, tkind.width)
    nodes.releaseLocIfNotUsedLater(arg1, arg2)
  }

  private def genIntegralCommOp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    val arg1Loc = nodes.loadToIReg(arg1)
    val arg2Loc = nodes.loadToIReg(arg2)
    nodes.releaseLocIfNotUsedLater(arg1, arg2)

    // This is a code optimization, may be removed.
    val resultLoc = nodes.bindToAnyFreeIRegWithPreferred(result, arg1Loc, arg2Loc)

    op match {
      case ADD => emit.add(resultLoc, arg1Loc, arg2Loc, tkind.width)
      case AND => emit.and(resultLoc, arg1Loc, arg2Loc, tkind.width)
      case OR  => emit.or (resultLoc, arg1Loc, arg2Loc, tkind.width)
      case XOR => emit.xor(resultLoc, arg1Loc, arg2Loc, tkind.width)
      case MUL => emit.mul(resultLoc, arg1Loc, arg2Loc, tkind.width)
    }
  }

  override def genTrapCheckInstruction(trapLoc: IReg, scratch: IReg, offset: Int, isGCPoint: Boolean): Unit = {
    val trapAM = M(r(trapLoc), offset)
    if (scratch != null) {
      asm.mov(r32(scratch), trapAM)
    } else {
      asm.cmp(r32(trapLoc), trapAM)
    }
  }

  private def genSimpleBinaryFloatArithOp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    val resultReg = nodes.bindToAnyFreeFReg(result)
    val src1Reg = nodes.loadToFReg(arg1)
    val src2Reg = nodes.loadToFReg(arg2)
    nodes.releaseLocIfNotUsedLater(arg1, arg2)

    op match {
      case ADD => emit.fadd(resultReg, src1Reg, src2Reg, tkind.width)
      case SUB => emit.fsub(resultReg, src1Reg, src2Reg, tkind.width)
      case MUL => emit.fmul(resultReg, src1Reg, src2Reg, tkind.width)
      case DIV => emit.fdiv(resultReg, src1Reg, src2Reg, tkind.width)
    }
  }

  private def genFloatCmp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    val arg1FReg = r(nodes.loadToFReg(arg1))
    val arg2FReg = r(nodes.loadToFReg(arg2))
    nodes.releaseLocIfNotUsedLater(arg1, arg2)
    if (tkind == TypeKind.DOUBLE) {
      asm.sse.ucomisd(arg1FReg, arg2FReg)
    } else {
      asm.sse.ucomiss(arg1FReg, arg2FReg)
    }
    asm.pushf()

    val tmpNode1 = Node.newTemporary(NodeType.INT)
    val tmpNode2 = Node.newTemporary(NodeType.INT)
    val tmpReg1 = r(nodes.bindToAnyFreeIReg(tmpNode1))
    val tmpReg2 = r(nodes.bindToAnyFreeIReg(tmpNode2))

    val resultReg = r(nodes.bindToAnyFreeIReg(result))

    val flagsReg = tmpReg1
    val ltReg = tmpReg2
    val gtReg = resultReg
    asm.pop(flagsReg)

    // Receive flags after floats comparison and return +1/0/-1 depending on comparison results.
    //        cf zf pf
    // a > b  0  0  0
    // a = b  0  1  0
    // a < b  1  0  0
    //  NaN   1  1  1
    val cfMask = 1 << 0
    val pfMask = 1 << 2
    val zfMask = 1 << 6

    asm.xor(gtReg, gtReg)
    asm.xor(ltReg, ltReg)

    op match {
      case CMPL =>
        // NaN is treated as -1

        //          / +1, if a > b
        // gtReg = |
        //          \ 0,  else
        asm.test(r8(flagsReg), cfMask | zfMask)
        asm.set(CC.Z, r8(gtReg))

        //          / +1, if a < b or NaN
        // ltReg = |
        //          \ 0, else
        asm.test(r8(flagsReg), cfMask)
        asm.set(CC.NZ, r8(ltReg))

      case CMPG =>
        // NaN is treated as +1

        //          / +1, if a > b or NaN
        // gtReg = |
        //          \ 0,  else
        asm.test(r8(flagsReg), cfMask | zfMask)
        asm.set(CC.P, r8(gtReg))

        //          / +1, if a < b
        // ltReg = |
        //          \ 0,  else
        asm.test(r8(flagsReg), cfMask | pfMask)
        asm.set(CC.NP, r8(ltReg))
    }

    asm.sub(r32(gtReg), r32(ltReg)) // gtReg contains result after all

    nodes.releaseLocIfNotUsedLater(tmpNode1)
    nodes.releaseLocIfNotUsedLater(tmpNode2)
  }

  private def genConvertFloatToFloat(op: ConvertOp, arg: Node, result: Node): Unit = {
    val argLoc = nodes.loadToFRegAndReleaseIfNotUsedLater(arg)
    val resultLoc = nodes.bindToAnyFreeFReg(result)

    val argReg = r(argLoc)
    val resultReg = r(resultLoc)

    op match {
      case D2F => asm.sse.cvtsd2ss(resultReg, argReg)
      case F2D => asm.sse.cvtss2sd(resultReg, argReg)
    }
  }

  override def genBinaryArithOp(op: ArithOp, tkind: TypeKind, arg1: Node, arg2: Node, result: Node): Unit = {
    if (tkind.isFloatingPoint) {
      op match {
        case ADD | SUB | MUL | DIV =>
          genSimpleBinaryFloatArithOp(op, tkind, arg1, arg2, result) // all simple arithmetic is strict

        case CMPL | CMPG =>
          genFloatCmp(op, tkind, arg1, arg2, result)

        case REM =>
          val proc = if (tkind == TypeKind.FLOAT) RTSProc.JR_frem else RTSProc.JR_drem
          rtsCall(proc, result, releaseBCParams = true)(arg1, arg2)
      }
    } else {
      op match {
        case ADD | AND | OR | XOR | MUL =>
          genIntegralCommOp(op, tkind, arg1, arg2, result)

        case LSL | ASR | LSR =>
          genIntegralShifts(op, tkind, arg1, arg2, result)

        case DIV =>
          genCommonIntegralDivRem(isDiv = true, isLong = tkind == TypeKind.LONG, arg1, arg2, result)

        case REM =>
          genCommonIntegralDivRem(isDiv = false, isLong = tkind == TypeKind.LONG, arg1, arg2, result)

        case SUB =>
          genIntegralSub(tkind, arg1, arg2, result)

        case CMP =>
          assert(tkind == TypeKind.LONG)
          genLongCmp(arg1, arg2, result)
      }
    }
  }

  private def genCommonIntegralDivRem(isDiv: Boolean, isLong: Boolean, arg1: Node, arg2: Node, result: Node): Unit = {
    nodes.rescueAndAcquireIRegs(R9)

    // We need any different from RAX and RDX registers as scratch for CodeEmitter template, and RAX is default one
    emit.withoutScratch(RAX) { emit.withScratch(R9) {
      val left = nodes.loadToIReg(arg1)
      val right = nodes.loadToIReg(arg2)
      nodes.releaseLocIfNotUsedLater(arg1)
      val dst = nodes.bindToAnyFreeIReg(result) // don't mix-up order, we need to free `right` only after acquiring `dst` so they aren't equal.
      if (arg1 != arg2) nodes.releaseLocIfNotUsedLater(arg2)

      val width = if (isLong) W64 else W32
      genDivisionByZeroCheck(right, width)

      if (isDiv) {
        emit.div(dst, left, right, width)
      } else {
        emit.rem(dst, left, right, width)
      }
    }}

    locations.release(R9)
  }

  private def genLongCmp(arg1: Node, arg2: Node, result: Node): Unit = {
    asm.cmp(r(nodes.loadToIReg(arg1)), r(nodes.loadToIReg(arg2)))
    nodes.releaseLocIfNotUsedLater(arg1, arg2)

    val resLoc = nodes.bindToAnyFreeIReg(result)
    asm.mov(r32(resLoc), -1)

    val done = asm.newLabel
    asm.jcc(CC.L, done)
    asm.set(CC.NE, r8(resLoc))
    asm.movzx(r32(resLoc), r8(resLoc))

    asm.bind(done)
  }

  override def genNeg(tkind: TypeKind, arg: Node, result: Node): Unit = {
    if (tkind.isFloatingPoint) {
      val argLoc = nodes.loadToFRegAndReleaseIfNotUsedLater(arg)
      val resultLoc = nodes.bindToAnyFreeFReg(result)

      emit.fmov(resultLoc, argLoc, tkind.width)

      if (tkind == TypeKind.DOUBLE) {
        val symbol = mem(NONE, symbolLinker.getRTSGlobalSymbol(RTSGlobal.JR_DOUBLE_SIGN_FLIP))
        tmEmit.withAddrMode(symbol) { am => asm.sse.xorpd(r(resultLoc), am) }
      } else {
        val symbol = mem(NONE, symbolLinker.getRTSGlobalSymbol(RTSGlobal.JR_FLOAT_SIGN_FLIP))
        tmEmit.withAddrMode(symbol) { am => asm.sse.xorps(r(resultLoc), am) }
      }
    } else {
      val argLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(arg)
      val resultLoc = nodes.bindToAnyFreeIRegWithPreferred(result, argLoc)

      emit.mov(resultLoc, argLoc)
      asm.neg(r(resultLoc, tkind))
    }
  }

  override protected def genCall(targetRef: MethodReference, target: AnyRef, ctmwFrameDescriptor: Frame.Slot,
                                 appendix: Node, params: collection.Seq[Node], result: Node, callToManaged: Boolean,
                                 callToNative: Boolean, forceAddXSite: Boolean, releaseBCParams: Boolean): Unit = {
    val abi = targetPlatform.abi(targetRef)
    val fakeAreaSize = if (abi.spoilsCallerFrameDescriptor(context.rootMethodType)) {
      frame.nb_allocateFakeParamsArea(abi)
    } else {
      frame.nb_reserveSpaceForCall(abi)
      0
    }

    frame.registerStackCheckForCall(targetRef)

    prepareRegs(targetRef)

    if (targetRef.hasMethod && targetRef.method.isNoTracedRegsOnEntry) {
      // Rescue and push all registers on stack, including TR register, to ensure that ThreadLocalRootsProvider correctly unwinds the stack.
      // It is possible to save less registers, but such an optimization does not seem to worth it.
      nodes.rescueAndSpoilRegs(_ => true)
    } else {
      nodes.rescueAndSpoilRegs(abi.isTouched)
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
        case symbol: Symbol => emit.call(symbol)
      }

      val targetMethodType = targetRef.methodType
      if (targetMethodType.callConv.hasManagedExecEnv || forceAddXSite) {
        addCallXSite(targetRef, isDeferred, paramLocations)
      }
    }

    dropFakeArea(fakeAreaSize)
    bindInvokeResult(abi, result)
  }

  private def bindInvokeResult(abi: ABI[_, _], result: Node): Unit = {
    if (result != null) {
      assert(checkNodeType(result, abi.returnType.jbcKind))
      val src = abi.resultLocation.asReg

      if (locations.isAllocatable(src)) {
        nodes.bind(result, src)
      } else {
        // ThunkGenerator defines special scratch register (e.g. RAX on amd64) but use standard Generator
        // primitives, like genCall, which tries to allocates call result on register taken from ABI (also RAX).
        // TODO: refactor ThunkGenerator register allocation policy
        emit.copyAny(nodes.bindToAnyFreeLoc(result), src)
      }
    }
  }

  private def genInvokeTargetViaDAI(target: DAITarget): Unit = {
    // Note: this instruction sequence is decoded at runtime (see com.huawei.excelsior.jet.runtime.classload.resolve.deferred.dai.DAILocator),
    //       so any changes to this code MUST be also reflected there.
    val symbol = target.symbol
    assert(symbolLinker.isDirectAccess(symbol))
    asm.call(M(symbol))
  }

  override protected def placeParam(abi: ABI[_, _], dst: Location, param: Node, paramIdx: Int): Seq[(Node, Location)] = {
    val secondaryLoc = abi.asInstanceOf[ABIAmd64].parameterSecondaryLocation(paramIdx)
    var result = Seq.empty[(Node, Location)]
    if (secondaryLoc != null) {
      // copy FP vararg passed on XMM register to GPR
      assert(param.`type` != NodeType.TREF)
      emit.copyAny(secondaryLoc, nodes.getLoc(param), param.asmType)
      result :+= (param, secondaryLoc)
    }
    super.placeParam(abi, dst, param, paramIdx) ++ result
  }

  override protected def passMethodParams(abi: ABI[_, _], targetParams: collection.Seq[Node], releaseBCParams: Boolean): Seq[(Node, Location)] = {
    val result = super.passMethodParams(abi, targetParams, releaseBCParams)
    if (abi.isVarArgs && targetOS.isLinux) {
      asm.mov(ABIAmd64.UNIX_VARARG_XMMS_COUNT_REG, abi.usedArgumentFRegs.size)
    }
    result
  }

  override protected def genConvertFloat(op: ConvertOp, arg: Node, result: Node): Unit = {
    val fromFloat = op.srcKind.isFloatingPoint
    val toFloat = op.dstKind.isFloatingPoint

    (fromFloat, toFloat) match {
      case (true, true) => genConvertFloatToFloat(op, arg, result)
      case (true, _)    => genConvertFloatToIntegral(op, arg, result)
      case (_, true)    => genConvertIntegralToFloat(op, arg, result)
    }
  }

  private def genConvertIntegralToFloat(op: ConvertOp, arg: Node, result: Node): Unit = {
    val argLoc = nodes.loadToIRegAndReleaseIfNotUsedLater(arg)
    val resultLoc = nodes.bindToAnyFreeFReg(result)

    op match {
      case I2F => asm.sse.cvtsi2ss(r(resultLoc), r32(argLoc))
      case I2D => asm.sse.cvtsi2sd(r(resultLoc), r32(argLoc))
      case L2F => asm.sse.cvtsi2ss(r(resultLoc), r(argLoc))
      case L2D => asm.sse.cvtsi2sd(r(resultLoc), r(argLoc))
    }
  }

  private def genConvertFloatToIntegral(op: ConvertOp, arg: Node, result: Node): Unit = {
    val argLoc = nodes.loadToFReg(arg)
    val resultLoc = nodes.bindToAnyFreeIReg(result)

    op match {
      case F2I => asm.sse.cvttss2si(r32(resultLoc), r(argLoc))
      case D2I => asm.sse.cvttsd2si(r32(resultLoc), r(argLoc))
      case F2L => asm.sse.cvttss2si(r(resultLoc), r(argLoc))
      case D2L => asm.sse.cvttsd2si(r(resultLoc), r(argLoc))
    }

    genConvertAdjustment(op, arg, result)
    nodes.releaseLocIfNotUsedLater(arg)
  }

  private def genConvertAdjustment(op: ConvertOp, arg: Node, result: Node): Unit = {
    val resultReg = r(nodes.loadToIReg(result), op.dstKind.width)
    val ok = asm.newLabel

    asm.cmp(resultReg, 1)
    asm.jcc(CC.NO, ok)

    nodes.withSavedState {
      val adjustProc = op match {
        case F2I => RTSProc.JR_Adjust_F2I
        case F2L => RTSProc.JR_Adjust_F2L
        case D2I => RTSProc.JR_Adjust_D2I
        case D2L => RTSProc.JR_Adjust_D2L
      }
      nodes.releaseLoc(result)
      rtsCall(adjustProc, result)(arg)
    }

    asm.bind(ok)
  }

  override protected def signExtendShortIntegralToInt(width: Width, signed: Boolean, dst: IReg, src: IReg): Unit = {
    if (signed) {
      asm.movsx(r32(dst), r(src, width))
    } else {
      asm.movzx(r32(dst), r(src, width))
    }
  }

  override def signExtendIntToLong(dst: IReg, src: IReg): Unit = asm.movsxd(r(dst), r32(src))

  override def andAddr(mem: Mem, value: Int): Unit = tmEmit.withAddrMode(mem) { am => asm.and(am, value) }

  override def andAddr(iReg: IReg, value: Int): Unit = asm.and(r(iReg), value)

  def depriveEOP(loc: Location): Unit = {
    val enrichmentSize = 64 - RTConst.Eop.ENRICHMENT_SHIFT.intValue
    if (loc.isIReg) {
      asm.shl(r(loc.asIReg), enrichmentSize)
      asm.shr(r(loc.asIReg), enrichmentSize)
    } else {
      tmEmit.withAddrMode(loc.asMem) { am =>
        asm.shl(am, enrichmentSize)
        asm.shr(am, enrichmentSize)
      }
    }
  }
}
