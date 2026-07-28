/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64.codegen

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.{FReg, MemBaseIndex, MemBased, mem, scaled}
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.arm64.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.WZR
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{IP0, LR, SP, XZR}
import com.huawei.excelsior.jet.assembler.arm64.PrfOp.*
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Location, Width}
import com.huawei.excelsior.jet.codeemitter.arm64.CodeEmitterArm64
import com.huawei.excelsior.jet.codeemitter.{BranchOp, ScratchPool}
import com.huawei.excelsior.jet.compiler.Env.{addressSize, frameAlignment, stackSlotSize}
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.backend.arm64.BackEndArm64
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.CodeGenerator
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.{Arm64CASBackupPath, PrefetchForWrite, PrefetchIsTemporal}
import com.huawei.excelsior.jet.compiler.options.NumOption
import com.huawei.excelsior.jet.compiler.options.NumOption.PrefetchLevel
import com.huawei.excelsior.jet.compiler.{Env, RTConst}
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.util.MathUtils.*

import java.lang.Long.numberOfTrailingZeros
import scala.PartialFunction.condOpt
import scala.annotation.nowarn

/** Generation of code segment for the root method.
  */
@nowarn("msg=match may not be exhaustive")
trait CodeGeneratorArm64 extends CodeGenerator { self: Universe with BackEndArm64 =>

  override protected lazy val asm: Assembler = new Assembler

  private lazy val scratchProvider = ScratchPool(stdCodeEmitterScratch)

  override protected lazy val emit: CodeEmitterArm64 = new CodeEmitterArm64(asm, scratchProvider, symbolLinker, Env.isJIT)

  class CodeGeneratorImplArm64 extends CodeGeneratorImpl with CodeGeneratorImplMach {

    /** Transforms Node's condition code to corresponding assembler condition codes. */
    private def conditionCode(source: Node, cond: Condition, isFP: Boolean): CC = {
      if (isFP) {
        cond match {
          case Condition.EQ              => CC.EQ
          case Condition.NE              => CC.NE
          case Condition.LT              => CC.LO
          case Condition.LE              => CC.LS
          case Condition.GT              => CC.GT
          case Condition.GE              => CC.GE
          case Condition.LT_OR_UNORDERED => CC.LT
          case Condition.LE_OR_UNORDERED => CC.LE
          case Condition.GT_OR_UNORDERED => CC.HI
          case Condition.GE_OR_UNORDERED => CC.HS
          case _ => shouldNotReachHere("unexpected float ConditionCode: " + cond)
        }
      } else {
        // Transformation convertAndToTest can exchange Cmp to Test nodes. Instructions tst(x,y) and cmp(and(x,y),0)
        // update only the N, Z and V flags equally so it's correct to use only conditions related to N, Z and V flags
        (source, cond) match {
          case (_, Condition.EQ)       => CC.EQ
          case (_, Condition.NE)       => CC.NE
          case (_, Condition.LT)       => CC.LT
          case (_, Condition.LE)       => CC.LE
          case (_, Condition.GT)       => CC.GT
          case (_, Condition.GE)       => CC.GE
          case (_: Cmp, Condition.ULT) => CC.LO
          case (_: Cmp, Condition.ULE) => CC.LS
          case (_: Cmp, Condition.UGT) => CC.HI
          case (_: Cmp, Condition.UGE) => CC.HS
          case _ => shouldNotReachHere("unexpected integral ConditionCode: " + cond)
        }
      }
    }

    //-----------------------------------------------------------------------------------------------------------

    private def pIRegByType(tpe: Type, xr: IRegister.X): IRegister =
      if (typeSize(tpe) == 4) xr.asW else xr

    private def pIReg(node: Node): IRegister =
      pIRegByType(node.tpe, node.resource.asInstanceOf[IRegister.X])

    private def pFRegByType(tpe: Type, r: VFPRegister.D): VFPRegister =
      typeSize(tpe) match {
        case 4 => r.asS
        case 8 => r.asD
      }

    private def pRegByType(tpe: Type, r: Register): Register = r match {
      case r: IRegister.X => pIRegByType(tpe, r)
      case r: VFPRegister.D => pFRegByType(tpe, r)
    }

    private def pFReg(node: Node): VFPRegister =
      pFRegByType(node.tpe, node.resource.asInstanceOf[VFPRegister.D])

    private def pReg(n: Node): Register =
      PRegNode.unapply(n).get

    private def spoiledIRegOpt(node: Node): Option[IRegister.X] =
      ScalaCollections.singleton(node.spoiled collect { case x: IRegister.X => x })

    private def spoiledIReg(node: Node): IRegister.X =
      spoiledIRegOpt(node).get

    private def spoiledFRegOpt(node: Node): Option[VFPRegister.D] =
      ScalaCollections.singleton(node.spoiled collect { case x: VFPRegister.D => x })

    private def spoiledFReg(node: Node): VFPRegister.D =
      spoiledFRegOpt(node).get

    private object PRegNode  { def unapply(n: Node) = condOpt(n) { case NodeWithResource(r: Register)        => pRegByType(n.tpe, r) }}
    private object PIRegNode { def unapply(n: Node) = condOpt(n) { case NodeWithResource(r: IRegister.X)     => pIRegByType(n.tpe, r) }}
    private object PFRegNode { def unapply(n: Node) = condOpt(n) { case NodeWithResource(r: VFPRegister.D)   => pFRegByType(n.tpe, r) }}

    //-----------------------------------------------------------------------------------------------------------

    override protected def genNullCheckImpl(nullCheck: AbstractNullCheck): Unit = {
      asm.ldr(XZR, Arg.M(iReg(nullCheck.obj)))
    }

    /** Action receives total amount of extra stack size and number of repushed slots.  */
    private def aroundCallSpoilingFrameTop(call: Call)(action: (Int, Int) => Unit): Unit = {
      if (call.abi.spoilsCallerFrameDescriptor(rootMethod.getMethodType)) {
        val stackParamsSize = call.abi.sizeOnCallerFrameInBytes
        assert((stackParamsSize >= 0) && isAligned(stackParamsSize, stackSlotSize))
        val extraFrameSize = alignUp(stackParamsSize, frameAlignment)
        action(extraFrameSize, stackParamsSize / stackSlotSize)
      }
    }

    override protected def beforeCallActions(call: Call): Unit = {
      aroundCallSpoilingFrameTop(call) { (extraFrameSize, slotsCount) =>
        asm.sub(SP, SP, extraFrameSize)

        // TODO: we could grab extra scratch register and repush two slots at once

        // We can spoil stdTmp register right before call because it was filtered out in `indirectCallTargetSet`
        val tmp = stdTmp

        for (i <- 0 until slotsCount) {
          asm.ldr(tmp, Arg.M(SP, extraFrameSize + stackSlotSize * (i + 1)))
          asm.str(tmp, Arg.M(SP, stackSlotSize * i))
          // SOE-Note: these accesses cannot provoke SOE because they are present only if target of call is unmanaged
          //           and hence stack check has been generated in prologue.
        }
      }
    }

    override protected def genCallImpl(call: Call): Unit = call match {
      case DirectCall(method) => asm.bl(method)
      case DAICall(daiSymbol) =>
        // NOTE: do not modify this code, because of ABI-specific format
        asm.adr(IP0, daiSymbol)
        asm.ldr(IP0, Arg.M(IP0))
        asm.blr(IP0)
      case _ => asm.blr(iReg(call.target))
    }

    override def afterCallActions(call: Call): Unit = {
      assert(!call.abi.allowShortIntegers)

      aroundCallSpoilingFrameTop(call) { (extraFrameSize, slotsCount) =>
        asm.add(SP, SP, extraFrameSize)
      }
    }

    private def genEnrich(enrich: Enrich): Unit = {
      val res = pIReg(enrich).asX
      val obj = pIReg(enrich.obj).asX

      valueOf(enrich.enrichment).producer match {
        case IntegralConst(enrichmentConst) =>
          if (0 < enrichmentConst && enrichmentConst <= enrichmentIMTOffsetLimit) {
            val enrichmentReg = pIReg(enrich.enrichment)
            asm.cmp(obj, 0) // null check
            asm.orr(res, obj, Arg.R(enrichmentReg, ShiftMode.LSL, enrichmentIMTOffsetShift)) // TODO: think on writing imm directly (e.g. with movk)
            asm.csel(res, XZR, res, CC.EQ)
          } else {
            emit.mov(res, obj)
          }

        case _ =>
          val exit = new Label()
          val imtOffsetIsOutOfLimit = new Label()
          val enrichment = pIReg(enrich.enrichment)

          // arm64 is unable to generate cmp with 0xFFFF, so we are using tst.
          assert(isPowerOf2(enrichmentIMTOffsetLimit + 1))
          asm.tst(enrichment, ~enrichmentIMTOffsetLimit)
          asm.b(CC.NE, imtOffsetIsOutOfLimit)

          asm.cmp(obj, 0) // null check
          asm.orr(res, obj, Arg.R(enrichment, ShiftMode.LSL, enrichmentIMTOffsetShift))
          asm.csel(res, XZR, res, CC.EQ)
          asm.b(exit)

          asm.bind(imtOffsetIsOutOfLimit)
          asm.mov(res, obj)
          asm.bind(exit)
      }
    }

    protected def genDeprive(dst: IREG, src: IREG): Unit = {
      asm.and(dst, src, ~enrichmentMask)
    }

    protected def mergeRichPointer(dst: IREG, imt: IREG, ptr: IREG): Unit = {
      asm.and(dst ensuring (_ != ptr), imt, enrichmentMask) // now dst contains imt offset only
      asm.orr(dst, dst, ptr) // enrich real copy with imt offset
    }

    private def genCmp(cmp: Cmp): Unit = {
      (cmp.l, cmp.r) match {
        case (PIRegNode(l), PIRegNode(r))          => asm.cmp(l, r)
        case (PFRegNode(l), PFRegNode(r))          => asm.fcmp(l, r)
        case (PIRegNode(l), ShiftedImm12Node(imm)) => asm.cmp(l, imm)
      }
      genCondVal(getAttachedCondVal(cmp))
    }

    private def genTest(test: Test): Unit = {
      (test.l, test.r) match {
        case (PIRegNode(l), IntegralConst(imm)) => asm.tst(l, imm)
        case (PIRegNode(l), PIRegNode(r))       => asm.tst(l, r)
      }
      genCondVal(getAttachedCondVal(test))
    }

    private def genCondVal(cval: CondVal): Unit = {
      val (condition, isFP) = flagProducerProperties(cval.condition, cval.negated)
      val condCode = conditionCode(cval.condition, condition, isFP)
      asm.cset(pIReg(cval), condCode)
    }

    private def genMulH(res: IRegister, arg1: IRegister, arg2: IRegister, unsigned: Boolean): Unit = {
      (res, arg1, arg2) match {
        case (res: IRegister.X, arg1: IRegister.X, arg2: IRegister.X) =>
          val mulh = if (unsigned) asm.umulh _ else asm.smulh _
          mulh(res, arg1, arg2)

        case (res: IRegister.W, arg1: IRegister.W, arg2: IRegister.W) =>
          val resX = res.asX
          val mull = if (unsigned) asm.umull _ else asm.smull _
          mull(resX, arg1, arg2)
          asm.lsr(resX, resX, 32)
      }
    }

    private def genCheckedOp(op: CheckedOp): Unit = {
      assert(op.width >= Width.W32)
      lazy val throwStub = slowPathThrowingStub(op, op.throwProc)

      val (PIRegNode(res), PIRegNode(l), PIRegNode(r)) = (op, op.l, op.r)
      op.kind match {
        case CheckedOp.Kind.ADD =>
          asm.adds(res, l, r)
          asm.b(if (op.signed) CC.VS else CC.CS, throwStub)
        case CheckedOp.Kind.SUB =>
          asm.subs(res, l, r)
          asm.b(if (op.signed) CC.VS else CC.CC, throwStub)
      }
    }

    private def genBinaryOp(op: BinaryOp): Unit = {
      (op, op.l, op.r) match {
        case (PIRegNode(res), PIRegNode(l), PIRegNode(r)) => op match {
          case _: Add => asm.add(res, l, r)
          case _: Sub => asm.sub(res, l, r)
          case _: Mul => asm.mul(res, l, r)
          case _: And => asm.and(res, l, r)
          case _: Or  => asm.orr(res, l, r)
          case _: Xor => asm.eor(res, l, r)

          case op: IDivRemOp =>
            assert(op.isDiv)
            assert(!op.hasImplicitCheck)
            val div = if (op.isUnsigned) asm.udiv _ else asm.sdiv _
            div(res, l, r)

          case _: MulH  => genMulH(res, l, r, unsigned = false)
          case _: UMulH => genMulH(res, l, r, unsigned = true)
        }

        case (PIRegNode(res), PIRegNode(l), IntegralConst(imm)) => op match {
          case _: Add => asm.add(res, l, Math.toIntExact(imm))
          case _: Sub => asm.sub(res, l, Math.toIntExact(imm))
          case _: And => asm.and(res, l, imm)
          case _: Or  => asm.orr(res, l, imm)
          case _: Xor => asm.eor(res, l, imm)
        }

        case (PFRegNode(res), PFRegNode(l), PFRegNode(r)) => op match {
          case _: Add  => asm.fadd(res, l, r)
          case _: Sub  => asm.fsub(res, l, r)
          case _: Mul  => asm.fmul(res, l, r)
          case _: FDiv => asm.fdiv(res, l, r)
        }
      }
    }

    private def genDivisorCheck(check: DivisorCheck): Unit = {
      assert(!check.isImplicit)
      assert(!check.trusted)
      val throwStub = slowPathStub {
        ensureFullFrame()
        asm.bl(env.getRTSProc(check.throwProc))
        addXSite(check)
      }
      asm.cbz(pIReg(check.divisor), throwStub)
    }

    private def genShift(shift: Shift): Unit = {
      val res = pIReg(shift)
      val value = pIReg(shift.value).as(res.width)
      shift.num match {
        case PIRegNode(num0) =>
          val num = num0.as(value.width)
          shift.op match {
            case ArithOp.LSL => asm.lsl(res, value, num)
            case ArithOp.ASR => asm.asr(res, value, num)
            case ArithOp.LSR => asm.lsr(res, value, num)
          }

        case DWordConst(imm) =>
          assert(shift.op == ArithOp.LSL)
          val num = bits(imm, 0, log2(typeSizeInBits(shift.tpe)))
          asm.lsl(res, value, num)
      }
    }

    private def genNeg(neg: Neg): Unit = {
      (neg, neg.arg) match {
        case (PIRegNode(res), PIRegNode(arg)) => asm.neg(res, arg)
        case (PFRegNode(res), PFRegNode(arg)) => asm.fneg(res, arg)
      }
    }

    private def genCast(cast: Cast): Unit = {
      cast match {
        case ReinterpretCast(fromType, toType, arg) =>
          assert(fromType.isFloatingPointType != toType.isFloatingPointType)
          if (fromType.isFloatingPointType) {
            asm.fmov(pIReg(cast), pFReg(arg))
          } else {
            asm.fmov(pFReg(cast), pIReg(arg))
          }

        case ValueConvert(fromType, toType, arg) => (cast, arg) match {
          case (PFRegNode(to), PFRegNode(from)) =>
            asm.fcvt(to, from)

          case (PFRegNode(to), PIRegNode(from)) =>
            if (fromType == F16) {
              asm.fmov(to, from.as(to.width))
              asm.fcvt(to, to.asH)
            } else {
              asm.scvtf(to, from)
            }

          case (PIRegNode(to), PFRegNode(from)) =>
            if (toType == F16) {
              val tmp = spoiledFReg(cast)
              asm.fcvt(tmp.asH, from)
              asm.fmov(to, tmp.as(to.width))
              asm.sxth(to, to.asW)
            } else {
              asm.fcvtzs(to, from)
            }
        }
      }
    }

    private def genBFX(bfx: BitFieldExtract): Unit = {
      bfx match {
        case BitFieldExtract.ZeroExtend.From32To64(value) if iReg(bfx) == iReg(value) =>
          // nothing to do

        case BitFieldExtract(0, 32, false, value) =>
          asm.mov(pIReg(bfx).asW, pIReg(value).asW)

        case BitFieldExtract(offset, size, sx, value) =>
          val width = Width(Math.max(typeSize(bfx.tpe), typeSize(bfx.argType)))
          val res = pIReg(bfx).as(width)
          val arg = pIReg(value).as(width)

          if (!sx || (bfx.tpe == IntType && size == 32)) {
            asm.ubfm(res, arg, offset, offset + size - 1)
          } else {
            asm.sbfm(res, arg, offset, offset + size - 1)
            if (bfx.tpe == IntType && bfx.argType == LongType) {
              asm.mov(res.asW, res.asW)
            }
          }
      }
    }

    private def genBitCount(bitCount: BitCount): Unit = {
      import BitCount.Kind.*

      val arg = pIReg(bitCount.arg)
      val res = pIReg(bitCount).as(arg.width)

      bitCount.kind match {
        case LEADING_ZEROS =>
          asm.clz(res, arg)

        case TRAILING_ZEROS =>
          asm.rbit(res, arg)
          asm.clz(res, res)

        case HIGHEST_BIT =>
          shouldNotReachHere("it should not be used on this platform")

        case BIT_COUNT =>
          val tmp = spoiledFReg(bitCount).asV

          asm.mov(tmp, 0, arg.asX)
          asm.cnt(tmp, tmp, 8)
          asm.addv(tmp, W8, tmp, 8)
          asm.mov(res.asX, tmp, 0)
      }
    }

    private def genLoadMemory(load: LoadMemory): Unit = {
      addXSite(load)

      // TODO: unify copy-paste with other archs (amd64)
      val (res, tpe, forceZX) = if (load.attachedResults.nonEmpty) {
        assert(!load.mayHaveResource)
        val attachedBFX = ScalaCollections.singleElement(load.attachedByReason(Group.AttachReason.LOAD_EXTEND_RESULT))
        val bfx @ BitFieldExtract(0, _, sx, _) = attachedBFX
        assert(bfx.attachedResults.isEmpty)
        val loadSize = Math.min(bfx.sizeInBytes, load.accessType.sizeInBytes)
        assert(bfx.tpe == IntType)
        val adjustedTpe = loadSize match {
          case 1 => if (sx) I8 else U8
          case 2 => if (sx) I16 else U16
          case 4 => if (sx) I32 else U32
        }
        (bfx, adjustedTpe, !sx)
      } else {
        (load, load.accessType, false)
      }

      val dst = pReg(res).as(W64).asReg
      val asmType = asmTypeForReadWrite(tpe, forceZX)
      emit.load(dst, memLoc(asmType, load.addr))
    }

    private def genStoreMemory(store: StoreMemory): Unit = {
      addXSite(store)
      val r = store.inValue0 match {
        case v @ ZeroValueNode() => pIRegByType(v.tpe, XZR)
        case v => pReg(v)
      }
      val src = r.as(W64).asReg
      val asmType = asmTypeForReadWrite(store.accessType)
      emit.store(memLoc(asmType, store.addr), src)
    }

    private def genPrefetch(n: Prefetch): Unit = {
      if (env.valueOf(PrefetchLevel) != 0) {
        val lvl = env.valueOf(PrefetchLevel) match {
          case 1 => L1
          case 2 => L2
          case 3 => L3
          case l => shouldNotReachHere(s"Incorrect cache level: $l")
        }

        val prfop = (if (env.enabled(PrefetchForWrite)) PST else PLD) |
          lvl |
          (if (env.enabled(PrefetchIsTemporal)) KEEP else STRM)

        val marg: Arg.Mem = memLoc(I64, n.addr) match {
          case mem: MemBased =>
            Arg.M(mem.base.asInstanceOf[IRegister.X], mem.disp)
          case mem: MemBaseIndex =>
            assert(mem.scale == W8)
            assert(mem.disp == 0)
            Arg.M(mem.base.asInstanceOf[IRegister.X], mem.index.asInstanceOf[IRegister.X])
        }

        asm.prfm(prfop, marg)
      }
    }

    private def genLea(lea: Lea): Unit = {
      val res = pIReg(lea)
      val width = res.width

      // index must be non-negative so it's safe to extend 32-bit to 64-bit one
      def indexReg(index: Node) = pIReg(index).as(width)

      lea match {
        case Lea.Base(base, disp) =>
          emit.add(res.asX, pIReg(base).asX, disp, width)

        case Lea.Baseless(index, 1, disp) =>
          emit.add(res.asX, indexReg(index).asX, disp, width)

        case Lea.Baseless(index, scale, disp) =>
          asm.lsl(res, indexReg(index), log2(scale))
          emit.add(res.asX, res.asX, disp, width)

        case Lea.Scaled(base, index, scale, disp) =>
          asm.add(res, pIReg(base), Arg.R(indexReg(index), ShiftMode.LSL, log2(scale)))
          emit.add(res.asX, res.asX, disp, width)
      }
    }

    // TODO: unify copy-paste with other archs
    private def genGCPoint(gcPoint: GCPoint): Unit = {
      asm.ldr(IP0, Arg.M(frame.EER, trapPageAddress))
      addXSite(gcPoint)
      asm.ldr(IP0, Arg.M(IP0, RTConst.GCPoints.usualTrapOffset.intValue))
    }

    private def genTrapCheck(trapCheck: TrapCheck): Unit = {
      asm.ldr(XZR, Arg.M(iReg(trapCheck.addr)))
    }
    
    private def genMSub(msub: MSub): Unit = {
      asm.msub(pIReg(msub), pIReg(msub.op1), pIReg(msub.op2), pIReg(msub.op3))
    }

    private def genArrayFill(n: ArrayFill): Unit = {
      val size = n.totalBytes
      val elemType = n.elemType
      val elemSize = n.elemType.sizeInBytes
      val dataSym = genConstBytes(size, addressSize, elemType, n.storedValues)
      assert(symbolLinker.isDirectAccess(dataSym))

      val Seq(arr, data, limit, tmp) = n.spoiled collect { case x: IRegister.X => x }

      val bodyOffs = if (n.arrayType.isAJArray) RTConst.AJArray.BODY_OFFS.intValue
      else if (n.arrayType.isCangjieArray) RTConst.CangjieArray.BODY_OFFS.intValue
      else if (n.arrayType.isXScalaArray) RTConst.ScalaArray.ARRAY_BODY_OFFS.intValue
      else RTConst.JavaArray.ARRAY_BODY_OFFS.intValue

      emit.addPtr(arr, iReg(n.array), bodyOffs)
      emit.lea(data, dataSym)
      emit.addPtr(limit, arr, size)

      val exit = asm.newLabel

      val header = asm.newBoundLabel
      emit.branchIf(BranchOp.EQ, arr, limit, WPTR, exit)
      emit.load(tmp, mem(elemType, data))
      emit.store(mem(elemType, arr), tmp)
      emit.addPtr(data, data, elemSize)
      emit.addPtr(arr, arr, elemSize)
      emit.jump(header)

      asm.bind(exit)
    }

    private def genStackZeroing(sz: StackZeroing): Unit = {
      // TODO: optimize code pattern
      // TODO: unify with ArrayFill

      val Seq(dst, counter) = sz.spoiled collect { case x: IRegister.X => x }

      emit.lea(dst, sz.slot.mem.disposed(sz.extraOffset))

      val size = sz.size
      assert(size % 4 == 0)
      val tailed = (size % 8) == 4

      emit.mov32(counter, size)

      val loopStart = asm.newLabel
      val loopEnd = asm.newLabel
      asm.bind(loopStart)
      asm.subs(counter, counter, 8)
      asm.b(CC.LO, loopEnd)
      asm.str(XZR, Arg.M(dst))
      asm.add(dst, dst, 8)
      asm.b(loopStart)
      asm.bind(loopEnd)

      if (tailed) asm.str(WZR, Arg.M(dst))
    }

    private def genMathIntrinsic(mi: MathIntrinsic): Unit = {
      import Java.Lang.MathIntrinsic.*
      mi.kind match {
        case D_SQRT => asm.fsqrt(pFReg(mi), pFReg(mi.arg))
        case F_SQRT => asm.fsqrt(pFReg(mi), pFReg(mi.arg))
        case D_ABS  => asm.fabs(pFReg(mi), pFReg(mi.arg))
        case F_ABS  => asm.fabs(pFReg(mi), pFReg(mi.arg))
      }
    }

    private def adjustAtomicResult(r: IRegister, accessType: AsmType): Unit = {
      // manually sign extend `r` because atomic operations produced its value do zero-extension
      val wr = r.asW
      if (accessType == I8) {
        asm.sxtb(wr, wr)
      } else if (accessType == I16) {
        asm.sxth(wr, wr)
      }
    }

    private def genCAS(cas: CAS): Unit = {
      val (oldValue, newValue, addr, mo) = (pIReg(cas.expectedValue0), pIReg(cas.newValue0), iReg(cas.addr), MemoryOrdering.ACQUIRE_RELEASE)
      val width = cas.accessType.width
      val res = pIReg(cas) ensuring { _ == oldValue }

      if (!env.enabled(Arm64CASBackupPath)) {
        asm.cas(width, oldValue, newValue, addr, mo)
        adjustAtomicResult(res, cas.accessType)

      } else {
        // Implement CAS logic with exclusive load-store loop
        // XR = CAS(expectedValue = XR, newValue = XN, addr = XA), additional spoiled = XS
        //
        // Note: XR can safely be same register with XN or XA, as the CAS will exit if its value is changed,
        // and all will be correct if it remains unchanged.
        //
        // retry: mov         XS, XR    <-----
        //        ldaxr[b,h]  XR, XA         |
        //        cmp         XR, XS         |
        //        b           NE, exit    ---|----
        //        stlxr[b,h]  WS, XN, XA     |   |
        //        cbz         WS, exit    ---|---|
        //        b           retry     ------   |
        // exit:                     <-------

        val exit = asm.newLabel

        // 1. Save expected value in temporal register
        val retry = asm.newBoundLabel
        val tmp = spoiledIReg(cas) ensuring { spoiled =>
          spoiled != oldValue && spoiled != newValue && spoiled.asX != addr
        }
        asm.mov(tmp, oldValue)

        // 2. Read actual value and compare it with saved expected
        asm.ldaxr(width, res, addr)
        adjustAtomicResult(res, cas.accessType)
        asm.cmp(res, tmp)
        asm.b(CC.NE, exit)

        // 3. Write new value (use saved value register as temporal because actual value remained same as expected)
        val success = tmp.asW
        asm.stlxr(width, success, newValue, addr)
        asm.cbz(success, exit)

        // 4. Retry if store fails
        asm.b(retry)
        asm.bind(exit)
      }
    }

    private def convertMemAtomicOperation(ai: MemAtomic): MemAtomicOp = {
      import MemAtomic.Kind.*

      assert(!env.enabled(Arm64CASBackupPath))
      ai.kind match {
        case ADD    => MemAtomicOp.ADD
        case ANDNOT => MemAtomicOp.BIC
        case OR     => MemAtomicOp.ORR
        case XOR    => MemAtomicOp.EOR
        case MIN    => MemAtomicOp.SMIN
        case UMIN   => MemAtomicOp.UMIN
        case MAX    => MemAtomicOp.SMAX
        case UMAX   => MemAtomicOp.UMAX
        case SWAP   => MemAtomicOp.SWP

        case AND    => shouldNotReachHere("arm64 v8.1 doesn't have logic AND, only ANDNOT")
      }
    }

    private def genMemAtomic(ai: MemAtomic): Unit = {
      import MemAtomic.Kind.*

      val width = ai.accessType.width
      val (arg, res, addr, mo) = (pIReg(ai.value), pIReg(ai), iReg(ai.addr), MemoryOrdering.ACQUIRE_RELEASE)

      if (!env.enabled(Arm64CASBackupPath)) {
        asm.memAtomic(convertMemAtomicOperation(ai), width, arg, res, addr, mo)
        adjustAtomicResult(res, ai.accessType)

      } else {
        // XR = OP(arg = XV, addr = XA), additional spoiled = XS
        // retry: ldaxr[b,h]  XR, XA      <---
        //        op          XS, XR, XV     |
        //        stlxr[b,h]  WS, XS, XA     |
        //        cbnz        WS, retry   ----

        assert(res != arg && res.asX != addr)
        emit.borrowScratch { tmp_ =>
          val tmp = tmp_ match { case x: IRegister => x }
          assert(tmp != arg && tmp != res && tmp.asX != addr)

          // 1. Read actual value
          val retry = asm.newBoundLabel
          asm.ldaxr(width, res, addr)
          adjustAtomicResult(res, ai.accessType)

          // 2. Modify
          val modified = if (ai.kind == SWAP) {
            arg

          } else {
            ai.kind match {
              case ADD    => asm.add(tmp, res, arg)
              case AND    => asm.and(tmp, res, arg)
              case OR     => asm.orr(tmp, res, arg)
              case XOR    => asm.eor(tmp, res, arg)

              case MIN | UMIN | MAX | UMAX =>
                asm.cmp(res, arg)
                val resMatchCond = ai.kind match {
                  case MIN => CC.LT
                  case UMIN => CC.LO
                  case MAX => CC.GT
                  case UMAX => CC.HI
                }
                asm.csel(tmp, res, arg, resMatchCond)
            }

            tmp
          }

          // 3. Try to store it back
          val success = tmp.asW
          asm.stlxr(width, success, modified, addr)
          asm.cbnz(success, retry)
        }
      }
    }

    override protected def genNop(): Unit = asm.nop()

    override protected def genNodeImpl(node: Node): Unit = node match {
      case x: Cmp               => genCmp(x)
      case x: Test              => genTest(x)
      case x: CheckedOp         => genCheckedOp(x)
      case x: BinaryOp          => genBinaryOp(x)
      case x: Shift             => genShift(x)
      case x: Neg               => genNeg(x)
      case x: Cast              => genCast(x)
      case x: BitFieldExtract   => genBFX(x)
      case x: BitCount          => genBitCount(x)
      case x: LoadMemory        => genLoadMemory(x)
      case x: StoreMemory       => genStoreMemory(x)
      case x: Prefetch          => genPrefetch(x)
      case x: Lea               => genLea(x)
      case x: CAS               => genCAS(x)
      case x: GCPoint           => genGCPoint(x)
      case x: ArrayFill         => genArrayFill(x)
      case x: StackZeroing      => genStackZeroing(x)
      case x: MathIntrinsic     => genMathIntrinsic(x)
      case x: MemAtomic         => genMemAtomic(x)
      case x: Enrich            => genEnrich(x)
      case x: DivisorCheck      => genDivisorCheck(x)
      case x: TrapCheck         => genTrapCheck(x)
      case x: MSub              => genMSub(x)

      case _ => super.genNodeImpl(node)
    }

    override def genBlockEnd(block: Block, isNext: Block => Boolean): Unit = block.blockEnd match {
      case tableJump: TableJump =>
        genAddressTable(tableJump.tableSym, tableJump.exits map { x => startOf(x.target) })
        emit.borrowScratch { scratch =>
          val caseAddr = scratch match { case x: IRegister.X => x }
          asm.ldr(caseAddr, Arg.M(iReg(tableJump.table), Arg.scaled(iReg(tableJump.selector))))
          asm.br(caseAddr)
        }

      case _ => super.genBlockEnd(block, isNext)
    }
  }
}
