/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64.codegen

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.FReg
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.amd64.*
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.*
import com.huawei.excelsior.jet.assembler.amd64.Feature.*
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.Register32.EAX
import com.huawei.excelsior.jet.assembler.amd64.Register16.AX
import com.huawei.excelsior.jet.assembler.amd64.Register8.AL
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Location, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64
import com.huawei.excelsior.jet.codeemitter.{BranchOp, ScratchPool}
import com.huawei.excelsior.jet.compiler.Env.{addressSize, frameAlignment, stackSlotSize, targetOS, targetPlatform}
import com.huawei.excelsior.jet.compiler.RTConst.CPUFeature
import com.huawei.excelsior.jet.compiler.abi.amd64.ABIAmd64
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.CodeGenerator
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.{PrefetchForWrite, PrefetchIsTemporal}
import com.huawei.excelsior.jet.compiler.options.NumOption.PrefetchLevel
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption}
import com.huawei.excelsior.jet.compiler.{RTConst, RTSGlobal, RTSProc}
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.properties.OS.{LINUX, WINDOWS}
import xscala.util.MathUtils.{alignUp, isAligned, isNBits, isNBitsSigned}

import java.lang.Float.floatToRawIntBits
import scala.PartialFunction.{cond, condOpt}
import scala.annotation.nowarn

/**
 * Generation of code segment for the root method.
 *
 * @author conwor
 * @author paul
 */
@nowarn("msg=match may not be exhaustive")
trait CodeGeneratorAmd64 extends CodeGenerator { self: Universe with BackEndAmd64 =>

  override protected lazy val asm: Assembler = new Assembler(SHORTJUMPS)
  private lazy val scratchProvider = ScratchPool.empty()
  override protected lazy val emit: CodeEmitterAmd64 = new CodeEmitterAmd64(asm, scratchProvider, symbolLinker)

  class CodeGeneratorImplAmd64 extends CodeGeneratorImpl with CodeGeneratorImplMach {

    /** Returns register, that contains node value, according to node value type width. */
    private def wreg(node: Node): Register = node.resource match {
      case xmm: XMM => xmm
      case gpr: GPR => if (node.tpe == IntType) gpr.asReg32 else gpr
    }

    /** Returns GP register, that contains node value. */
    private def gpr(node: Node) = reg(node).asInstanceOf[GPR]

    /** Returns XMM register, that contains node value. */
    private def xmm(node: Node) = reg(node).asInstanceOf[XMM]

    private def spoiledGpr(node: Node) = ScalaCollections.singleton(node.spoiled) match {
      case Some(x: GPR) => x
      case _ => shouldNotReachHere("spoiled gpr for: "+ node)
    }

    private def spoiledXMM(node: Node) = ScalaCollections.singleton(node.spoiled) match {
      case Some(x: XMM) => x
      case _ => shouldNotReachHere("spoiled xmm for: "+ node)
    }

    private def addrMode(slot: FrameSlot): AddrMode = {
      val m = slot.mem
      M(m.base.asInstanceOf[GPR], m.disp)
    }

    /** Returns AddrMode result of the node. */
    private def addrMode(n: Node): AddrMode = n match {
      case lea: Lea if lea.attachedAsArg => convertLeaArgsToAddrMode(lea)
      case ac: AddrConst => M(ac.symbol, ac.offset)
      case DWordConst(addr) => absolute(addr)
      case sa: HasFrameSlot => addrMode(sa.slot)
      case _ => M(gpr(n))
    }

    private def convertLeaArgsToAddrMode(lea: Lea) = lea match {
      case Lea.Base(base, disp) => base match {
        case sa: HasFrameSlot => addrMode(sa.slot).disposed(disp)
        case _ => M(gpr(base), disp)
      }

      case Lea.Scaled(base, index, scale, disp) => base match {
        case sa: HasFrameSlot => addrMode(sa.slot).indexed(Width(scale), gpr(index)).disposed(disp)
        case _ => M(gpr(base), scaled(Width(scale), gpr(index)), disp)
      }

      case Lea.Baseless(index, scale, disp) =>
        val ValueType(s) = lea.tpe
        M(s.width, scaled(Width(scale), gpr(index)), disp)
    }

    private object PRegNode        { def unapply(n: Node) = condOpt(n) { case NodeWithResource(_: Register)              => wreg(n) } }
    private object PIRegNode       { def unapply(n: Node) = condOpt(n) { case NodeWithResource(r: Register) if r.isIReg  => wreg(n) } }
    private object PFRegNode       { def unapply(n: Node) = condOpt(n) { case NodeWithResource(r: XMM)                   => r } }

    /** Transforms Node's condition code to assembler condition code. */
    private def conditionCode(condition: Condition, isFP: Boolean): Seq[(CC, Boolean)] = {
      if (isFP) {
        // Memo:
        //   FP Comparison sets the following flags:
        //                 ZF  PF  CF
        //   UNORDERED     1   1   1
        //      >          0   0   0
        //      <          0   0   1
        //      =          1   0   0
        //
        //   Used condition codes:
        //     CC.B  = CF
        //     CC.BE = CF || ZF
        //     CC.A  = !CF && !ZF
        //     CC.AE = !CF
        //     CC.E  = ZF
        //     CC.NE = !ZF

        condition match {
          case Condition.EQ              => Seq((CC.P,  false), (CC.E,  true))
          case Condition.NE              => Seq((CC.P,  true),  (CC.NE, true))

          case Condition.LT              => Seq((CC.P,  false), (CC.B,  true))
          case Condition.LT_OR_UNORDERED => Seq(                (CC.B,  true))

          case Condition.LE              => Seq((CC.P,  false), (CC.BE, true))
          case Condition.LE_OR_UNORDERED => Seq(                (CC.BE, true))

          case Condition.GT              => Seq(                (CC.A,  true))
          case Condition.GT_OR_UNORDERED => Seq((CC.P,  true),  (CC.A,  true))

          case Condition.GE              => Seq(                (CC.AE, true))
          case Condition.GE_OR_UNORDERED => Seq((CC.P,  true),  (CC.AE, true))
        }

      } else {
        val cc = condition match {
          case Condition.EQ  => CC.E
          case Condition.NE  => CC.NE
          case Condition.LT  => CC.L
          case Condition.LE  => CC.LE
          case Condition.GT  => CC.G
          case Condition.GE  => CC.GE
          case Condition.UGT => CC.A
          case Condition.UGE => CC.AE
          case Condition.ULT => CC.B
          case Condition.ULE => CC.BE
        }
        Seq((cc, true))
      }
    }

    private def useSignFlag(op: Condition) = cond(op) {
      case Condition.LT | Condition.LE | Condition.GT | Condition.GE => true
    }

    /** Returns integer constant from given node, or register. */
    private def intOrReg(node: Node) = node match {
      case DWordConst(v) => v
      case fc: FConst => floatToRawIntBits(fc.value)
      case _: AnyNull => 0
      case x => wreg(x)
    }

    private def symbolToAddrMode(sym: Symbol, tmp: => GPR) = {
      symbolLinker.accessKind(sym) match {
        case AccessKind.DIRECT => M(sym)
        case AccessKind.FAR =>
          shouldNotReachHere("not implemented yet") // TODO: implement
        // mov(tmpReg, addr(sym)); M(tmpReg)
      }
    }

    /** Write value from given `src` of given `tpe` to given `dst` address. */
    private def writeMemory(tpe: AsmType, dst: AddrMode, src: Any): Unit = {
      val width = tpe.width

      src match {
        case reg: Register =>
          tpe match {
            case F32 => asm.sse.movss(dst as width, reg.asXMM)
            case F64 => asm.sse.movsd(dst as width, reg.asXMM)
            case _           => asm.mov(dst as width, reg.asGPR as width)
          }

        case x: Int =>
          val imm = width match {
            case W8 => x.toByte.toInt
            case W16 => x.toShort.toInt
            case _ => x
          }
          asm.mov(dst as width, imm)
      }
    }

    /** Load immediate `imm` to given `dst` register. */
    private def loadImm(dst: Register, imm: Long, maySpoilFlags: Boolean = true): Unit = {
      val (r32, gpr) = (dst.asReg32, dst.asGPR)
      assert(dst == r32 || dst == gpr)

      imm match {
        case  0 if maySpoilFlags    => asm.xor(r32, r32)
        case -1 if maySpoilFlags    => asm.or(dst, -1)
        case _  if isNBits(imm, 32) => asm.mov(r32, imm.toInt)
        case _ =>
          assert(dst == gpr || isNBitsSigned(imm, 32))
          asm.mov(dst, imm)
      }
    }

    private def cmpImm(reg: Register, imm: Int, cond: Condition): Unit = {
      if (imm == 0) {
        // Cmp(x, 0) and Test(x, x) give the same result
        asm.test(reg, reg)
      } else {
        asm.cmp(reg, imm)
      }
    }

    private def adjustRegFromType(reg: Register, kind: AsmType): Unit = {
      import AsmType.*
      kind match {
        case I8  => asm.movsx(reg.asReg32, reg.asReg8)
        case U8  => asm.movzx(reg.asReg32, reg.asReg8)
        case U16 => asm.movsx(reg.asReg32, reg.asReg16)
        case I16 => asm.movzx(reg.asReg32, reg.asReg16)
        case I32 | U32 => asm.mov(reg.asReg32, reg.asReg32)
        case F16 => shouldNotReachHere()
        case _ =>
      }
    }


    //-----------------------------------------------------------------------------------------------------------

    /**
      * For {float, double} -> {int, long} conversions when value is NaN or does not fit into int/long range,
      * JVM Spec requires to return specific values: 0 for NaN, Integer.MIN_VALUE for negative argument and
      * Integer.MAX_VALUE for positive argument.
      * On the contrary, SSE instructions cvttsd2si/cvttss2si (used to implement such conversions) indicate
      * out-of-range situation with a single 0x80000000 value. So, conversion is split into two parts
      * and out-of-range is handled in the JR_Adjust_{F,D}2{I,L} RTS procedures.
      *
      * Generates additional adjustment for conversion operation.
      */
    private def convertAdjustment(from: XMM, fromType: AsmType, to: GPR, toType: AsmType, exit: Label) = slowPathStub {
      val rtsProc = (fromType, toType) match {
        case (F32, I32) => RTSProc.JR_Adjust_F2I
        case (F32, I64) => RTSProc.JR_Adjust_F2L
        case (F64, I32) => RTSProc.JR_Adjust_D2I
        case (F64, I64) => RTSProc.JR_Adjust_D2L
      }

      val convertMethod = env.getRTSProc(rtsProc)
      val abi = targetPlatform.abi(convertMethod)
      assert(abi.methodType.callConv.isJET)
      val argReg = abi.allArgumentFRegs(0)
      val resReg = abi.resultLocation.asIReg
      assert(abi.volatileIRegs.toSet == Set(resReg))

      emit.mov(to, resReg)
      emit.fswap(from, argReg, fromType.width)

      ensureFullFrame() // For stack alignment before call
      asm.call(convertMethod)

      emit.fswap(from, argReg, fromType.width)
      emit.swap(to, resReg)

      asm.jmp(exit)
    }

    private def genCast(cast: Cast): Unit = {
      val from = wreg(cast.arg)
      val to = wreg(cast)

      cast match {
        case ReinterpretCast(fromType, toType, _) =>
          (fromType, toType) match {
            case (FloatType, IntType) => asm.sse.movd(to.asReg32, from.asXMM)
            case (IntType, FloatType) => asm.sse.movd(to.asXMM, from.asReg32)

            case (DoubleType, LongType) => asm.sse.movq(to.asGPR, from.asXMM)
            case (LongType, DoubleType) => asm.sse.movq(to.asXMM, from.asGPR)

            case _ =>
              shouldNotReachHere(s"Unexpected reinterpret cast from $fromType to $toType")
          }

        case ValueConvert(fromType, toType, _) =>
          if (fromType == I32 || fromType == I64) {
            assert(toType.isFloatingPoint)
            // cvtsi2ss/cvtsi2sd instructions do not clear high part of destination register,
            // which cause partial register stall, so we clear it manually (JET-13055)
            asm.sse.xorps(to.asXMM, to.asXMM) // xorpd takes an extra 66 prefix
          }

          def genAdjustment(): Unit = {
            val end = asm.newLabel
            asm.cmp(to, 1)
            asm.jcc(CC.O, convertAdjustment(from.asXMM, fromType, to.asGPR, toType, end))
            asm.bind(end)
          }

          (fromType, toType) match {
            case (I32, F32)  => asm.sse.cvtsi2ss(to.asXMM, from.asReg32)
            case (I32, F64) => asm.sse.cvtsi2sd(to.asXMM, from.asReg32)

            case (I64, F32)  => asm.sse.cvtsi2ss(to.asXMM, from.asGPR)
            case (I64, F64) => asm.sse.cvtsi2sd(to.asXMM, from.asGPR)

            case (F32, I32)    => asm.sse.cvttss2si(to.asReg32, from.asXMM); genAdjustment()
            case (F32, I64)   => asm.sse.cvttss2si(to.asGPR, from.asXMM); genAdjustment()
            case (F32, F64) => asm.sse.cvtss2sd(to.asXMM, from.asXMM)

            case (F64, I32)   => asm.sse.cvttsd2si(to.asReg32, from.asXMM); genAdjustment()
            case (F64, I64)  => asm.sse.cvttsd2si(to.asGPR, from.asXMM); genAdjustment()
            case (F64, F32) => asm.sse.cvtsd2ss(to.asXMM, from.asXMM)

            case (F16, F32) =>
              // FIXME: require F16C cpu feature during runtime initialization or check it here (e.g. like POPCNT)
              asm.sse.movd(to.asXMM, from.asReg32)
              asm.avx.vcvtph2ps(to.asXMM, to.asXMM)

            case (F32, F16) =>
              // FIXME: require F16C cpu feature during runtime initialization or check it here (e.g. like POPCNT)
              val tmp = spoiledXMM(cast)
              asm.avx.vcvtps2ph(tmp, from.asXMM)
              asm.sse.movd(to.asReg32, tmp)
              asm.movsx(to.asReg32, to.asReg16)

            case (_, _) => shouldNotReachHere(s"Unexpected conversion from $fromType to $toType")
          }
      }
    }

    private def genBFX(bfx: BitFieldExtract): Unit = {
      import BitFieldExtract.*

      val from = gpr(bfx.arg)
      val to = gpr(bfx)

      bfx match {
        case BFX(0, 8,  false, _) => asm.movzx(to.asReg32, from.asReg8)
        case BFX(0, 8,  true,  _) => asm.movsx(wreg(bfx),   from.asReg8)
        case BFX(0, 16, false, _) => asm.movzx(to.asReg32, from.asReg16)
        case BFX(0, 16, true,  _) => asm.movsx(wreg(bfx),   from.asReg16)

        case ZeroExtend.From32To64(arg) => // I -> UL
          emit.mov32(to, from)

        case BFX(0, 32, sx, _) =>
          if (bfx.tpe == IntType || bfx.argType == LongType && !sx) { // L -> I || L -> UI || L -> UL
            asm.mov(to.asReg32, from.asReg32)
          } else { // I -> L || L -> L
            assert(bfx.tpe == LongType && sx)
            asm.movsxd(to, from.asReg32)
          }

        case BFX(offset, size, sx, _) =>
          val argWidth = Width(typeSize(bfx.argType))
          emit.mov(to, from, argWidth)

          val width = Width(Math.max(typeSize(bfx.tpe), typeSize(bfx.argType)))
          val dst = to.as(width)

          if (bfx.tpe == IntType && size == 32) {
            assert(bfx.argType == LongType)
            asm.shr(dst, offset)

          } else {
            val leftBits = width.nbits - (offset + size)
            assert(leftBits >= 0)
            if (leftBits > 0) {
              asm.shl(dst, leftBits)
            }
            if (!sx) {
              asm.shr(dst, leftBits + offset)
            } else {
              asm.sar(dst, leftBits + offset)
              if (bfx.tpe == IntType && bfx.argType == LongType) {
                asm.mov(dst.asReg32, dst.asReg32)
              }
            }
          }
      }
    }

    override protected def genNullCheckImpl(nullCheck: AbstractNullCheck): Unit = {
      asm.cmp(wreg(nullCheck.obj), M(gpr(nullCheck.obj)))
    }

    private def genCAS(asmType: AsmType, addr: Node, newValue: Node): Unit = {
      asm.lock(); asm.cmpxchg(addrMode(addr).as(asmType.width), wreg(newValue).as(asmType.width))

      // cmpxchg writes only to the sub-register of the given width, but an extended value is expected.
      asmType match
        case I8  => asm.movsx(EAX, AL)
        case U8  => asm.movzx(EAX, AL)
        case I16 => asm.movsx(EAX, AX)
        case U16 => asm.movzx(EAX, AX)
        case _   =>
    }

    private def genCAS(cas: CAS): Unit = {
      assert(wreg(cas) == wreg(cas.expectedValue0))
      genCAS(cas.accessType, cas.addr, cas.newValue0)
    }

    private def genShift(shift: Shift): Unit = {
      val r = wreg(shift)
      shift.num match {
        case IConst(ic) =>
          shift.op match {
            case ArithOp.LSL => asm.shl(r, ic)
            case ArithOp.ASR => asm.sar(r, ic)
            case ArithOp.LSR => asm.shr(r, ic)
          }
        case _ =>
          assert(gpr(shift.num) == RCX)
          shift.op match {
            case ArithOp.LSL => asm.shl(r, Register8.CL)
            case ArithOp.ASR => asm.sar(r, Register8.CL)
            case ArithOp.LSR => asm.shr(r, Register8.CL)
          }
      }
    }

    private def genBitCount(bitCount: BitCount): Unit = {
      import BitCount.Kind.*

      val src = wreg(bitCount.arg)
      val dst = wreg(bitCount)
      val width = ValueType.width(bitCount.argTpe)

      // bsr, bsf notes:
      // * dst and src must have the same width
      // * if src = 0, dst is undefined

      bitCount.kind match {
        case TRAILING_ZEROS =>
          // dst = nonEmpty ? bsf(src) : maxCount
          asm.bsf(dst.as(width), src)
          val nonEmpty = asm.newLabel
          asm.jcc(CC.NZ, nonEmpty)
          asm.mov(dst, width.nbits)
          asm.bind(nonEmpty)

        case HIGHEST_BIT =>
          // dst = nonEmpty ? bsr(src) : -1
          asm.bsr(dst.as(width), src)
          val nonEmpty = asm.newLabel
          asm.jcc(CC.NZ, nonEmpty)
          asm.mov(dst, -1)
          asm.bind(nonEmpty)

        case BIT_COUNT =>
          val exit = asm.newLabel
          val bitcountBackupPath = slowPathStub {
            val rtsProc = bitCount.argTpe match {
              case IntType  => RTSProc.JR_Bitcount_I_backupPath
              case LongType => RTSProc.JR_Bitcount_L_backupPath
            }
            val bitCountMethod = env.getRTSProc(rtsProc)
            val abi = targetPlatform.abi(bitCountMethod)
            assert(bitCountMethod.getMethodType.callConv.isJET)
            val headReg = abi.allArgumentIRegs(0)
            assert(abi.volatileIRegs.toSet == bitCount.spoiled.toSet ++ Set(headReg))

            if (src.asGPR == dst.asGPR) {
              emit.swap(src.asGPR, headReg)
            } else {
              emit.mov(dst.asGPR, headReg)
              emit.mov(headReg, src.asGPR, width)
            }
            ensureFullFrame()
            asm.call(bitCountMethod)
            emit.swap(headReg, dst.asGPR)
            asm.jmp(exit)
          }

          val compDescriptor = symbolLinker.getRTSGlobalSymbol(RTSGlobal.JR_ComponentDescriptor)
          val cpuFeatureOffset = RTConst.ComponentDescriptor.cpuFeatures.offset
          val popcntFeatureMask = 1 << CPUFeature.POPCNT.intValue

          asm.test(M(compDescriptor, cpuFeatureOffset) as W64, popcntFeatureMask)
          asm.jcc(CC.Z, bitcountBackupPath)
          asm.popcnt(dst.as(width), src)
          asm.bind(exit)

        case LEADING_ZEROS =>
          shouldNotReachHere("it should be converted into MAX_SET earlier")
      }
    }

    private def genBitSwap(bitSwap: BitSwap): Unit = {
      val src = wreg(bitSwap.arg)
      val dst = wreg(bitSwap)
      val width = ValueType.width(bitSwap.tpe)

      if (dst != src) {
        asm.mov(dst, src)
      }

      if (width.nbits == 32) {
        asm.bswap(dst.asReg32)
      } else {
        assert(width.nbits == 64)
        asm.bswap(dst.asGPR)
      }
    }

    private def signFlipAddr(tpe: Type) = symbolToAddrMode(symbolLinker.getRTSGlobalSymbol(tpe match {
      case FloatType => RTSGlobal.JR_FLOAT_SIGN_FLIP
      case DoubleType => RTSGlobal.JR_DOUBLE_SIGN_FLIP
    }), { null /*TODO: tmpReg*/ })

    private def signMaskAddr(tpe: Type) = symbolToAddrMode(symbolLinker.getRTSGlobalSymbol(tpe match {
      case FloatType => RTSGlobal.JR_FLOAT_SIGN_MASK
      case DoubleType => RTSGlobal.JR_DOUBLE_SIGN_MASK
    }), { null /*TODO: tmpReg*/ })

    private def genNeg(neg: Neg): Unit = neg.tpe match {
      case FloatType          => asm.sse.xorps(xmm(neg), signFlipAddr(FloatType))
      case DoubleType         => asm.sse.xorpd(xmm(neg), signFlipAddr(DoubleType))
      case IntType | LongType => asm.neg(wreg(neg))
      }

    private def genCmp(cmp: Cmp): Unit = {
      cmp.r match {
        case DWordConst(imm) => cmpImm(wreg(cmp.l), imm, cmp.op)
        case fc: FConst => asm.sse.ucomiss(xmm(cmp.l), M(floatConstant(fc.value)))
        case dc: DConst => asm.sse.ucomisd(xmm(cmp.l), M(doubleConstant(dc.value)))
        case _: AnyNull => cmpImm(wreg(cmp.l), 0, cmp.op)
        case _ => cmp.l.tpe match {
          case FloatType => asm.sse.ucomiss(xmm(cmp.l), xmm(cmp.r))
          case DoubleType => asm.sse.ucomisd(xmm(cmp.l), xmm(cmp.r))
          case _ => asm.cmp(wreg(cmp.l), wreg(cmp.r))
        }
      }
      genCondVal(getAttachedCondVal(cmp))
    }

    private def genTest(test: Test): Unit = {
      val (l, r) = (test.l, test.r)

      r match {
        case IntegralConst(0) =>
          shouldNotReachHere("test with zero should be optimized before backend")

        case IntegralConst(imm) if isNBits(imm, 7) || (isNBits(imm, 8) && !useSignFlag(test.op)) =>
          // we cannot use 8-bit register for conditions that check SF if sign bit is set
          asm.test(wreg(l).asReg8, imm.toByte)

        case IntegralConst(imm) if isNBits(imm, 31) || (isNBits(imm, 32) && !useSignFlag(test.op)) || (test.tpe == IntType) =>
          // test with reg32 is more efficient than test with reg16
          asm.test(wreg(l).asReg32, imm.toInt)

        case DWordConst(imm) =>
          asm.test(wreg(l).asGPR, imm)

        case _ => asm.test(wreg(l), wreg(r))
      }

      genCondVal(getAttachedCondVal(test))
    }

    private def genCondVal(cval: CondVal): Unit = {
      val (condition, isFP) = flagProducerProperties(cval.condition, cval.negated)
      val res = gpr(cval)

      conditionCode(condition, isFP) match {
        case Seq((cc, true)) =>
          asm.set(cc, res.asReg8)
          asm.movzx(res.asReg32, res.asReg8)

        case condCodes =>
          assert(condCodes.size > 1)

          val falseBranch = asm.newLabel
          val trueBranch = asm.newLabel
          val end = asm.newLabel

          for ((cc, b) <- condCodes) {
            val target = if (b) trueBranch else falseBranch
            asm.jcc(cc, target)
          }

          asm.bind(falseBranch)
          loadImm(res.asReg32, 0)
          asm.jmp(end)

          asm.bind(trueBranch)
          loadImm(res.asReg32, 1)

          asm.bind(end)
      }
    }

    override protected def beforeCallActions(call: Call): Unit = {
      val abi = call.abi

      if (abi.spoilsCallerFrameDescriptor(rootMethod.getMethodType)) {
        val shadowSpaceSize = abi.shadowSpaceSize
        assert(isAligned(shadowSpaceSize, frameAlignment))

        val realStackParamsSize = abi.sizeOnCallerFrameInBytes - shadowSpaceSize
        assert((realStackParamsSize >= 0) && isAligned(realStackParamsSize, stackSlotSize))

        // We may push one extra garbage slot for stack alignment
        val repushOffset = alignUp(realStackParamsSize, frameAlignment)
        for (_ <- 0 until (repushOffset / stackSlotSize)) {
          asm.push(M(RSP, repushOffset + shadowSpaceSize))
        }

        if (shadowSpaceSize > 0) {
          asm.sub(RSP, shadowSpaceSize)
        }
      }

      if (call.methodType.isCVarArgs) {
        targetOS match {
          case WINDOWS =>
            // copy FP varargs passed on XMM registers to GPRs
            for (i <- 0 until abi.parameterCount) {
              val loc2 = abi.parameterSecondaryLocation(i)
              if (loc2 != null) {
                val loc1 = abi.paramLocations(i).asInstanceOf[XMM]
                asm.sse.movq(loc2, loc1)
              }
            }

          case LINUX =>
            val xmmArgsCount = abi.paramLocations count { _.isInstanceOf[XMM] }
            assert ((0 <= xmmArgsCount) && (xmmArgsCount <= 8))
            asm.mov(ABIAmd64.UNIX_VARARG_XMMS_COUNT_REG, xmmArgsCount)
        }
      }
    }

    override protected def genCallImpl(call: Call): Unit = call match {
      case DirectCall(method) => asm.call(method)
      case DAICall(daiSymbol) => asm.call(M(daiSymbol))
      case _ => asm.call(gpr(call.target))
    }

    override def afterCallActions(call: Call): Unit = {
      if (!call.methodType.returnType.isZST) {
        val returnType = call.methodType.returnType.toAsm
        if ((returnType.isIntegral || returnType.isPointer) && call.abi.allowShortIntegers) {
          // Return value handling: expand all short integer values to int.
          val resLoc = call.abi.resultLocation.asInstanceOf[GPR]
          adjustRegFromType(resLoc.nn, returnType)
        }
      }

      if (call.abi.spoilsCallerFrameDescriptor(rootMethod.getMethodType)) {
        val offset = alignUp(call.abi.sizeOnCallerFrameInBytes, frameAlignment)
        if (offset != 0) {
          asm.add(RSP, offset)
        }
      }
    }

    private def genIntegralDivRem(op: IDivRemOp): Unit = {
      assert (!op.isFP)
      if (op.isUnsigned)            loadImm(RDX, 0)
      else if (op.tpe == IntType)   asm.cdq()
      else                          asm.cqo()

      addXSite(op)

      if (op.isUnsigned)  asm.div(wreg(op.r))
      else                asm.idiv(wreg(op.r))
    }

    private def genCheckedOp(op: CheckedOp): Unit = {
      import CC.*
      lazy val throwStub = slowPathThrowingStub(op, op.throwProc)

      val l = wreg(op.l).as(op.width)
      val r = wreg(op.r).as(op.width)
      val res = wreg(op).as(op.width)
      assert(res == l)

      op.kind match {
        case CheckedOp.Kind.ADD =>
          asm.add(l, r)
          asm.jcc(if (op.signed) O else C, throwStub)

        case CheckedOp.Kind.SUB =>
          asm.sub(l, r)
          asm.jcc(if (op.signed) O else C, throwStub)

        case CheckedOp.Kind.MUL =>
          assert(res.asGPR == RAX)
          assert(spoiledGpr(op) == RDX)
          if (op.signed) {
            // TODO: consider to use two-arguments mnemonic which do not spoil RDX register (not suitable for W8).
            asm.imul(r)
          } else {
            asm.mul(r)
          }
          asm.jcc(C, throwStub)
      }

      if (op.width < W32) {
        if (op.signed) {
          asm.movsx(res.asReg32, res)
        } else {
          asm.movzx(res.asReg32, res)
        }
      }
    }

    private def genBinaryOp(node: BinaryOp): Unit = {
      val (l, r) = node match {
        case _: IDivRemOp | _: MulH | _: UMulH | Mul(_, DWordConst(_)) =>
          (node.l, node.r)

        case _: Add if !node.isFP =>
          (node.l, node.r)

        case _ if wreg(node) == wreg(node.l) =>
          (node.l, node.r)

        case _ =>
          assert(wreg(node) == wreg(node.r))
          (node.r, node.l)
      }

      node match {
        case div: FDiv => r match {
          case fc: FConst => asm.sse.divss(xmm(div), M(floatConstant(fc.value)))
          case dc: DConst => asm.sse.divsd(xmm(div), M(doubleConstant(dc.value)))
          case _ => div.tpe match {
            case FloatType  => asm.sse.divss(xmm(div), xmm(r))
            case DoubleType => asm.sse.divsd(xmm(div), xmm(r))
          }
        }

        case divrem: IDivRemOp => genIntegralDivRem(divrem)

        case add: Add => r match {
          case DWordConst(imm) => emit.add(iReg(add), iReg(l), imm, asmType(add).width)
          case fc: FConst      => asm.sse.addss(xmm(add), M(floatConstant(fc.value)))
          case dc: DConst      => asm.sse.addsd(xmm(add), M(doubleConstant(dc.value)))
          case _ => add.tpe match {
            case FloatType          => asm.sse.addss(xmm(add), xmm(r))
            case DoubleType         => asm.sse.addsd(xmm(add), xmm(r))
            case IntType | LongType => emit.add(iReg(add), iReg(l), iReg(r), asmType(add).width)
          }
        }

        case sub: Sub => r match {
          case DWordConst(_) => shouldNotReachHere("All integral Sub should be converted to Add, during preparation we will not re-create Sub with constant")
          case fc: FConst => asm.sse.subss(xmm(sub), M(floatConstant(fc.value)))
          case dc: DConst => asm.sse.subsd(xmm(sub), M(doubleConstant(dc.value)))
          case _ => sub.tpe match {
            case FloatType          => asm.sse.subss(xmm(sub), xmm(r))
            case DoubleType         => asm.sse.subsd(xmm(sub), xmm(r))
            case IntType | LongType => asm.sub(wreg(sub), wreg(r))
          }
        }

        case mul: Mul => r match {
          case DWordConst(imm) => asm.imul(wreg(mul), wreg(l), imm)
          case fc: FConst => asm.sse.mulss(xmm(mul), M(floatConstant(fc.value)))
          case dc: DConst => asm.sse.mulsd(xmm(mul), M(doubleConstant(dc.value)))
          case _ => mul.tpe match {
            case FloatType  => asm.sse.mulss(xmm(mul), xmm(r))
            case DoubleType => asm.sse.mulsd(xmm(mul), xmm(r))
            case IntType | LongType => asm.imul(wreg(mul), wreg(r))
          }
        }

        case mul: MulH => mul.tpe match {
          case IntType | LongType => asm.imul(wreg(r))
        }

        case mul: UMulH => mul.tpe match {
          case IntType | LongType => asm.mul(wreg(r))
        }

        case and : And => r match {
          case DWordConst(imm) => asm.and(wreg(and), imm)
          case _ => asm.and(wreg(and), wreg(r))
        }

        case or : Or => r match {
          case DWordConst(imm) => asm.or(wreg(or), imm)
          case _ => asm.or(wreg(or), wreg(r))
        }

        case xor : Xor => r match {
          case DWordConst(imm) => asm.xor(wreg(xor), imm)
          case _ => asm.xor(wreg(xor), wreg(r))
        }
      }
    }

    private def genStackZeroing(sz: StackZeroing): Unit = {
      assert(RepeatedMoveAmd64.dstTmp == RDI)
      assert(StackZeroingAmd64.srcTmp == RAX)

      emit.lea(RDI, sz.slot.mem.disposed(sz.extraOffset))
      loadImm(RAX, 0)
      genRepeatedStringMovs(sz.size, asm.stos)
    }

    private def genArrayFill(arrayFill: ArrayFill): Unit = {
      assert(RepeatedMoveAmd64.dstTmp == RDI)
      assert(ArrayFillAmd64.srcTmp == RSI)

      val bodyOffs = if (arrayFill.arrayType.isAJArray) RTConst.AJArray.BODY_OFFS.intValue
      else if (arrayFill.arrayType.isCangjieArray) RTConst.CangjieArray.BODY_OFFS.intValue
      else if (arrayFill.arrayType.isXScalaArray) RTConst.ScalaArray.ARRAY_BODY_OFFS.intValue
      else RTConst.JavaArray.ARRAY_BODY_OFFS.intValue

      // Note: do not spoil srcTmp before we calculate dstTmp because arrayFill.array may be on srcTmp
      emit.addPtr(RDI, gpr(arrayFill.array), bodyOffs)

      val size = arrayFill.totalBytes
      val bytes = genConstBytes(size, addressSize, arrayFill.elemType, arrayFill.storedValues)
      emit.lea(RSI, bytes)

      genRepeatedStringMovs(size, asm.movs)
    }

    private def genRepeatedStringMovs(size: Int, asmMov: Width => Unit): Unit = {
      if (RepeatedMoveAmd64.generateUnrolled(size)) {
        for (_ <- 0 until size/8) asmMov(W64)

      } else {
        assert(RepeatedMoveAmd64.sizeTmp == RCX)
        loadImm(RCX, size/8)
        asm.rep(); asmMov(W64)
      }

      if ((size & 0x4) != 0) asmMov(W32)
      if ((size & 0x2) != 0) asmMov(W16)
      if ((size & 0x1) != 0) asmMov(W8)
    }

    private def genMathIntrinsic(node: MathIntrinsic): Unit = {
      def loadArgToFPU(arg: Node): Unit = {
        arg match {
          case FConst(fc) => asm.x87.fld(M(W32, floatConstant(fc)))
          case DConst(dc) => asm.x87.fld(M(W64, doubleConstant(dc)))
          case FRegNode(arg) =>
            val slot = temporalSlot(0).mem as asmType(node)
            emit.withAddrMode(slot) { am =>
              emit.store(slot, arg)
              asm.x87.fld(am)
            }
        }
      }

      def takeResultFromFPU(): Unit = {
        val slot = temporalSlot(0).mem as asmType(node)
        emit.withAddrMode(slot) { am =>
          asm.x87.fstp(am)
          emit.load(xmm(node), slot)
        }
      }

      def unaryFPU(action: => Unit): Unit = {
        loadArgToFPU(node.arg)
        action
        takeResultFromFPU()
      }

      def binaryFPU(lFirst: Boolean)(action: => Unit): Unit = {
        loadArgToFPU(if (lFirst) node.l else node.r)
        loadArgToFPU(if (lFirst) node.r else node.l)
        action
        takeResultFromFPU()
      }

      import Java.Lang.MathIntrinsic.*
      node.kind match {
        case D_SQRT   => asm.sse.sqrtsd(xmm(node), xmm(node.arg))
        case F_SQRT   => asm.sse.sqrtss(xmm(node), xmm(node.arg))
        case D_ABS    => asm.sse.andpd(xmm(node) ensuring { _ == xmm(node.arg) }, signMaskAddr(DoubleType))
        case F_ABS    => asm.sse.andps(xmm(node) ensuring { _ == xmm(node.arg) }, signMaskAddr(FloatType))

        case D_ATAN2  => binaryFPU(lFirst = true) { asm.x87.fpatan() }

        case D_LOG    => asm.x87.fldln2(); unaryFPU { asm.x87.fyl2x() }

        case D_REM1 | D_REM | F_REM => binaryFPU(lFirst = false) {
          assert(spoiledGpr(node) == RAX)
          val loop = asm.newBoundLabel
          if (node.kind == D_REM1) {
            asm.x87.fprem1()
          } else {
            asm.x87.fprem()
          }
          asm.x87.fnstsw(Register16.AX)
          asm.test(Register8.AH, 4)
          asm.jcc(CC.NZ, loop)

          asm.x87.fstp(FPURegister.ST1)
        }
      }
    }

    private def genMemAtomic(ai: MemAtomic): Unit = {
      import MemAtomic.Kind.*

      val width = ai.accessType.width
      val addr = addrMode(ai.addr).as(width)

      ai.kind match {
        case AND | OR | XOR | ADD => asm.lock()
        case SWAP => // no need for lock prefix as xchg is locked implicitly
      }

      if (ai.hasValueUses) {
        val r = wreg(ai).ensuring(_ == wreg(ai.value)).as(width)
        ai.kind match { // AND | OR | XOR with value uses are lowered to rt-calls
          case ADD  => asm.xadd(addr, r)
          case SWAP => asm.lockxchg(addr, r)
        }

        if (width < W32) {
          // manually sign extend obtained value because operations above do zero-extension
          asm.movsx(r.asReg32, r)
        }

      } else {
        assert(ai.resource == InvalidResource)
        ai.value match {
          case DWordConst(c) =>
            ai.kind match {
              case ADD => asm.add(addr, c)
              case AND => asm.and(addr, c)
              case OR  => asm.or(addr, c)
              case XOR => asm.xor(addr, c)
            }

          case PRegNode(gpr) =>
            val r = gpr.as(width)
            ai.kind match {
              case ADD  => asm.add(addr, r)
              case AND  => asm.and(addr, r)
              case OR   => asm.or(addr, r)
              case XOR  => asm.xor(addr, r)
              case SWAP => asm.lockxchg(addr, r)
            }
        }
      }
    }

    protected def genDeprive(dst: IREG, src: IREG): Unit = {
      emit.mov(dst, src)
      asm.shl(dst, 64 - enrichmentIMTOffsetShift)
      asm.shr(dst, 64 - enrichmentIMTOffsetShift)
    }

    protected def mergeRichPointer(dst: IREG, imt: IREG, ptr: IREG): Unit = {
      emit.mov(dst ensuring (_ != ptr), imt)
      asm.shr(dst, enrichmentIMTOffsetShift)
      asm.shl(dst, enrichmentIMTOffsetShift) // now dst contains imt offset only
      asm.or(dst, ptr) // enrich real copy with imt offset
    }

    private def genEnrich(enrich: Enrich): Unit = {
      val res = gpr(enrich)
      val obj = gpr(enrich.obj)

      enrich.enrichment match {
        case IntegralConst(enrichmentConst) =>
          emit.mov(res, obj, WPTR)
          if (0 < enrichmentConst && enrichmentConst <= enrichmentIMTOffsetLimit) {
            val exit = asm.newLabel
            asm.shl(res, 64 - enrichmentIMTOffsetShift)
            asm.jcc(CC.Z, exit)
            asm.add(res, enrichmentConst.toInt)
            asm.rol(res, enrichmentIMTOffsetShift)
            asm.bind(exit)
          }

        case _ =>
          val enrichmentReg = gpr(enrich.enrichment)

          if (res != enrichmentReg) {
            assert(obj != enrichmentReg)
            val exit = asm.newLabel
            emit.mov(res, obj, WPTR)
            cmpImm(enrichmentReg, enrichmentIMTOffsetLimit, Condition.UGT)
            asm.jcc(CC.A, exit)
            asm.shl(res, 64 - enrichmentIMTOffsetShift)
            asm.jcc(CC.Z, exit)
            asm.add(res, enrichmentReg)
            asm.rol(res, enrichmentIMTOffsetShift)
            asm.bind(exit)

          } else {
            val plainCase = asm.newLabel
            val exit = asm.newLabel
            cmpImm(obj, 0, Condition.EQ);
            asm.jcc(CC.E, plainCase)
            cmpImm(enrichmentReg, enrichmentIMTOffsetLimit, Condition.UGT)
            asm.jcc(CC.A, plainCase)
            asm.shl(enrichmentReg, enrichmentIMTOffsetShift)
            asm.add(enrichmentReg, obj);
            asm.jmp(exit)

            asm.bind(plainCase)
            emit.mov(res, obj, WPTR)

            asm.bind(exit)
          }
      }
    }

    private def genGCPoint(gcPoint: GCPoint): Unit = {
      val tmp = spoiledGpr(gcPoint)
      asm.mov(tmp, M(frame.EER, trapPageAddress))
      addXSite(gcPoint)
      asm.mov(tmp.asReg32, M(tmp, RTConst.GCPoints.usualTrapOffset.intValue))
    }

    private def genTrapCheck(trapCheck: TrapCheck): Unit = {
      asm.mov(spoiledGpr(trapCheck).asReg32, M(gpr(trapCheck.addr)))
    }

    private def genLoadMemory(load: LoadMemory): Unit = {
      val src = addrMode(load.addr)

      if (load.attachedResults.nonEmpty) {
        val attachedBFX = ScalaCollections.singleElement(load.attachedByReason(Group.AttachReason.LOAD_EXTEND_RESULT))
        val bfx @ BitFieldExtract(0, _, sx, _) = attachedBFX
        val loadSize = Math.min(bfx.sizeInBytes, load.accessType.sizeInBytes)
        val dst = wreg(attachedBFX)
        val wSrc = src as Width(loadSize)

        addXSite(load)
        (loadSize, sx) match {
          case (1, false) => asm.movzx(dst.asReg32, wSrc)
          case (1, true)  => asm.movsx(dst.asGPR, wSrc)
          case (2, false) => asm.movzx(dst.asReg32, wSrc)
          case (2, true)  =>
            if (bfx.tpe == IntType) {
              assert(load.accessType.sizeInBytes == 2)
              asm.movsx(dst.asReg32, wSrc)
            } else {
              asm.movsx(dst.asGPR, wSrc)
            }
          case (4, false) => asm.mov(dst.asReg32, wSrc)
          case (4, true)  => asm.movsxd(dst.asGPR, wSrc)
        }

      } else {
        import AsmType._
        val dst = wreg(load)

        addXSite(load)
        load.accessType match {
          case U8        => asm.movzx(dst.asReg32, src as W8)
          case I8        => asm.movsx(dst.asReg32, src as W8)
          case U16       => asm.movzx(dst.asReg32, src as W16)
          case I16 | F16 => asm.movsx(dst.asReg32, src as W16)
          case I32 | U32 => asm.mov  (dst.asReg32, src as W32)
          case I64 | U64 => asm.mov  (dst.asGPR,   src as W64)
          case PTR       => asm.mov  (dst.asGPR,   src as WPTR)
          case F32       => asm.sse.movss(dst.asXMM, src)
          case F64       => asm.sse.movsd(dst.asXMM, src)
        }
      }
    }

    private def genStoreMemory(store: StoreMemory): Unit = {
      addXSite(store)
      writeMemory(store.accessType, addrMode(store.addr), intOrReg(store.inValue0))
    }

    private def genPrefetch(prefetch: Prefetch): Unit = {
      if (env.valueOf(PrefetchLevel) != 0) {
        if (!env.enabled(PrefetchIsTemporal)) {
          asm.sse.prefetchnta(addrMode(prefetch.addr))
        } else if (env.enabled(PrefetchForWrite)) {
          asm.prefetchw(addrMode(prefetch.addr))
        } else {
          env.valueOf(PrefetchLevel) match {
            case 1 => asm.sse.prefetcht0(addrMode(prefetch.addr))
            case 2 => asm.sse.prefetcht1(addrMode(prefetch.addr))
            case 3 => asm.sse.prefetcht2(addrMode(prefetch.addr))
            case l => shouldNotReachHere(s"Incorrect cache level: $l")
          }
        }
      }
    }

    private def genLea(lea: Lea): Unit = {
      val resReg = wreg(lea)

      lea match {
        case Lea.AnyWithBase(_: StackAlloc, _) =>
          asm.lea(resReg, convertLeaArgsToAddrMode(lea))

        case Lea.Base(base, 0) =>
          emit.mov(resReg.asGPR, iReg(base), resReg.width)

        case Lea.Base(base, disp) if resReg == wreg(base) =>
          asm.add(resReg, disp)

        case Lea.Scaled(base, index, 1, 0) if resReg == wreg(base) =>
          asm.add(resReg, wreg(index).as(resReg.width))

        case Lea.Scaled(base, index, 1, 0) if resReg == wreg(index).as(resReg.width) =>
          asm.add(resReg, wreg(base))

        case _ =>
          asm.lea(resReg, convertLeaArgsToAddrMode(lea))
      }
    }

    private def genDivisorCheck(check: DivisorCheck): Unit = {
      assert(!check.isImplicit)
      assert(!check.trusted)
      val throwStub = slowPathStub {
        ensureFullFrame()
        asm.call(env.getRTSProc(check.throwProc))
        addXSite(check)
      }
      val div = wreg(check.divisor)
      asm.test(div, div)
      asm.jcc(CC.Z, throwStub)
    }

    override protected def genTransferImpl(transfer: Transfer): Unit = (transfer, transfer.transferArg) match {
      // TODO: optimize zero bits constants with loadImm(XMM, const)
      case (PFRegNode(to), arg: Constant) => arg match {
        case IConst(0) | LConst(0) | _: AnyNull => asm.sse.pxor(to, to)

        case FConst(value) => asm.sse.movss(to, M(floatConstant(value)))
        case DConst(value) => asm.sse.movsd(to, M(doubleConstant(value)))

        case IConst(value) => asm.sse.movd(to, M(intConstant(value)))
        case LConst(value) => asm.sse.movq(to, M(longConstant(value)))

        case _ => super.genTransferImpl(transfer)
      }

      case _ => super.genTransferImpl(transfer)
    }

    override protected def genNop(): Unit = asm.nop()

    /** Generates assembler for given `node`. */
    override protected def genNodeImpl(node: Node): Unit = node match {
      case x: Cmp                   => genCmp(x)
      case x: Test                  => genTest(x)
      case x: CheckedOp             => genCheckedOp(x)
      case x: BinaryOp              => genBinaryOp(x)
      case x: Cast                  => genCast(x)
      case x: BitFieldExtract       => genBFX(x)
      case x: CAS                   => genCAS(x)
      case x: Shift                 => genShift(x)
      case x: Neg                   => genNeg(x)
      case x: BitCount              => genBitCount(x)
      case x: BitSwap               => genBitSwap(x)
      case x: StackZeroing          => genStackZeroing(x)
      case x: ArrayFill             => genArrayFill(x)
      case x: Enrich                => genEnrich(x)
      case x: GCPoint               => genGCPoint(x)
      case x: LoadMemory            => genLoadMemory(x)
      case x: StoreMemory           => genStoreMemory(x)
      case x: Prefetch              => genPrefetch(x)
      case x: Lea                   => genLea(x)
      case x: MathIntrinsic         => genMathIntrinsic(x)
      case x: MemAtomic             => genMemAtomic(x)
      case x: DivisorCheck          => genDivisorCheck(x)
      case x: TrapCheck             => genTrapCheck(x)

      case _ => super.genNodeImpl(node)
    }


    //-----------------------------------------------------------------------------------------------------------

    override protected def genBranchIfCmp(op: BranchOp, l: Node, r: Node, width: Width, target: Label): Unit = r match {
      case FConst(imm) => emit.branchIf(op, fReg(l), M(floatConstant(imm)),  width, target)
      case DConst(imm) => emit.branchIf(op, fReg(l), M(doubleConstant(imm)), width, target)

      case _ => super.genBranchIfCmp(op, l, r, width, target)
    }

    override protected def genBlockEnd(block: Block, isNext: Block => Boolean): Unit = {
      block.blockEnd match {
        case tableJump: TableJump =>
          genAddressTable(tableJump.tableSym, tableJump.exits map { x => startOf(x.target) })
          asm.jmp(M(gpr(tableJump.table), scaled(WPTR, gpr(tableJump.selector))))

        case branch: If if branch.hasAttachedByReason(Group.AttachReason.COND_BRANCH_ARG_CAS) =>
          val cmpCAS = branch.selector.asInstanceOf[CmpCAS]
          genCAS(cmpCAS.accessType, cmpCAS.addr, cmpCAS.newValue)

          val (condition, isFP, directJmpBlock, condJmpBlock) = prepareGenBranch(branch, isNext)
          for ((cc, b) <- conditionCode(condition, isFP)) {
            val target = if (b) condJmpBlock else directJmpBlock
            asm.jcc(cc, startOf(target))
          }
          genJump(directJmpBlock, isNext)

        case _ => super.genBlockEnd(block, isNext)
      }
    }
  }
}
