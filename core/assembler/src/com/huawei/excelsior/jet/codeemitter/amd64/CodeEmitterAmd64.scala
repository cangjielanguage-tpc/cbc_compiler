/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter.amd64

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.amd64.AddrMode.{scaled, *}
import com.huawei.excelsior.jet.assembler.amd64.GPR.{RAX, RCX, RDX, RSP}
import com.huawei.excelsior.jet.assembler.amd64.Register32.EDX
import com.huawei.excelsior.jet.assembler.amd64.*
import com.huawei.excelsior.jet.assembler.amd64.Register8.CL
import com.huawei.excelsior.jet.assembler.{Label, *}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.SL_MASK
import com.huawei.excelsior.jet.codeemitter.CodeEmitter.{ShiftKind, SignedI32, verifyImmForCompare, verifyStandardWidth}
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64.isMemBarrierNop
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter, ScratchProvider, SymbolInfo}
import xscala.util.MathUtils.*

import scala.annotation.{nowarn, tailrec}
import scala.util.chaining.scalaUtilChainingOps


@nowarn("msg=match may not be exhaustive")
final class CodeEmitterAmd64(val asm: Assembler, _scratchProvider: ScratchProvider, _symbolInfo: SymbolInfo)
  extends CodeEmitter(64, _scratchProvider, _symbolInfo, asm) {

  private def r(reg: IReg): GPR = reg.asInstanceOf[GPR]
  private def r(reg: IReg, width: Width): Register = r(reg).as(width tap verifyStandardWidth)
  private def r32(reg: IReg): Register = r(reg, W32)
  private def r(reg: FReg): XMM = reg.asInstanceOf[XMM]

  private def conditionCode(op: BranchOp) = {
    import BranchOp._
    op match {
      case REQ | EQ | TESTZ => CC.E
      case RNE | NE | TESTNZ => CC.NE
      case TESTBIT => CC.NAE
      case TESTNBIT => CC.AE

      case LT => CC.L
      case LE => CC.LE
      case GT => CC.G
      case GE => CC.GE

      case ULT => CC.B
      case ULE => CC.BE
      case UGT => CC.A
      case UGE => CC.AE
    }
  }

  @tailrec private def m(mem: Mem, temporalForFarSymbols: IReg): AddrMode = mem match {
    case mem: MemStatic if isFarAccess(mem.symbol) =>
      lea(temporalForFarSymbols, mem)
      M(mem.width, r(temporalForFarSymbols))

    case mem: MemLocal     => m(mem.toMemBased, temporalForFarSymbols)
    case mem: MemStatic    => M(mem.width, mem.symbol, mem.disp)
    case mem: MemBased     => M(mem.width, r(mem.base), mem.disp)
    case mem: MemBaseIndex => M(mem.width, r(mem.base), scaled(mem.scale, r(mem.index)), mem.disp)
  }

  def withAddrMode(mem: Mem)(action: AddrMode => Unit): Unit = mem match {
    case mem: MemStatic if isFarAccess(mem.symbol) => borrowScratch { tmp => action(m(mem, tmp)) }
    case _ => action(m(mem, null))
  }

  override def jump(target: IReg): Unit = asm.jmp(r(target))

  override def jump(target: Symbol): Unit = {
    if (isFarAccess(target)) {
      borrowScratch { tmp =>
        lea(tmp, target)
        jump(tmp)
      }
    } else {
      asm.jmp(target)
    }
  }

  override def jumpIndirect(target: Mem): Unit = withAddrMode(target) { asm.jmp }

  override def call(target: IReg): Unit = asm.call(r(target))

  override def call(target: Symbol): Unit = {
    if (isFarAccess(target)) {
      asm.call(M(asm.literal(target)))
    } else {
      asm.call(target)
    }
  }

  override def callIndirect(target: Mem): Unit = withAddrMode(target) { asm.call }

  override def branchIf(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit = {
    if (op.isTest) {
      asm.test(r(arg1, width), r(arg2, width))
    } else if (op.isTestBit) {
      asm.bt(r(arg1, width), r(arg2, width))
    } else {
      asm.cmp(r(arg1, width), r(arg2, width))
    }
    asm.jcc(conditionCode(op), target)
  }

  override def branchIf(_op: BranchOp, arg1: FReg, arg2: FReg, width: Width, target: Label): Unit = {
    import BranchOp._

    val (op: BranchOp, fl: XMM, fr: XMM) = (_op: @unchecked) match {
      case FNLT | FLT | FNLE | FLE =>
        // Swap args and op to generate less jcc instructions.
        // Example: FLT requires to check P and C flags, there is no such CC on amd64 which can check both at the same time,
        // instead we can swap arguments and use FGT, which checks !C && !Z, using CC.A.
        (_op.swap, r(arg2), r(arg1))
      case FNE | FEQ | FGE | FGT | FNGT | FNGE =>
        // Keep args and op as is, since it's optimal this way in terms of number of asm instructions.
        (_op, r(arg1), r(arg2))
    }

    (width: @unchecked) match {
      case W32 => asm.sse.ucomiss(fl, fr)
      case W64 => asm.sse.ucomisd(fl, fr)
    }

    (op: @unchecked) match {
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
      //     CC.P = PF
      case FNGE =>
        asm.jcc(CC.B, target)

      case FNGT =>
        asm.jcc(CC.BE, target)

      case FGT =>
        asm.jcc(CC.A, target)

      case FGE =>
        asm.jcc(CC.AE, target)

      case FNE =>
        asm.jcc(CC.P, target)
        asm.jcc(CC.NE, target)

      case FEQ =>
        val cancel = asm.newLabel
        asm.jcc(CC.P, cancel)
        asm.jcc(CC.E, target)
        asm.bind(cancel)
    }

  }

  override def branchIf(op: BranchOp, arg1: IReg, arg2: Long, width: Width, target: Label): Unit = {
    verifyImmForCompare(arg2, op, size(width), op.isSigned)

    arg2 match {
      case SignedI32(arg2) =>
        val r1 = r(arg1, width)
        val code = conditionCode(op)
        if (op.isTest) {
          asm.test(r1, arg2)
        } else if (op.isTestBit) {
          asm.bt(r(arg1, width), arg2)
        } else if (arg2 == 0) { // Cmp(x, 0) and Test(x, x) give the same result
          asm.test(r1, r1)
        } else {
          asm.cmp(r1, arg2)
        }
        asm.jcc(code, target)

      case _ =>
        assert(size(width) == 8)
        // TODO: optimize case of test with 64-bit constant with one set bit
        withImm64OnScratch(arg2) { tmp => branchIf(op, arg1, tmp, W64, target) }
    }
  }

  override def branchIf(arg1: Mem, op: BranchOp, arg2: Long, target: Label): Unit = {
    verifyImmForCompare(arg2, op, size(arg1), arg1.`type`.signed)
    assert(arg1.`type`.isIntegral || arg1.`type`.isPointer)

    arg2 match {
      case SignedI32(arg2) =>
        withAddrMode(arg1) { am => if (op.isTest) asm.test(am, arg2) else asm.cmp(am, arg2) }
        asm.jcc(conditionCode(op), target)

      case _ =>
        assert(size(arg1) == 8)
        withImm64OnScratch(arg2) { branchIf(_, op.swap, arg1, target) }
    }
  }

  def branchIf(arg1: IReg, op: BranchOp, arg2: Mem, target: Label): Unit = {
    val r1 = r(arg1, arg2.width)
    withAddrMode(arg2) { am => if (op.isTest) asm.test(r1, am) else asm.cmp(r1, am) }
    asm.jcc(conditionCode(op), target)
  }

  override def branchIfTest(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit = {
    asm.test(r(arg1, width), r(arg2, width))
    asm.jcc(conditionCode(op), target)
  }

  // TODO: eliminate copy-paste with branchIf (more details in BranchOp TODO)
  override def branchIfTest(op: BranchOp, arg1: IReg, arg2: Long, width: Width, target: Label): Unit = {
    verifyImmForCompare(arg2, op, size(width), op.isSigned)
    asm.test(r(arg1, width), arg2.toInt) // TODO: rethink about it
    asm.jcc(conditionCode(op), target)
  }

  override protected def movImpl(dst: IReg, src: IReg, width: Width): Unit =
    asm.mov(r(dst, width), r(src, width))

  override protected def fmovImpl(dst: FReg, src: FReg, width: Width): Unit = size(width) match {
    // DO NOT use `movss/movsd rd, rs` instructions here!
    // `movss/movsd` do not clear high bits of `rd` so using such instructions
    // will incur huge performance degradation due to partial register stall.
    case 4      => asm.sse.movaps(r(dst), r(src))
    case 8 | 16 => asm.sse.movapd(r(dst), r(src))
  }

  override protected def movImpl(dst: AnyReg, imm: Long, width: Width): Unit = dst match {
    case dst: GPR =>
      // TODO: use xor when imm == 0 and if emitter can spoil flag register
      if ((width == W32) || isNBits(imm, 32)) {
        asm.mov(dst.asReg32, imm.toInt)
      } else {
        assert(size(width) == 8)
        asm.mov(dst, imm)
      }

    case dst: XMM =>
      if (imm == 0) {
        size(width) match {
          case 4 => asm.sse.xorps(dst, dst)
          case 8 => asm.sse.xorpd(dst, dst)
        }
      } else {
        withImmOnScratch(imm, width) { tmp => mov(dst, tmp, width) }
      }
  }

  override def mov(dst: IReg, src: FReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.movd(r(dst, width), r(src))
    case 8 => asm.sse.movq(r(dst, width), r(src))
  }

  override def mov(dst: FReg, src: IReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.movd(r(dst), r(src, width))
    case 8 => asm.sse.movq(r(dst), r(src, width))
  }

  override def swap(r1: IReg, r2: IReg): Unit = {
    if (r1 != r2) asm.xchg(r(r1), r(r2))
  }

  override def fswap(_r1: FReg, _r2: FReg, width: Width): Unit = {
    val (r1, r2) = (r(_r1), r(_r2))
    width match {
      case W32 | W64 if r1 == r2 =>
        // nop
      case W32 =>
        asm.sse.xorps(r1, r2)
        asm.sse.xorps(r2, r1)
        asm.sse.xorps(r1, r2)
      case W64 =>
        asm.sse.xorpd(r1, r2)
        asm.sse.xorpd(r2, r1)
        asm.sse.xorpd(r1, r2)
    }
  }

  override def lea(dst: IReg, src: Mem): Unit = src match {
    case src: MemStatic if isFarAccess(src.symbol) =>
      asm.mov(r(dst), Immediate.addr64(src.symbol, src.disp))
    case _ =>
      asm.lea(r(dst), m(src, null).as(WPTR))
  }

  override def loadLabelPosition(dst: IReg, src: Label): Unit =
    asm.mov(r(dst), Immediate.offset32InSegment(src))

  override def load(dst: AnyReg, src: Mem): Unit = dst match {
    case dst: GPR =>
      val am = m(src, dst)
      (size(src), src.`type`.signed) match {
        case (1 | 2, true)  => asm.movsx(r32(dst), am)
        case (1 | 2, false) => asm.movzx(r32(dst), am)
        case (4, _)         => asm.mov(r32(dst), am)
        case (8, _)         => asm.mov(dst, am)
      }
    case dst: XMM =>
      withAddrMode(src) { am =>
        size(src) match {
          case 4 => asm.sse.movss(dst, am)
          case 8 => asm.sse.movsd(dst, am)
        }
      }
  }

  override def store(dst: Mem, src: AnyReg): Unit = {
    withAddrMode(dst) { am => src match {
      case src: GPR => asm.mov(am, src.as(dst.width))
      case src: XMM => size(dst) match {
        case 4 => asm.sse.movss(am, src)
        case 8 => asm.sse.movsd(am, src)
      }
    }}
  }

  override protected def storeImpl(dst: Mem, imm: Long): Unit = {
    if ((size(dst) <= 4) || isNBitsSigned(imm, 32)) {
      val signedImm = if (dst.`type`.signed) imm else signExtend(imm, size(dst) * 8)
      withAddrMode(dst) { asm.mov(_, signedImm) }
    } else {
      withImm64OnScratch(imm) { store(dst, _) }
    }
  }

  override def add(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (dst == src1) {
      asm.add(r(src1, width), r(src2, width))
    } else if (dst == src2) {
      asm.add(r(src2, width), r(src1, width))
    } else {
      asm.lea(r(dst, width), M(r(src1), r(src2)))
    }
  }

  override def sub(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (src1 == src2) {
      verifyStandardWidth(width)
      val reg = r(dst, W32)
      asm.sub(reg, reg)
    } else if (dst == src2) {
      asm.neg(r(dst, width))
      asm.add(r(dst, width), r(src1, width))
    } else {
      mov(dst, src1, width)
      asm.sub(r(dst, width), r(src2, width))
    }
  }

  override def mul(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (src2 == dst)
      asm.imul(r(dst, width), r(src1, width))
    else {
      mov(dst, src1, width)
      asm.imul(r(dst, width), r(src2, width))
    }
  }

  override def mul(dst: IReg, src1: IReg, src2: Long, width: Width): Unit = {
    if (src2 == src2.toInt) {
      asm.imul(r(dst, width), r(src1, width), src2.toInt)
    } else {
      assert(width == W64)
      borrowScratch { tmp =>
        assert(tmp != dst)
        assert(tmp != src1)
        mov(tmp, src2, width)
        mul(dst, src1, tmp, width)
      }
    }
  }

  override def and(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (src2 == dst) {
      asm.and(r(dst, width), r(src1, width))
    } else {
      mov(dst, src1, width)
      asm.and(r(dst, width), r(src2, width))
    }
  }

  override def and(dst: IReg, src1: IReg, src2: Long, width: Width): Unit = {
    if (src2 == src2.toInt) {
      mov(dst, src1, width)
      asm.and(r(dst, width), src2.toInt)
    } else {
      assert(width == W64)
      borrowScratch { tmp =>
        assert(tmp != dst)
        assert(tmp != src1)
        mov(tmp, src2, width)
        and(dst, src1, tmp, width)
      }
    }
  }

  override def or(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (src2 == dst) {
      asm.or(r(dst, width), r(src1, width))
    } else {
      mov(dst, src1, width)
      asm.or(r(dst, width), r(src2, width))
    }
  }

  override def or(dst: IReg, src1: IReg, src2: Long, width: Width): Unit = {
    if (src2 == src2.toInt) {
      mov(dst, src1, width)
      asm.or(r(dst, width), src2.toInt)
    } else {
      assert(width == W64)
      borrowScratch { tmp =>
        assert(tmp != dst)
        assert(tmp != src1)
        mov(tmp, src2, width)
        or(dst, src1, tmp, width)
      }
    }
  }

  override def xor(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = {
    if (src2 == dst) {
      asm.xor(r(dst, width), r(src1, width))
    } else {
      mov(dst, src1, width)
      asm.xor(r(dst, width), r(src2, width))
    }
  }

  override def xor(dst: IReg, src1: IReg, src2: Long, width: Width): Unit = {
    if (src2 == src2.toInt) {
      mov(dst, src1, width)
      asm.xor(r(dst, width), src2.toInt)
    } else {
      assert(width == W64)
      borrowScratch { tmp =>
        assert(tmp != dst)
        assert(tmp != src1)
        mov(tmp, src2, width)
        xor(dst, src1, tmp, width)
      }
    }
  }

  private inline def same(r1: IReg, r2: IReg): Boolean = {
    r1 == r2
  }

  private inline def same(r1: IReg, r2: IReg, r3: IReg): Boolean = {
    same(r1, r2) && same(r2, r3)
  }

  private inline def same(r1: IReg, r2: IReg, r3: IReg, r4: IReg): Boolean = {
    same(r1, r2, r3) && same(r3, r4)
  }

  private inline def distinct(r1: IReg, r2: IReg): Boolean = {
    r1 != r2
  }

  private inline def distinct(r1: IReg, r2: IReg, r3: IReg): Boolean = {
    distinct(r1, r2) && distinct(r1, r3) && distinct(r2, r3)
  }

  private inline def distinct(r1: IReg, r2: IReg, r3: IReg, r4: IReg): Boolean = {
    distinct(r1, r2, r3) && distinct(r1, r4) && distinct(r2, r4) && distinct(r3, r4)
  }

  private def divRem(dst: IReg, left: IReg, right: IReg, width: Width, signed: Boolean, div: Boolean): Unit = {
    if (dst == right) {
      borrowScratch { tmp =>
        mov(tmp, right, width)
        divRem(dst, left, tmp, width, signed, div)
        return
      }
    }

    def withoutScratches(scratches: IReg*)(action: => Unit): Unit = {
      val spoiledScratches = scratches.filter(canSpoil)
      spoiledScratches.foreach(scratchProvider.removeScratch)
      try action finally spoiledScratches.foreach(scratchProvider.appendScratch)
    }

    def doDivRem(divisor: IReg): Unit = {
      assert(divisor != RAX && divisor != RDX)
      if (signed) {
        if (width == W32) asm.cdq() else asm.cqo()
        asm.idiv(r(divisor, width))
      } else {
        asm.xor(EDX, EDX)
        asm.div(r(divisor, width))
      }
    }

    withoutScratches(RAX, RDX) { borrowScratch { tmp =>
      (dst, right) match {
        case (RAX, RDX) =>
          mov(tmp, RDX)
          mov(RAX, left, width)
          doDivRem(tmp)
          mov(dst, if (div) RAX else RDX, width)
          mov(RDX, tmp)

        case (RDX, RAX) =>
          mov(tmp, RAX)
          mov(RAX, left, width)
          doDivRem(tmp)
          mov(dst, if (div) RAX else RDX, width)
          mov(RAX, tmp)

        case (RAX, _) => // right in `allIRegs \ {RAX, RDX}`
          mov(tmp, RDX)
          mov(RAX, left, width)
          doDivRem(right)
          mov(dst, if (div) RAX else RDX, width)
          mov(RDX, tmp)

        case (RDX, _) => // right in `allIRegs \ {RAX, RDX}`
          mov(tmp, RAX)
          mov(RAX, left, width)
          doDivRem(right)
          mov(dst, if (div) RAX else RDX, width)
          mov(RAX, tmp)

        case (_, RDX) => // dst in `allIRegs \ {RAX, RDX}`
          mov(dst, left, width)
          swap(dst, RAX)
          mov(tmp, RDX)
          doDivRem(tmp)
          swap(dst, RAX)
          if (!div) mov(dst, RDX, width)
          mov(RDX, tmp)

        case (_, _) => // dst in `allIRegs \ {RAX, RDX}`, right in `allIRegs \ {RDX}`
          mov(dst, left, width)
          swap(dst, RDX)
          val divisor = if (right == RAX) tmp else right
          mov(tmp, RAX)
          mov(RAX, RDX, width)
          doDivRem(divisor)
          swap(dst, RDX)
          if (div) mov(dst, RAX, width)
          mov(RAX, tmp)
      }
    }}
  }

  def div (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = divRem(dst, src1, src2, width, signed = true,  div = true)
  def udiv(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = divRem(dst, src1, src2, width, signed = false, div = true)
  def rem (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = divRem(dst, src1, src2, width, signed = true,  div = false)
  def urem(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = divRem(dst, src1, src2, width, signed = false, div = false)

  override protected def shift(dst: IReg, src1: IReg, src2: Int, width: Width, tp: ShiftKind): Unit = {
      mov(dst, src1, width)
      tp match {
        case ShiftKind.LEFT  => asm.shl(r(dst, width), src2)
        case ShiftKind.RIGHT => asm.shr(r(dst, width), src2)
        case ShiftKind.ARITH => asm.sar(r(dst, width), src2)
      }
  }

  /** Performs shift src1 by src2 bits and save result in dst. Changes only dst register. */
  override protected def shift(dst: IReg, src1: IReg, src2: IReg, width: Width, tp: ShiftKind): Unit = {
    //
    // Shift operation on amd64 without BMI2 requires shift amount argument in CL register.
    //
    // There are 15 options for input registers (4-th Bell number).
    //
    // Legend:
    //  - d - destination register (dst)
    //  - l - register with value to be shifted (src1)
    //  - r - register with shift amount value (src2)
    //  - cl - RCX register
    //
    // TODO if BMI2 available generate optimal version with sarx/shlx/shrx
    // TODO rework with possible liveness of CL

    def withSaved(saved: IReg)(action: => Unit): Unit = {
      borrowScratch { tmp =>
        mov(tmp, saved)
        action
        mov(saved, tmp)
      }
    }

    val d = dst
    val l = src1
    val r = src2
    val cl = RCX

    def doShift(reg: IReg): Unit = {
      shift(reg, width, tp)
    }

    if (distinct(d, l, r, cl)) {
      mov(d, l)
      withSaved(cl) {
        mov(cl, r)
        doShift(d)
      }

    } else if (same(d, cl) && distinct(d, l, r)) {
      withSaved(l) {
        mov(d, r)
        doShift(l)
        mov(d, l)
      }

    } else if (same(cl, l) && distinct(d, cl, r)) {
      mov(d, l)
      withSaved(l) {
        mov(l, r)
        doShift(d)
      }

    } else if (same(cl, r) && distinct(d, l, cl)) {
      mov(d, l)
      doShift(d)

    } else if (same(d, l) && distinct(d, r, cl)) {
      withSaved(cl) {
        mov(cl, r)
        doShift(d)
      }

    } else if (same(r, l) && distinct(d, r, cl)) {
      mov(d, r)
      withSaved(cl) {
        mov(cl, r)
        doShift(d)
      }

    } else if (same(d, r) && distinct(d, l, cl)) {
      withSaved(cl) {
        mov(cl, d)
        mov(d, l)
        doShift(d)
      }

    } else if (same(d, l, r) && distinct(d, cl)) {
      withSaved(cl) {
        mov(cl, d)
        doShift(d)
      }

    } else if (same(d, l, cl) && distinct(d, r)) {
      borrowScratch { tmp =>
        mov(tmp, r)
        mov(r, d)
        mov(d, tmp)
        doShift(r)
        mov(d, r)
        mov(r, tmp)
      }

    } else if (same(d, r, cl) && distinct(d, l)) {
      withSaved(l) {
        doShift(l)
        mov(d, l)
      }

    } else if (same(l, r, cl) && distinct(d, l)) {
      mov(d, l)
      doShift(d)

    } else if (same(d, cl) && same(l, r) && distinct(d, l)) {
      mov(d, l)
      doShift(d)

    } else if (same(d, r) && same(cl, l) && distinct(d, cl)) {
      borrowScratch { tmp =>
        mov(tmp, l)
        mov(cl, d)
        mov(d, tmp)
        doShift(d)
        mov(l, tmp)
      }

    } else if (same(d, l) && same(cl, r) && distinct(d, cl)) {
      doShift(d)

    } else if (same(d, l, r, cl)) {
      doShift(d)

    } else {
      shouldNotReachHere(s"Unexpected shift operands: ${dst}, ${src1}, ${src2}")
    }
  }

  private def shift(reg: IReg, width: Width, tp: ShiftKind): Unit = tp match {
    case ShiftKind.LEFT => asm.shl(r(reg, width), CL)
    case ShiftKind.RIGHT => asm.shr(r(reg, width), CL)
    case ShiftKind.ARITH => asm.sar(r(reg, width), CL)
  }

  private def faddImpl(dst: FReg, src: FReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.addss(r(dst), r(src))
    case 8 => asm.sse.addsd(r(dst), r(src))
  }

  override def fadd(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit = {
    if (dst == src2) {
      faddImpl(dst, src1, width)
    } else {
      fmov(dst, src1, width)
      faddImpl(dst, src2, width)
    }
  }

  def fadd(dst: FReg, src1: FReg, src2: Symbol, width: Width): Unit = {
    fmov(dst, src1, width)
    size(width) match {
      case 4 => asm.sse.addss(r(dst), M(src2))
      case 8 => asm.sse.addsd(r(dst), M(src2))
    }
  }

  private def fsubImpl(dst: FReg, src: FReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.subss(r(dst), r(src))
    case 8 => asm.sse.subsd(r(dst), r(src))
  }

  override def fsub(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit = {
    if (dst == src2 && src1 != src2) {
      borrowScratch { tmp =>
        mov(tmp, src1, width)
        fsubImpl(src1, src2, width)
        fmov(dst, src1, width)
        mov(src1, tmp, width)
      }
    } else {
      fmov(dst, src1, width)
      fsubImpl(dst, src2, width)
    }
  }

  def fsub(dst: FReg, src1: FReg, src2: Symbol, width: Width): Unit = {
    fmov(dst, src1, width)
    size(width) match {
      case 4 => asm.sse.subss(r(dst), M(src2))
      case 8 => asm.sse.subsd(r(dst), M(src2))
    }
  }

  private def fmulImpl(dst: FReg, src: FReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.mulss(r(dst), r(src))
    case 8 => asm.sse.mulsd(r(dst), r(src))
  }

  override def fmul(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit = {
    if (dst == src2) {
      fmulImpl(dst, src1, width)
    } else {
      fmov(dst, src1, width)
      fmulImpl(dst, src2, width)
    }
  }

  def fmul(dst: FReg, src1: FReg, src2: Symbol, width: Width): Unit = {
    fmov(dst, src1, width)
    size(width) match {
      case 4 => asm.sse.mulss(r(dst), M(src2))
      case 8 => asm.sse.mulsd(r(dst), M(src2))
    }
  }

  private def fdivImpl(dst: FReg, src: FReg, width: Width): Unit = size(width) match {
    case 4 => asm.sse.divss(r(dst), r(src))
    case 8 => asm.sse.divsd(r(dst), r(src))
  }

  override def fdiv(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit = {
    if (dst == src2 && src1 != src2) {
      borrowScratch { tmp =>
        mov(tmp, src1, width)
        fdivImpl(src1, src2, width)
        fmov(dst, src1, width)
        mov(src1, tmp, width)
      }
    } else {
      fmov(dst, src1, width)
      fdivImpl(dst, src2, width)
    }
  }

  def fdiv(dst: FReg, src1: FReg, src2: Symbol, width: Width): Unit = {
    fmov(dst, src1, width)
    size(width) match {
      case 4 => asm.sse.divss(r(src1), M(src2))
      case 8 => asm.sse.divsd(r(src1), M(src2))
    }
  }

  override protected def addImpl(dst: IReg, src: IReg, imm: Long, width: Width): Unit = {
    imm match {
      case SignedI32(imm) =>
        if (dst == src) {
          asm.add(r(dst, width), imm)
        } else {
          asm.lea(r(dst, width), M(r(src), imm))
        }
      case _ =>
        assert(size(width) == 8)
        if (dst == src) {
          withImm64OnScratch(imm) { add64(dst, _, src) }
        } else {
          mov64(dst, imm)
          add64(dst, dst, src)
        }
    }
  }

  override protected def memBarrierImpl(mask: Int): Unit = {
    if (!isMemBarrierNop(mask)) {
      asm.lock()
      asm.add(M(W32, RSP), 0)
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  // Platform-specific public API
  //

  // TODO: eliminate copy-paste with another branchIf in this CodeEmitter
  def branchIf(op: BranchOp, _arg1: FReg, arg2: AddrMode, width: Width, target: Label): Unit = {
    import BranchOp._

    val arg1 = r(_arg1)

    width match {
      case W32 => asm.sse.ucomiss(arg1, arg2)
      case W64 => asm.sse.ucomisd(arg1, arg2)
    }

    val exit = newLabel
    op match {
      case FLT =>
        asm.jcc(CC.P, exit)
        asm.jcc(CC.B, target)

      case FLE =>
        asm.jcc(CC.P, exit)
        asm.jcc(CC.BE, target)

      case FNLT =>
        asm.jcc(CC.P, target)
        asm.jcc(CC.AE, target)

      case FNLE =>
        asm.jcc(CC.P, target)
        asm.jcc(CC.A, target)

      case FGT =>
        asm.jcc(CC.A, target)

      case FGE =>
        asm.jcc(CC.AE, target)

      case FNGE =>
        asm.jcc(CC.B, target)

      case FNGT =>
        asm.jcc(CC.BE, target)

      case FNE =>
        asm.jcc(CC.P, target)
        asm.jcc(CC.NE, target)

      case FEQ =>
        asm.jcc(CC.P, exit)
        asm.jcc(CC.E, target)
    }

    bind(exit)
  }
}

object CodeEmitterAmd64 {
  def isMemBarrierNop(mask: Int) = (mask & SL_MASK) == 0
}