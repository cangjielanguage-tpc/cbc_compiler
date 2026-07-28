/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter.arm64

import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.Width.{W32, W8, WPTR}
import com.huawei.excelsior.jet.assembler.arm64.Arg.{M, MemRI, MemRR, R}
import com.huawei.excelsior.jet.assembler.arm64.Assembler.isValidImmForLdrOrStr
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{LR, SP, XZR}
import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode.{POST_IDX, PRE_IDX}
import com.huawei.excelsior.jet.assembler.arm64.ShiftMode.LSL
import com.huawei.excelsior.jet.assembler.arm64.*
import com.huawei.excelsior.jet.assembler.arm64.immediates.{BitMaskImm, FloatImm, ShiftedImm12}
import com.huawei.excelsior.jet.assembler.{Label, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.{LL_MASK, LS_MASK, SS_MASK}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter.{ShiftKind, adjustWidthToStandardReg, verifyImmForCompare, verifyStandardWidth}
import com.huawei.excelsior.jet.codeemitter.{BranchOp, CodeEmitter, ScratchProvider, SymbolInfo}
import xscala.util.MathUtils.*

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import java.lang.Long.numberOfTrailingZeros
import scala.math.toIntExact

/** Implementation of CodeEmitter on ARM64
  *
  * @author orangebyte256
  */
final class CodeEmitterArm64(val asm: Assembler, _scratchProvider: ScratchProvider, _symbolInfo: SymbolInfo,
                             // TODO: remove [[isJIT]] and make JITSymbolLinker#accessKind great again, JET-13397
                             val isJIT: Boolean)
  extends CodeEmitter(64, _scratchProvider, _symbolInfo, asm) {

  private def r(loc: IReg): IRegister.X =
    loc.asInstanceOf[IRegister.X]

  private def r(loc: FReg, width: Width): VFPRegister =
    loc.asInstanceOf[VFPRegister].as(width)

  private def r(loc: AnyReg, width: Width): Register =
    if (loc.isIReg) r(loc.asIReg, width) else r(loc.asFReg, width)

  private def r(loc: IReg, width: Width): IRegister = {
    verifyStandardWidth(width)
    loc.asInstanceOf[IRegister].as(width)
  }

  private def conditionCode(op: BranchOp) = {
    import BranchOp._
    (op: @unchecked) match {
      case REQ | EQ | TESTZ => CC.EQ
      case RNE | NE | TESTNZ => CC.NE

      case LT => CC.LT
      case LE => CC.LE
      case GT => CC.GT
      case GE => CC.GE

      case ULT => CC.LO
      case ULE => CC.LS
      case UGT => CC.HI
      case UGE => CC.HS

      case FEQ  => CC.EQ
      case FNE  => CC.NE
      case FLT  => CC.LO
      case FLE  => CC.LS
      case FGT  => CC.GT
      case FGE  => CC.GE
      case FNGE => CC.LT
      case FNGT => CC.LE
      case FNLE => CC.HI
      case FNLT => CC.HS
    }
  }

  private def convertBaseIndex(mem: MemBaseIndex): MemRR =
    if (mem.scale == W8) M(r(mem.base), r(mem.index)) else M(r(mem.base), Arg.scaled(r(mem.index)))

  private def convertBased(mem: MemBased): MemRI = M(r(mem.base), mem.disp)

  private def needSignExtend(mem: Mem) = size(mem.width) < 4 && mem.`type`.signed

  private def sizeOf(action: () => Unit): Int = asm.withNewSegment(action()).getActualLength / 4

  private def genShortest(actions: (() => Unit)*): Unit = (actions minBy sizeOf).apply()

  private def simplifyBased(loc: MemBased, tmp: IReg) = {
    assert(tmp != SP)
    val MemBased(t, base, disp) = loc
    val shift = log2(size(loc))

    if (isAlignedToNBits(disp, shift) && (sizeOf(() => movPtr(tmp, disp >> shift)) < sizeOf(() => movPtr(tmp, disp)))) {
      movPtr(tmp, disp >> shift)
      mem(t, base, scaled(tmp, loc.width))

    } else if (sizeOf(() => addPtr(tmp, base, disp)) < sizeOf(() => movPtr(r(tmp), disp))) {
      addPtr(tmp, base, disp)
      mem(t, tmp)

    } else {
      movPtr(tmp, disp)
      mem(t, base, scaled(tmp, W8))
    }
  }

  private def simplifyBaseIndexWithDisp(loc: MemBaseIndex, tmp: IReg) = {
    val MemBaseIndex(t, base, index, scale, disp) = loc
    if (disp % size(scale) == 0) {
      addPtr(tmp, index, disp / size(scale))
      mem(t, base, scaled(tmp, scale))
    } else {
      addPtr(tmp, base, disp)
      mem(t, tmp, scaled(index, scale))
    }
  }

  private def simplifyBaseIndexToBased(loc: MemBaseIndex, tmp: IReg) = {
    val MemBaseIndex(t, base, index: IRegister.X, scale, disp) = loc
    assert(isPowerOf2(size(scale)))
    asm.add(r(tmp), r(base), R(index, LSL, log2(size(scale))))
    mem(t, tmp, disp)
  }

  private def simplifyBaseIndexForMem(mem: MemBaseIndex, tmp: IReg): Mem = {
    if ((mem.disp != 0) && isValidScaleForOneLdrStrInstruction(mem)) {
      simplifyBaseIndexWithDisp(mem, tmp)
    } else {
      simplifyBaseIndexToBased(mem, tmp)
    }
  }

  private def normalizeStatic(loc: MemStatic, tmp: IReg) = {
    val MemStatic(t, symbol, disp) = loc
    // TODO: try to replace this check by isFarAccess(mem.symbol) and remove isJIT field
    if (isJIT && !symbol.isInstanceOf[Label]) {
      asm.ldr(r(tmp), asm.literal(symbol, disp))
      mem(t, tmp)
    } else {
      asm.adr(r(tmp), symbol)
      mem(t, tmp, disp)
    }
  }

  private def isValidScaleForOneLdrStrInstruction(mem: MemBaseIndex) =
    size(mem.scale) == 1 || size(mem.scale) == size(mem)

  private def isValidMemForOneLdrStrInstruction(mem: MemBaseIndex) =
    (mem.disp == 0) && isValidScaleForOneLdrStrInstruction(mem)

  override def branchIf(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit = {
    if (op.isTest) {
      asm.tst(r(arg1, width), r(arg2, width))
    } else {
      asm.cmp(r(arg1, width), r(arg2, width))
    }
    asm.b(conditionCode(op), target)
  }

  override def branchIf(op: BranchOp, arg1: FReg, arg2: FReg, width: Width, target: Label): Unit = {
    assert(op.isFloatingPoint, s"$op $arg1 $arg2 $width $target")
    asm.fcmp(r(arg1, width), r(arg2, width))
    asm.b(conditionCode(op), target)
  }

  override def branchIf(op: BranchOp, _arg1: IReg, arg2: Long, width: Width, target: Label): Unit = {
    verifyImmForCompare(arg2, op, size(width), signed = true)
    val arg1 = r(_arg1, width)

    if (op == BranchOp.TESTBIT) {
      asm.tbz(arg1, toIntExact(arg2), target)

    } else if (op == BranchOp.TESTNBIT) {
      asm.tbnz(arg1, toIntExact(arg2), target)

    } else if ((op == BranchOp.EQ) && arg2 == 0) {
      asm.cbz(arg1, target)

    } else if ((op == BranchOp.NE) && arg2 == 0) {
      asm.cbnz(arg1, target)

    } else if ((op == BranchOp.TESTZ) && (arg1 != SP) && bitCount(arg2) == 1) {
      asm.tbz(arg1, numberOfTrailingZeros(arg2), target)

    } else if ((op == BranchOp.TESTNZ) && (arg1 != SP) && bitCount(arg2) == 1) {
      asm.tbnz(arg1, numberOfTrailingZeros(arg2), target)

    } else if (op.isTest && (arg1 != SP) && BitMaskImm.canEncode(arg2, width)) {
      asm.tst(arg1, arg2)
      asm.b(conditionCode(op), target)

    } else if (!op.isTest && ShiftedImm12.canEncode(arg2)) {
      asm.cmp(arg1, toIntExact(arg2))
      asm.b(conditionCode(op), target)

    } else {
      withImmOnScratch(arg2, width) { tmp => branchIf(op, _arg1, tmp, width, target) }
    }
  }

  override def branchIfTest(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit = {
    asm.tst(r(arg1, width), r(arg2, width))
    asm.b(conditionCode(op), target)
  }

  // TODO: eliminate copy-paste with branchIf (more details in BranchOp TODO)
  override def branchIfTest(op: BranchOp, _arg1: IReg, arg2: Long, width: Width, target: Label): Unit = {
    val arg1 = r(_arg1, width)

    if ((op == BranchOp.EQ) && (arg1 != SP) && bitCount(arg2) == 1) {
      asm.tbz(arg1, numberOfTrailingZeros(arg2), target)

    } else if ((op == BranchOp.NE) && (arg1 != SP) && bitCount(arg2) == 1) {
      asm.tbnz(arg1, numberOfTrailingZeros(arg2), target)

    } else if ((arg1 != SP) && BitMaskImm.canEncode(arg2, width)) {
      asm.tst(arg1, arg2)
      asm.b(conditionCode(op), target)

    } else {
      withImmOnScratch(arg2, width) { tmp => branchIf(op, _arg1, tmp, width, target) }
    }
  }

  override protected def movImpl(dst: IReg, src: IReg, width: Width): Unit =
    asm.mov(r(dst, width), r(src, width))

  override protected def fmovImpl(dst: FReg, src: FReg, width: Width): Unit =
    asm.fmov(r(dst, width), r(src, width))

  private def movByParts(dst: IRegister, imm: Long, negate: Boolean): Unit = {
    var isFirst = true
    var i = 0
    for (i <- 0 until dst.width.nbits by 16) {
      val part = bits(imm, i, (i + 16) - 1).toInt
      if (part != 0) {
        if (negate) {
          if (isFirst) {
            asm.movn(dst, part, i)
          } else {
            asm.movk(dst, bits(~part, 0, 15), i)
          }
        } else {
          if (isFirst) {
            asm.movz(dst, part, i)
          } else {
            asm.movk(dst, part, i)
          }
        }
        isFirst = false
      }
    }
    assert(!isFirst)
  }

  override protected def movImpl(_dst: AnyReg, rawImm: Long, width: Width): Unit = _dst match {
    case _dst: IReg =>
      val imm = bits(rawImm, 0, size(width) * 8 - 1)

      val dst = r(_dst, width)
      assert(dst != SP)

      // TODO: consider using literal for complicated imm that isn't covered with fast paths
      if (!asm.tryMovImm(dst, imm)) {
        genShortest(
          () => movByParts(dst, imm, negate = false),
          () => movByParts(dst, ~imm, negate = true))
      }

    case _dst: FReg =>
      val dst = r(_dst, width)
      val imm = if (width == W32) intBitsToFloat(rawImm.toInt).toDouble else longBitsToDouble(rawImm)

      if (rawImm == 0) {
        asm.fmov(dst, r(XZR, width))

      } else if (FloatImm.canEncode(imm, width)) {
        asm.fmov(dst, imm)

      } else {
        asm.ldrLiteral(dst, rawImm)
      }
  }

  override def mov(dst: IReg, src: FReg, width: Width): Unit =
    asm.fmov(r(dst, width), r(src, width))

  override def mov(dst: FReg, src: IReg, width: Width): Unit =
    asm.fmov(r(dst, width), r(src, width))

  override def swap(_r1: IReg, _r2: IReg): Unit = if (_r1 != _r2) {
    val (r1, r2) = (r(_r1), r(_r2))
    asm.eor(r1, r1, r2)
    asm.eor(r2, r2, r1)
    asm.eor(r1, r1, r2)
  }

  override def fswap(r1: FReg, r2: FReg, width: Width): Unit = notImplemented("fswap for ARM64 code emitter")

  override def lea(dst: IReg, src: Mem): Unit = {
    assert(dst != XZR)

    src match {
      case src: MemStatic  => lea(dst, normalizeStatic(src, dst))
      case src: MemLocal   => lea(dst, src.toMemBased)
      case src: MemBased   => addPtr(dst, src.base, src.disp)

      case src: MemBaseIndex =>
        if (src.disp == 0) {
          lea(dst, simplifyBaseIndexToBased(src, dst))
        } else if (dst != src.base && dst != src.index) {
          lea(dst, simplifyBaseIndexWithDisp(src, dst))
        } else {
          borrowScratch { tmp => lea(dst, simplifyBaseIndexWithDisp(src, tmp)) }
        }
    }
  }

  override def loadLabelPosition(dst: IReg, src: Label): Unit = asm.movOffs32InMethod(r(dst), src)

  override def load(dst: AnyReg, src: Mem): Unit = {
    val adjustedWidth = adjustWidthToStandardReg(src.width)

    if (dst == SP) {
      borrowScratch { tmp =>
        load(tmp, src)
        mov(dst.asIReg, tmp, WPTR)
      }

    } else {
      val tryReuseDst = dst.isIReg && dst != XZR

      src match {
        case src: MemStatic =>
          if (tryReuseDst) {
            load(dst, normalizeStatic(src, dst.asIReg))
          } else {
            borrowScratch { tmp => load(dst, normalizeStatic(src, tmp)) }
          }

        case src: MemLocal =>
          load(dst, src.toMemBased)

        case src @ MemBased(t, base, disp) =>
          if (isValidImmForLdrOrStr(disp, src.width)) {
            asm.ldr(needSignExtend(src), t.width, r(dst, adjustedWidth), convertBased(src))
          } else if (tryReuseDst && dst != base) {
            load(dst, simplifyBased(src, dst.asIReg))
          } else {
            borrowScratch { tmp => load(dst, simplifyBased(src, tmp)) }
          }

        case src @ MemBaseIndex(t, base, index, _, _) =>
          if (isValidMemForOneLdrStrInstruction(src)) {
            asm.ldr(needSignExtend(src), t.width, r(dst, adjustedWidth), convertBaseIndex(src))
          } else if (tryReuseDst && dst != base && dst != index) {
            load(dst, simplifyBaseIndexForMem(src, dst.asIReg))
          } else {
            borrowScratch { tmp => load(dst, simplifyBaseIndexForMem(src, tmp)) }
          }
      }
    }
  }

  override def store(dst: Mem, src: AnyReg): Unit = {
    val adjustedWidth = adjustWidthToStandardReg(dst.width)

    if (src == SP) {
      borrowScratch { tmp =>
        mov(tmp, src.asIReg, WPTR)
        store(dst, tmp)
      }

    } else {
      dst match {
        case dst: MemStatic =>
          borrowScratch { tmp => store(normalizeStatic(dst, tmp), src) }

        case dst: MemLocal =>
          store(dst.toMemBased, src)

        case dst @ MemBased(t, _, disp) =>
          if (isValidImmForLdrOrStr(disp, dst.width)) {
            asm.str(t.width, r(src, adjustedWidth), convertBased(dst))
          } else {
            borrowScratch { tmp => store(simplifyBased(dst, tmp), src) }
          }

        case dst: MemBaseIndex =>
          if (isValidMemForOneLdrStrInstruction(dst)) {
            asm.str(dst.width, r(src, adjustedWidth), convertBaseIndex(dst))
          } else {
            borrowScratch { tmp => store(simplifyBaseIndexForMem(dst, tmp), src) }
          }
      }
    }
  }

  override protected def storeImpl(dst: Mem, imm: Long): Unit = {
    if (imm == 0) {
      store(dst, XZR)
    } else {
      // May be implemented using literal. TODO: investigate the difference.
      withImmOnScratch(imm, dst.width) { tmp => store(dst, tmp) }
    }
  }

  override def add(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.add(r(dst, width), r(src1, width), r(src2, width))

  override def sub(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.sub(r(dst, width), r(src1, width), r(src2, width))

  override def mul(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.mul(r(dst, width), r(src1, width), r(src2, width))

  override def mul(dst: IReg, src1: IReg, imm: Long, width: Width): Unit = {
    if (dst == src1) {
      borrowScratch { tmp =>
        mov(tmp, imm, width)
        asm.mul(r(dst, width), r(src1, width), r(tmp, width))
      }
    } else {
      mov(dst, imm, width)
      asm.mul(r(dst, width), r(src1, width), r(dst, width))
    }
  }

  override def and(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.and(r(dst, width), r(src1, width), r(src2, width))

  override def and(dst: IReg, src1: IReg, imm: Long, width: Width): Unit = {
    if (BitMaskImm.canEncode(imm, width)) {
      asm.and(r(dst, width), r(src1, width), imm)
    } else if (dst == src1) {
      borrowScratch { tmp =>
        assert(tmp != dst)
        mov(tmp, imm, width)
        and(dst, src1, tmp, width)
      }
    } else {
      mov(dst, imm, width)
      and(dst, src1, dst, width)
    }
  }

  override def or(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.orr(r(dst, width), r(src1, width), r(src2, width))

  override def or(dst: IReg, src1: IReg, imm: Long, width: Width): Unit = {
    if (BitMaskImm.canEncode(imm, width)) {
      asm.orr(r(dst, width), r(src1, width), imm)
    } else if (dst == src1) {
      borrowScratch { tmp =>
        assert(tmp ne dst)
        mov(tmp, imm, width)
        or(dst, src1, tmp, width)
      }
    } else {
      mov(dst, imm, width)
      or(dst, src1, dst, width)
    }
  }

  override def xor(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit =
    asm.eor(r(dst, width), r(src1, width), r(src2, width))

  override def xor(dst: IReg, src1: IReg, imm: Long, width: Width): Unit = {
    if (BitMaskImm.canEncode(imm, width)) {
      asm.eor(r(dst, width), r(src1, width), imm)
    } else if (dst == src1) {
      borrowScratch { tmp =>
        assert(tmp != dst)
        mov(tmp, imm, width)
        xor(dst, src1, tmp, width)
      }
    } else {
      mov(dst, imm, width)
      xor(dst, src1, dst, width)
    }
  }

  override def div (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = asm.sdiv(r(dst, width), r(src1, width), r(src2, width))
  override def udiv(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = asm.udiv(r(dst, width), r(src1, width), r(src2, width))
  override def rem (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = remImpl(dst, src1, src2, width, isSigned = true)
  override def urem(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = remImpl(dst, src1, src2, width, isSigned = false)

  private def remImpl(dst: IReg, src1: IReg, src2: IReg, width: Width, isSigned: Boolean): Unit = {
    val tmp = if (dst == src1 || dst == src2) acquireScratch() else dst
    if (isSigned) {
      div(tmp, src1, src2, width)
    } else {
      udiv(tmp, src1, src2, width)
    }
    asm.msub(r(dst, width), r(tmp, width), r(src2, width), r(src1, width))
    if (tmp != dst) releaseScratch(tmp)
  }

  override protected def shift(dst: IReg, src1: IReg, src2: Int, width: Width, tp: ShiftKind): Unit = {
    val imm = bits(src2, 0, width.log2bits - 1)
    tp match {
      case ShiftKind.LEFT  => asm.lsl(r(dst, width), r(src1, width), imm)
      case ShiftKind.RIGHT => asm.lsr(r(dst, width), r(src1, width), imm)
      case ShiftKind.ARITH => asm.asr(r(dst, width), r(src1, width), imm)
    }
  }

  override protected def shift(dst: IReg, src1: IReg, src2: IReg, width: Width, tp: ShiftKind): Unit = {
    tp match {
      case ShiftKind.LEFT  => asm.lsl(r(dst, width), r(src1, width), r(src2, width))
      case ShiftKind.RIGHT => asm.lsr(r(dst, width), r(src1, width), r(src2, width))
      case ShiftKind.ARITH => asm.asr(r(dst, width), r(src1, width), r(src2, width))
    }
  }

  override protected def addImpl(_dst: IReg, _src: IReg, imm: Long, width: Width): Unit = {
    val dst = r(_dst, width)
    val src = r(_src, width)

    // 1. Fast paths
    if (imm == imm.toInt) {
      val imm32 = imm.toInt
      if (ShiftedImm12.canEncode(imm32)) {
        asm.add(dst, src, imm32)
        return
      }
      if (ShiftedImm12.canEncode(-imm32)) {
        asm.sub(dst, src, -imm32)
        return
      }
      if (isNBits(imm32, 24)) {
        asm.add(dst, src, bits(imm32, 0, 11))
        asm.add(dst, dst, bits(imm32, 12, 23) << 12)
        return
      }
      if (isNBits(-imm32, 24)) {
        asm.sub(dst, src, bits(-imm32, 0, 11))
        asm.sub(dst, dst, bits(-imm32, 12, 23) << 12)
        return
      }
    }

    // 2. General case
    val tmp = if ((dst != SP) && (dst != src)) _dst else acquireScratch()
    try {
      genShortest(
        () => { mov(tmp,  imm, width); asm.add(dst, src, r(tmp, width))},
        () => { mov(tmp, -imm, width); asm.sub(dst, src, r(tmp, width))})
    } finally {
      if (tmp != _dst) {
        releaseScratch(tmp)
      }
    }
  }

  override def fadd(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit =
    asm.fadd(r(dst, width), r(src1, width), r(src2, width))

  override def fsub(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit =
    asm.fsub(r(dst, width), r(src1, width), r(src2, width))

  override def fmul(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit =
    asm.fmul(r(dst, width), r(src1, width), r(src2, width))

  override def fdiv(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit =
    asm.fdiv(r(dst, width), r(src1, width), r(src2, width))

  override protected def memBarrierImpl(mask: Int): Unit = {
    if (mask == SS_MASK) {
      asm.dmb(DBOption.ST)
    } else if ((mask & ~(LL_MASK | LS_MASK)) == 0) {
      asm.dmb(DBOption.LD)
    } else {
      asm.dmb(DBOption.SY)
    }
  }

  override def jump(target: IReg): Unit = asm.br(r(target))

  override def jump(target: Symbol): Unit = {
    if (isFarAccess(target)) {
      borrowScratch { tmp =>
        lea(tmp, target)
        jump(tmp)
      }
    } else {
      asm.b(target)
    }
  }

  override def jumpIndirect(target: Mem): Unit = borrowScratch { tmp =>
    load(tmp, target)
    jump(tmp)
  }

  override def call(target: IReg): Unit = asm.blr(r(target))

  override def call(target: Symbol): Unit = {
    if (isFarAccess(target)) {
      lea(LR, target)
      call(LR)
    } else {
      asm.bl(target)
    }
  }

  override def callIndirect(target: Mem): Unit = {
    load(LR, target)
    call(LR)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Platform-specific public API
  //
  // Push/pop pair of registers

  /** Store pair of registers on stack. `r1` will have the least address. */
  def pushPair(r1: Register, r2: Register): Unit = asm.stp(r1, r2, M(PRE_IDX, SP, -2 * r1.width.nbytes))

  /** Load pair of registers from stack, assuming `r1` have the least address. */
  def popPair(r1: Register, r2: Register): Unit = asm.ldp(r1, r2, M(POST_IDX, SP, 2 * r1.width.nbytes))
}
