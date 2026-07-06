/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT.Kind.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.IRZ
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.SignedImmCompactEncoding.{EncodedImmParts, calculateMemoryCompactImm}
import com.huawei.excelsior.jet.assembler.cbc.StackSlot.OffHeapMemory
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.BFX.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Common.SRem
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.ImmEXT.N.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Fixups.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.MemoryAccess.{ArrayType, LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl as SOC
import com.huawei.excelsior.jet.assembler.cbc.{Bits, CbcAssembler, CbcTypeKind, FExtBCC, FieldReference, MemExpr, OpcodePrefix, RawData, Register, StackSlot, Assembler as OldAssembler}
import com.huawei.excelsior.jet.assembler.fixups.{CoverageLocs, Relocation}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{CBC_ID16, CBC_ID32}
import com.huawei.excelsior.jet.assembler.{AsmType, Fixup, Label, Symbol, Width as AsmWidth}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils
import xscala.util.MathUtils.{bitsSigned, isNBitsSigned, rightNBits32, rightNBits64, signExtend, isNBits as isNBitsUnsigned}

import scala.PartialFunction.condOpt

trait MeaningfulNewIsaParts {
  def movRef(dst: IR, src: IR): Unit
  def fmov(frd: FR, frs: FR, w: AsmWidth): Unit
  def fmov (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def movi2f(frd: FR, irs: IR, w: AsmWidth): Unit
  def movf2i(frd: IR, irs: FR, w: AsmWidth): Unit
  def fneg (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def fabs (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def fsqrt(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def ret(v: IR, w: AsmWidth): Unit
  def retRef(v: IR): Unit
  def fret(v: FR, w: AsmWidth): Unit
  def initConstString(ts: StackSlot.Typed, stringId: Symbol): Unit
  def faddi(d: FR, l: FR, r: Double, w: AsmWidth): Unit
  def fsubi(d: FR, l: FR, r: Double, w: AsmWidth): Unit
  def fmuli(d: FR, l: FR, r: Double, w: AsmWidth): Unit
  def fdivi(d: FR, l: FR, r: Double, w: AsmWidth): Unit
  def bfx(dst: IR, src: IR, resW: AsmWidth, argW: AsmWidth, sx: Boolean, offset: Int, size: Int): Unit
}

trait NewIsaParts extends MeaningfulNewIsaParts {
  def callDirect(rd: IR, methodId: Symbol): Unit
  def callVirt(rd: IR, methodId: Symbol): Unit
  def callInterf(rd: IR, sig_id: Symbol, methodId: Symbol): Unit
  def callInterfPlain(rd: IR, methodId: Symbol): Unit
  def callInterfRich(rx: IR, methodId: Symbol): Unit
  def callInterfConst(rd: IR, enrichment: Int, methodId: Symbol): Unit
  def cFuncWrap(dst: IR, method_id: Symbol): Unit
  def aliveReference(data: Symbol): Unit
  def unmovableReference(data: Symbol): Unit
  def aliveRefDifference(data: Symbol): Unit
  def aliveUnmovableDifference(data: Symbol): Unit
  def aliveRefCheck(data: Symbol): Unit
  def copyRec(dst: MemExpr, src: MemExpr, sigId: Symbol): Unit
  def movVST(dst: IR, src: IR): Unit
}

class Assembler extends OldAssembler with MemoryAccess with SymbolicObjectControl with NewIsaParts { self =>
  protected class NewIsaBits extends Bits {
    override def seg = segment

    override def addFixup(fixup: Fixup): Unit = {
      shouldNotReachHere(s"switch to old isa, $fixup")
    }

    override def op(opcode: Int): Bits = {
      shouldNotReachHere(s"switch to old isa, $opcode")
    }

    override def op(prefix: OpcodePrefix, opcode: Int): Bits = {
      shouldNotReachHere(s"switch to old isa, $prefix, $opcode")
    }

    override def op(width: AsmWidth, opcode32: Int, opcode64: Int): Bits = {
      shouldNotReachHere(s"switch to old isa, $width, $opcode32, $opcode64")
    }

    override def op(prefix: OpcodePrefix, width: AsmWidth, opcode32: Int, opcode64: Int): Bits = {
      shouldNotReachHere(s"switch to old isa, $prefix, $width, $opcode32, $opcode64")
    }

    override def id16(id16: Symbol): Bits = {
      addFixupISA12(new Relocation(CBC_ID16, id16))
      this
    }

    override def id32(id32: Symbol): Bits = {
      addFixupISA12(new Relocation(CBC_ID32, id32))
      this
    }
  }

  override protected val emit: NewIsaBits = new NewIsaBits

  private[isa12] def addFixupISA12(f: Fixup): Unit = {
    super.addFixup(f)
  }

  override def add (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Add,  w, d, l, r)
  override def sub (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Sub,  w, d, l, r)
  override def mul (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Mul,  w, d, l, r)
  override def pow (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Pow,  w, d, l, r)
  override def and (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.And,  w, d, l, r)
  override def or  (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Or,   w, d, l, r)
  override def xor (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.Xor,  w, d, l, r)
  override def div (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.SDiv, w, d, l, r, prohibitB2r = true)
  override def rem (w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.SRem, w, d, l, r, prohibitB2r = true)
  override def udiv(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.UDiv, w, d, l, r)
  override def urem(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.URem, w, d, l, r)
  override def lsl(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.LSL,  w, d, l, r)
  override def lsr(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.LSR, w, d, l, r)
  override def asr(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genCommon(Common.ASR,  w, d, l, r)

  override def neg(ird: IR, irs: IR, w: AsmWidth): Unit = {
    if (ird == irs) {
      genB2rr(IRZ, irs, Common.Sub, Width(w))
    } else {
      sub(w, ird, IRZ, irs)
    }
  }

  override def addi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.Add,  w, Sign.Signed, d, l, imm)
  override def subi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = addi(w, d, l, -imm)
  override def muli (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.Mul,  w, Sign.Signed, d, l, imm)
  override def powi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.Pow,  w, Sign.Signed, d, l, imm)
  override def andi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.And,  w, Sign.Signed, d, l, imm)
  override def ori  (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.Or ,  w, Sign.Signed, d, l, imm)
  override def xori (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.Xor,  w, Sign.Signed, d, l, imm)
  override def divi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.SDiv, w, Sign.Signed, d, l, imm, prohibitB2r = true)
  override def remi (w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.SRem, w, Sign.Signed, d, l, imm, prohibitB2r = true)
  override def udivi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.UDiv, w, Sign.Unsigned, d, l, imm)
  override def uremi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genCommonImm(Common.URem, w, Sign.Unsigned, d, l, imm)

  override def lsli(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = {
    assert(0 < imm && imm < w.nbits)
    genCommonImm(Common.LSL, w, Sign.Unsigned, d, l, imm - 1)
  }

  override def lsri(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = {
    assert(0 < imm && imm < w.nbits)
    genCommonImm(Common.LSR, w, Sign.Unsigned, d, l, imm - 1)
  }

  override def asri(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = {
    assert(0 < imm && imm < w.nbits)
    genCommonImm(Common.ASR, w, Sign.Unsigned, d, l, imm - 1)
  }

  override def cadd(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Add, Width(width), Sign.Signed)
  override def csub(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Sub, Width(width), Sign.Signed)
  override def cmul(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Mul, Width(width), Sign.Signed)
  override def cdiv(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Div, Width(width), Sign.Signed)
  override def cuadd(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Add, Width(width), Sign.Unsigned)
  override def cusub(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Sub, Width(width), Sign.Unsigned)
  override def cumul(dst: IR, src1: IR, src2: IR, width: AsmWidth): Unit = genB3xrrrChecked(dst, src1, src2, Checked.Mul, Width(width), Sign.Unsigned)
  override def caddi(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Add, Width(width), Sign.Signed)
  override def csubi(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Sub, Width(width), Sign.Signed)
  override def cmuli(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Mul, Width(width), Sign.Signed)
  override def cuaddi(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Add, Width(width), Sign.Unsigned)
  override def cusubi(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Sub, Width(width), Sign.Unsigned)
  override def cumuli(dst: IR, src1: IR, src2: Long, width: AsmWidth): Unit = genB3xrrt4iKChecked(dst, src1, src2, Checked.Mul, Width(width), Sign.Unsigned)

  override def bcc(op: BranchOp, src1: IR, src2: IR, width: AsmWidth, target: Label): Unit = {
    assert(!op.isFloatingPoint, s"$op")
    addFixupISA12(new Fixups.BCC(op, src1, src2, width, target))
  }

  override def bcc(op: BranchOp, src1: FR, src2: FR, width: AsmWidth, target: Label): Unit = {
    assert(op.isFloatingPoint, s"$op")
    addFixupISA12(new Fixups.BCC(op, src1, src2, width, target))
  }

  override def bcc(op: BranchOp, src: IR, imm: Long, width: AsmWidth, target: Label): Unit = {
    assert(!op.isFloatingPoint, s"$op") // TODO implement floating point branch imm
    if (imm == 0 && !op.isTestBit) {
      addFixupISA12(new Fixups.BCC(op, src, IR.IRZ, width, target))
    } else {
      addFixupISA12(new Fixups.BCCImm(op, src, imm, width, target))
    }
  }

  override def scc(_op: BranchOp, dst: IR, _src1: IR, _src2: IR, _width: AsmWidth): Unit = {
    val (op, swap, _) = FExtBCC.normalize(_op)
    val (src1, src2) = if swap then (_src2, _src1) else (_src1, _src2)
    val width = Width(_width)
    genB3xrrr(dst, src1, src2, SetIf.prepareBits(CC.from(op), width))
  }

  override def scc(_op: BranchOp, dst: IR, _src1: FR, _src2: FR, _width: AsmWidth): Unit = {
    val (op, swap, _) = FExtBCC.normalize(_op)
    val (src1, src2) = if swap then (_src2, _src1) else (_src1, _src2)
    val width = Width(_width)
    genB3xrrr(dst, src1, src2, SetIf.prepareBits(CC.from(op), width))
  }

  override def scc(_op: BranchOp, dst: IR, src: IR, _imm: Long, _width: AsmWidth): Unit = {
    val (op, imm) = OldAssembler.normalizeImm(_op, _imm, _width)
    val cc = CC.from(op)
    val sign = cc match {
      case CC.ULT | CC.UGE | CC.TESTBIT | CC.TESTNBIT => Sign.Unsigned
      case _ => Sign.Signed
    }
    val width = Width(_width)
    genB3xrrt4iK(dst, src, imm, SetIf.prepareBits(cc, width), width, sign)
  }

  override def jmp(target: Label): Unit = addFixupISA12(new Fixups.Jump(target))
  override def bttCHA(arg: IR, negated: Boolean, target: Label): Unit = addFixupISA12(new Fixups.BCHA(arg, negated, target))
  override def bttLevel(arg: IR, negated: Boolean, level: Int, target: Label): Unit = addFixupISA12(new Fixups.BTTLevel(arg, level, negated, target))
  override def bttCone(arg: IR, negated: Boolean, sig_id: Symbol, closed: Boolean, target: Label): Unit = addFixupISA12(new Fixups.BTTBySymbol(if (closed) CLOSED_CONE else OPEN_CONE, arg, sig_id, negated, target))
  override def bttPoint(arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit = addFixupISA12(new Fixups.BTTBySymbol(POINT, arg, sig_id, negated, target))
  override def bttIOFC (arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit = addFixupISA12(new Fixups.BTTBySymbol(IOFC,  arg, sig_id, negated, target))
  override def bttIOFI (arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit = addFixupISA12(new Fixups.BTTBySymbol(IOFI,  arg, sig_id, negated, target))
  override def bttIOFA (arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit = addFixupISA12(new Fixups.BTTBySymbol(IOFA,  arg, sig_id, negated, target))

  private[isa12] def genB3xrrrChecked(d: IR, l: IR, r: IR, op: Checked, width: Width, sign: Sign): Unit = {
    genB3xrrr(d, l, r, Checked.prepareBits(op, width, sign))
  }

  private[isa12] def genB3xrrt4iKChecked(dst: IR, l: IR, imm: Long, op: Checked, width: Width, sign: Sign): Unit = {
    val parts = Checked.prepareBits(op, width, sign)
    genB3xrrt4iK(dst, l, imm, parts, width, sign)
  }

  private[isa12] def genCommon(op: Common, w: AsmWidth, d: IR, l: IR, r: IR, prohibitB2r: Boolean = false): Unit = {
    if (d == l && op.b2rAllowed && !prohibitB2r) {
      genB2rr(d, r, op, Width(w))
    } else {
      genB3xrrr(d, l, r, Common.prepareBitsForB3Formats(op, Width(w)))
    }
  }

  private[isa12] def genCommonImm(op: Common, w: AsmWidth, sign: Sign, d: IR, l: IR, imm: Long, prohibitB2r: Boolean = false): Unit = {
    if (d == l && op.b2rAllowed && !prohibitB2r) {
      genB2riKCommon(d, imm, op, Width(w))
    } else {
      val parts = Common.prepareBitsForB3Formats(op, Width(w))
      genB3xrrt4iK(d, l, imm, parts, Width(w), sign)
    }
  }

  private def genB2riKCommon(dst: IR, imm: Long, op: Common, width: Width): Unit = {
    if (isNBitsSigned(imm, 4)) {
      genB2ri4(dst, imm.toInt, op, width)
    } else {
      val low16 = bitsSigned(imm, 0, 16)
      if (isNBitsSigned(low16, 4)) {
        val hi48 = (imm >> 16) + ((low16 >>> 3) & 1)
        val immext = getImmext(hi48)
        if (immext.isDefined) {
          genImmExt(immext.get)
        }
        genB2ri4(dst, (low16 & 0xF).toInt, op, width)
      } else {
        val parts = Common.prepareBitsForB3Formats(op, width)
        genB3xrrt4iK(dst, dst, imm, parts, width, Sign.Signed)
      }
    }
  }

  private def genB3xrrt4iK(dst: IR, l: IR, imm: Long, parts: B3xrr_parts, width: Width, sign: Sign): Unit = {
    val imm32 = imm.toInt

    if (isNBits(imm, 4, sign)) {
      genB3xrrt4i0(dst, l, imm32, parts)
    } else if (isNBits(imm, 12, sign)) {
      genB3xrrt4i8(dst, l, imm32 & 0xF, (imm32 >>> 4) & 0xFF, parts)
    } else if (isNBits(imm, 16, sign)) {
      genB3xrrt4i16(dst, l, 0, imm32 & 0xFFFF, parts)
    } else if (sign == Sign.Unsigned) {
      // memory compact encoding does not support unsigned imm
      genImmExt(getImmext(imm >> 16).get)
      genB3xrrt4i16(dst, l, 0, imm32 & 0xFFFF, parts)
    } else {
      val encImm @ EncodedImmParts(t4, shortImm, calculatedK, immext) = calculateMemoryCompactImm(imm, width)
      if (immext.isDefined) {
        genImmExt(immext.get)
      }
      calculatedK match {
        case K0 =>
          val packedImm = (shortImm << K0.bits) | t4
          assert((packedImm & rightNBits32(encImm.encodedImmBits)) == packedImm)
          genB3xrrt4i0(dst, l, t4 = t4, parts)

        case K8 =>
          val packedImm = (shortImm << K0.bits) | t4
          assert((packedImm & rightNBits32(encImm.encodedImmBits)) == packedImm)
          genB3xrrt4i8(dst, l, t4 = t4, shortImm, parts)

        case K16 =>
          assert((shortImm & rightNBits32(encImm.encodedImmBits)) == shortImm)
          genB3xrrt4i16(dst, l, t4 = t4, shortImm, parts)
      }
    }
  }

  override def mov(dst: Register, src: Register, hasMemExpr: Boolean, reference: Boolean = false): Unit = {
    (dst, src) match {
      case (_, _) if hasMemExpr => super.mov(dst, src, hasMemExpr, reference.ensuring(!_)) // TODO support generic MemExpr in Isa12
      case (dst: IR, src: IR) if reference => movRef(dst, src)
      case (dst: IR, src: IR) => mov(dst, src, AsmWidth.W64)
      case (dst: FR, src: FR) => fmov(dst, src, AsmWidth.W64)
      case (dst: FR, src: IR) => movi2f(dst, src, AsmWidth.W64)
      case (dst: IR, src: FR) => movf2i(dst, src, AsmWidth.W64)
    }
  }

  override def movVST(dst: IR, src: IR): Unit = genB2rr(dst, src, Common.MovVst, W32)

  override def mov(dst: Register, src: MemExpr): Unit = {
    if (src.isGeneric) {
      genGenericMemExprLdSt(isStore = false, dst, src)
    } else {
      genMemExprLdSt(isStore = false, dst, src)
    }
  }

  override def mov(dst: MemExpr, src: Register): Unit = {
    if (dst.isGeneric) {
      genGenericMemExprLdSt(isStore = true, src, dst)
    } else {
      genMemExprLdSt(isStore = true, src, dst)
    }
  }

  override def mov(dst: MemExpr, src: MemExpr): Unit = {
    (dst.body, src.body) match {
      case (Array(ohm: OffHeapMemory), Array(s: Symbol)) if dst.isGeneric && src.isGeneric =>
        s match {
          case _: FieldReference =>
            genGenericMemExprLdSt(isStore = false, dst.headEncoding, src, ohm)
          case _ =>
            genB3xrrzII(SOC.B3xrrzII.CopyVst, dst.headEncoding, src.headEncoding, ohm, s)
        }
      case _ => shouldNotReachHere(s"$dst $src")
    }
  }

  override def copyRec(dst: MemExpr, src: MemExpr, sigId: Symbol): Unit = {
    assert(!dst.isGeneric && !src.isGeneric)
    genCopyRec(dst, src, sigId)
  }

  override def recordCopy(dst: IR, src: IR, sig_id: Symbol): Unit = {
    copyRec(MemExpr(dst, CbcTypeKind.REC), MemExpr(src, CbcTypeKind.REC), sig_id)
  }

  override def mov(dst: IR, src: IR, width: AsmWidth): Unit = genB2rr(dst, src, Common.Mov, Width(width))
  override def movi32(r: IR, imm: Int): Unit = if isNBitsSigned(imm, 4) then genB2ri4(r, imm, Common.Mov, Width.W32) else addi(AsmWidth.W32, r, IRZ, imm)
  override def movi64(r: IR, imm: Long): Unit = if isNBitsSigned(imm, 4) then genB2ri4(r, imm.toInt, Common.Mov, Width.W64) else addi(AsmWidth.W64, r, IRZ, imm)

  def movRef(dst: IR, src: IR): Unit = genB2rr(dst, src, Common.MovRef, W64)

  override def movi32(dst: MemExpr, imm: Int): Unit = {
    genMemExprStImm(dst, imm, W32)
  }

  override def movi64(dst: MemExpr, imm: Long): Unit = {
    genMemExprStImm(dst, imm, W64)
  }

  override def bfx(dst: IR, src: IR, resW: AsmWidth, argW: AsmWidth, sx: Boolean, offset: Int, size: Int): Unit = {
    debugAssert(size > 0)
    debugAssert(offset + size <= argW.nbits)

    (Width(resW), Width(argW), offset, size) match {
      case BFX.Extend(t4) =>
        if (dst == src) {
          genB2ri4(dst, t4, Common.Extend, Sign(sx))
        } else {
          genB3xrrt4i0(dst, src, t4, Common.prepareBitsForB3Formats(Common.Extend, Sign(sx)))
        }

      case BFX.Shift(nbytes, imm) =>
        if (sx) {
          asri(AsmWidth(nbytes), dst, src, imm)
        } else {
          lsri(AsmWidth(nbytes), dst, src, imm)
        }

      case BFX.B3xrrt4i8(t4, imm8) =>
        genB3xrrt4i8(dst, src, t4, imm8, Checked.prepareBFX(Width(resW), Width(argW), Sign(sx)))

      case _ => shouldNotReachHere(s"$dst $src $resW $argW $sx $offset $size")
    }
  }

  override def ldarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit =
    genLdArr(rd, ra, ri, indexCheck = false, ArrayType.Raw, LoadAccessKind.from(CbcTypeKind(asmType)))
  override def ldarrObj(rd: Register, ra: IR, ri: IR): Unit =
    genLdArr(rd, ra, ri, indexCheck = false, ArrayType.Raw, LoadAccessKind.LD_REF)
  override def ldarrRecord(rd: IR, ra: IR, ri: IR, sig_id: Symbol): Unit =
    genLdArrRecord(rd, ra, ri, sig_id)

  override def starr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit =
    genStArr(rv, ra, ri, indexCheck = false, ArrayType.Raw, StoreAccessKind.from(CbcTypeKind(asmType)))
  override def starrObj(ra: IR, ri: IR, rv: Register): Unit =
    genStArr(rv, ra, ri, indexCheck = false, ArrayType.Raw, StoreAccessKind.ST_REF)

  override def javaLdarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit =
    genLdArr(rd, ra, ri, indexCheck = false, ArrayType.Java,
      if asmType.isPointer then LoadAccessKind.LD_REF else LoadAccessKind.from(CbcTypeKind(asmType)))

  override def javaStarr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit =
    genStArr(rv, ra, ri, indexCheck = false, ArrayType.Java,
      if asmType.isPointer then StoreAccessKind.ST_REF else StoreAccessKind.from(CbcTypeKind(asmType)))

  override def prepareRecord(ts: StackSlot.Typed): Unit = genAdr(IR.IRZ, ts)

  override def ldstackrec(dst: IR, ts: StackSlot.Typed): Unit = genLdStackRec(dst, ts)
  override def ldstackobj(dst: IR, ts: StackSlot.Typed): Unit = genAdr(dst, ts)
  override def lea_us(dst: IR, us: StackSlot.Untyped): Unit = genAdr(dst, us)
  override def lea_static(dst: IR, field_id: Symbol): Unit = genAdr(dst, field_id)

  override def combineHostAndOffset(dst: IR, src: MemExpr): Unit = genAdr(dst, src)
  override def offsetFromHost(dstHost: IR, dstOffset: IR, r: IR): Unit = genMakeHandle(dstHost, dstOffset, r)
  override def offsetFromHost(dstHost: IR, dstOffset: IR, src: MemExpr): Unit = genMakeHandle(dstHost, dstOffset, src)

  private def genB2rr(d: IR, r: IR, op: Common, width: Width): Unit = {
    emit.seg.putW8(B2rr.format(Common.prepareBitsForB2Formats(op, width)))
    emit.seg.putW8(pack8(d, r))
  }

  private def genB2ri4(d: IR, r: Int, op: Common, width: Width): Unit = genB2ri4(d, r, op, width.opcCommon)
  private def genB2ri4(d: IR, r: Int, op: Common, sign: Sign): Unit = genB2ri4(d, r, op, sign.opc)

  private def genB2ri4(d: IR, r: Int, op: Common, b1: Int): Unit = {
    emit.seg.putW8(B2ri4.format(Common.prepareBitsForB2Formats(op, b1)))
    emit.seg.putW8(pack8(r & 0xF, d))
  }

  private def genB3xrrr(d: Register, l: Register, r: Register, parts: B3xrr_parts): Unit = {
    emit.seg.putW8(B3xrrr.format(parts.low3BitsOfFormatByte))
    emit.seg.putW8(pack8(parts.low4BitsOfSecondByte, d))
    emit.seg.putW8(pack8(l, r))
  }

  private def genB3xrrt4i0(d: Register, l: Register, t4: Int, parts: B3xrr_parts): Unit = {
    genB3xrrt4iKBase(K0, d, l, t4, parts)
  }

  private def genB3xrrt4i8(d: Register, l: Register, t4: Int, imm8: Int, parts: B3xrr_parts): Unit = {
    genB3xrrt4iKBase(K8, d, l, t4, parts)
    emit.seg.putW8(imm8)
  }

  private def genB3xrrt4i16(d: Register, l: Register, t4: Int, imm16: Int, parts: B3xrr_parts): Unit = {
    genB3xrrt4iKBase(K16, d, l, t4, parts)
    emit.seg.putW16(imm16)
  }

  private def genB3xrrt4iKBase(k: K, d: Register, l: Register, t4: Int, parts: B3xrr_parts): Unit = {
    emit.seg.putW8(B3xrrt4iK.format(k, parts.low3BitsOfFormatByte))
    emit.seg.putW8(pack8(parts.low4BitsOfSecondByte, d))
    emit.seg.putW8(pack8(l, t4 & 0xF))
  }

  // Float operations

  override def fadd(w: AsmWidth, d: FR, l: FR, r: FR): Unit = genB3xrrrFloat(d, l, r, FloatOperations.FAdd, Width(w))
  override def fsub(w: AsmWidth, d: FR, l: FR, r: FR): Unit = genB3xrrrFloat(d, l, r, FloatOperations.FSub, Width(w))
  override def fmul(w: AsmWidth, d: FR, l: FR, r: FR): Unit = genB3xrrrFloat(d, l, r, FloatOperations.FMul, Width(w))
  override def fdiv(w: AsmWidth, d: FR, l: FR, r: FR): Unit = genB3xrrrFloat(d, l, r, FloatOperations.FDiv, Width(w))
  override def faddi(d: FR, l: FR, r: Double, w: AsmWidth): Unit = genB3xrrt4iK(d, l, r, FloatOperations.FAdd, Width(w))
  override def fsubi(d: FR, l: FR, r: Double, w: AsmWidth): Unit = genB3xrrt4iK(d, l, r, FloatOperations.FSub, Width(w))
  override def fmuli(d: FR, l: FR, r: Double, w: AsmWidth): Unit = genB3xrrt4iK(d, l, r, FloatOperations.FMul, Width(w))
  override def fdivi(d: FR, l: FR, r: Double, w: AsmWidth): Unit = genB3xrrt4iK(d, l, r, FloatOperations.FDiv, Width(w))

  override def fmovi(r: FR, fimm: Double, w: AsmWidth): Unit = fmovi(r, r, fimm, w)

  def fmov(frd: FR, frs: FR, w: AsmWidth): Unit = fmov(frd, frd, frs, w)

  def fmov (frd2: FR, frd1: FR, frs: FR,     w: AsmWidth): Unit = genB3xrrrFloat(frd2, frd1, frs, FloatOperations.FMov, Width(w))
  private def fmovi(frd2: FR, frd1: FR, frs: Double, w: AsmWidth): Unit = genB3xrrt4iK  (frd2, frd1, frs, FloatOperations.FMov, Width(w))

  def movi2f(frd: FR, irs: IR, w: AsmWidth): Unit = genB3xrrrFloat(frd, frd, irs, FloatOperations.Movi2f, Width(w))
  def movf2i(frd: IR, irs: FR, w: AsmWidth): Unit = genB3xrrrFloat(frd, frd, irs, FloatOperations.Movf2i, Width(w))

  override def fneg(frd: FR, frs: FR, w: AsmWidth): Unit = fneg(frd, frd, frs, w)
  override def fabs(frd: FR, frs: FR, w: AsmWidth): Unit = fabs(frd, frd, frs, w)
  override def fsqrt(frd: FR, frs: FR, w: AsmWidth): Unit = fsqrt(frd, frd, frs, w)

  def fneg (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit = genB3xrrrFloat(frd2, frd1, frs, FloatOperations.FNeg,  Width(w))
  def fabs (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit = genB3xrrrFloat(frd2, frd1, frs, FloatOperations.FAbs,  Width(w))
  def fsqrt(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit = genB3xrrrFloat(frd2, frd1, frs, FloatOperations.FSqrt, Width(w))

  override def convert(toType: AsmType, fromType: AsmType, to: Register, from: Register): Unit = {
    def extendWidthUpToW32(w: AsmWidth) = AsmWidth.apply(Math.max(w.nbytes, W32.nbytes))

    import AsmType.*
    import FloatOperations.*

    val parts = (fromType, toType) match {
      // Integer casts
      case _ if fromType.isIntegral && toType.isIntegral =>
        bfx(to.asInstanceOf[IR], from.asInstanceOf[IR], extendWidthUpToW32(toType.width), extendWidthUpToW32(fromType.width), toType.signed, offset = 0, size = toType.sizeInBits)
        return

      // T.to.fw
      case _ if fromType.isIntegral && toType.isFloatingPoint =>
        prepareConvertWithIntBits(toInteger = false, fromType.signed, fromType.width, toType.width)

      // fw.to.T
      case _ if fromType.isFloatingPoint && toType.isIntegral =>
        prepareConvertWithIntBits(toInteger = true, toType.signed, toType.width, fromType.width)

      // f32.to.F
      case (F32, _) if toType.isFloatingPoint =>
        prepareConvertBits(F32ToF, toType.width)

      // F.to.f32
      case (_, F32) if fromType.isFloatingPoint =>
        prepareConvertBits(FToF32, fromType.width)

      case _ => shouldNotReachHere(s"Unknown cast in convert: $fromType -> $toType")
    }
    genB3xrrt4i0(to, from, 0, parts)
  }

  private[isa12] def genB3xrrt4iK(dst: FR, l: FR, fimm: Double, op: FloatOperations, width: Width): Unit = {
    val parts = FloatOperations.prepareBits(op, width)

    val FloatImm.EncodeData(t4, k, iK, immext) = FloatImm.encode(fimm, width)

    if (immext != 0) {
      genImmExt(ImmEXT(ImmEXT.N.N48, Sign.Signed, immext))
    }
    k match {
      case K.K0 => assert(iK == 0)
        genB3xrrt4i0(dst, l, t4, parts)
      case K.K8 => genB3xrrt4i8(dst, l, t4, iK, parts)
      case K.K16 => genB3xrrt4i16(dst, l, t4, iK, parts)
    }
  }

  private[isa12] def genB3xrrrFloat(d: Register, l: Register, r: Register, op: FloatOperations, width: Width): Unit = {
    genB3xrrr(d, l, r, FloatOperations.prepareBits(op, width))
  }

  private[assembler] def genImmExt(ie: ImmEXT): Unit = {

    emit.seg.putW8(ImmEXT.calculateOPCode(ie))

    ie.n match {
      case N8 => emit.seg.putW8(ie.value.toInt)
      case N16 => emit.seg.putW16(ie.value.toInt)
      case N32 => emit.seg.putW32(ie.value.toInt)
      case N48 => emit.seg.putW32(ie.value.toInt); emit.seg.putW16((ie.value >> 32).toInt)
    }
  }

  override def nop(): Unit = {
    emit.seg.putW8(B1.Nop)
  }

  def ret(v: IR, w: AsmWidth): Unit = (Width(w): @unchecked) match {
    case Width.W32 => genB2xr(SOC.B2xr.Opc0100.Ret32, v)
    case Width.W64 => genB2xr(SOC.B2xr.Opc0100.Ret64, v)
  }

  def retRef(v: IR): Unit = ret(v, AsmWidth.W64) // TODO: mark as primitive/ref when ret.ref is separated from ret.64

  def fret(v: FR, w: AsmWidth): Unit = (Width(w): @unchecked) match {
    case Width.W32 => genB2xr(SOC.B2xr.Opc0100.Fret32, v)
    case Width.W64 => genB2xr(SOC.B2xr.Opc0100.Fret64, v)
  }

  override def throwEx(ex: IR): Unit = genB2xr(SOC.B2xr.Opc0100.Throw, ex)
  override def catchEx(dst: IR): Unit = genB2xr(SOC.B2xr.Opc0100.Catch, dst)

  def callDirect(rd: IR, methodId: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1000.CallDirect, rd, methodId)
  def callVirt(rd: IR, methodId: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1000.CallVirt, rd, methodId)

  def callInterf(rd: IR, sig_id: Symbol, methodId: Symbol): Unit = genB2xrII(SOC.B2xrII.CallInterf, rd, sig_id, methodId)
  def callInterfPlain(rd: IR, methodId: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1000.CallInterfPlain, rd, methodId)
  def callInterfRich(rx: IR, methodId: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1000.CallInterfRich, rx, methodId)
  def callInterfConst(rd: IR, enrichment: Int, methodId: Symbol): Unit = genB2xrII(SOC.B2xrII.CallInterfConst, rd, enrichment, methodId)

  override def callIndirect(targetReg: IR, sigId: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1000.CallIndirect, targetReg, sigId)

  override def divisorCheck(r: IR): Unit = genB2xr(SOC.B2xr.Opc0100.CheckDivZero64, r)
  override def nullcheck(r: IR): Unit = genB2xr(SOC.B2xr.Opc0100.CheckNull, r)

  override def lenarr(rl: IR, ra: IR): Unit = genLenArr(rl, ra, ArrayType.Raw)
  override def arrIC(ri: IR, rl: IR): Unit = genAIC(ri, rl, ArrayType.Raw)
  override def javaArrSC(arr: IR, value: IR): Unit = genArrStJava(arr, value, ArrayType.Raw)

  override def javaLenarr(rl: IR, ra: IR): Unit = genLenArr(rl, ra, ArrayType.Java)
  override def javaArrIC(ri: IR, rl: IR): Unit = genAIC(ri, rl, ArrayType.Java)

  override def gcpoint(): Unit = genB2xz(SOC.B2xr.Opc0101.Gcpoint)

  override def eopPack(dst: IR, obj: IR, enrichment: IR): Unit = genB3xrrr(SOC.B3xrrr.EopPack, dst, obj, enrichment)
  override def eopPack(dst: IR, obj: IR, enrichment_u16: Int): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.EopPack, dst, obj, enrichment_u16)
  override def eopPack(dst: IR, obj: IR, typeId: Symbol, interfaceId: Symbol): Unit = genB3xrrzII(SOC.B3xrrzII.EopPack, dst, obj, typeId, interfaceId)
  override def eopPlain(dst: IR, obj: IR): Unit = genB3xrrz(SOC.B3xrrr.EopToPlain, dst, obj)
  override def eopEnrichment(dst: IR, obj: IR): Unit = genB3xrrz(SOC.B3xrrr.EopGetRich, dst, obj)

  override def evacuate(): Unit = genB3xrrz(SOC.B3xrrr.Evacuate, IR.IR1, IR.IR1)
  override def singleton(dst: IR, sig_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1001.Singleton, dst, sig_id)

  override def newobj(sig_idx: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1011.Newobj, IR.IR1, sig_idx)
  override def newobjVST(ftc_idx: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1011.NewobjVst, IR.IR1, ftc_idx)

  override def newarr(sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1100.Newarr, IR.IR1, IR.IR2, sig_id)
  override def newarrVST(ftc_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1100.NewarrVst, IR.IR1, IR.IR2, ftc_id)
  override def newarrzv(ftc_sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1100.NewarrNoInit, IR.IR1, IR.IR2, ftc_sig_id)

  override def javaNewarr(sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1100.Newarr, IR.IR1, IR.IR2, sig_id)

  override def newarrfillnonconst(dst: IR, len: IR, value: IR, ftc_sig_id: Symbol): Unit = genB3xrrrI(SOC.B3xrrrI.Opc1100.Newarrfill, dst, len, value, ftc_sig_id)

  override def isInstanceOfClass(dst: IR, obj: IR, sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.Iof, dst, obj, sig_id)
  override def isInstanceOfInterface(dst: IR, obj: IR, sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.Iof, dst, obj, sig_id)
  override def isInstanceOfArray(dst: IR, obj: IR, sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.Iof, dst, obj, sig_id)

  override def weakCast(dst: IR, obj: IR, sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.WeakCast, dst, obj, sig_id)
  override def javaCheckCast(dst: IR, src: IR, sig_id: Symbol): Unit = genB3xrrzI(SOC.B3xrrrI.Opc1101.RichCheckcast, dst, src, sig_id)

  override def lea_cforeign(dst: IR, method_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1001.CfuncPtr, dst, method_id)

  override def loadTypeInfoSig(dst: IR, sig_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1010.LoadTypeInfoSig, dst, sig_id)
  override def loadTypeInfoFTC(dst: IR, ftc_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1010.LoadTypeInfoFTC, dst, ftc_id)
  override def loadTypeInfoObj(dst: IR, obj: IR): Unit = genB3xrrz(SOC.B3xrrr.LoadTypeInfoObj, dst, obj)

  def cFuncWrap(dst: IR, method_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1001.CfuncWrap, dst, method_id)

  override def javaClinit(sig_id: Symbol): Unit = genB2xzI16(SOC.B2xrI.Opc1010.JavaClinit, sig_id)
  override def packageInit(sig_id: Symbol): Unit = genB2xzI16(SOC.B2xrI.Opc1010.PkgInit, sig_id)
  override def packageInitCheck(sig_id: Symbol): Unit = genB2xzI16(SOC.B2xrI.Opc1010.PkgInitCheck, sig_id)

  override def covinc(locs: Array[(String, Array[Int])]): Unit = {
    genB2xz(SOC.B2xr.Opc0101.Covinc)
    addFixupISA12(CoverageLocs(locs))
  }

  override def beginLocalUnmovable(r: IR): Unit = genB2xr(SOC.B2xr.Opc0101.BeginUnmovable, r)
  override def endLocalUnmovable(r: IR): Unit = genB2xr(SOC.B2xr.Opc0101.EndUnmovable, r)

  override def initobj(ts: StackSlot.Typed): Unit = genB2xzI16(SOC.B2xrI.Opc1010.InitObj, ts.idx)
  override def zerorefs(ts: StackSlot.Typed): Unit = genB2xzI16(SOC.B2xrI.Opc1010.Zerorefs, ts.idx)

  override def aliveReference(data: Array[Byte]): Unit = aliveReference(new RawData(data, 0))
  override def unmovableReference(data: Array[Byte]): Unit = unmovableReference(new RawData(data, 0))
  override def aliveRefDifference(data: Array[Byte]): Unit = aliveRefDifference(new RawData(data, 0))
  override def aliveUnmovableDifference(data: Array[Byte]): Unit = aliveUnmovableDifference(new RawData(data, 0))
  override def aliveRefCheck(data: Array[Byte]): Unit = aliveRefCheck(new RawData(data, 0))

  def aliveReference(data: Symbol): Unit = genB2xzI32(SOC.B2xrI.Opc1010.AliveRef, data)
  def unmovableReference(data: Symbol): Unit = genB2xzI32(SOC.B2xrI.Opc1010.AliveUnmovable, data)
  def aliveRefDifference(data: Symbol): Unit = genB2xzI32(SOC.B2xrI.Opc1010.AliveRefDiff, data)
  def aliveUnmovableDifference(data: Symbol): Unit = genB2xzI32(SOC.B2xrI.Opc1010.AliveUnmovableDiff, data)
  def aliveRefCheck(data: Symbol): Unit = genB2xzI32(SOC.B2xrI.Opc1010.AliveRefCheck, data)

  override def arrFill(arr: IR, data: Array[Byte]): Unit = genB2xrI32(SOC.B2xrI.Opc1001.ArrfillData, arr, new RawData(data, 0))
  override def loadConstDataAddr(dst: IR, data: Array[Byte], alignment: Int): Unit = genB2xrI32(SOC.B2xrI.Opc1001.AdrData, dst, new RawData(data, alignment))
  override def javaLdaStr(dst: IR, string_id: Symbol): Unit = genB2xrI32(SOC.B2xrI.Opc1001.JavaStrConst, dst, string_id)

  def initConstString(ts: StackSlot.Typed, stringId: Symbol): Unit = genB2xzI16I32(SOC.B2xrII.InitStrConst, ts.idx, stringId)

  // UG operations

  override def callGTDSig(sig_idx: Symbol, method_id: Symbol): Unit = genB2xrII(SOC.B2xrII.CallGtdSig, IR.IR1, sig_idx, method_id)
  override def callGTDFTC(ftc_idx: Symbol, method_id: Symbol): Unit = genB2xrII(SOC.B2xrII.CallGtdFtc, IR.IR1, ftc_idx, method_id)

  override def callGFDSig(sig_idx: Symbol, method_id: Symbol): Unit = genB2xrII(SOC.B2xrII.CallAtcSig, IR.IR1, sig_idx, method_id)
  override def callGFDFTC(ftc_idx: Symbol, method_id: Symbol): Unit = genB2xrII(SOC.B2xrII.CallAtcFtc, IR.IR1, ftc_idx, method_id)

  override def callConstraint(ftc_idx: Symbol, method_id: Symbol): Unit = genB2xrII(SOC.B2xrII.CallConstraint, IR.IR1, ftc_idx, method_id)

  override def copyResultVST(rv: IR, rr: IR, ftc_symbol_id: Symbol): Unit = {
    genB3xrrrI(SOC.B3xrrrI.Opc1101.CopyVst, IR.IR1, rv, rr, ftc_symbol_id)
  }

  override def ohmsPtr(rd: IR, ohms: StackSlot.OffHeapMemory): Unit = genB2xrI16(SOC.B2xrI.Opc1001.AdrOhm, rd, ohms)

  override def doTypeVarIsRef(dst: IR, ftc_id: Symbol): Unit = genB2xrI16(SOC.B2xrI.Opc1010.TypeVarIsRef, dst, ftc_id)

  def putZeroes(n: Int): Unit = segment.putZeroes(n)
}

object Assembler {
  val BYTECODE_VERSION: Byte = 1

  object B1 { // op8
    inline def FormatBits: Int = 0x9 // 01001
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(low3Bits: Int): Int = e3(ByteMask) | s3(low3Bits)

    val Nop = B1.format(0x0) // 0100_1000
  }

  object B2rr { // op8_rx_ry
    inline def FormatBits: Int = 0x0
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(low4Bits: Int): Int = e4(ByteMask) | s4(low4Bits)
  }

  object B2ri4 { // op8_i4_rx
    inline def FormatBits: Int = 0x1
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(low4Bits: Int): Int = e4(ByteMask) | s4(low4Bits)
  }

  case class B3xrr_parts(low3BitsOfFormatByte: Int, low4BitsOfSecondByte: Int)

  object B3xrrt4iK { // op5_ccc3_cccc4_rd_rx_imm4
    inline def FormatBits: Int = 0x1
    inline def FormatFreeBits: Int = 5
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(k: K, low3Bits: Int): Int = e5(ByteMask) | p(s2(k.opc), freeBits = 3) | s3(low3Bits)

    /** For K0 and K8 imm bits length is {0, 8} + 4
      * For K16 and K32 bits length is {16, 32}
      */
    enum K(val bits: Int) {
      case K0  extends K(0)
      case K8  extends K(8)
      case K16 extends K(16)

      def opc: Int = ordinal
    }
  }

  object B3xrrr { // op8_op4_rd_rx_ry
    inline def FormatBits: Int = 0x8
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(low3Bits: Int): Int = e3(ByteMask) | s3(low3Bits)
  }

  enum OP7A {
    case Common
    case Checked
    case SetIf
    case Float

    def opc: Int = ordinal
  }

  enum Checked {
    case Add
    case Sub
    case Mul
    case Div
    case UAdd
    case USub
    case UMul
    case Pow

    inline def opc: Int = ordinal
    inline def format(width: Width): Int = p(s2(opc), freeBits = 2) | s2(width.opc)
    inline def format(resW: Width, argW: Width): Int = p(s2(opc), freeBits = 2) | p(s1(resW.opcCommon), 1) | s1(argW.opcCommon)
  }

  object Checked {
    val BFX: Checked = Checked.Div

    private[Assembler] def prepareBits(op: Checked, width: Width, sign: Sign) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Checked.opc), freeBits = 1) | s1(sign.opc),
        low4BitsOfSecondByte = op.format(width)
      )
    }

    private[Assembler] def prepareBFX(resW: Width, argW: Width, sign: Sign) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Checked.opc), freeBits = 1) | s1(sign.opc),
        low4BitsOfSecondByte = Checked.BFX.format(resW, argW)
      )
    }
  }

  enum Common {
    case Add  // 0b0000
    case Sub  // 0b0001, `ext.u` for B2ri4 and B3xrrt+iK
    case Mul  // 0b0010
    case And  // 0b0011
    case Or   // 0b0100
    case Xor  // 0b0101
    case SDiv // 0b0110, `mov` for B2r* encodings
    case SRem // 0b0111, `mov.vst` and `mov.ref` for B2rr

    case UDiv // 0b1000
    case URem // 0b1001
    case LSR  // 0b1010
    case ASR  // 0b1011
    case LSL  // 0b1100

    case Pow // 0b1101

    def b2rAllowed: Boolean = this.ordinal >> 3 == 0
  }

  object Common {
    final val Mov = Common.SDiv
    final val MovVst = Common.SRem
    final val MovRef = MovVst

    final val Extend = Common.Sub

    private[Assembler] def prepareBitsForB2Formats(op: Common, width: Width): Int = prepareBitsForB2Formats(op, width.opcCommon)
    private[Assembler] def prepareBitsForB2Formats(op: Common, b1: Int): Int = p(s3(op.ordinal), freeBits = 1) | s1(b1)

    private[Assembler] def prepareBitsForB3Formats(op: Common, sign: Sign): B3xrr_parts = prepareBitsForB3Formats(op, sign.opc)
    private[Assembler] def prepareBitsForB3Formats(op: Common, width: Width): B3xrr_parts = prepareBitsForB3Formats(op, width.opcCommon)

    private[Assembler] def prepareBitsForB3Formats(op: Common, b1: Int): B3xrr_parts = {
      val opCode = op.ordinal
      val page = opCode >>> 3
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Common.opc), freeBits = 1) | s1(page),
        low4BitsOfSecondByte = p(s3(opCode & 0x7), freeBits = 1) | s1(b1)
      )
    }
  }

  enum Sign {
    case Signed
    case Unsigned

    def opc: Int = ordinal
  }

  object Sign {
    inline def apply(signed: Boolean): Sign = if (signed) Sign.Signed else Sign.Unsigned
  }

  enum CC {
    case EQ
    case NE
    case LT
    case GE
    case ULT
    case UGE
    case REQ
    case RNE
    case FEQ
    case FNE
    case FLT
    case FNLT
    case FGE
    case FNGE
    case TESTZ
    case TESTNZ

    def opc(width: Width): Int = p(s4(ordinal), 1) | s1(width.opcCommon)
    def opcWithoutPage(width: Width): Int = opc(width) & 0xF
    def page: Int = s1(ordinal >> 3)
  }
  
  object CC {
    val TESTBIT: CC = REQ
    val TESTNBIT: CC = RNE

    /** Any case not mentioned below should be transformed in one of mentioned.
      * For example:
      *   integral:          x  <= y ~~ y  >= x
      *   floating-point:    x !>= y ~~ y !<= x
      *   integral constant: x  >= c ~~ x > c - 1 and e.t.c. (edge cases must be optimized with identities to avoid integer overflow)
      */
    def from(op: BranchOp): CC = tryFrom(op).getOrElse {
      shouldNotReachHere(s"Unexpected kind $op")
    }

    def tryFrom(op: BranchOp): Option[CC] = condOpt(op) {
      case BranchOp.EQ  => CC.EQ
      case BranchOp.NE  => CC.NE
      case BranchOp.LT  => CC.LT
      case BranchOp.GE  => CC.GE
      case BranchOp.ULT => CC.ULT
      case BranchOp.UGE => CC.UGE

      case BranchOp.REQ      => CC.REQ
      case BranchOp.RNE      => CC.RNE
      case BranchOp.TESTBIT  => CC.TESTBIT
      case BranchOp.TESTNBIT => CC.TESTNBIT

      case BranchOp.FEQ  => CC.FEQ
      case BranchOp.FNE  => CC.FNE
      case BranchOp.FLT  => CC.FLT
      case BranchOp.FNLT => CC.FNLT
      case BranchOp.FGE  => CC.FGE
      case BranchOp.FNGE => CC.FNGE

      case BranchOp.TESTZ  => CC.TESTZ
      case BranchOp.TESTNZ => CC.TESTNZ
    }
  }

  // TODO as this class more and more resembles assembler.Width, consider replacing it's uses
  enum Width(val nbytes: Int) {
    case W8  extends Width(1)
    case W16 extends Width(2)
    case W32 extends Width(4)
    case W64 extends Width(8)

    def opc: Int = ordinal
    def opcCommon: Int = {
      debugAssert((opc & 0x2) != 0)
      opc - W32.opc
    }

    def nbits: Int = nbytes * 8
  }

  object Width {
    def apply(w: AsmWidth): Width = (w: @unchecked) match {
      case AsmWidth.W8  => Width.W8
      case AsmWidth.W16 => Width.W16
      case AsmWidth.W32 => Width.W32
      case AsmWidth.W64 | AsmWidth.WPTR => Width.W64
    }
  }

  object BFX {
    object Extend {
      def unapply(packed: (Width, Width, Int, Int)): Option[Int] = condOpt(packed) {
        case (resW, argW, 0, size) if size % 8 == 0 && isNBitsUnsigned(size / 8 - 1, 2) =>
          p(s1(resW.opcCommon), 3) | p(s1(argW.opcCommon), 2) | (size / 8 - 1)
      }
    }
    // encoded as shift
    object Shift {
      def unapply(packed: (Width, Width, Int, Int)): Option[(Int, Int)] = condOpt(packed) {
        case (resW, argW, offset, size) if resW == argW && (offset + size) == argW.nbits =>
          (argW.nbytes, offset)
      }
    }
    object B3xrrt4i8 {
      /** Unpack (t4, imm8) */
      def unapply(packed: (Width, Width, Int, Int)): Option[(Int, Int)] = condOpt(packed) {
        case (_, _, offset, size) if isNBitsUnsigned(offset, 6) && isNBitsUnsigned(size, 6) =>
          val packed12 = p(s6(size), freeBits = 6) | s6(offset)
          (packed12 & 0xF, packed12 >>> 4)
      }
    }
  }

  def getImmext(imm: Long): Option[ImmEXT] = if (imm != 0) {
    import ImmEXT.N
    def getSizeAndSign: (ImmEXT.N, Sign) = {
      for (size <- Seq(N8, N16, N32) ; sign <- Sign.values) {
        if (isNBits(imm << 16, size.nBits + 16, sign)) {
          return (size, sign)
        }
      }
      (N.N48, Sign.Signed)
    }

    val (size, sign) = getSizeAndSign
    Some(ImmEXT(size, sign, imm & rightNBits64(size.nBits)))
  } else None

  case class ImmEXT(n: ImmEXT.N, sign: Sign, value: Long) {
    def decodeImmEXT(w: Width): Long = {
      (if (sign == Sign.Signed) signExtend(value, n.nBits) else value & rightNBits64(n.nBits)) << 16
    }
    
    def genSize = n.nBits / 8 + 1
  }

  object ImmEXT {
    private val opc: Int = 0xe // 0b01110 should be shifted to left by 3

    enum N(val nBits: Int) {
      case N8 extends N(8)
      case N16 extends N(16)
      case N32 extends N(32)
      case N48 extends N(48)
    }

    def calculateOPCode(imm: ImmEXT) = {
      assert(!(imm.sign == Sign.Unsigned && imm.n == N.N48)) // not encodable, opc 0b01110111 is reserved

      p(opc, 3) | p(imm.sign.opc, 2) | s2(imm.n.ordinal)
    }
  }

  private inline def debugAssert(inline f: Boolean): Unit = {
    inline val debugEnabled = true // it's declared in method to avoid messing with Scala incremental compilation
    inline if (debugEnabled) {
      assert(f)
    }
  }

  enum FloatOperations {
    case FAdd
    case FSub
    case FMul
    case FDiv

    case FMov
    case FNeg
    case FAbs
    case FSqrt

    case Movi2f
    case Movf2i

    inline def opc: Int = ordinal
  }

  object FloatOperations {
    val F32ToF = FAbs  // only in B3xrrt+iK format (t should be 0, K should be 0)
    val FToF32 = FSqrt // only in B3xrrt+iK format (t should be 0, K should be 0)

    private[Assembler] def prepareBits(op: FloatOperations, width: Width) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | s1(width.opcCommon),
        low4BitsOfSecondByte = s4(op.opc)
      )
    }

    private[Assembler] def prepareConvertWithIntBits(toInteger: Boolean, isSigned: Boolean, tWidth: AsmWidth, width: AsmWidth) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | s1(Width(width).opcCommon),
        low4BitsOfSecondByte = p(1, 3) | p(Sign(isSigned).opc, 2) | p(Width(tWidth).opcCommon, 1) | (if (toInteger) 1 else 0)
      )
    }

    private[Assembler] def prepareConvertBits(op: FloatOperations, width: AsmWidth) = {
      assert(op == FloatOperations.F32ToF || op == FToF32)
      assert(width == AsmWidth.W16 || width == AsmWidth.W64)

      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | (if (width == AsmWidth.W64) 1 else 0),
        low4BitsOfSecondByte = s4(op.opc)
      )
    }
  }

  object SetIf {
    private[Assembler] def prepareBits(op: CC, width: Width): B3xrr_parts = {
      assert(!(op == CC.REQ || op == CC.RNE) || width == W64)
      val opCode = op.ordinal
      val page = opCode >>> 3
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.SetIf.opc), freeBits = 1) | s1(page),
        low4BitsOfSecondByte = p(s3(opCode & 0x7), freeBits = 1) | s1(width.opcCommon)
      )
    }
  }

  private[isa12] def pack8 (r1: Register, r2: Register): Int = pack8(r1.idx, r2.idx)
  private[isa12] def pack8 (r1: Register, v2: Int     ): Int = pack8(r1.idx, v2)
  private[isa12] def pack8 (v1: Int,      r2: Register): Int = pack8(v1, r2.idx)
  private[isa12] def pack16(r: Register,  high12: Int ): Int = p(s(high12, 12), freeBits = 4) | s4(r.idx)
  private[isa12] def pack8 (low4: Int,    high4: Int  ): Int = p(s4(high4), freeBits = 4) | s4(low4)
  private[isa12] def pack16(low8: Int,    high8: Int  ): Int = p(s8(high8), freeBits = 8) | s8(low8)

  inline def p(value: Int, freeBits: Int): Int = value << freeBits

  /** Checks, that only right n-bits are set. */
  inline def s(value: Int, n: Int): Int = {
    debugAssert((value & rightNBits32(n)) == value)
    value
  }

  inline def s8(value: Int): Int = s(value, 8)
  inline def s7(value: Int): Int = s(value, 7)
  inline def s6(value: Int): Int = s(value, 6)
  inline def s5(value: Int): Int = s(value, 5)
  inline def s4(value: Int): Int = s(value, 4)
  inline def s3(value: Int): Int = s(value, 3)
  inline def s2(value: Int): Int = s(value, 2)
  inline def s1(value: Int): Int = s(value, 1)

  /** Checks, that right n-bits are empty. */
  inline def e(value: Int, n: Int): Int = {
    debugAssert((value & rightNBits32(n)) == 0)
    value
  }

  inline def e8(value: Int): Int = e(value, 8)
  inline def e7(value: Int): Int = e(value, 7)
  inline def e6(value: Int): Int = e(value, 6)
  inline def e5(value: Int): Int = e(value, 5)
  inline def e4(value: Int): Int = e(value, 4)
  inline def e3(value: Int): Int = e(value, 3)
  inline def e2(value: Int): Int = e(value, 2)
  inline def e1(value: Int): Int = e(value, 1)

  def isNBits(v: Long, n: Int, sign: Sign): Boolean = MathUtils.isNBits(sign == Sign.Signed, v, n)
  def isNBits(v: Int,  n: Int, sign: Sign): Boolean = MathUtils.isNBits(sign == Sign.Signed, v, n)
}
