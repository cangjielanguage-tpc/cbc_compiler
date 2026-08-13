/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.IReg
import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.Assembler.{canBeEncodedInBCC, normalizeImm}
import com.huawei.excelsior.jet.assembler.cbc.Bits.*
import com.huawei.excelsior.jet.assembler.cbc.FExtBCC
import com.huawei.excelsior.jet.assembler.cbc.Fixups.*
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT.Kind.*
import com.huawei.excelsior.jet.assembler.cbc.FormatExtension
import com.huawei.excelsior.jet.assembler.cbc.Local.Loc8
import com.huawei.excelsior.jet.assembler.cbc.OpcodePrefix
import com.huawei.excelsior.jet.assembler.cbc.OpcodePrefix.*
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.fixups.CoverageLocs
import com.huawei.excelsior.jet.assembler.{AsmEmitter, AsmType, Fixup, Label, Literal, Segment, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils.{isNBits, rangeMask32, rangeMask64, rightNBits64}

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits

/**
  * Poor class hierarchy of cbc assemblers resulted to this trait,
  * that attempts to provide an actual interface to use for CodeGeneratorCBC.
  *
  * The initial reason for appearance of this interface is to find which functions are actually been called
  * from the generator. So, it is just a list of functions now.
  *
  * TODO: Properly name, document and organize the codebase. Get rid of ISA12 and old ISA
  */
trait CbcAssembler {
  def nop(): Unit
  def mov(dst: Register, src: Register, reference: Boolean): Unit
  def mov(dst: Register, src: Register, hasMemExpr: Boolean, reference: Boolean = false): Unit
  def mov(dst: Register, src: MemExpr): Unit
  def mov(dst: MemExpr, src: Register): Unit
  def movi32(r: IR, imm: Int): Unit
  def movi64(r: IR, imm: Long): Unit
  def fmovi(r: FR, fimm: Double, w: Width): Unit
  def movbp(dst: IR, local: Boolean): Unit
  def movi32(dst: MemExpr, imm: Int): Unit
  def movi64(dst: MemExpr, imm: Long): Unit
  def arrFill(arr: IR, data: Array[Byte]): Unit
  def aliveReference(data: Array[Byte]): Unit
  def unmovableReference(data: Array[Byte]): Unit
  def beginLocalUnmovable(r: IR): Unit
  def endLocalUnmovable  (r: IR): Unit
  def aliveRefDifference(data: Array[Byte]): Unit
  def aliveUnmovableDifference(data: Array[Byte]): Unit
  def aliveRefCheck(data: Array[Byte]): Unit
  def loadConstDataAddr(dst: IR, data: Array[Byte], alignment: Int): Unit
  def blkzero(start: StackSlot.Untyped, count: Int): Unit
  def zerorefs(ts: StackSlot.Typed): Unit
  def jmp(target: Label): Unit
  def scc(op: BranchOp, dst: IR, src: IR, imm: Long, width: Width): Unit
  def scc(op: BranchOp, dst: IR, src1: IR, src2: IR, width: Width): Unit
  def scc(op: BranchOp, dst: IR, src1: FR, src2: FR, width: Width): Unit
  def bcc(op: BranchOp, src: IR, imm: Long, width: Width, target: Label): Unit
  def bcc(op: BranchOp, src1: IR, src2: IR, width: Width, target: Label): Unit
  def bcc(op: BranchOp, src1: FR, src2: FR, width: Width, target: Label): Unit
  def bttCHA(arg: IR, negated: Boolean, target: Label): Unit
  def bttLevel(arg: IR, negated: Boolean, level: Int, target: Label): Unit
  def bttPoint(arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit
  def bttIOFC(arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit
  def bttIOFI(arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit
  def bttIOFA(arg: IR, negated: Boolean, sig_id: Symbol, target: Label): Unit
  def bttCone(arg: IR, negated: Boolean, sig_id: Symbol, closed: Boolean, target: Label): Unit
  def throwEx(ex: IR): Unit
  def neg(ird: IR, irs: IR, w: Width): Unit
  def add(w: Width, d: IR, l: IR, r: IR): Unit
  def sub(w: Width, d: IR, l: IR, r: IR): Unit
  def mul(w: Width, d: IR, l: IR, r: IR): Unit
  def pow(w: Width, d: IR, l: IR, r: IR): Unit
  def and(w: Width, d: IR, l: IR, r: IR): Unit
  def or(w: Width, d: IR, l: IR, r: IR): Unit
  def xor(w: Width, d: IR, l: IR, r: IR): Unit
  def mulh(w: Width, d: IR, l: IR, r: IR): Unit
  def umulh(w: Width, d: IR, l: IR, r: IR): Unit
  def lsl(w: Width, d: IR, l: IR, r: IR): Unit
  def lsr(w: Width, d: IR, l: IR, r: IR): Unit
  def asr(w: Width, d: IR, l: IR, r: IR): Unit
  def addi(w: Width, d: IR, l: IR, imm: Long): Unit
  def subi(w: Width, d: IR, l: IR, imm: Long): Unit
  def muli(w: Width, d: IR, l: IR, imm: Long): Unit
  def powi(w: Width, d: IR, l: IR, imm: Long): Unit
  def andi(w: Width, d: IR, l: IR, imm: Long): Unit
  def ori(w: Width, d: IR, l: IR, imm: Long): Unit
  def xori(w: Width, d: IR, l: IR, imm: Long): Unit
  def mulhi(w: Width, d: IR, l: IR, imm: Long): Unit
  def umulhi(w: Width, d: IR, l: IR, imm: Long): Unit
  def lsli(w: Width, d: IR, l: IR, imm: Long): Unit
  def lsri(w: Width, d: IR, l: IR, imm: Long): Unit
  def asri(w: Width, d: IR, l: IR, imm: Long): Unit

  // Three address floating point binary operations on registers
  // d <-- l <op> r
  def fadd(w: Width, d: FR, l: FR, r: FR): Unit
  def fsub(w: Width, d: FR, l: FR, r: FR): Unit
  def fmul(w: Width, d: FR, l: FR, r: FR): Unit
  def fdiv(w: Width, d: FR, l: FR, r: FR): Unit

  def divisorCheck(r: IR): Unit

  def udiv(w: Width, d: IR, l: IR, r: IR): Unit
  def udivi(w: Width, d: IR, l: IR, imm: Long): Unit
  def urem(w: Width, d: IR, l: IR, r: IR): Unit
  def uremi(w: Width, d: IR, l: IR, imm: Long): Unit

  def div(w: Width, d: IR, l: IR, r: IR): Unit
  def divi(w: Width, d: IR, l: IR, imm: Long): Unit
  def rem(w: Width, d: IR, l: IR, r: IR): Unit
  def remi(w: Width, d: IR, l: IR, imm: Long): Unit
  def fneg(frd: FR, frs: FR, w: Width): Unit
  def fabs(frd: FR, frs: FR, w: Width): Unit
  def fsqrt(frd: FR, frs: FR, w: Width): Unit
  def convert(toType: AsmType, fromType: AsmType, to: Register, from: Register): Unit
  def mov(dst: IR, src: IR, width: Width): Unit
  def ldarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit
  def ldarrObj(rd: Register, ra: IR, ri: IR): Unit
  def ldarrRecord(rd: IR, ra: IR, ri: IR, sig_id: Symbol): Unit
  def starr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit
  def starrObj(ra: IR, ri: IR, rv: Register): Unit
  def lenarr(rl: IR, ra: IR): Unit
  def newarr(ftc_sig_id: Symbol): Unit
  def newarrzv(ftc_sig_id: Symbol): Unit
  def newarrfillconst(dst: IR, len: IR, value: Long, ftc_sig_id: Symbol): Unit
  def newarrfillnonconst(dst: IR, len: IR, value: IR, ftc_sig_id: Symbol): Unit
  def newobj(ftc_sig_idx: Symbol): Unit
  def isInstanceOfClass(dst: IR, obj: IR, sig_id: Symbol): Unit
  def isInstanceOfInterface(dst: IR, obj: IR, sig_id: Symbol): Unit
  def isInstanceOfArray(dst: IR, obj: IR, sig_id: Symbol): Unit
  def callIndirect(targetReg: IR, sig_id: Symbol): Unit
  def gcpoint(): Unit
  def catchEx(dst: IR): Unit
  def evacuate(): Unit
  def nullcheck(r: IR): Unit
  def prepareRecord(ts: StackSlot.Typed): Unit
  def loadUntyped(dst: Register, tk: CbcTypeKind, src: StackSlot.Untyped): Unit
  def storeUntyped(src: Register, tk: CbcTypeKind, dst: StackSlot.Untyped): Unit
  def storeUntypedImm(src: Long, dst: StackSlot.Untyped): Unit
  def ldstackrec(dst: IR, ts: StackSlot.Typed): Unit
  def ldstackobj(dst: IR, ts: StackSlot.Typed): Unit
  def offsetFromHost(dstHost: IR, dstOffset: IR, r: IR): Unit
  def offsetFromHost(dstHost: IR, dstOffset: IR, src: MemExpr): Unit
  def combineHostAndOffset(dst: IR, src: MemExpr): Unit
  def initobj(ts: StackSlot.Typed): Unit
  def singleton(dst: IR, sig_id: Symbol): Unit
  def recordCopy(dst: IR, src: IR, sig_id: Symbol): Unit
  def lea_static(dst: IR, field_id: Symbol): Unit
  def lea_us(dst: IR, us: StackSlot.Untyped): Unit
  def lea_cforeign(dst: IR, method_id: Symbol): Unit
  def loadTypeInfoSig(dst: IR, sig_id: Symbol): Unit
  def loadTypeInfoFTC(dst: IR, ftc_id: Symbol): Unit
  def loadTypeInfoObj(dst: IR, obj: IR): Unit
  def cadd (d: IR, l: IR, r: IR, w: Width): Unit
  def csub (d: IR, l: IR, r: IR, w: Width): Unit
  def cmul (d: IR, l: IR, r: IR, w: Width): Unit
  def cdiv (d: IR, l: IR, r: IR, w: Width): Unit
  def cuadd(d: IR, l: IR, r: IR, w: Width): Unit
  def cusub(d: IR, l: IR, r: IR, w: Width): Unit
  def cumul(d: IR, l: IR, r: IR, w: Width): Unit
  def caddi (d: IR, l: IR, r: Long, w: Width): Unit
  def csubi (d: IR, l: IR, r: Long, w: Width): Unit
  def cmuli (d: IR, l: IR, r: Long, w: Width): Unit
  def cuaddi(d: IR, l: IR, r: Long, w: Width): Unit
  def cusubi(d: IR, l: IR, r: Long, w: Width): Unit
  def cumuli(d: IR, l: IR, r: Long, w: Width): Unit
  def cpow (d: IR, l: IR, r: IR, w: Width): Unit
  def cpowi (d: IR, l: IR, r: Long, w: Width): Unit
  def eopPlain(dst: IR, obj: IR): Unit
  def eopEnrichment(dst: IR, obj: IR): Unit
  def eopPack(dst: IR, obj: IR, enrichment: IR): Unit
  def eopPack(dst: IR, obj: IR, enrichment_u16: Int): Unit
  def eopPack(dst: IR, obj: IR, typeId: Symbol, interfaceId: Symbol): Unit
  def weakCast(dst: IR, obj: IR, sig_id: Symbol): Unit
  def arrIC(ri: IR, rl: IR): Unit
  def packageInit(sig_id: Symbol): Unit
  def packageInitCheck(sig_id: Symbol): Unit
  def covinc(locs: Array[(String, Array[Int])]): Unit
  def javaNewarr(sig_id: Symbol): Unit
  def javaLdarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit
  def javaStarr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit
  def javaArrOp(asmType: AsmType): Int
  def javaLenarr(rl: IR, ra: IR): Unit
  def javaArrIC(ri: IR, rl: IR): Unit
  def javaArrSC(arr: IR, value: IR): Unit
  def javaLdaStr(dst: IR, string_id: Symbol): Unit
  def javaCheckCast(dst: IR, src: IR, sig_id: Symbol): Unit
  def javaClinit(sig_id: Symbol): Unit

  def callGTDSig(sig_idx: Symbol, method_id: Symbol): Unit
  def callGTDFTC(ftc_idx: Symbol, method_id: Symbol): Unit
  def callGFDSig(sig_idx: Symbol, method_id: Symbol): Unit
  def callGFDFTC(ftc_idx: Symbol, method_id: Symbol): Unit
  def callConstraint(ftc_idx: Symbol, method_id: Symbol): Unit
  def copyResultVST(rv: IR, rr: IR, ftc_symbol_id: Symbol): Unit
  def ohmsPtr(rd: IR, ohms: StackSlot.OffHeapMemory): Unit
  def doTypeVarIsRef(dst: IR, ftc_id: Symbol): Unit
  def newobjVST(ftc_id: Symbol): Unit
  def newarrVST(ftc_id: Symbol): Unit
}

trait OldIsaParts {
  def ret(): Unit
  def mov(dst: MemExpr, src: MemExpr): Unit
  def initConstString(dst: MemExpr, stringId: Symbol): Unit
  def iCallPref(enrichment: IR): Unit
  def iCallPref(enrichment_u16: Int): Unit
  def iCallPref(method_id: Symbol): Unit
  def call(method_id: Symbol): Unit
  def callVirt(method_id: Symbol): Unit
  def callVirtStatic(method_id: Symbol): Unit
  def cFuncWrapOld(dst: IR, method_id: Symbol): Unit
}

class Assembler extends AsmEmitter.WithLiterals with FormatExtension with CbcAssembler with OldIsaParts { self =>
  protected val emit = new Bits {
    override def seg = segment
    override def addFixup(fixup: Fixup): Unit = self.addFixup(fixup)
  }

  override def alignCode(alignment: Int): Unit = notImplemented("alignCode for CBC Assembler")

  override protected def symbolLiteralKind = notImplemented("symbol literals for CBC Assembler")

  ////////////////////////////////////////////////////////////////////////////////

  def nop(): Unit = emit.op(0x0)

  def initConstString(dst: MemExpr, stringId: Symbol): Unit = {
    fextMovArgs(dst, null)
    emit.op(0x2B).r4_gap4(dst.headEncoding).id32(stringId)
  }

  // Move register-to-register
  // dst <-- src
  def mov(dst: Register, src: Register, reference: Boolean): Unit = mov(dst, src, hasMemExpr = false, reference)

  def mov(dst: Register, src: Register, hasMemExpr: Boolean, reference: Boolean = false): Unit = {
    val op = (dst, src) match {
      case (_: IR, _: IR) if reference => 0x08
      case (_: IR, _: IR) => 0xa5
      case (_: FR, _: IR) => 0xa6
      case (_: FR, _: FR) => 0xa7
      case (_: IR, _: FR) => 0xa8
    }
    emit.op(op).r4_r4(dst, src)
  }

  // Move mem-to-register
  // dst <-- src
  def mov(dst: Register, src: MemExpr): Unit = {
    fextMovArgs(dst, src)
    mov(dst, src.headEncoding, hasMemExpr = true)
  }

  // Move register-to-mem
  // dst <-- src
  def mov(dst: MemExpr, src: Register): Unit = {
    fextMovArgs(dst, src)
    mov(dst.headEncoding, src, hasMemExpr = true)
  }

  // Copy mem-to-mem
  // dst <-- src
  def mov(dst: MemExpr, src: MemExpr): Unit = {
    fextMovArgs(dst, src)
    mov(dst.headEncoding, src.headEncoding, hasMemExpr = true)
  }

  // Move immediate-to-register
  // r <-- imm
  def movi32(r: IR, imm: Int): Unit = {
    val imm4 = fextImm(imm)
    emit.op(0x4).r4_u4(r, imm4)
  }

  def movi64(r: IR, imm: Long): Unit = {
    val imm4 = fextImm(imm)
    emit.op(0x5).r4_u4(r, imm4)
  }

  def fmovi(r: FR, fimm: Double, w: Width): Unit = (w: @unchecked) match {
    case W32 => emit.op(0x6).gap4_r4(r).imm32(floatToRawIntBits(fimm.toFloat))
    case W64 => emit.op(0x7).gap4_r4(r).imm64(doubleToRawLongBits(fimm))
  }

  // Move immediate-to-mem
  // r <-- imm
  def movi32(dst: MemExpr, imm: Int): Unit = {
    fextMovArgs(dst, null)
    movi32(dst.headEncoding, imm)
  }

  def movi64(dst: MemExpr, imm: Long): Unit = {
    fextMovArgs(dst, null)
    movi64(dst.headEncoding, imm)
  }

  def movbp(dst: IR, local: Boolean): Unit = shouldNotReachHere("standalone CBC ISA")

  // Fill array from register `arr` with constant data
  // Note: alignment is not used in op arrfill, we read directly into an instance of managed array
  def arrFill(arr: IR, data: Array[Byte]): Unit = emit.op(0x23).gap4_r4(arr).id32(new RawData(data, 0))
  
  def aliveReference(data: Array[Byte]): Unit = emit.op(0x8b).id32(new RawData(data, 0))
  // TODO: eliminate following instruction when new `mut` function ABI is introduced (?)
  def unmovableReference(data: Array[Byte]): Unit = emit.op(0x18).id32(new RawData(data, 0))

  // TODO: eliminate these instructions when new `mut` function ABI is introduced (?)
  def beginLocalUnmovable(r: IR): Unit = emit.op(0x16).r4(r)
  def endLocalUnmovable  (r: IR): Unit = emit.op(0x17).r4(r)
  def aliveRefDifference(data: Array[Byte]): Unit = emit.op(0x88).id32(new RawData(data, 0))
  def aliveUnmovableDifference(data: Array[Byte]): Unit = emit.op(0x8a).id32(new RawData(data, 0))
  def aliveRefCheck(data: Array[Byte]): Unit = emit.op(0x89).id32(new RawData(data, 0))

  // load const data address to register `dst`
  def loadConstDataAddr(dst: IR, data: Array[Byte], alignment: Int): Unit = emit.op(0xa4).gap4_r4(dst).id32(new RawData(data, alignment))

  // Zero out a continuous block of untyped stack slots [start, start + count)
  def blkzero(start: StackSlot.Untyped, count: Int): Unit = emit.op(0x24).us(start).imm16(count)

  // Zero out all record refs.
  def zerorefs(ts: StackSlot.Typed): Unit = emit.op(0xaf).ts(ts)

  // Unconditional jump
  def jmp(target: Label): Unit = addFixup(new Fixups.Jump(target))


  ////////////////////////////////////////////////////////////////////////////////
  // Compare and set if

  private val sccOp = 0x93

  def scc(op: BranchOp, dst: IR, src: IR, imm: Long, width: Width): Unit = {
    assert(!op.isFloatingPoint, s"$op")
    assert(!op.isReference || imm == 0, s"$op $imm")
    val (normalizedOp, normalizedImm) = normalizeImm(op, imm, width)
    assert(canBeEncodedInBCC(normalizedOp), s"($op $imm) ($normalizedOp $normalizedImm)")
    val (condByte, swap) = FExtBCC.condOp(normalizedOp, isImm = true, width)
    assert(!swap, s"$op $dst $src $imm $width")
    val imm4 = fextImmDst(dst, src, normalizedImm)
    emit.op(sccOp).byte(condByte).r4_u4(src, imm4)
  }

  def scc(op: BranchOp, dst: IR, src1: IR, src2: IR, width: Width): Unit = {
    assert(!op.isFloatingPoint, s"$op")
    _scc(op, dst, src1, src2, width)
  }

  def scc(op: BranchOp, dst: IR, src1: FR, src2: FR, width: Width): Unit = {
    assert(op.isFloatingPoint, s"$op")
    _scc(op, dst, src1, src2, width)
  }

  private def _scc(op: BranchOp, dst: IR, src1: Register, src2: Register, width: Width): Unit = {
    val (condByte, swap) = FExtBCC.condOp(op, isImm = false, width)
    val (l, r) = if (swap) (src2, src1) else (src1, src2)
    fextDst(dst, l)
    emit.op(sccOp).byte(condByte).r4_r4(l, r)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Compare and jump (branchIf)

  def bcc(op: BranchOp, src: IR, imm: Long, width: Width, target: Label): Unit = {
    assert(!op.isFloatingPoint, s"$op")
    assert(!op.isReference || imm == 0, s"$op $imm")
    val (normalizedOp, normalizedImm) = normalizeImm(op, imm, width)
    assert(canBeEncodedInBCC(normalizedOp), s"($op $imm) ($normalizedOp $normalizedImm)")
    val imm4 = fextImm(normalizedImm)
    addFixup(new Fixups.BCC(normalizedOp, src, imm4, width, target))
  }

  def bcc(op: BranchOp, src1: IR, src2: IR, width: Width, target: Label): Unit = {
    assert(!op.isFloatingPoint, s"$op")
    addFixup(new Fixups.BCC(op, src1, src2, width, target))
  }

  def bcc(op: BranchOp, src1: FR, src2: FR, width: Width, target: Label): Unit = {
    assert(op.isFloatingPoint, s"$op")
    addFixup(new Fixups.BCC(op, src1, src2, width, target))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Type (or instance of) test and jump (BTT)

  def bttCHA    (arg: IR, negated: Boolean,                                  target: Label): Unit = addFixup(new CHA_BTT     (arg, negated,         target))
  def bttLevel  (arg: IR, negated: Boolean, level: Int,                      target: Label): Unit = addFixup(new Level_BTT   (arg, negated, level,  target))
  def bttPoint  (arg: IR, negated: Boolean, sig_id: Symbol,                  target: Label): Unit = addFixup(new BTTBySymbol (arg, negated, sig_id, target, POINT))
  def bttIOFC   (arg: IR, negated: Boolean, sig_id: Symbol,                  target: Label): Unit = addFixup(new BTTBySymbol (arg, negated, sig_id, target, IOFC))
  def bttIOFI   (arg: IR, negated: Boolean, sig_id: Symbol,                  target: Label): Unit = addFixup(new BTTBySymbol (arg, negated, sig_id, target, IOFI))
  def bttIOFA   (arg: IR, negated: Boolean, sig_id: Symbol,                  target: Label): Unit = addFixup(new BTTBySymbol (arg, negated, sig_id, target, IOFA))
  def bttCone   (arg: IR, negated: Boolean, sig_id: Symbol, closed: Boolean, target: Label): Unit = addFixup(new BTTBySymbol (arg, negated, sig_id, target, if (closed) CLOSED_CONE else OPEN_CONE))

  ////////////////////////////////////////////////////////////////////////////////
  
  // Return from method
  // head registers should contain return value, if any
  def ret(): Unit = emit.op(0x3b)

  // throw(ex)
  def throwEx(ex: IR): Unit = emit.op(0x3c).r4(ex)

  // Unary operation on registers
  // ird <-- <op> irs
  def neg(ird: IR, irs: IR, w: Width): Unit = emit.op(w, 0x40, 0x41).r4_r4(ird, irs)

  // FExt operations
  // Three address integer binary operations on registers
  // d <-- l <op> r
  def add(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x42, 0x43, d, l, r)
  def sub(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x44, 0x45, d, l, r)
  def mul(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x46, 0x47, d, l, r)
  def pow(w: Width, d: IR, l: IR, r: IR): Unit = shouldNotReachHere("not implemented")
  def and(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x5C, 0x5D, d, l, r)
  def or(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x5E, 0x5F, d, l, r)
  def xor(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x60, 0x61, d, l, r)
  def mulh(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0xB1, 0xB2, d, l, r)
  def umulh(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0xB3, 0xB4, d, l, r)
  def lsl(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x7E, 0x7F, d, l, r)
  def lsr(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x80, 0x81, d, l, r)
  def asr(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x82, 0x83, d, l, r)

  // Two address integer binary operations on registers with immediate
  // d <-- l <op> imm
  def addi(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x58, 0x59, d, l, imm)
  def subi(w: Width, d: IR, l: IR, imm: Long): Unit = addi(w, d, l, -imm)
  def muli(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x5A, 0x5B, d, l, imm)
  def powi(w: Width, d: IR, l: IR, imm: Long): Unit = shouldNotReachHere("not implemented")
  def andi(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x62, 0x63, d, l, imm)
  def ori(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x64, 0x65, d, l, imm)
  def xori(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x66, 0x67, d, l, imm)
  def mulhi(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x68, 0x69, d, l, imm)
  def umulhi(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0xB5, 0xB6, d, l, imm)
  def lsli(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0xAA, 0xAB, d, l, imm)
  def lsri(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x3D, 0x3E, d, l, imm)
  def asri(w: Width, d: IR, l: IR, imm: Long): Unit = fextOp(w, 0x3F, 0xB0, d, l, imm)
  
  // Three address floating point binary operations on registers
  // d <-- l <op> r
  def fadd(w: Width, d: FR, l: FR, r: FR): Unit = fextOp(w, 0x4E, 0x4F, d, l, r)
  def fsub(w: Width, d: FR, l: FR, r: FR): Unit = fextOp(w, 0x50, 0x51, d, l, r)
  def fmul(w: Width, d: FR, l: FR, r: FR): Unit = fextOp(w, 0x52, 0x53, d, l, r)
  def fdiv(w: Width, d: FR, l: FR, r: FR): Unit = fextOp(w, 0x54, 0x55, d, l, r)

  def divisorCheck(r: IR): Unit = emit.op(0x19).r4(r)

  def udiv(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x1a, 0x1b, d, l, r)
  def udivi(w: Width, d: IR, l: IR, imm: Long): Unit = shouldNotReachHere("only in ISA-12 mode")
  def urem(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x1c, 0x1d, d, l, r)
  def uremi(w: Width, d: IR, l: IR, imm: Long): Unit = shouldNotReachHere("only in ISA-12 mode")

  def div(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x48, 0x49, d, l, r)
  def divi(w: Width, d: IR, l: IR, imm: Long): Unit = shouldNotReachHere("only in ISA-12 mode")
  def rem(w: Width, d: IR, l: IR, r: IR): Unit = fextOp(w, 0x4a, 0x4b, d, l, r)
  def remi(w: Width, d: IR, l: IR, imm: Long): Unit = shouldNotReachHere("only in ISA-12 mode")

  // Unary floating-point operation on registers
  // frd <-- <op> frs
  def fneg(frd: FR, frs: FR, w: Width): Unit = emit.op(w, 0x4c, 0x4d).r4_r4(frd, frs)
  def fabs(frd: FR, frs: FR, w: Width): Unit = emit.op(w, 0x84, 0x85).r4_r4(frd, frs)
  def fsqrt(frd: FR, frs: FR, w: Width): Unit = emit.op(w, 0x86, 0x87).r4_r4(frd, frs)

  // Conversions between integer and floating point types
  // Integer truncations and extensions
  // dst <-- convert(fromType, toType, src)
  def convert(toType: AsmType, fromType: AsmType, to: Register, from: Register): Unit = {
    val op = convertOp(fromType, toType) ensuring (_ >= 0, "unsupported conversion operation")
    emit.op(Cast, op).r4_r4(to, from)
  }

  def mov(dst: IR, src: IR, width: Width): Unit = shouldNotReachHere("ISA12 only")
  
  // Load from array
  // ir <-- ir_a[ir_i]
  def ldarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit = {
    val op = ldarrOp(asmType) ensuring (_ >= 0, "unsupported ldarr operation")
    fextDst(rd, ra)
    emit.op(0x6a + op).r4_r4(ra, ri)
  }

  def ldarrObj(rd: Register, ra: IR, ri: IR): Unit = {
    fextDst(rd, ra)
    emit.op(0x72).r4_r4(ra, ri)
  }

  def ldarrRecord(rd: IR, ra: IR, ri: IR, sig_id: Symbol): Unit = {
    fextDst(rd, ra)
    emit.op(0x2C).r4_r4(ra, ri).id16(sig_id)
  }

  // Store to array
  // ir_a[ir_i] <-- ir_v
  def starr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit = {
    val op = starrOp(asmType) ensuring (_ >= 0, "unsupported starr operation")
    emit.op(0x73 + op).gap4_r4(rv).r4_r4(ra, ri)
  }

  def starrObj(ra: IR, ri: IR, rv: Register): Unit = {
    emit.op(0x79).gap4_r4(rv).r4_r4(ra, ri)
  }

  // Array length
  // rl <-- array_length(ra)
  def lenarr(rl: IR, ra: IR): Unit = emit.op(0x7a).r4_r4(rl, ra)

  // Create new array
  // IR1 <-- new_array(ftc_sig_id, IR2)
  def newarr(ftc_sig_id: Symbol): Unit = emit.op(0x7b).id16(ftc_sig_id)

  // Create new uninitialized array
  // IR1 <-- new_array(ftc_sig_id, IR2)
  def newarrzv(ftc_sig_id: Symbol): Unit = emit.op(0x91).id16(ftc_sig_id)

  // Create new array and fill it with default constant primitive value
  // dst <-- new_array_fill(len, value, ftc_sig_id)
  def newarrfillconst(dst: IR, len: IR, value: Long, ftc_sig_id: Symbol): Unit = {
    val imm4 = fextImmDst(dst, len, value)
    emit.op(0xac).r4_u4(len, imm4).id16(ftc_sig_id)
  }

  // Create new array and fill it with default nonconstant primitive value
  // dst <-- new_array_fill(len, value, ftc_sig_id)
  def newarrfillnonconst(dst: IR, len: IR, value: IR, ftc_sig_id: Symbol): Unit = {
    fextDst(dst, len)
    emit.op(0xad).r4_r4(len, value).id16(ftc_sig_id)
  }
  
  // Create new object
  // IR1 <-- new_object(ftc_sig_idx)
  def newobj(ftc_sig_idx: Symbol): Unit = emit.op(0x7c).id16(ftc_sig_idx)

  // dst <-- (obj instanceof type) ? 1 : 0
  def isInstanceOfClass(dst: IR, obj: IR, sig_id: Symbol): Unit = emit.op(0x7d).r4_r4(dst, obj).id16(sig_id)
  def isInstanceOfInterface(dst: IR, obj: IR, sig_id: Symbol): Unit = emit.op(0x56).r4_r4(dst, obj).id16(sig_id)
  def isInstanceOfArray(dst: IR, obj: IR, sig_id: Symbol): Unit = emit.op(0x57).r4_r4(dst, obj).id16(sig_id)

  // Direct call
  def call(method_id: Symbol): Unit =
    emit.op(0x8f).id16(method_id)

  // Object calls (virtual/interface)
  def callVirt(method_id: Symbol): Unit =
    emit.op(0x92).id16(method_id)

  def callVirtStatic(method_id: Symbol): Unit =
    emit.op(0xe).id16(method_id)
  
  // Indirect calls
  def callIndirect(targetReg: IR, sig_id: Symbol): Unit =
    emit.op(0x94).gap4_r4(targetReg).id16(sig_id)

  def cFuncWrapOld(dst: IR, method_id: Symbol): Unit = emit.op(0x95).id16(method_id)

  def gcpoint(): Unit = emit.op(0x96)
  def catchEx(dst: IR): Unit = emit.op(0x97).r4(dst)
  def evacuate(): Unit = emit.op(0x9b)

  def nullcheck(r: IR): Unit = emit.op(0xa9).r4(r)

  def prepareRecord(ts: StackSlot.Typed): Unit = emit.op(0x1E).ts(ts)

  def loadUntyped(dst: Register, tk: CbcTypeKind, src: StackSlot.Untyped): Unit = mov(dst, MemExpr(src, tk))
  def storeUntyped(src: Register, tk: CbcTypeKind, dst: StackSlot.Untyped): Unit = mov(MemExpr(dst, tk), src)
  def storeUntypedImm(src: Long, dst: StackSlot.Untyped): Unit = movi64(MemExpr(dst, CbcTypeKind.I64), src)

  // dst <-- address of [loc(recId)]
  def ldstackrec(dst: IR, ts: StackSlot.Typed): Unit = emit.op(0x2A).gap4_r4(dst).ts(ts)

  // dst <-- address of [loc(objId)]
  def ldstackobj(dst: IR, ts: StackSlot.Typed): Unit = emit.op(0x2E).gap4_r4(dst).ts(ts)

  def offsetFromHost(dstHost: IR, dstOffset: IR, r: IR): Unit = {
    emit.op(0x8C).r4_r4(dstHost, dstOffset).gap4_r4(r)
  }

  def offsetFromHost(dstHost: IR, dstOffset: IR, src: MemExpr): Unit = {
    fextMovArgs(null, src)
    emit.op(0x8C).r4_r4(dstHost, dstOffset).gap4_r4(src.headEncoding)
  }

  def combineHostAndOffset(dst: IR, src: MemExpr): Unit = {
    fextMovArgs(dst, src)
    emit.op(0x8D).r4_r4(dst, src.headEncoding)
  }

  // initialize object by objId
  def initobj(ts: StackSlot.Typed): Unit = emit.op(0x2F).ts(ts)

  def singleton(dst: IR, sig_id: Symbol): Unit = emit.op(0x8E).gap4_r4(dst).id16(sig_id)

  def recordCopy(dst: IR, src: IR, sig_id: Symbol): Unit = emit.op(0x2D).r4_r4(dst, src).id16(sig_id)

  // Load effective field address
  // dst <-- address of field
  def lea_static(dst: IR, field_id: Symbol): Unit = emit.op(0x09).gap4_r4(dst).id16(field_id)

  // dst <- address of [us]
  def lea_us(dst: IR, us: StackSlot.Untyped): Unit = emit.op(0x0A).gap4_r4(dst).us(us)

  // dst <- address of foreign C func
  def lea_cforeign(dst: IR, method_id: Symbol): Unit = emit.op(0x0B).gap4_r4(dst).id16(method_id)

  // dst <- ThisTypeInfo of type or object
  private def loadTypeInfo(dst: IR, ftc_sig_id: Symbol): Unit = emit.op(0x0c).gap4_r4(dst).id16(ftc_sig_id)
  def loadTypeInfoSig(dst: IR, sig_id: Symbol): Unit = loadTypeInfo(dst, sig_id)
  def loadTypeInfoFTC(dst: IR, ftc_id: Symbol): Unit = loadTypeInfo(dst, ftc_id)
  def loadTypeInfoObj(dst: IR, obj: IR): Unit = emit.op(0x0d).r4_r4(dst, obj)

  private def checkedOp(base: Int, w: Width, d: IR, l: IR, r: IR): Unit = {
    fextDst(d, l)
    emit.op(Checked, base + w.log2bytes).r4_r4(l, r)
  }

  private def checkedOp(base: Int, w: Width, d: IR, l: IR, r: Long): Unit = {
    val imm4 = fextImmDst(d, l, r)
    emit.op(Checked, base + w.log2bytes).r4_u4(l, imm4)
  }

  def cadd (d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x00, w, d, l, r)
  def csub (d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x04, w, d, l, r)
  def cmul (d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x08, w, d, l, r)
  def cdiv (d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x0c, w, d, l, r)
  def cuadd(d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x10, w, d, l, r)
  def cusub(d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x14, w, d, l, r)
  def cumul(d: IR, l: IR, r: IR, w: Width): Unit = checkedOp(0x18, w, d, l, r)
  def cpow (d: IR, l: IR, r: IR, w: Width): Unit = notImplemented("not implemented")

  def caddi (d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x1C, w, d, l, r)
  def csubi (d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x20, w, d, l, r)
  def cmuli (d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x24, w, d, l, r)
  def cuaddi(d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x28, w, d, l, r)
  def cusubi(d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x2c, w, d, l, r)
  def cumuli(d: IR, l: IR, r: Long, w: Width): Unit = checkedOp(0x30, w, d, l, r)
  def cpowi (d: IR, l: IR, r: Long, w: Width): Unit = notImplemented("not implemented")


  def eopPlain(dst: IR, obj: IR): Unit = emit.op(0x30).r4_r4(dst, obj)

  def eopEnrichment(dst: IR, obj: IR): Unit = emit.op(0x31).r4_r4(dst, obj)

  def eopPack(dst: IR, obj: IR, enrichment: IR): Unit = {
    fextDst(dst, obj)
    emit.op(0x32).r4_r4(obj, enrichment)
  }

  def eopPack(dst: IR, obj: IR, enrichment_u16: Int): Unit = shouldNotReachHere()

  def eopPack(dst: IR, obj: IR, typeId: Symbol, interfaceId: Symbol): Unit = {
    emit.op(0x33).r4_r4(dst, obj).id16(typeId).id16(interfaceId)
  }

  def weakCast(dst: IR, obj: IR, sig_id: Symbol): Unit = emit.op(0x34).r4_r4(dst, obj).id16(sig_id)

  def iCallPref(enrichment: IR): Unit = emit.op(0x35).r4(enrichment)

  def iCallPref(enrichment_u16: Int): Unit = emit.op(0x36).uimm16(enrichment_u16)

  def iCallPref(method_id: Symbol): Unit = emit.op(0x37).id16(method_id)

  def arrIC(ri: IR, rl: IR): Unit = emit.op(0xa0).r4_r4(ri, rl)

  def packageInit(sig_id: Symbol): Unit = emit.op(0xa1).id16(sig_id)

  def packageInitCheck(sig_id: Symbol): Unit = emit.op(0xa2).id16(sig_id)

  def covinc(locs: Array[(String, Array[Int])]): Unit = {
    emit.op(0xae)
    emit.addFixup(CoverageLocs(locs))
  }

  // region Java support

  // IR1 <-- java_new_array(sig_id, IR2)
  def javaNewarr(sig_id: Symbol): Unit = emit.op(Java, 0x00).id16(sig_id)

  def javaLdarr(asmType: AsmType, rd: Register, ra: IR, ri: IR): Unit = {
    val op = javaArrOp(asmType) ensuring (_ >= 0, s"unsupported javaLdarr operation with type $asmType")
    fextDst(rd, ra)
    emit.op(Java, 0x01 + op).r4_r4(ra, ri)
  }

  def javaStarr(asmType: AsmType, ra: IR, ri: IR, rv: Register): Unit = {
    val op = javaArrOp(asmType) ensuring (_ >= 0, s"unsupported javaStarr operation with type $asmType")
    emit.op(Java, 0x09 + op).gap4_r4(rv).r4_r4(ra, ri)
  }

  def javaArrOp(asmType: AsmType): Int = asmType match {
    case I8 => 0
    case I16 => 1
    case U16 => 2
    case I32 => 3
    case I64 => 4
    case F32 => 5
    case F64 => 6
    case PTR => 7
    case _ => -1
  }

  def javaLenarr(rl: IR, ra: IR): Unit = emit.op(Java, 0x11).r4_r4(rl, ra)

  def javaArrIC(ri: IR, rl: IR): Unit = emit.op(Java, 0x12).r4_r4(ri, rl)

  def javaArrSC(arr: IR, value: IR): Unit = {
    emit.op(Java, 0x13).r4_r4(arr, value)
  }

  def javaLdaStr(dst: IR, string_id: Symbol): Unit = emit.op(Java, 0x14).gap4_r4(dst).id32(string_id)

  def javaCheckCast(dst: IR, src: IR, sig_id: Symbol): Unit = emit.op(Java, 0x15).r4_r4(dst, src).id16(sig_id)

  def javaClinit(sig_id: Symbol): Unit = emit.op(Java, 0x16).id16(sig_id)

  // endregion

  // region UG support

  // FIXME-ISA12 provide fixup to define whether a symbol corresponds to FTC idx or sig idx instead of detecting this information on the call site
  def callGTDSig(sig_idx: Symbol, method_id: Symbol): Unit = callGenericGTD(sig_idx, method_id)
  def callGTDFTC(ftc_idx: Symbol, method_id: Symbol): Unit = callGenericGTD(ftc_idx, method_id)
  private def callGenericGTD(ftc_sig_idx: Symbol, method_id: Symbol): Unit = emit.op(UG, 0x02).id16(ftc_sig_idx).id16(method_id)

  // FIXME-ISA12 provide fixup to define whether a symbol corresponds to FTC idx or sig idx instead of detecting this information on the call site
  def callGFDSig(sig_idx: Symbol, method_id: Symbol): Unit = callGenericGFD(sig_idx, method_id)
  def callGFDFTC(ftc_idx: Symbol, method_id: Symbol): Unit = callGenericGFD(ftc_idx, method_id)
  private def callGenericGFD(ftc_sig_idx: Symbol, method_id: Symbol): Unit = emit.op(UG, 0x01).id16(ftc_sig_idx).id16(method_id)

  def callConstraint(ftc_idx: Symbol, method_id: Symbol): Unit = emit.op(UG, 0x00).id16(ftc_idx).id16(method_id)

  def copyResultVST(rv: IR, rr: IR, ftc_symbol_id: Symbol): Unit = emit.op(UG, 0x04).r4_r4(rv, rr).id16(ftc_symbol_id)

  def ohmsPtr(rd: IR, ohms: StackSlot.OffHeapMemory): Unit = emit.op(UG, 0x05).r4(rd).ohms(ohms)

  def doTypeVarIsRef(dst: IR, ftc_id: Symbol): Unit = emit.op(UG, 0x06).gap4_r4(dst).id16(ftc_id)

  def newobjVST(ftc_id: Symbol): Unit = newobj(ftc_id)
  def newarrVST(ftc_id: Symbol): Unit = newarr(ftc_id)

  // endregion
}

object Assembler {
  def normalizeImm(op: BranchOp, c: Long, width: Width): (BranchOp, Long) = {
    if (canBeEncodedInBCC(op)) {
      return (op, c)
    }

    def incrementUnsigned(c: Long): Long = {
      assert(c != rangeMask64(0, width.nbits - 1), s"$c $width")
      width match {
        case W32 => c.toInt + 1
        case W64 => c + 1
        case w => notImplemented(s"feel free to implement: $w")
      }
    }

    def incrementSigned(c: Long): Long = {
      assert(c != rangeMask64(0, width.nbits - 2), s"$c $width") // c != MaxSignedValue(width)
      c + 1
    }

    import BranchOp.*
    (op: @unchecked) match {
      case LE => (LT, incrementSigned(c))
      case GT => (GE, incrementSigned(c))
      case ULE => (ULT, incrementUnsigned(c))
      case UGT => (UGE, incrementUnsigned(c))
    }
  }

  /** See specification "Branch If (FExt BCC)" */
  private def canBeEncodedInBCC(op: BranchOp): Boolean = {
    import BranchOp.*
    op match {
      case EQ | NE |
           LT | GE | // can't do LE and GT
           ULT | UGE | // can't do ULE and UGT
           REQ | RNE |
           FEQ | FNE | FLT | FNLT | FGE | FNGE | // can't do FLE, FNLE and FGT and FNGT
           TESTZ | TESTNZ | TESTBIT => true
      case _ => false // anything, not listed here, must be achievable through swap of arguments or, in case of constant, normalization
    }
  }
}