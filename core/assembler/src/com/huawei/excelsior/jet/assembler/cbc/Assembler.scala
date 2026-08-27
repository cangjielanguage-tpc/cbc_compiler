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
import com.huawei.excelsior.jet.assembler.cbc.Bits.*
import com.huawei.excelsior.jet.assembler.cbc.FExtBCC
import com.huawei.excelsior.jet.assembler.cbc.Fixups.*
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT.Kind.*
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
  def movi32(r: IR, imm: Int): Unit
  def movi64(r: IR, imm: Long): Unit
  def fmovi(r: FR, fimm: Double, w: Width): Unit
  def movbp(dst: IR, local: Boolean): Unit
  def arrFill(arr: IR, data: Array[Byte]): Unit
  def aliveReference(data: Array[Byte]): Unit
  def unmovableReference(data: Array[Byte]): Unit
  def beginLocalUnmovable(r: IR): Unit
  def endLocalUnmovable  (r: IR): Unit
  def aliveRefDifference(data: Array[Byte]): Unit
  def aliveUnmovableDifference(data: Array[Byte]): Unit
  def aliveRefCheck(data: Array[Byte]): Unit
  def loadConstDataAddr(dst: IR, data: Array[Byte], alignment: Int): Unit
  def zerorefs(ts: StackSlot.Typed): Unit
  def jmp(target: Label): Unit
  def scc(op: BranchOp, dst: IR, src: IR, imm: Long, width: Width): Unit
  def scc(op: BranchOp, dst: IR, src1: IR, src2: IR, width: Width): Unit
  def scc(op: BranchOp, dst: IR, src1: FR, src2: FR, width: Width): Unit
  def bcc(op: BranchOp, src: IR, imm: Long, width: Width, target: Label): Unit
  def bcc(op: BranchOp, src1: IR, src2: IR, width: Width, target: Label): Unit
  def bcc(op: BranchOp, src1: FR, src2: FR, width: Width, target: Label): Unit
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
  def nullcheck(r: IR): Unit
  def prepareRecord(ts: StackSlot.Typed): Unit
  def loadUntyped(dst: Register, tk: CbcTypeKind, src: StackSlot.Untyped): Unit
  def storeUntyped(src: Register, tk: CbcTypeKind, dst: StackSlot.Untyped): Unit
  def storeUntypedImm(src: Long, dst: StackSlot.Untyped): Unit
  def ldstackrec(dst: IR, ts: StackSlot.Typed): Unit
  def initobj(ts: StackSlot.Typed): Unit
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
  def arrIC(ri: IR, rl: IR): Unit
  def packageInitCheck(sig_id: Symbol): Unit

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