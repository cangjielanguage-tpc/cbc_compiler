/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.{AsmType, Label, Segment, Symbol, Width}
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.ImmEXT
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Sign.{Signed, Unsigned}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.isa12.{LivenessAnalyzer, LivenessInfoCollector, MemoryAccess}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.MemSpace.Builder as MemBuilder
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.{FlowAnalyzer, MemSpace, SymbolAdapter}
import com.huawei.excelsior.jet.codeemitter.BranchOp

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class CodeGenerator extends isa12.forked.Assembler with SymbolAdapter {
  private val labels = mutable.Map.empty[String, Label]

  private var memSpaceBuilder: Option[MemBuilder] = None

  private def getLabel(name: String): Label = labels.getOrElseUpdate(name, newLabel)
  def bind(target: String): Unit = {
    val label = getLabel(target)
    bind(label)
    analyzer.merge(label)
  }

  def adapt(symbol: Symbol): BytecodeReference = symbol.asInstanceOf[BytecodeReferenceSymbol].ref

  // region instruction methods, accessed via reflection

  private def resourceEntity(r: Any): FlowAnalyzer.Resource = r match {
    case r: IR => r
    case ts: Long => StackSlot.Untyped(ts.toInt)
  }
  def dead(rs: Array[Any]): Unit = rs map resourceEntity foreach analyzer.dead
  private def live(rs: Array[Any], action: FlowAnalyzer.Resource => Unit): Unit = rs map resourceEntity foreach action
  def live_prim(rs: Array[Any]): Unit = analyzer.op(live(rs, analyzer.prim))
  def live_rec(rs: Array[Any]): Unit = analyzer.op(live(rs, analyzer.rec))
  def live_ref(rs: Array[Any]): Unit = analyzer.op(live(rs, analyzer.ref))

  def save_state(): Unit = saveState()

  def mov_32(r: IR, imm: Long): Unit = movi32(r, imm.toInt)
  def mov_64(r: IR, imm: Long): Unit = movi64(r, imm)

  def mov_32(r: FR, imm: Double): Unit = fmovi(r, imm, Width.W32)
  def mov_64(r: FR, imm: Double): Unit = fmovi(r, imm, Width.W64)

  override def nop(): Unit = super.nop()

  def mov_32(rd: IR, rs: IR): Unit = mov(rd, rs, W32)
  def mov_32(rd: FR, rs: FR): Unit = fmov(rd, rs, W32)
  def mov_32(rd: FR, rs: IR): Unit = movi2f(rd, rs, W32)
  def mov_32(rd: IR, rs: FR): Unit = movf2i(rd, rs, W32)

  def mov_64(rd: IR, rs: IR): Unit = mov(rd, rs, W64)
  def mov_64(rd: FR, rs: FR): Unit = fmov(rd, rs, W64)
  def mov_64(rd: FR, rs: IR): Unit = movi2f(rd, rs, W64)
  def mov_64(rd: IR, rs: FR): Unit = movf2i(rd, rs, W64)

  def mov_ref(rd: IR, rs: IR): Unit = movRef(rd, rs)

  def convert(toType: String, fromType: String, to: IR, from: IR): Unit = convert(AsmType.valueOf(toType), AsmType.valueOf(fromType), to, from)
  def convert(toType: String, fromType: String, to: IR, from: FR): Unit = convert(AsmType.valueOf(toType), AsmType.valueOf(fromType), to, from)
  def convert(toType: String, fromType: String, to: FR, from: IR): Unit = convert(AsmType.valueOf(toType), AsmType.valueOf(fromType), to, from)
  def convert(toType: String, fromType: String, to: FR, from: FR): Unit = convert(AsmType.valueOf(toType), AsmType.valueOf(fromType), to, from)

  def add_32 (rd: IR, rl: IR, rr: IR): Unit = add (Width.W32, rd, rl, rr)
  def sub_32 (rd: IR, rl: IR, rr: IR): Unit = sub (Width.W32, rd, rl, rr)
  def mul_32 (rd: IR, rl: IR, rr: IR): Unit = mul (Width.W32, rd, rl, rr)
  def and_32 (rd: IR, rl: IR, rr: IR): Unit = and (Width.W32, rd, rl, rr)
  def or_32  (rd: IR, rl: IR, rr: IR): Unit = or  (Width.W32, rd, rl, rr)
  def xor_32 (rd: IR, rl: IR, rr: IR): Unit = xor (Width.W32, rd, rl, rr)
  def div_32 (rd: IR, rl: IR, rr: IR): Unit = div (Width.W32, rd, rl, rr)
  def rem_32 (rd: IR, rl: IR, rr: IR): Unit = rem (Width.W32, rd, rl, rr)
  def udiv_32(rd: IR, rl: IR, rr: IR): Unit = udiv(Width.W32, rd, rl, rr)
  def urem_32(rd: IR, rl: IR, rr: IR): Unit = urem(Width.W32, rd, rl, rr)
  def lsl_32 (rd: IR, rl: IR, rr: IR): Unit = lsl (Width.W32, rd, rl, rr)
  def lsr_32 (rd: IR, rl: IR, rr: IR): Unit = lsr (Width.W32, rd, rl, rr)
  def asr_32 (rd: IR, rl: IR, rr: IR): Unit = asr (Width.W32, rd, rl, rr)

  def add_64 (rd: IR, rl: IR, rr: IR): Unit = add (Width.W64, rd, rl, rr)
  def sub_64 (rd: IR, rl: IR, rr: IR): Unit = sub (Width.W64, rd, rl, rr)
  def mul_64 (rd: IR, rl: IR, rr: IR): Unit = mul (Width.W64, rd, rl, rr)
  def and_64 (rd: IR, rl: IR, rr: IR): Unit = and (Width.W64, rd, rl, rr)
  def or_64  (rd: IR, rl: IR, rr: IR): Unit = or  (Width.W64, rd, rl, rr)
  def xor_64 (rd: IR, rl: IR, rr: IR): Unit = xor (Width.W64, rd, rl, rr)
  def div_64 (rd: IR, rl: IR, rr: IR): Unit = div (Width.W64, rd, rl, rr)
  def rem_64 (rd: IR, rl: IR, rr: IR): Unit = rem (Width.W64, rd, rl, rr)
  def udiv_64(rd: IR, rl: IR, rr: IR): Unit = udiv(Width.W64, rd, rl, rr)
  def urem_64(rd: IR, rl: IR, rr: IR): Unit = urem(Width.W64, rd, rl, rr)
  def lsl_64 (rd: IR, rl: IR, rr: IR): Unit = lsl (Width.W64, rd, rl, rr)
  def lsr_64 (rd: IR, rl: IR, rr: IR): Unit = lsr (Width.W64, rd, rl, rr)
  def asr_64 (rd: IR, rl: IR, rr: IR): Unit = asr (Width.W64, rd, rl, rr)

  def neg_32(rd: IR, rs: IR): Unit = neg(rd, rs, Width.W32)
  def neg_64(rd: IR, rs: IR): Unit = neg(rd, rs, Width.W64)

  def addi_32 (d: IR, l: IR, imm: Long): Unit = addi (Width.W32, d, l, imm)
  def subi_32 (d: IR, l: IR, imm: Long): Unit = subi (Width.W32, d, l, imm)
  def muli_32 (d: IR, l: IR, imm: Long): Unit = muli (Width.W32, d, l, imm)
  def andi_32 (d: IR, l: IR, imm: Long): Unit = andi (Width.W32, d, l, imm)
  def ori_32  (d: IR, l: IR, imm: Long): Unit = ori  (Width.W32, d, l, imm)
  def xori_32 (d: IR, l: IR, imm: Long): Unit = xori (Width.W32, d, l, imm)
  def divi_32 (d: IR, l: IR, imm: Long): Unit = divi (Width.W32, d, l, imm)
  def remi_32 (d: IR, l: IR, imm: Long): Unit = remi (Width.W32, d, l, imm)
  def udivi_32(d: IR, l: IR, imm: Long): Unit = udivi(Width.W32, d, l, imm)
  def uremi_32(d: IR, l: IR, imm: Long): Unit = uremi(Width.W32, d, l, imm)
  def lsli_32 (d: IR, l: IR, imm: Long): Unit = lsli (Width.W32, d, l, imm)
  def lsri_32 (d: IR, l: IR, imm: Long): Unit = lsri (Width.W32, d, l, imm)
  def asri_32 (d: IR, l: IR, imm: Long): Unit = asri (Width.W32, d, l, imm)

  def addi_64 (d: IR, l: IR, imm: Long): Unit = addi (Width.W64, d, l, imm)
  def subi_64 (d: IR, l: IR, imm: Long): Unit = subi (Width.W64, d, l, imm)
  def muli_64 (d: IR, l: IR, imm: Long): Unit = muli (Width.W64, d, l, imm)
  def andi_64 (d: IR, l: IR, imm: Long): Unit = andi (Width.W64, d, l, imm)
  def ori_64  (d: IR, l: IR, imm: Long): Unit = ori  (Width.W64, d, l, imm)
  def xori_64 (d: IR, l: IR, imm: Long): Unit = xori (Width.W64, d, l, imm)
  def divi_64 (d: IR, l: IR, imm: Long): Unit = divi (Width.W64, d, l, imm)
  def remi_64 (d: IR, l: IR, imm: Long): Unit = remi (Width.W64, d, l, imm)
  def udivi_64(d: IR, l: IR, imm: Long): Unit = udivi(Width.W64, d, l, imm)
  def uremi_64(d: IR, l: IR, imm: Long): Unit = uremi(Width.W64, d, l, imm)
  def lsli_64 (d: IR, l: IR, imm: Long): Unit = lsli (Width.W64, d, l, imm)
  def lsri_64 (d: IR, l: IR, imm: Long): Unit = lsri (Width.W64, d, l, imm)
  def asri_64 (d: IR, l: IR, imm: Long): Unit = asri (Width.W64, d, l, imm)

  def cadd_32 (dst: IR, src1: IR, src2: IR): Unit = cadd (dst, src1, src2, Width.W32)
  def csub_32 (dst: IR, src1: IR, src2: IR): Unit = csub (dst, src1, src2, Width.W32)
  def cmul_32 (dst: IR, src1: IR, src2: IR): Unit = cmul (dst, src1, src2, Width.W32)
  def cdiv_32 (dst: IR, src1: IR, src2: IR): Unit = cdiv (dst, src1, src2, Width.W32)
  def cuadd_32(dst: IR, src1: IR, src2: IR): Unit = cuadd(dst, src1, src2, Width.W32)
  def cusub_32(dst: IR, src1: IR, src2: IR): Unit = cusub(dst, src1, src2, Width.W32)
  def cumul_32(dst: IR, src1: IR, src2: IR): Unit = cumul(dst, src1, src2, Width.W32)

  def cadd_64 (dst: IR, src1: IR, src2: IR): Unit = cadd (dst, src1, src2, Width.W64)
  def csub_64 (dst: IR, src1: IR, src2: IR): Unit = csub (dst, src1, src2, Width.W64)
  def cmul_64 (dst: IR, src1: IR, src2: IR): Unit = cmul (dst, src1, src2, Width.W64)
  def cdiv_64 (dst: IR, src1: IR, src2: IR): Unit = cdiv (dst, src1, src2, Width.W64)
  def cuadd_64(dst: IR, src1: IR, src2: IR): Unit = cuadd(dst, src1, src2, Width.W64)
  def cusub_64(dst: IR, src1: IR, src2: IR): Unit = cusub(dst, src1, src2, Width.W64)
  def cumul_64(dst: IR, src1: IR, src2: IR): Unit = cumul(dst, src1, src2, Width.W64)

  def caddi_32 (dst: IR, src1: IR, src2: Long): Unit = caddi (dst, src1, src2, Width.W32)
  def csubi_32 (dst: IR, src1: IR, src2: Long): Unit = csubi (dst, src1, src2, Width.W32)
  def cmuli_32 (dst: IR, src1: IR, src2: Long): Unit = cmuli (dst, src1, src2, Width.W32)
  def cuaddi_32(dst: IR, src1: IR, src2: Long): Unit = cuaddi(dst, src1, src2, Width.W32)
  def cusubi_32(dst: IR, src1: IR, src2: Long): Unit = cusubi(dst, src1, src2, Width.W32)
  def cumuli_32(dst: IR, src1: IR, src2: Long): Unit = cumuli(dst, src1, src2, Width.W32)

  def caddi_64 (dst: IR, src1: IR, src2: Long): Unit = caddi (dst, src1, src2, Width.W64)
  def csubi_64 (dst: IR, src1: IR, src2: Long): Unit = csubi (dst, src1, src2, Width.W64)
  def cmuli_64 (dst: IR, src1: IR, src2: Long): Unit = cmuli (dst, src1, src2, Width.W64)
  def cuaddi_64(dst: IR, src1: IR, src2: Long): Unit = cuaddi(dst, src1, src2, Width.W64)
  def cusubi_64(dst: IR, src1: IR, src2: Long): Unit = cusubi(dst, src1, src2, Width.W64)
  def cumuli_64(dst: IR, src1: IR, src2: Long): Unit = cumuli(dst, src1, src2, Width.W64)

  def fadd_32(d: FR, l: FR, r: FR): Unit = fadd(Width.W32, d, l, r)
  def fsub_32(d: FR, l: FR, r: FR): Unit = fsub(Width.W32, d, l, r)
  def fmul_32(d: FR, l: FR, r: FR): Unit = fmul(Width.W32, d, l, r)
  def fdiv_32(d: FR, l: FR, r: FR): Unit = fdiv(Width.W32, d, l, r)
  def fadd_64(d: FR, l: FR, r: FR): Unit = fadd(Width.W64, d, l, r)
  def fsub_64(d: FR, l: FR, r: FR): Unit = fsub(Width.W64, d, l, r)
  def fmul_64(d: FR, l: FR, r: FR): Unit = fmul(Width.W64, d, l, r)
  def fdiv_64(d: FR, l: FR, r: FR): Unit = fdiv(Width.W64, d, l, r)

  def fneg_32(frd: FR, frs: FR): Unit = fneg(frd, frs, Width.W32)
  def fabs_32(frd: FR, frs: FR): Unit = fabs(frd, frs, Width.W32)
  def fsqrt_32(frd: FR, frs: FR): Unit = fsqrt(frd, frs, Width.W32)
  def fneg_64(frd: FR, frs: FR): Unit = fneg(frd, frs, Width.W64)
  def fabs_64(frd: FR, frs: FR): Unit = fabs(frd, frs, Width.W64)
  def fsqrt_64(frd: FR, frs: FR): Unit = fsqrt(frd, frs, Width.W64)

  def ret_32(r: IR): Unit = ret(r, Width.W32)
  def ret_64(r: IR): Unit = ret(r, Width.W64)
  def ret_ref(r: IR): Unit = retRef(r)
  def fret_32(r: FR): Unit = fret(r, Width.W32)
  def fret_64(r: FR): Unit = fret(r, Width.W64)

  def branch_if(op: String, rx: IR, ry: IR, target: String): Unit = bcc(BranchOp.valueOf(op), rx, ry, Width.W64, getLabel(target))
  def branch_if(op: String, r: IR, imm: Long, target: String): Unit = bcc(BranchOp.valueOf(op), r, imm, Width.W64, getLabel(target))
  def branch_if(op: String, rx: FR, ry: FR, target: String): Unit = bcc(BranchOp.valueOf(op), rx, ry, Width.W64, getLabel(target))

  def wide_branch_if(op: String, rx: IR, ry: IR, target: String): Unit = doBcc(BranchOp.valueOf(op), rx, ry, Width.W64, getLabel(target), wide = true)
  def wide_branch_if(op: String, r: IR, imm: Long, target: String): Unit = doBccImm(BranchOp.valueOf(op), r, imm, Width.W64, getLabel(target), wide = true)
  def wide_branch_if(op: String, rx: FR, ry: FR, target: String): Unit = doBcc(BranchOp.valueOf(op), rx, ry, Width.W64, getLabel(target), wide = true)

  def set_if(op: String, dst: IR, rx: IR, ry: IR, target: String): Unit = scc(BranchOp.valueOf(op), dst, rx, ry, Width.W64)
  def set_if(op: String, dst: IR, r: IR, imm: Long, target: String): Unit = scc(BranchOp.valueOf(op), dst, r, imm, Width.W64)

  def jmp(target: String): Unit = jmp(getLabel(target))
  def wide_jmp(target: String): Unit = doJmp(getLabel(target), wide = true)

  // SOC
  def throw_ex(ex: IR): Unit = throwEx(ex)
  def catch_ex(dst: IR): Unit = catchEx(dst)
  def divisor_check(r: IR): Unit = divisorCheck(r)
  def lenarr_raw(rl: IR, ra: IR): Unit = lenarr(rl, ra)
  def arr_ic_raw(ri: IR, rl: IR): Unit = arrIC(ri, rl)
  override def gcpoint(): Unit = super.gcpoint()

  def zero_refs(ts: Long): Unit = zerorefs(StackSlot.Typed(ts.toInt))
  def init_const_string(ts: Long, string: String): Unit = initConstString(StackSlot.Typed(ts.toInt), BytecodeReferenceSymbol(StringLiteral(string)))

  // memory access
  def ld_ref_field(rd: IR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().obj(rb).field(field).load(rd).gen(this)
  def ld_ref_field(rd: FR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().obj(rb).field(field).load(rd).gen(this)
  def st_ref_field(rs: IR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().obj(rb).field(field).store(rs).gen(this)
  def st_ref_field(rs: FR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().obj(rb).field(field).store(rs).gen(this)
  def ld_rec_field(rd: IR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().rec(rb).field(field).load(rd).gen(this)
  def ld_rec_field(rd: FR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().rec(rb).field(field).load(rd).gen(this)
  def st_rec_field(rs: IR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().rec(rb).field(field).store(rs).gen(this)
  def st_rec_field(rs: FR, rb: IR, field: FieldReference): Unit = MemSpace.Builder().rec(rb).field(field).store(rs).gen(this)
  def ld_static(rd: IR, field: FieldReference): Unit = MemSpace.Builder().static(field).load(rd).gen(this)
  def ld_static(rd: FR, field: FieldReference): Unit = MemSpace.Builder().static(field).load(rd).gen(this)
  def st_static(rs: IR, field: FieldReference): Unit = MemSpace.Builder().static(field).store(rs).gen(this)
  def st_static(rs: FR, field: FieldReference): Unit = MemSpace.Builder().static(field).store(rs).gen(this)
  def prepare_record(ts: Long): Unit = prepareRecord(StackSlot.Typed(ts.toInt))

  def ld_uslot(dst: IR, tk: CbcTypeKind, us: Long): Unit = loadUntyped(dst, tk, StackSlot.Untyped(us.toInt))
  def ld_uslot(dst: FR, tk: CbcTypeKind, us: Long): Unit = loadUntyped(dst, tk, StackSlot.Untyped(us.toInt))
  def st_uslot(src: IR, tk: CbcTypeKind, us: Long): Unit = storeUntyped(src, tk, StackSlot.Untyped(us.toInt))
  def st_uslot(src: FR, tk: CbcTypeKind, us: Long): Unit = storeUntyped(src, tk, StackSlot.Untyped(us.toInt))
  def st_uslot(src: Long, us: Long): Unit = storeUntypedImm(src, StackSlot.Untyped(us.toInt))

  def ld_tslot(dst: IR, ts: Long, field: FieldReference): Unit = MemSpace.Builder().typed(StackSlot.Typed(ts.toInt)).field(field).load(dst).gen(this)
  def ld_tslot(dst: FR, ts: Long, field: FieldReference): Unit = MemSpace.Builder().typed(StackSlot.Typed(ts.toInt)).field(field).load(dst).gen(this)
  def st_tslot(src: IR, ts: Long, field: FieldReference): Unit = MemSpace.Builder().typed(StackSlot.Typed(ts.toInt)).field(field).store(src).gen(this)
  def st_tslot(src: FR, ts: Long, field: FieldReference): Unit = MemSpace.Builder().typed(StackSlot.Typed(ts.toInt)).field(field).store(src).gen(this)
  def st_tslot(src: Long, ts: Long, field: FieldReference): Unit = MemSpace.Builder().typed(StackSlot.Typed(ts.toInt)).field(field).storeImm(src).gen(this)

  // memory access openers
  def mem_rec_head(rt: IR, rb: IR, mOpen: String): Unit = {
    assert(mOpen == "{" && rt == IR.IRZ)
    memSpaceBuilder = Some(MemBuilder().rec(rb))
  }

  def mem_obj_head(rt: IR, rb: IR, mOpen: String): Unit = {
    assert(mOpen == "{" && rt == IR.IRZ)
    memSpaceBuilder = Some(MemBuilder().obj(rb))
  }

  def mem_tslot(rt: IR, ts: Int, mOpen: String): Unit = {
    assert(mOpen == "{" && rt == IR.IRZ)
    memSpaceBuilder = Some(MemBuilder().typed(StackSlot.Typed(ts)))
  }

  def mem_field(rb: IR, field: FieldReference, mOpen: String): Unit = {
    assert(mOpen == "{" && rb == IR.IRZ)
    memSpaceBuilder = Some(MemBuilder().static(field))
  }

  // memory access modifiers
  private def fieldRefFixups(fields: Array[Any]) = fields.map { case f: FieldReference => f } ensuring(_.length == fields.length)
  def fieldseq(fields: Array[Any]): Unit = {
    val builder = memSpaceBuilder.get
    fieldRefFixups(fields).foreach(builder.field)
  }

  // memory access closers
  def ld_fieldseq(rd: IR, fields: Array[Any], mClose: String): Unit = {
    assert(mClose == "}")
    val builder = memSpaceBuilder.get
    fieldRefFixups(fields).foreach(builder.field)
    builder.load(rd).gen(this)
    memSpaceBuilder = None
  }

  def ld_fieldseq(rd: FR, fields: Array[Any], mClose: String): Unit = {
    assert(mClose == "}")
    val builder = memSpaceBuilder.get
    fieldRefFixups(fields).foreach(builder.field)
    builder.load(rd).gen(this)
    memSpaceBuilder = None
  }

  def st_fieldseq(rv: IR, fields: Array[Any], mClose: String): Unit = {
    assert(mClose == "}")
    val builder = memSpaceBuilder.get
    fieldRefFixups(fields).foreach(builder.field)
    builder.store(rv).gen(this)
    memSpaceBuilder = None
  }

  def st_fieldseq(rv: FR, fields: Array[Any], mClose: String): Unit = {
    assert(mClose == "}")
    val builder = memSpaceBuilder.get
    fieldRefFixups(fields).foreach(builder.field)
    builder.store(rv).gen(this)
    memSpaceBuilder = None
  }

  def ld_reg(rx: IR, mClose: String): Unit = {
    val builder = memSpaceBuilder.get
    builder.load(rx).gen(this)
    memSpaceBuilder = None
  }

  def ld_reg(rx: FR, mClose: String): Unit = {
    val builder = memSpaceBuilder.get
    builder.load(rx).gen(this)
    memSpaceBuilder = None
  }
  
  def st_reg(rx: IR, mClose: String): Unit = {
    val builder = memSpaceBuilder.get
    builder.store(rx).gen(this)
    memSpaceBuilder = None
  }
  
  def st_reg(rx: FR, mClose: String): Unit = {
    val builder = memSpaceBuilder.get
    builder.store(rx).gen(this)
    memSpaceBuilder = None
  }

  // calls
  def call_direct(mr: MethodReference, rd: IR): Unit = callDirect(rd, BytecodeReferenceSymbol(mr))
  def call_virt(mr: MethodReference, rd: IR): Unit   = callVirt(rd, BytecodeReferenceSymbol(mr))
  def call_interf(mr: MethodReference, rd: IR): Unit = callInterf(rd, BytecodeReferenceSymbol(mr))

  // allocs
  def newobj(rd: IR, sig: Signature): Unit = newobj(BytecodeReferenceSymbol(sig)).ensuring(rd == IR.IR1)
  def newarr(rd: IR, ry: IR, sig: Signature): Unit = newarr(sig).ensuring(rd == IR.IR1 && ry == IR.IR2)

  // bfx
  def bfxs_32_32(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W32, Width.W32, true,  offset.toInt, size.toInt)
  def bfxs_32_64(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W32, Width.W64, true,  offset.toInt, size.toInt)
  def bfxs_64_32(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W64, Width.W32, true,  offset.toInt, size.toInt)
  def bfxs_64_64(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W64, Width.W64, true,  offset.toInt, size.toInt)
  def bfxz_32_32(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W32, Width.W32, false, offset.toInt, size.toInt)
  def bfxz_32_64(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W32, Width.W64, false, offset.toInt, size.toInt)
  def bfxz_64_32(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W64, Width.W32, false, offset.toInt, size.toInt)
  def bfxz_64_64(dst: IR, src: IR, offset: Long, size: Long): Unit = bfx(dst, src, Width.W64, Width.W64, false, offset.toInt, size.toInt)

  // endregion
}
