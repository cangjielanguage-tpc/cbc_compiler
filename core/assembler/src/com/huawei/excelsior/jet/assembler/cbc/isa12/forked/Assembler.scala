/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BuiltinSignature, FieldReference, FieldReferenceWithType, MethodReference, Signature, SingleFieldReference, StringLiteral}
import com.huawei.excelsior.jet.assembler.cbc.{CbcFileFormat, CbcTypeKind, StackSlot, Register as Rg}
import com.huawei.excelsior.jet.assembler.cbc.Register.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR1, IRZ}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W16, W32, W64, W8}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{CC, Checked, Common, Width}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.LoadAccessKind.{LD_F32, LD_F64, LD_REC, LD_REF}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.StoreAccessKind.{ST_F32, ST_F64, ST_REF}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{LoadAccessKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.Opcode.{Ld, Ld_Static, St, St_Static}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.RegGroup.{DivCheck, NullCheck}
import com.huawei.excelsior.jet.assembler.cbc.isa12.{LivenessAnalyzer, LivenessInfoCollector, Assembler as OldAssembler}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.{FloatOperations, Opcode, RegGroup, RegSymGroup, low4, scut4}
import com.huawei.excelsior.jet.assembler.{AsmEmitter, AsmType, Fixup, Label, Symbol, Width as AsmWidth}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.annotation.nowarn

trait ForkedAssembler {
  self: AsmEmitter.WithLiterals & SymbolAdapter =>

  private def segment = self.seg

  var analyzer: FlowAnalyzer = FlowAnalyzer.Stub
  private lazy val livenessCollector = LivenessInfoCollector()

  private val FILLER = 0
  private implicit def _asm: ForkedAssembler = this

  private val IR_ACC_IDX = 14

  /** Save gc map state gathered using [[LivenessAnalyzer]] */
  def saveState(): Unit = analyzer match {
    case analyzer: LivenessAnalyzer => livenessCollector.saveStates(segment, analyzer.state.toSeq)
    case _ =>
  }

  def collectLiveness: LivenessInfoCollector.AllStates = livenessCollector.collect

  def stream = new SegmentByteStream(segment)
  def instr(action: => Unit): Unit = analyzer.op(action)
  def fixup(fixup: Fixup): Unit = self.addFixup(fixup)

  def mov(dst: Rg, src: Rg, reference: Boolean): Unit = {
    (dst, src) match {
      case (dst: IR, src: IR) if reference => movRef(dst, src)
      // FIXME: support W32
      case (dst: IR, src: IR) => mov(dst, src, W64)
      case (dst: FR, src: FR) => fmov(dst, src, W64)
      case (dst: FR, src: IR) => movi2f(dst, src, W64)
      case (dst: IR, src: FR) => movf2i(dst, src, W64)
    }
  }

  def mov(dst: IR, src: IR, width: Width): Unit = instr {
    analyzer.trans(dst, src)
    stream
      .opc8(if width == W32 then Opcode.Mov32 else Opcode.Mov64)
      .bits(_.w4(dst).w4(src))
  }

  def movAcc(src: IR): Unit = instr {
    stream
      .opc8(Opcode.Mov64)
      .bits(_.w4(IR_ACC_IDX).w4(src))
  }

  def movRef(dst: IR, src: IR): Unit = instr {
    analyzer.trans(dst, src)
    stream
      .opc8(Opcode.MovRef)
      .bits(_.w4(dst).w4(src))
  }

  def movi32(r: IR, imm: Int): Unit = instr {
    stream
      .opc8(Opcode.Mov32i)
      .bits(_.w4(analyzer.prim(r)).w4(low4(imm)))
      .sleb(scut4(imm))
  }

  def movi64(r: IR, imm: Long): Unit = instr {
    stream
      .opc8(Opcode.Mov64i)
      .bits(_.w4(analyzer.prim(r)).w4(low4(imm)))
      .sleb(scut4(imm))
  }

  def fmovi(r: FR, fimm: Double, w: AsmWidth): Unit = {
    if (Width(w) == W32) {
      stream
        .opc8(Opcode.FMov32i)
        .bits(_.w4(FILLER).w4(r))
        .write32(floatToRawIntBits(fimm.toFloat))
    } else {
      stream
        .opc8(Opcode.FMov64i)
        .bits(_.w4(FILLER).w4(r))
        .write64(doubleToRawLongBits(fimm))
    }
  }

  def movbp(dst: IR, local: Boolean): Unit = {
    stream
      .opc8(Opcode.MovBP)
      .bits(_.w4(dst).write(0, 3).w1(local))
  }

  def nop(): Unit = stream.opc8(Opcode.Nop)

  def neg(ird: IR, irs: IR, w: AsmWidth): Unit = sub(w, ird, IRZ, irs)

  def add(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Add, w, d, l, r)
  def sub(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Sub, w, d, l, r)
  def mul(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Mul, w, d, l, r)
  def pow(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Pow, w, d, l, r)
  def and(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.And, w, d, l, r)
  def or(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Or, w, d, l, r)
  def xor(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.Xor, w, d, l, r)
  def div(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.SDiv, w, d, l, r)
  def rem(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.SRem, w, d, l, r)
  def udiv(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.UDiv, w, d, l, r)
  def urem(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.URem, w, d, l, r)
  def lsl(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.LSL, w, d, l, r)
  def lsr(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.LSR, w, d, l, r)
  def asr(w: AsmWidth, d: IR, l: IR, r: IR): Unit = genBinary(Common.ASR, w, d, l, r)
  def subi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = addi(w, d, l, -imm)
  def addi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.Add, w, d, l, imm)
  def muli(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.Mul, w, d, l, imm)
  def powi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.Pow, w, d, l, imm)
  def andi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.And, w, d, l, imm)
  def ori(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.Or, w, d, l, imm)
  def xori(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.Xor, w, d, l, imm)
  def divi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.SDiv, w, d, l, imm)
  def remi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.SRem, w, d, l, imm)
  def udivi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.UDiv, w, d, l, imm)
  def uremi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.URem, w, d, l, imm)
  def lsli(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.LSL, w, d, l, imm)
  def lsri(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.LSR, w, d, l, imm)
  def asri(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = genBinaryImm(Common.ASR, w, d, l, imm)
  def cadd(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.Add, w, d, l, r)
  def csub(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.Sub, w, d, l, r)
  def cmul(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.Mul, w, d, l, r)
  def cdiv(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.Div, w, d, l, r)
  def cuadd(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.UAdd, w, d, l, r)
  def cusub(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.USub, w, d, l, r)
  def cumul(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.UMul, w, d, l, r)
  def caddi(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.Add, w, d, l, imm)
  def csubi(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.Sub, w, d, l, imm)
  def cmuli(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.Mul, w, d, l, imm)
  def cuaddi(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.UAdd, w, d, l, imm)
  def cusubi(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.USub, w, d, l, imm)
  def cumuli(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.UMul, w, d, l, imm)
  def cpow(d: IR, l: IR, r: IR, w: AsmWidth): Unit = genCheckedBinary(Checked.Pow, w, d, l, r)
  def cpowi(d: IR, l: IR, imm: Long, w: AsmWidth): Unit = genCheckedBinaryImm(Checked.Pow, w, d, l, imm)

  def atomicLoad(dst: IR, obj: IR, f: FieldReference): Unit = {
    analyzer.useRef(obj)
    if (f.asInstanceOf[SingleFieldReference].fieldType.isReference) {
      analyzer.ref(dst)
    } else {
      analyzer.prim(dst)
    }
    stream
      .opc8(Opcode.AtomicLoad)
      .bits(_.w4(dst).w4(obj))
      .sym16(f)
  }

  def atomicStore(src: IR, obj: IR, f: FieldReference): Unit = {
    analyzer.useRef(obj)
    if (f.asInstanceOf[SingleFieldReference].fieldType.isReference) {
      analyzer.useRef(src)
    } else {
      analyzer.usePrim(src)
    }
    stream
      .opc8(Opcode.AtomicStore)
      .bits(_.w4(src).w4(obj))
      .sym16(f)
  }

  def cas(dst: IR, obj: IR, src1: IR, src2: IR, f: FieldReference): Unit = {
    analyzer.useRef(obj)
    analyzer.prim(dst)
    if (f.asInstanceOf[SingleFieldReference].fieldType.isReference) {
      analyzer.useRef(src1)
      analyzer.useRef(src2)
    } else {
      analyzer.usePrim(src1)
      analyzer.usePrim(src2)
    }
    stream
      .opc8(Opcode.CAS)
      .bits(_.w4(dst).w4(obj))
      .bits(_.w4(src1).w4(src2))
      .sym16(f)
  }

  def atomicSwap    (dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicSwap,     dst, obj, src, f)
  def atomicFetchAdd(dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicFetchAdd, dst, obj, src, f)
  def atomicFetchSub(dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicFetchSub, dst, obj, src, f)
  def atomicFetchAnd(dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicFetchAnd, dst, obj, src, f)
  def atomicFetchOr (dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicFetchOr,  dst, obj, src, f)
  def atomicFetchXor(dst: IR, obj: IR, src: IR, f: FieldReference): Unit = genAtomicOp(Opcode.AtomicFetchXor, dst, obj, src, f)

  private def genAtomicOp(o: Opcode, dst: IR, obj: IR, src: IR, f: FieldReference): Unit = {
    analyzer.useRef(obj)
    if (f.asInstanceOf[SingleFieldReference].fieldType.isReference) {
      analyzer.ref(dst)
      analyzer.useRef(src)
    } else {
      analyzer.prim(dst)
      analyzer.usePrim(src)
    }
    stream
      .opc8(o)
      .bits(_.w4(dst).w4(obj))
      .bits(_.w4(src).w4(0))
      .sym16(f)
  }

  private def floatOperation(w: Width, op: FloatOperations, r1: FR | IR, r2: FR | IR, r3: FR | IR) = {
    stream
      .opc8(if w == W32 then Opcode.Float32 else Opcode.Float64)
      .bits(_.w4(op).w4(r1))
      .bits(_.w4(r2).w4(r3))
  }

  private def floatOperation(w: AsmWidth, op: FloatOperations, r1: FR | IR, r2: FR | IR, r3: FR | IR): Unit =
    floatOperation(Width(w), op, r1, r2, r3)

  def fadd(w: AsmWidth, d: FR, l: FR, r: FR): Unit = floatOperation(w, FloatOperations.Add, d, l, r)
  def fsub(w: AsmWidth, d: FR, l: FR, r: FR): Unit = floatOperation(w, FloatOperations.Sub, d, l, r)
  def fmul(w: AsmWidth, d: FR, l: FR, r: FR): Unit = floatOperation(w, FloatOperations.Mul, d, l, r)
  def fdiv(w: AsmWidth, d: FR, l: FR, r: FR): Unit = floatOperation(w, FloatOperations.Div, d, l, r)
  def fneg(d: FR, s: FR, w: AsmWidth): Unit  = fneg(d, d, s, w)
  def fsqrt(d: FR, s: FR, w: AsmWidth): Unit = fsqrt(d, d, s, w)
  def fabs(d: FR, s: FR, w: AsmWidth): Unit  = fabs(d, d, s, w)
  def fmov(d: FR, s: FR, w: Width): Unit     = floatOperation(w, FloatOperations.Mov, d, d, s)

  def fmov(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit  = floatOperation(w, FloatOperations.Mov, frd2, frd1, frs)
  def fneg(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit  = floatOperation(w, FloatOperations.Neg, frd2, frd1, frs)
  def fabs(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit  = floatOperation(w, FloatOperations.Abs, frd2, frd1, frs)
  def fsqrt(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit = floatOperation(w, FloatOperations.Sqrt, frd2, frd1, frs)

  def movi2f(d: FR, s: IR, w: Width): Unit = instr {
    floatOperation(w, FloatOperations.I2f, d, d, s); analyzer.usePrim(s);
  }

  def movf2i(d: IR, s: FR, w: Width): Unit = instr {
    floatOperation(w, FloatOperations.F2i, d, d, s); analyzer.prim(d)
  }

  private def genCheckedBinary(op: Checked, w: AsmWidth, d: IR, l: IR, r: IR): Unit = instr {
    analyzer.prim(d)
    analyzer.usePrim(l)
    analyzer.usePrim(r)
    stream
      .opc8(Opcode.ThreeAddress.checkedBinary(Width(w)))
      .bits(_.w4(op.ordinal).w4(d))
      .bits(_.w4(l).w4(r))
  }

  private def genCheckedBinaryImm(op: Checked, w: AsmWidth, d: IR, l: IR, imm: Long): Unit = instr {
    analyzer.trans(d, l)
    stream
      .opc8(Opcode.ThreeAddress.checkedBinaryImm(Width(w)))
      .bits(_.w4(op.ordinal).w4(d))
      .bits(_.w4(l).w4(low4(imm)))
      .sleb(scut4(imm))
  }

  private def genBinary(op: Common, w: AsmWidth, d: IR, l: IR, r: IR): Unit = instr {
    analyzer.prim(d)
    analyzer.usePrim(l)
    analyzer.usePrim(r)
    Opcode.TwoAddress.binary(op, Width(w)) match {
      case Some(opc) if d == l =>
        stream
          .opc8(opc)
          .bits(_.w4(d).w4(r))
      case _ =>
        stream
          .opc8(Opcode.ThreeAddress.binary(Width(w)))
          .bits(_.w4(op.ordinal).w4(d))
          .bits(_.w4(l).w4(r))
    }
  }

  private def genBinaryImm(op: Common, w: AsmWidth, d: IR, l: IR, imm: Long): Unit = instr {
    analyzer.trans(d, l)
    stream
      .opc8(if Width(w) == W32 then Opcode.BinaryImm32 else Opcode.BinaryImm64)
      .bits(_.w4(op.ordinal).w4(d))
      .bits(_.w4(l).w4(low4(imm)))
      .sleb(scut4(imm))
  }

  def convertFromTo(fromType: AsmType, toType: AsmType, from: Rg, to: Rg): Unit = convert(toType, fromType, to, from)

  def convert(toType: AsmType, fromType: AsmType, to: Rg, from: Rg): Unit = instr {
    if (!fromType.isFloatingPoint) analyzer.usePrim(from.asInstanceOf[IR])
    if (!toType.isFloatingPoint) analyzer.prim(to.asInstanceOf[IR])
    stream
      .opc8(Opcode.Cast)
      .bits(_.w4(toType).w4(fromType))
      .bits(_.w4(to).w4(from))
  }

  private def regGroup(opc: RegGroup, r: IR | FR): Unit = {
    stream
      .opc8(Opcode.RegGroup)
      .bits(_.w4(opc).w4(r))
  }

  def nullcheck(r: IR): Unit = instr { regGroup(NullCheck, analyzer.useRef(r))}
  def divisorCheck(r: IR): Unit =       instr { regGroup(RegGroup.DivCheck, analyzer.usePrim(r)) }
  def catchEx(dst: IR): Unit =          instr { regGroup(RegGroup.Catch, analyzer.ref(dst)) }
  def throwEx(dst: IR): Unit =          instr { regGroup(RegGroup.Throw, analyzer.useRef(dst)) }
  def ret(v: IR, w: AsmWidth): Unit =   instr { regGroup(RegGroup.iret(Width(w)), analyzer.usePrim(v)) }
  def retRef(v: IR): Unit =             instr { regGroup(RegGroup.RetRef, analyzer.useRef(v)) }

  def fret(v: FR, w: AsmWidth): Unit = regGroup(RegGroup.fret(Width(w)), v)

  private def regSymGroup(opc: RegSymGroup, rd: IR, methodId: MethodReference | Signature): Unit = {
    stream
      .opc8(Opcode.RegSymGroup)
      .bits(_.w4(opc).w4(rd))
      .sym16(methodId)
  }

  def spawn(closure: IR, closureType: Signature): Unit = instr {
    regSymGroup(RegSymGroup.Spawn, analyzer.useRef(closure), closureType)
    saveState()
  }

  def spawnFuture(future: IR, retType: Signature): Unit = instr {
    regSymGroup(RegSymGroup.SpawnFuture, analyzer.useRef(future), retType)
    analyzer.ref(IR.IR1)
    saveState()
  }

  def newobj(t: Signature): Unit = instr {
    regSymGroup(RegSymGroup.NewObj, IR.IR1, t); analyzer.ref(IR.IR1)
    saveState()
  }

  def newobjGeneric(ti: IR, t: Signature): Unit = instr {
    regSymGroup(RegSymGroup.NewObjGeneric, analyzer.usePrim(ti), t); analyzer.ref(IR.IR1)
    saveState()
  }

  def newClosureGeneric(ti: IR, t: Signature): Unit = instr {
    regSymGroup(RegSymGroup.NewClosureGeneric, analyzer.usePrim(ti), t); analyzer.ref(IR.IR1)
    saveState()
  }

  def loadTypeInfoSig(dst: IR, sig_id: Signature): Unit = instr {
    regSymGroup(RegSymGroup.LoadTypeInfoSig, analyzer.prim(dst), sig_id)
  }

  def loadTypeInfoGeneric(dst: IR, sig_id: Signature): Unit = instr {
    regSymGroup(RegSymGroup.LoadTypeInfoGeneric, analyzer.prim(dst), sig_id)
  }

  // load type argument `idx` from `ti` and put it on `dst`
  def typeArg(ti: IR, idx: Int, dst: IR): Unit = instr {
    stream
      .opc8(Opcode.TypeArg)
      .bits(_.w4(analyzer.usePrim(ti)).w4(analyzer.prim(dst)))
      .sleb(idx)
  }

  def box(src: StackSlot.Typed, dst: IR): Unit = instr {
    stream // - allocate box at `dst`
      .opc8(Opcode.BoxT) // - store primitive value at `src` to box or copy record pointed by `src` to the box
      .bits(_.w4(0).w4(analyzer.ref(dst)))
      .ts16(src)
    saveState()
  }

  def box(src: Rg, dst: IR, tpe: Signature): Unit = instr {
    src match {
      case x: IR => analyzer.useAny(x)
      case _ =>
    }
    tpe match {
      case t: BuiltinSignature => stream // - allocate box at `dst`
        .opc8(Opcode.Box) // - store primitive value at `src` to box
        .bits(_.w4(src).w4(analyzer.ref(dst)))
        .write8(t.id)
      case _ => stream // - allocate box at `dst`
        .opc8(Opcode.BoxRec) // - copy record pointed by `src` to box
        .bits(_.w4(src).w4(analyzer.ref(dst)))
        .sym16(tpe)
    }
    saveState()
  }

  def unbox(dst: StackSlot.Typed, src: IR): Unit = instr {
    stream
      .opc8(Opcode.UnboxT)
      .bits(_.w4(0).w4(analyzer.useRef(src)))
      .ts16(dst)
  }

  def unbox(dst: Rg, src: IR, tpe: Signature): Unit = instr {
    dst match {
      case x: IR if tpe.isInstanceOf[BuiltinSignature] => analyzer.prim(x)
      case x: IR => analyzer.rec(x)
      case _ =>
    }
    tpe match {
      case t: BuiltinSignature => stream
        .opc8(Opcode.Unbox)
        .bits(_.w4(dst).w4(analyzer.useRef(src)))
        .write8(t.id)
      case _ => stream
        .opc8(Opcode.UnboxRec)
        .bits(_.w4(dst).w4(analyzer.useRef(src)))
        .sym16(tpe)
    }
  }

  def addOffset(dst: IR, fr: FieldReference, ti: IR): Unit = instr {
    analyzer.prim(dst)
    analyzer.usePrim(dst)
    stream
      .opc8(Opcode.AddOffset)
      .bits(_.w4(dst).w4(ti))
      .sym16(fr)
  }

  def offset(dst: IR, fr: FieldReference, ti: IR): Unit = instr {
    analyzer.prim(dst)
    stream
      .opc8(Opcode.Offset)
      .bits(_.w4(dst).w4(ti))
      .sym16(fr)
  }

  def tagGeneric(dst: IR, src: IR, underlyingTi: IR, optionType: Signature): Unit = instr {
    analyzer.prim(dst)
    analyzer.useAny(src)
    analyzer.usePrim(underlyingTi)
    stream
      .opc8(Opcode.TagGeneric)
      .bits(_.w4(dst).w4(src))
      .bits(_.w4(underlyingTi).w4(underlyingTi))
      .sym16(optionType)
  }

  def payloadGeneric(dst: IR, src: IR, underlyingTi: IR, optionTi: IR, optionType: Signature): Unit = instr {
    analyzer.ref(dst)
    analyzer.useAny(src)
    analyzer.usePrim(underlyingTi)
    analyzer.usePrim(optionTi)
    stream
      .opc8(Opcode.PayloadGeneric)
      .bits(_.w4(dst).w4(src))
      .bits(_.w4(underlyingTi).w4(optionTi))
      .sym16(optionType)
    saveState()
  }

  def newNoneGeneric(dst: IR, underlyingTi: IR, optionTi: IR, optionType: Signature): Unit = instr {
    analyzer.ref(dst)
    analyzer.usePrim(underlyingTi)
    analyzer.usePrim(optionTi)
    stream
      .opc8(Opcode.NewNoneGeneric)
      .bits(_.w4(dst).w4(underlyingTi))
      .bits(_.w4(optionTi).w4(optionTi))
      .sym16(optionType)
    saveState()
  }

  def newSomeGeneric(dst: IR, src: IR, underlyingTi: IR, optionTi: IR, optionType: Signature): Unit = instr {
    analyzer.ref(dst)
    analyzer.useAny(src)
    analyzer.usePrim(underlyingTi)
    analyzer.usePrim(optionTi)
    stream
      .opc8(Opcode.NewSomeGeneric)
      .bits(_.w4(dst).w4(src))
      .bits(_.w4(underlyingTi).w4(optionTi))
      .sym16(optionType)
    saveState()
  }

  def assignGeneric(dst: IR, src: IR, baseTi: IR): Unit = instr {
    analyzer.useRef(dst)
    analyzer.useRef(src)
    analyzer.usePrim(baseTi)
    stream
      .opc8(Opcode.AssignGeneric)
      .bits(_.w4(dst).w4(src))
      .bits(_.w4(baseTi).w4(0))
    saveState()
  }

  def instanceOfGeneric(dst: IR, obj: IR, typeInfo: IR): Unit = instr {
    analyzer.prim(dst)
    analyzer.useRef(obj)
    analyzer.usePrim(typeInfo)
    stream
      .opc8(Opcode.InstanceOfGeneric)
      .bits(_.w4(dst).w4(obj))
      .bits(_.w4(typeInfo).w4(0))
  }

  private def checkAotData(opc: (RegSymGroup | Opcode), ref: MethodReference): Unit = {
    ((opc, ref.aotData): @unchecked) match {
      case (_, None) =>
      case (RegSymGroup.CallInterf, Some(_: CbcFileFormat.InterfaceCallAotData)) =>
      case (Opcode.CallInterfGeneric, Some(_: CbcFileFormat.InterfaceCallAotData)) =>
      case (RegSymGroup.CallDirect, Some(_: CbcFileFormat.DirectCallAotData)) =>
      case (RegSymGroup.CallVirt, Some(_: CbcFileFormat.VirtualCallAotData)) =>
    }
  }

  def callInterf(rd: IR, ref: MethodReference): Unit = {
    checkAotData(RegSymGroup.CallInterf, ref)
    regSymGroup(RegSymGroup.CallInterf, rd, ref)
    saveState()
  }

  def callInterfGeneric(outerTiLoc: (IR | StackSlot.Untyped), ref: MethodReference): Unit = {
    checkAotData(Opcode.CallInterfGeneric, ref)
    val outerTiIdx = outerTiLoc match {
      case x: IR => x.idx
      case x: StackSlot.Untyped => x.slot + IR.count
    }
    stream
      .opc8(Opcode.CallInterfGeneric)
      .write16(outerTiIdx)
      .sym16(ref)
    saveState()
  }

  def callDirect(rd: IR, ref: MethodReference): Unit = {
    checkAotData(RegSymGroup.CallDirect, ref)
    regSymGroup(RegSymGroup.CallDirect, rd, ref)
    saveState()
  }

  def callVirt(rd: IR, ref: MethodReference): Unit = {
    checkAotData(RegSymGroup.CallVirt, ref)
    regSymGroup(RegSymGroup.CallVirt, rd, ref)
    saveState()
  }

  def callClosure(rd: IR, tpe: Signature): Unit = {
    assert(tpe.isInstanceOf[CbcFileFormat.Functional])
    regSymGroup(RegSymGroup.CallClosure, rd, tpe)
    saveState()
  }

  def callClosureGeneric(rd: IR, tpe: Signature): Unit = {
    assert(tpe.isInstanceOf[CbcFileFormat.Functional])
    regSymGroup(RegSymGroup.CallClosureGeneric, rd, tpe)
    saveState()
  }

  def newClosure(rd: IR, t: Signature): Unit = instr {
    regSymGroup(RegSymGroup.NewClosure, rd, t); analyzer.ref(rd)
    saveState()
  }

  def prepareRecord(ts: StackSlot.Typed): Unit = {
    stream
      .opc8(Opcode.PrepareRecord)
      .ts16(ts)
  }

  def loadRawMemory(dst: Rg, base: IR, ldk: LoadAccessKind, offset: Long): Unit = {
    lazy val idst = dst.asInstanceOf[IR]
    lazy val fdst = dst.asInstanceOf[FR]

    ldk match {
      case LD_F32 =>
      case LD_F64 =>
      case LD_REC => analyzer.rec(idst)
      case LD_REF => analyzer.ref(idst)
      case _      => analyzer.prim(idst) // records?
    }

    // TODO: optimize VLE encoding, so next `sleb` will be present only if low4 `offset` is too big.
    stream
      .opc8(Opcode.LoadRawMemory)
      .bits(_.w4(dst).w4(analyzer.useAny(base)))
      .bits(_.w4(ldk).w4(low4(offset)))
      .sleb(scut4(offset))
  }

  def storeRawMemory(src: IR | FR, base: IR, stk: StoreAccessKind, offset: Long): Unit = {
    src match {
      case x: IR => analyzer.useAny(x)
      case _ =>
    }

    // TODO: optimize VLE encoding, so next `sleb` will be present only if low4 `offset` is too big.
    stream
      .opc8(Opcode.StoreRawMemory)
      .bits(_.w4(src).w4(analyzer.useAny(base)))
      .bits(_.w4(stk).w4(low4(offset)))
      .sleb(scut4(offset))
  }

  def jmp(target: Label): Unit = doJmp(target)
  def bcc(op: BranchOp, lhs: IR, rhs: IR, width: AsmWidth, target: Label): Unit = doBcc(op, lhs, rhs, width, target)
  def bcc(op: BranchOp, lhs: FR, rhs: FR, width: AsmWidth, target: Label): Unit = doBcc(op, lhs, rhs, width, target)
  def bcc(op: BranchOp, lhs: IR, imm: Long, width: AsmWidth, target: Label): Unit = doBccImm(op, lhs, imm, width, target)

  final def doJmp(target: Label, wide: Boolean = false): Unit =
    addFixup(Fixups.Jump(target, wide))

  final def doBcc(op: BranchOp, lhs: IR | FR, rhs: IR | FR, width: AsmWidth, target: Label, wide: Boolean = false): Unit = instr {
    if (op.isReference) {
      analyzer.useRef(lhs.asInstanceOf[IR]);
      analyzer.useRef(rhs.asInstanceOf[IR])
    } else if (op.isIntegral) {
      analyzer.usePrim(lhs.asInstanceOf[IR]);
      analyzer.usePrim(rhs.asInstanceOf[IR])
    }
    addFixup(Fixups.Bcc(op, lhs, rhs, Width(width), target, wide))
  }

  final def doBccImm(op: BranchOp, lhs: IR, imm: Long, width: AsmWidth, target: Label, wide: Boolean = false): Unit = instr {
    if (op.isReference) analyzer.useRef(lhs).ensuring(imm == 0) else analyzer.usePrim(lhs)
    if (imm != 0) {
      addFixup(Fixups.BccImm(op, lhs, imm, Width(width), target, wide))
    } else {
      bcc(op, lhs, IR.IRZ, width, target)
    }
  }

  def scc(op: BranchOp, dst: IR, l: IR, r: IR, width: AsmWidth): Unit = instr {
    analyzer.prim(dst);
    analyzer.usePrim(l);
    analyzer.usePrim(r)

    val w = Width(width)
    val (cc, swap) = CondConversions.normalize(op)
    val (lhs, rhs) = if (!swap) (l, r) else (r, l)
    assert(w == W32 || w == W64)

    stream
      .opc8(if (w == W32) Opcode.Scc32 else Opcode.Scc64)
      .bits(_.w4(cc).w4(dst))
      .bits(_.w4(lhs).w4(rhs))
  }

  def scc(op: BranchOp, dst: IR, lhs: IR, _imm: Long, width: AsmWidth): Unit = instr {
    analyzer.trans(dst, lhs)

    val w = Width(width)
    val (cc, imm) = CondConversions.normalizeImm(op, _imm, w)
    assert(w == W32 || w == W64)

    stream
      .opc8(if (w == W32) Opcode.SccImm32 else Opcode.SccImm64)
      .bits(_.w4(cc).w4(dst))
      .bits(_.w4(lhs).w4(low4(imm)))
      .sleb(scut4(imm))
  }

  def ldstackrec(dst: IR, ts: StackSlot.Typed): Unit = instr {
    stream
      .opc8(Opcode.LoadStackRec)
      .bits(_.w4(analyzer.rec(dst)).w4(0))
      .ts16(ts)
  }

  def newarr(sig: Signature): Unit = instr {
    stream
      .opc8(Opcode.NewArr)
      .bits(_.w4(analyzer.ref(IR.IR1)).w4(analyzer.usePrim(IR.IR2)))
      .sym16(sig)
    saveState()
  }

  def loadArray(dst: Rg, ldk: LoadAccessKind, arr: IR, idx: IR): Unit = instr {
    dst match {
      case dst: IR => if (ldk == LD_REF) analyzer.ref(dst) else analyzer.prim(dst)
      case _ =>
    }
    stream
      .opc8(Opcode.LoadArray)
      .bits(_.w4(dst).w4(ldk))
      .bits(_.w4(analyzer.useRef(arr)).w4(analyzer.usePrim(idx)))
  }

  def storeArray(src: Rg, stk: StoreAccessKind, arr: IR, idx: IR): Unit = instr {
    src match {
      case dst: IR => if (stk == ST_REF) analyzer.ref(dst) else analyzer.prim(dst)
      case _ =>
    }
    stream
      .opc8(Opcode.StoreArray)
      .bits(_.w4(src).w4(stk))
      .bits(_.w4(analyzer.useRef(arr)).w4(analyzer.usePrim(idx)))
  }

  def loadTypeInfoObj(dst: IR, obj: IR): Unit = instr {
    stream
      .opc8(Opcode.LoadTypeInfoObj)
      .bits(_.w4(analyzer.prim(dst)).w4(analyzer.useRef(obj)))
  }

  def isInstanceOf(dst: IR, obj: IR, sig_id: Signature): Unit = instr {
    stream
      .opc8(Opcode.InstanceOf)
      .bits(_.w4(analyzer.prim(dst)).w4(analyzer.useRef(obj)))
      .sym16(sig_id)
  }

  def gcpoint(): Unit = {
    instr { stream.opc8(Opcode.GcPoint) }
    saveState()
  }

  def initobj(ts: StackSlot.Typed): Unit = {
    stream
      .opc8(Opcode.InitObj)
      .ts16(ts)
  }

  def initConstString(ts: StackSlot.Typed, stringId: StringLiteral): Unit = {
    // FIXME: `stringId` can use uleb encoding, but actual id is knowm much later.
    //        RawData and String must be resolved at later step in the same time when label fixups are resolved.
    stream
      .opc8(Opcode.InitString)
      .sym32(stringId)
      .ts16(ts)
  }

  def zerorefs(ts: StackSlot.Typed): Unit = {
    prepareRecord(ts)
  }

  def lenarr(dst: IR, ra: IR): Unit = instr {
    stream
      .opc8(Opcode.ArrayLength)
      .bits(_.w4(analyzer.prim(dst)).w4(analyzer.useRef(ra)))
  }

  def arrIC(ri: IR, rl: IR): Unit = instr {
    stream
      .opc8(Opcode.ArrayIndexCheck)
      .bits(_.w4(analyzer.usePrim(rl)).w4(analyzer.usePrim(ri)))
  }

  def loadUntypedAcc(ldk: LoadAccessKind, src: StackSlot.Untyped): Unit = instr {
    stream
      .opc8(Opcode.LoadUntyped)
      .bits(_.w4(IR_ACC_IDX).w4(ldk))
      .write16(src.slot)
  }

  def loadUntyped(dst: Rg, ldk: LoadAccessKind, src: StackSlot.Untyped): Unit = instr {
    if (ldk != LD_F32 && ldk != LD_F64) analyzer.trans(dst.asInstanceOf[IR], src)
    stream
      .opc8(Opcode.LoadUntyped)
      .bits(_.w4(dst).w4(ldk))
      .write16(src.slot)
  }

  def storeUntyped(src: Rg, stk: StoreAccessKind, dst: StackSlot.Untyped): Unit = instr {
    if (stk != ST_F32 && stk != ST_F64) analyzer.trans(dst, src.asInstanceOf[IR])
    stream
      .opc8(Opcode.StoreUntyped)
      .bits(_.w4(src).w4(stk))
      .write16(dst.slot)
  }

  def loadUntyped(dst: Rg, tk: CbcTypeKind, src: StackSlot.Untyped): Unit = loadUntyped(dst, LoadAccessKind.from(tk), src)

  def storeUntyped(src: Rg, tk: CbcTypeKind, dst: StackSlot.Untyped): Unit = storeUntyped(src, StoreAccessKind.from(tk), dst)

  def storeUntypedImm(src: Long, dst: StackSlot.Untyped): Unit = {
    stream
      .opc8(Opcode.StoreUntypedImm)
      .write16(analyzer.prim(dst).slot)
      .sleb(src)
  }

  def recordCopy(dst: IR, src: IR, sigId: Signature): Unit = {
    shouldNotReachHere()
  }

  def bfx(dst: IR, src: IR, resW: AsmWidth, argW: AsmWidth, sx: Boolean, offset: Int, size: Int): Unit = instr {
    assert(resW.nbits == 32 || resW.nbits == 64)
    assert(argW.nbits == 32 || argW.nbits == 64)
    stream
      .opc8(Opcode.BFX)
      .bits(_.w4(analyzer.prim(dst)).w4(analyzer.usePrim(src)))
      .bits(_.w1(resW.nbits == 64).w1(argW.nbits == 64).write(offset, 6))
      .bits(_.w1(sx).write(size, 7))
  }

  // region NewMemops

  private def markLoadStoreValue(r: Rg, fr: FieldReference, load: Boolean): Unit = {
    val hasRefFieldType = fr match {
      case fr: FieldReferenceWithType => fr.fieldType.isReference
      case _ => false
    }
    r match {
      case r: IR if hasRefFieldType => if (load) analyzer.ref(r) else analyzer.useRef(r)
      case r: IR => if (load) analyzer.prim(r) else analyzer.usePrim(r)
      case _ =>
    }
  }

  private def markMemBase(base: IR, fr: FieldReference) = fr match {
    case fr: FieldReferenceWithType if fr.refType.isReference => analyzer.useRef(base)
    case _ => analyzer.useRec(base)
  }

  def ld(dst: Rg, base: IR, fr: FieldReference): Unit = instr {
    stream
      .opc8(Ld)
      .bits(_.w4(dst).w4(base))
      .sym16(fr)
    markMemBase(base, fr)
    markLoadStoreValue(dst, fr, load = true)
  }

  def ld(dst: Rg, fr: FieldReference): Unit = instr {
    stream
      .opc8(Ld_Static)
      .bits(_.w4(dst).w4(dst))
      .sym16(fr)
    markLoadStoreValue(dst, fr, load = true)
  }

  def lea(dst: IR, base: IR, fr: FieldReference): Unit = {
    stream
      .opc8(Opcode.Lea)
      .bits(_.w4(dst).w4(base))
      .sym16(fr)
    markMemBase(base, fr)
    analyzer.prim(dst)
  }

  def st(src: Rg, base: IR, fr: FieldReference): Unit = instr {
    stream
      .opc8(St)
      .bits(_.w4(src).w4(base))
      .sym16(fr)
    markMemBase(base, fr)
    markLoadStoreValue(src, fr, load = false)
  }

  def st(src: Rg, fr: FieldReference): Unit = instr {
    stream
      .opc8(St_Static)
      .bits(_.w4(src).w4(src))
      .sym16(fr)
    markLoadStoreValue(src, fr, load = false)
  }

  // endregion
}

class Assembler extends AsmEmitter.WithLiterals with ForkedAssembler { self: SymbolAdapter =>
  def alignCode(alignment: Int): Unit = shouldNotReachHere("cbc doesn't have it")
  protected def symbolLiteralKind     = shouldNotReachHere("cbc doesn't have it")

  def adapter: SymbolAdapter = self

  def callDirect(rd: IR, methodId: Symbol): Unit = callDirect(rd, adapter.method(methodId))

  def callVirt(rd: IR, methodId: Symbol): Unit = callInterf(rd, adapter.method(methodId))

  def callInterf(rd: IR, sig_id: Symbol, methodId: Symbol): Unit = callInterf(rd, adapter.method(methodId))

  def callInterf(rd: IR, methodId: Symbol): Unit = callInterf(rd, adapter.method(methodId))

  def newobj(sig_idx: Symbol): Unit = {
    newobj(adapter.sigType(sig_idx))
  }

  def loadTypeInfoSig(dst: IR, sig_id: Symbol): Unit             = loadTypeInfoSig(dst, adapter.sigType(sig_id))
  def loadTypeInfoFTC(dst: IR, sig_id: Symbol): Unit             = loadTypeInfoSig(dst, adapter.sigType(sig_id))


  def isInstanceOfClass(dst: IR, obj: IR, sig_id: Symbol): Unit     = isInstanceOf(dst, obj, sig_id)
  def isInstanceOfInterface(dst: IR, obj: IR, sig_id: Symbol): Unit = isInstanceOf(dst, obj, sig_id)
  def isInstanceOfArray(dst: IR, obj: IR, sig_id: Symbol): Unit     = isInstanceOf(dst, obj, sig_id)

  def isInstanceOf(dst: IR, obj: IR, sig_id: Symbol): Unit = isInstanceOf(dst, obj, adapter.sigType(sig_id))

  def initConstString(ts: StackSlot.Typed, stringId: Symbol): Unit = initConstString(ts, adapter.string(stringId))

  def recordCopy(dst: IR, src: IR, sigId: Symbol): Unit = recordCopy(dst, src, adapter.sigType(sigId))

  // FIXME: either delete it or use it for debugging of liveness map gathering
  private def doNothing: Unit = { /* no-op */ }
  private def doNotUseOldLiveness: Unit = shouldNotReachHere("Cannot use old liveness analyzer in standalone")

  def aliveRefCheck(data: Symbol): Unit = doNotUseOldLiveness
  def aliveReference(data: Array[Byte]): Unit = doNotUseOldLiveness
  def unmovableReference(data: Array[Byte]): Unit = doNotUseOldLiveness
  def beginLocalUnmovable(r: IR): Unit = doNothing
  def endLocalUnmovable(r: IR): Unit = doNothing
  def aliveRefDifference(data: Array[Byte]): Unit = doNotUseOldLiveness
  def aliveUnmovableDifference(data: Array[Byte]): Unit = doNotUseOldLiveness
  def aliveRefCheck(data: Array[Byte]): Unit = doNotUseOldLiveness
  def aliveReference(data: Symbol): Unit = doNotUseOldLiveness
  def unmovableReference(data: Symbol): Unit = doNotUseOldLiveness
  def aliveRefDifference(data: Symbol): Unit = doNotUseOldLiveness
  def aliveUnmovableDifference(data: Symbol): Unit = doNotUseOldLiveness

  // TODO: Move meaningful parts to the trait
  def packageInitCheck(sig_id: Symbol): Unit = doNothing

  def arrFill(arr: IR, data: Array[Byte]): Unit = notImplemented("todo")

  def loadConstDataAddr(dst: IR, data: Array[Byte], alignment: Int): Unit = shouldNotReachHere("aj strings")

  def scc(op: BranchOp, dst: IR, src1: FR, src2: FR, width: AsmWidth): Unit = notImplemented("todo")

  def mulh(w: AsmWidth, d: IR, l: IR, r: IR): Unit = notImplemented("todo")
  def umulh(w: AsmWidth, d: IR, l: IR, r: IR): Unit = notImplemented("todo")
  def mulhi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = notImplemented("todo")
  def umulhi(w: AsmWidth, d: IR, l: IR, imm: Long): Unit = notImplemented("todo")

  def ldarr(asmType: AsmType, rd: Rg, ra: IR, ri: IR): Unit = loadArray(rd, LoadAccessKind.from(CbcTypeKind(asmType)), ra, ri)
  def ldarrObj(rd: Rg, ra: IR, ri: IR): Unit = loadArray(rd, LD_REF, ra, ri)

  def ldarrRecord(rd: IR, ra: IR, ri: IR, sig_id: Symbol): Unit = shouldNotReachHere("gc unsafe operation")

  def starr(asmType: AsmType, ra: IR, ri: IR, rv: Rg): Unit = storeArray(rv, StoreAccessKind.from(CbcTypeKind(asmType)), ra, ri)
  def starrObj(ra: IR, ri: IR, rv: Rg): Unit = storeArray(rv, ST_REF, ra, ri)

  def newarr(ftc_sig_id: Symbol): Unit = newarr(adapter.sigType(ftc_sig_id))
  def newarrzv(ftc_sig_id: Symbol): Unit = notImplemented("todo")
  def newarrfillconst(dst: IR, len: IR, value: Long, ftc_sig_id: Symbol): Unit = notImplemented("todo")
  def newarrfillnonconst(dst: IR, len: IR, value: IR, ftc_sig_id: Symbol): Unit = notImplemented("todo")

  def callIndirect(targetReg: IR, sig_id: Symbol): Unit = notImplemented("todo")

  def lea_static(dst: IR, field_id: Symbol): Unit = shouldNotReachHere("gc unsafe operation")
  def lea_us(dst: IR, us: StackSlot.Untyped): Unit = shouldNotReachHere("rec tracing unsafe operation. TODO: special tail instruction")
  def lea_cforeign(dst: IR, method_id: Symbol): Unit = notImplemented("todo")
}

object Assembler {
  sealed trait Ordinal {
    def ordinal: Int
  }

  // TODO remove opcodes for old memops
  enum Opcode extends Ordinal {
    case Bcc32Eq
    case Bcc32Ne
    case Bcc32Lt
    case Bcc32Ge
    case Bcc32Ult
    case Bcc32Uge
    case Bcc64Eq
    case Bcc64Ne
    case Bcc64Lt
    case Bcc64Ge
    case Bcc64Ult
    case Bcc64Uge
    case BccReq
    case BccRne
    case Bcc32
    case Bcc64
    case BccImm32
    case BccImm64
    case Jump
    case WidePrefix // for jumps
    case Mov32
    case Mov64
    case Mov32i
    case Mov64i
    case MovRef
    case FMov32
    case FMov64
    case FMov32i
    case FMov64i
    case MovBP
    case BFX
    case LoadTyped
    case StoreTyped
    case StoreTypedImm
    case Add32
    case Sub32
    case Mul32
    case And32
    case Or32
    case Xor32
    case UDiv32
    case URem32
    case LSR32
    case ASR32
    case LSL32
    case Add64
    case Sub64
    case Mul64
    case And64
    case Or64
    case Xor64
    case UDiv64
    case URem64
    case LSR64
    case ASR64
    case LSL64
    case Binary32
    case Binary64
    case BinaryImm32
    case BinaryImm64
    case Cast
    case NewArr
    case GcPoint
    case PrepareRecord
    case Unused
    case Scc32
    case Scc64
    case SccImm32
    case SccImm64
    case InstanceOf
    case LoadTypeInfoObj
    case RegSymGroup
    case RegGroup
    case InitObj
    case InitString
    case ArrayLength
    case ArrayIndexCheck
    case Float32
    case Float64
    case LoadStatic
    case StoreStatic
    case LoadField
    case StoreField
    case LoadStackRec
    case Nop
    case MemHeadReg
    case MemHeadField
    case MemHeadStatic
    case MemHeadHandle
    case MemHeadTyped
    case LoadUntyped
    case StoreUntyped
    case StoreUntypedImm
    case LoadArray
    case StoreArray
    case TypeArg
    case Box
    case BoxT
    case Unbox
    case UnboxT
    case BoxRec
    case UnboxRec
    case Offset
    case AddOffset
    case TagGeneric
    case PayloadGeneric
    case NewNoneGeneric
    case NewSomeGeneric
    case LoadRawMemory
    case StoreRawMemory
    case CallInterfGeneric
    case AssignGeneric
    case InstanceOfGeneric
    case AtomicLoad
    case AtomicStore
    case CAS
    case AtomicSwap
    case AtomicFetchAdd
    case AtomicFetchSub
    case AtomicFetchAnd
    case AtomicFetchOr
    case AtomicFetchXor
    case CBinary8
    case CBinary16
    case CBinary32
    case CBinary64
    case CBinaryImm8
    case CBinaryImm16
    case CBinaryImm32
    case CBinaryImm64
    case Ld
    case Ld_Static
    case Lea
    case St
    case St_Static
  }

  enum MemOpcode extends Ordinal {
    case Field1
    case Field2
    case Field3
    case Field4
    case Index
    case Load
    case Store
    case StoreImm
    case CopyRegTo
    case CopyRegFrom
    case ConstIndex
    case FieldGeneric
    case ConstIndexGeneric
    case IndexGeneric
    case LoadGeneric
    case StoreGeneric
    case Offset
  }

  enum RegSymGroup extends Ordinal {
    case LoadTypeInfoSig
    case LoadTypeInfoGeneric
    case NewObj
    case CallDirect
    case CallVirt
    case CallInterf
    case Spawn
    case SpawnFuture
    case CallClosure
    case NewClosure
    case CallClosureGeneric
    case NewObjGeneric
    case NewClosureGeneric
  }

  enum RegGroup extends Ordinal {
    case Ret32
    case Ret64
    case FRet32
    case FRet64
    case DivCheck
    case Catch
    case Throw
    case RetRef
    case NullCheck
  }

  enum FloatOperations extends Ordinal {
    case Add
    case Sub
    case Mul
    case Div
    case Mov
    case Neg
    case Abs
    case Sqrt
    case I2f
    case F2i
  }

  @nowarn("msg=match may not be exhaustive")
  object RegGroup {
    def fret(w: Width): RegGroup = w match {
      case W32 => RegGroup.FRet32
      case W64 => RegGroup.FRet64
    }

    def iret(w: Width): RegGroup = w match {
      case W32 => RegGroup.Ret32
      case W64 => RegGroup.Ret64
    }
  }

  @nowarn("msg=match may not be exhaustive")
  object Opcode {

    object TwoAddress {
      def binary(op: Common, w: Width): Option[Opcode] = {
        val hasEncoding = op match {
          case Common.UDiv => true
          case Common.URem => true
          case _ => false
        }
        if (!hasEncoding) {
          return None
        }

        Some((op, w) match {
          case (Common.Add, W32) => Opcode.Add32
          case (Common.Add, W64) => Opcode.Add64
          case (Common.Sub, W32) => Opcode.Sub32
          case (Common.Sub, W64) => Opcode.Sub64
          case (Common.Mul, W32) => Opcode.Mul32
          case (Common.Mul, W64) => Opcode.Mul64
          case (Common.And, W32) => Opcode.And32
          case (Common.And, W64) => Opcode.And64
          case (Common.Or, W32) => Opcode.Or32
          case (Common.Or, W64) => Opcode.Or64
          case (Common.Xor, W32) => Opcode.Xor32
          case (Common.Xor, W64) => Opcode.Xor64
          case (Common.UDiv, W32) => Opcode.UDiv32
          case (Common.UDiv, W64) => Opcode.UDiv64
          case (Common.URem, W32) => Opcode.URem32
          case (Common.URem, W64) => Opcode.URem64
          case (Common.LSR, W32) => Opcode.LSR32
          case (Common.LSR, W64) => Opcode.LSR64
          case (Common.ASR, W32) => Opcode.ASR32
          case (Common.ASR, W64) => Opcode.ASR64
          case (Common.LSL, W32) => Opcode.LSL32
          case (Common.LSL, W64) => Opcode.LSL64
        })
      }
    }

    object ThreeAddress {
      def binary(w: Width): Opcode = w match {
        case W32 => Opcode.Binary32
        case W64 => Opcode.Binary64
      }

      def checkedBinary(w: Width): Opcode = w match {
        case W8  => Opcode.CBinary8
        case W16 => Opcode.CBinary16
        case W32 => Opcode.CBinary32
        case W64 => Opcode.CBinary64
      }

      def checkedBinaryImm(w: Width): Opcode = w match {
        case W8  => Opcode.CBinaryImm8
        case W16 => Opcode.CBinaryImm16
        case W32 => Opcode.CBinaryImm32
        case W64 => Opcode.CBinaryImm64
      }
    }
  }

  def low4(x: Long): Int = MathUtils.bits(x, 0, 3).toInt
  def scut4(x: Long): Long = x >> 4
  def scut4(x: Int): Int   = x >> 4
  def ucut4(x: Long): Long = x >>> 4
  def ucut4(x: Int): Int   = x >>> 4
}
