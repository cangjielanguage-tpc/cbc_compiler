/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{CangjieArray, FieldReference, Signature}
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.MemOpcode.Field1
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.Opcode.{LoadStatic, LoadTyped}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.{MemOpcode, Opcode}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.MemSpace.*
import com.huawei.excelsior.jet.assembler.cbc.{CbcFileFormat, Register, StackSlot}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object MemSpace {
  import BodyOperation.*
  import Head.*
  import TailOperation.*

  val MAX_FIELD_SEQ = 4
  val MAX_COPY_REC_FIELDS = 15

  // TODO: remove FieldReference as field reference usages.

  /**
    * Head of [[Chain]]. Each head is representing a `mem.space.head` operation.
    */
  sealed trait Head

  /**
    * Operation that is performed as part of mem-space.
    */
  sealed trait SpaceOperation

  /**
    * Operation of mem-space that have guaranteed successor ([[SpaceOperation]]).
    */
  sealed trait BodyOperation extends SpaceOperation

  /**
    * Last operation of mem-space, which would leave mem-space to base opcode space.
    */
  sealed trait TailOperation extends SpaceOperation

  private object Head {
    // Separation of `object` and `record` is mostly redundant in run time,
    // since the actual kind of operation is determined by tail or field.
    case class Handle(base: IR, derived: IR) extends Head
    case class Typed(ts: StackSlot.Typed) extends Head
    case class Static(fr: FieldReference) extends Head

    // synthetic! can be created as part of chain optimization.
    case class MemField(base: IR, fr: FieldReference) extends Head
    case class Reg(base: IR, isRef: Boolean) extends Head
  }

  private object BodyOperation {
    case class Field(fr: FieldReference) extends BodyOperation
    case class Index(reg: IR, sigId: Signature, checked: Boolean) extends BodyOperation

    // FIXME: remove refType
    // refType == Tuple[Ts...] | Array<T>
    case class ConstIndex(idx: Long, refType: Signature) extends BodyOperation

    case class FieldGeneric(fr: FieldReference, ti: IR) extends BodyOperation
    case class ConstIndexGeneric(idx: Long, refType: Signature, ti: IR) extends BodyOperation
    case class IndexGeneric(reg: IR, refType: Signature, ti: IR) extends BodyOperation
    case class Offset(reg: IR) extends BodyOperation

    // synthetic! can be created as part of chain optimization.
    case class FieldN(refs: Seq[FieldReference]) extends BodyOperation with BoundedRefs(refs, MAX_FIELD_SEQ)
  }

  private object TailOperation {
    case class Load(reg: Register, isRef: Boolean) extends TailOperation
    case class Store(reg: Register, isRef: Boolean) extends TailOperation
    case class StoreImm(imm: Long, isRef: Boolean) extends TailOperation

    case class LoadGeneric(dst: IR, ti: IR) extends TailOperation
    case class StoreGeneric(src: IR, ti: IR) extends TailOperation

    case class CopyReg(reg: IR, recType: Signature) extends TailOperation
    case class CopyInterior(reg: IR, refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_COPY_REC_FIELDS)
    case class CopyInteriorArr(reg: IR, idx: IR, refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_COPY_REC_FIELDS)
    case class CopyStatic(refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_COPY_REC_FIELDS)
    case class CopyTyped(ts: StackSlot.Typed, refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_COPY_REC_FIELDS)
    case class CopyHandle(base: IR, offs: IR) extends TailOperation

    // synthetic! can be created as part of chain optimization.
    case class LoadFieldSeq(reg: Register, refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_FIELD_SEQ)
    case class StoreFieldSeq(reg: Register, refs: Seq[FieldReference]) extends TailOperation with BoundedRefs(refs, MAX_FIELD_SEQ)
  }

  private trait BoundedRefs(refs: Seq[FieldReference], bound: Int) {
    assert(refs.size <= bound, s"Number of references exceeded possible bound of $bound: $refs")
  }

  /**
    * [[Chain]] builder.
    */
  class Builder {
    private var head: Head = _
    private val operations = ArrayBuffer.empty[BodyOperation]

    private var lastType: Signature = _

    def obj(base: IR) = head(Reg(base, isRef = true))
    def rec(base: IR) = head(Reg(base, isRef = false))
    def handle(base: IR, derived: IR) = head(Head.Handle(base, derived))
    def typed(ts: StackSlot.Typed)   = head(Head.Typed(ts))

    def static(fr: FieldReference) = {
      lastType = fr.fieldType
      head(Head.Static(fr))
    }

    def field(fr: FieldReference): Builder = {
      op(BodyOperation.Field(fr))
      lastType = fr.fieldType
      this
    }

    def fieldGeneric(fr: FieldReference, ti: IR): Builder = {
      op(BodyOperation.FieldGeneric(fr, ti))
      lastType = fr.fieldType
      this
    }

    // TODO: Consider removing `checked`.
    //       Is it allowed to throw out of the middle of memory expressions?
    def index(reg: IR, sigId: Signature, checked: Boolean = false): Builder = {
      lastType = sigId
      op(BodyOperation.Index(reg, sigId, checked))
    }

    def indexGeneric(reg: IR, sigId: Signature, ti: IR): Builder = {
      lastType = sigId
      op(BodyOperation.IndexGeneric(reg, sigId, ti))
    }

    def constIndex(idx: Int, refType: Signature): Builder = {
      lastType = refType match {
        case CangjieArray(elem) => elem
        case CbcFileFormat.Tuple(args) => args(idx)
        case x => shouldNotReachHere(s"unexpected type $x")
      }
      op(BodyOperation.ConstIndex(idx, refType))
    }

    def constIndexGeneric(idx: Int, refType: Signature, ti: IR): Builder = {
      lastType = refType match {
        case CangjieArray(elem) => elem
        case CbcFileFormat.Tuple(args) => args(idx)
        case x => shouldNotReachHere(s"unexpected type $x")
      }
      op(BodyOperation.ConstIndexGeneric(idx, refType, ti))
    }

    def offset(reg: IR): Builder = {
      // last type?
      op(BodyOperation.Offset(reg))
    }

    def loadGeneric(dst: IR, ti: IR): Chain = {
      tail(TailOperation.LoadGeneric(dst, ti))
    }

    def storeGeneric(src: IR, ti: IR): Chain = {
      tail(TailOperation.StoreGeneric(src, ti))
    }

    def load(dst: Register): Chain = {
      assert(lastType != null, "load operation require type")
      tail(TailOperation.Load(dst, lastType.isReference))
    }

    def store(src: Register): Chain = {
      assert(lastType != null, "store operation require type")
      tail(TailOperation.Store(src, lastType.isReference))
    }

    def storeImm(imm: Long): Chain = {
      assert(lastType != null, "store operation require type")
      tail(TailOperation.StoreImm(imm, lastType.isReference))
    }

    def copyReg(reg: IR, recType: Signature): Chain                         = tail(CopyReg(reg, recType))
    def copyInterior(reg: IR, refs: Seq[FieldReference]): Chain             = tail(CopyInterior(reg, refs))
    def copyInteriorArr(reg: IR, idx: IR, refs: Seq[FieldReference]): Chain = tail(CopyInteriorArr(reg, idx, refs))
    def copyStatic(refs: Seq[FieldReference]): Chain                        = tail(CopyStatic(refs))
    def copyTyped(ts: StackSlot.Typed, refs: Seq[FieldReference]): Chain    = tail(CopyTyped(ts, refs))
    def copyHandle(base: IR, offs: IR): Chain                               = tail(CopyHandle(base, offs))

    private def op(op: BodyOperation): Builder = {
      operations += op
      this
    }

    private def head(h: Head): Builder = {
      assert(head == null)
      head = h
      this
    }

    private def tail(tail: TailOperation): Chain = Chain(head.nn, operations.toSeq, tail)
  }

  /**
    * Represents a chain of operations that would be encoded in memspace.
    * Note that actual encoding could be represented as-is (like in chain),
    * but often could be represented more efficiently.
    * The actual encoding is chosen in [[ChainGenerator.gen]]
    */
  case class Chain(head: Head, ops: Seq[BodyOperation], tail: TailOperation) {
    def gen(asm: ForkedAssembler): Unit = ChainGenerator(asm).gen(this)
  }

  private class ChainGenerator(_asm: ForkedAssembler) {

    import BodyOperation.*
    import Head.*
    import TailOperation.*

    implicit val asm: ForkedAssembler = _asm

    private def stream = asm.stream

    def gen(c: Chain): Unit = c match {
      // attempt to generate specialized versions of common memory operations with fallback of full memspace generation.
      case Chain(Reg(base, _), Seq(f: Field), Load(dst, _)) => load(Opcode.LoadField, base, dst, f.fr)
      case Chain(Reg(base, _), Seq(f: Field), Store(src, _)) => store(Opcode.StoreField, base, src, f.fr)
      case Chain(Typed(ts), Seq(f: Field), Load(dst, _)) => loadStoreTyped(Opcode.LoadTyped, ts, dst, f.fr)
      case Chain(Typed(ts), Seq(f: Field), Store(src, _)) => loadStoreTyped(Opcode.StoreTyped, ts, src, f.fr)
      case Chain(Typed(ts), Seq(f: Field), StoreImm(src, _)) => storeImmTyped(Opcode.StoreTypedImm, ts, src, f.fr)
      case Chain(Static(fr), Seq(), Store(src, _)) => loadStoreStatic(Opcode.StoreStatic, src, fr)
      case Chain(Static(fr), Seq(), Load(dst, _)) => loadStoreStatic(Opcode.LoadStatic, dst, fr)
      case _ => asm.instr {
        val chain = normalize(c)
        genChainHead(chain.head)
        for (op <- chain.ops) {
          genChainBody(op)
        }
        genChainTail(chain.tail)
      }
    }

    /**
      * Transform chain to the form that is possible to generate directly.
      */
    @tailrec
    private def normalize(chain: Chain): Chain = chain match {
      // accumulate head operation
      case Chain(Reg(base, _), Seq(f: Field, ops*), tail) => normalize(Chain(MemField(base, f.fr), ops, tail))

      // normalize body
      case Chain(head, ops, tail) =>
        val acc = ArrayBuffer.empty[BodyOperation]
        val newTail = normalizeSpaceOperations(ops :+ tail, acc)
        Chain(head, acc.toSeq, newTail)
    }

    /**
      * Traverse operations from mem-space and normalize it to the form that would be generated.
      * As part of routine, performs optimizations by selecting better encodings.
      */
    @tailrec
    private def normalizeSpaceOperations(ops: Seq[SpaceOperation], accumulator: ArrayBuffer[BodyOperation]): TailOperation = {
      ops match {
        case FieldSeq(fs, Seq(TailOperation.Load(reg, _))) => TailOperation.LoadFieldSeq(reg, fs)
        case FieldSeq(fs, Seq(TailOperation.Store(reg, _))) => TailOperation.StoreFieldSeq(reg, fs)
        case FieldSeq(fs, xs) => normalizeSpaceOperations(xs, accumulator.addOne(BodyOperation.FieldN(fs)))
        case Seq(x: BodyOperation, xs*) => normalizeSpaceOperations(xs, accumulator.addOne(x))
        case Seq(x: TailOperation) => x
        case _ => shouldNotReachHere("traverse should end with tail operation")
      }
    }

    private def markLoadStoreValue(r: Register, ref: Boolean, load: Boolean): Unit = {
      r match {
        case r: IR if ref => if (load) asm.analyzer.ref(r) else asm.analyzer.useRef(r)
        case r: IR        => if (load) asm.analyzer.prim(r) else asm.analyzer.usePrim(r)
        case _ =>
      }
    }

    private def loadStoreStatic(opc: Opcode, r: Register, field: FieldReference): Unit = asm.instr {
      stream
        .opc8(opc)
        .bits(_.w4(r).w4(r))
        .sym16(field)

      markLoadStoreValue(r, field.fieldType.isReference, opc == LoadStatic)
    }

    private def loadStoreTyped(opc: Opcode, ts: StackSlot.Typed, r: Register, field: FieldReference): Unit = asm.instr {
      stream
        .opc8(opc)
        .bits(_.w4(r).w4(r))
        .write16(ts.idx)
        .sym16(field)

      markLoadStoreValue(r, field.fieldType.isReference, opc == LoadTyped)
    }

    private def storeImmTyped(opc: Opcode, ts: StackSlot.Typed, src: Long, field: FieldReference): Unit = asm.instr {
      stream
        .opc8(opc)
        .write16(ts.idx)
        .sym16(field)
        .sleb(src)

      assert(!field.fieldType.isReference || src == 0)
    }

    private def store(opc: Opcode, base: IR, r: Register, field: FieldReference): Unit = asm.instr {
      stream
        .opc8(opc)
        .bits(_.w4(base).w4(r))
        .sym16(field)

      if (field.refType.isReference) asm.analyzer.useRef(base) else asm.analyzer.useRec(base)
      markLoadStoreValue(r, field.fieldType.isReference, load = false)
    }

    private def load(opc: Opcode, base: IR, r: Register, field: FieldReference): Unit = asm.instr {
      stream
        .opc8(opc)
        .bits(_.w4(asm.analyzer.useRef(base)).w4(r))
        .sym16(field)

      if (field.refType.isReference) asm.analyzer.useRef(base) else asm.analyzer.useRec(base)
      markLoadStoreValue(r, field.fieldType.isReference, load = true)
    }

    private def genChainHead(head: Head): Unit = head match {
      case h: Reg =>
        // TODO: isRef is computable in runtime
        if (h.isRef) asm.analyzer.useRef(h.base) else asm.analyzer.useRec(h.base)
        stream
          .opc8(Opcode.MemHeadReg)
          .bits(_.w4(h.base).write(0, 3).w1(h.isRef))
      case h: MemField =>
        if (h.fr.refType.isReference) asm.analyzer.useRef(h.base) else asm.analyzer.useRec(h.base)
        // TODO: isRef is computable in runtime
        stream
          .opc8(Opcode.MemHeadField)
          .bits(_.w4(h.base).write(0, 3).w1(h.fr.refType.isReference))
          .sym16(h.fr)
      case h: Static => stream
        .opc8(Opcode.MemHeadStatic)
        .sym16(h.fr)
      case h: Handle => stream
        .opc8(Opcode.MemHeadHandle)
        .bits(_.w4(asm.analyzer.useRef(h.base)).w4(asm.analyzer.useRec(h.derived)))
      case h: Typed => stream
        .opc8(Opcode.MemHeadTyped)
        .write16(h.ts.idx)
    }

    private def genChainBody(op: BodyOperation): Unit = op match {
      case op: Field => stream
        .mem8(MemOpcode.Field1)
        .sym16(op.fr)
      case op: FieldN =>
        val opc = op.refs.length match {
          case 4 => MemOpcode.Field4
          case 3 => MemOpcode.Field3
          case 2 => MemOpcode.Field2
          case 1 => MemOpcode.Field1
          case _ => shouldNotReachHere()
        }
        val s = stream
        s.mem8(opc)
        for (fr <- op.refs) {
          s.sym16(fr)
        }
      case Index(reg: IR, sigId, checked) => stream
        .mem8(MemOpcode.Index)
        .bits(_.w4(asm.analyzer.usePrim(reg)).write(0, 3).w1(checked))
        .sym16(sigId)
      case ConstIndex(idx, sigId) => stream
        .mem8(MemOpcode.ConstIndex)
        .sleb(idx)
        .sym16(sigId)
      case FieldGeneric(fr: FieldReference, ti: IR) =>
        asm.analyzer.usePrim(ti)
        stream
          .mem8(MemOpcode.FieldGeneric)
          .sym16(fr)
          .bits(_.w4(ti).w4(ti))
      case ConstIndexGeneric(idx: Long, refType: Signature, ti: IR) =>
        asm.analyzer.usePrim(ti)
        stream
          .mem8(MemOpcode.ConstIndexGeneric)
          .sleb(idx)
          .sym16(refType)
          .bits(_.w4(ti).w4(ti))
      case IndexGeneric(reg: IR, refType: Signature, ti: IR) =>
        stream
          .mem8(MemOpcode.IndexGeneric)
          .sym16(refType)
          .bits(_.w4(asm.analyzer.usePrim(reg)).w4(asm.analyzer.usePrim(ti)))
      case Offset(reg: IR) =>
        asm.analyzer.usePrim(reg)
        stream
          .mem8(MemOpcode.Offset)
          .bits(_.w4(reg).w4(reg))
    }

    private def genChainTail(op: TailOperation): Unit = op match {
      case TailOperation.CopyReg(reg, recType) => {
        stream
          .mem8(MemOpcode.CopyReg)
          .bits(_.w4(asm.analyzer.useRec(reg)).w4(0))
          .sym16(recType)
      }
      case TailOperation.CopyInterior(reg, refs) => {
        if (refs.head.refType.isReference) asm.analyzer.useRef(reg) else asm.analyzer.useRec(reg)
        val s = stream
        s.mem8(MemOpcode.CopyInterior)
          .bits(_.w4(reg).w4(refs.size))
        for (ref <- refs) {
          s.sym16(ref)
        }
      }
      case TailOperation.CopyInteriorArr(reg, idx, refs) => {
        val s = stream
        s.mem8(MemOpcode.CopyInteriorArr)
          .bits(_.w4(asm.analyzer.useRef(reg)).w4(asm.analyzer.usePrim(idx)))
          .bits(_.w4(refs.size).w4(0))
        for (ref <- refs) {
          s.sym16(ref)
        }
      }
      case op: TailOperation.CopyStatic =>
        val s = stream
        s.mem8(MemOpcode.CopyStatic)
          .write8(op.refs.size)
        for (ref <- op.refs) {
          s.sym16(ref)
        }
      case op: TailOperation.CopyTyped =>
        val s = stream
        s.mem8(MemOpcode.CopyTyped)
          .write8(op.refs.size)
          .write16(op.ts.idx)
        for (ref <- op.refs) {
          s.sym16(ref)
        }
      case op: TailOperation.CopyHandle =>
        val s = stream
        s.mem8(MemOpcode.CopyHandle)
          .bits(_.w4(asm.analyzer.useRef(op.base)).w4(asm.analyzer.usePrim(op.offs)))
      case Load(reg, isRef) => {
        markLoadStoreValue(reg, isRef, load = true)
        stream
          .mem8(MemOpcode.Load)
          .bits(_.w4(reg).w4(0))
      }
      case Store(reg, isRef) => {
        markLoadStoreValue(reg, isRef, load = false)
        stream
          .mem8(MemOpcode.Store)
          .bits(_.w4(reg).w4(0))
      }
      case StoreImm(imm, isRef) => {
        stream
          .mem8(MemOpcode.StoreImm)
          .write64(imm)
      }
      case LoadFieldSeq(reg, refs) => {
        markLoadStoreValue(reg, refs.last.fieldType.isReference, load = true)
        stream
          .mem8(MemOpcode.Load)
          .bits(_.w4(reg).w4(refs.size))
        for (ref <- refs) {
          stream.sym16(ref)
        }
      }
      case StoreFieldSeq(reg, refs) => {
        markLoadStoreValue(reg, refs.last.fieldType.isReference, load = false)
        stream
          .mem8(MemOpcode.Store)
          .bits(_.w4(reg).w4(refs.size))
        for (ref <- refs) {
          stream.sym16(ref)
        }
      }
      case LoadGeneric(dst: IR, ti: IR) =>
        stream
          .mem8(MemOpcode.LoadGeneric)
          .bits(_.w4(asm.analyzer.ref(dst)).w4(asm.analyzer.usePrim(ti)))
        asm.saveState()
      case StoreGeneric(src: IR, ti: IR) =>
        stream
          .mem8(MemOpcode.StoreGeneric)
          .bits(_.w4(asm.analyzer.useRef(src)).w4(asm.analyzer.usePrim(ti)))
    }
  }

  private object FieldSeq {
    import BodyOperation.Field

    def unapply(seq: Seq[SpaceOperation]): Option[(Seq[FieldReference], Seq[SpaceOperation])] = condOpt(seq) {
      case Seq(f1: Field, f2: Field, f3: Field, f4: Field, xs*) => (Seq(f1.fr, f2.fr, f3.fr, f4.fr), xs)
      case Seq(f1: Field, f2: Field, f3: Field, xs*) => (Seq(f1.fr, f2.fr, f3.fr), xs)
      case Seq(f1: Field, f2: Field, xs*) => (Seq(f1.fr, f2.fr), xs)
      case Seq(f1: Field, xs*) => (Seq(f1.fr), xs)
    }
  }

}
