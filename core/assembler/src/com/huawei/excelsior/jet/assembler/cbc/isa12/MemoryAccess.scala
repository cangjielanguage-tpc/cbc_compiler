/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Symbol
import Assembler.*
import Assembler.Width.*
import MemoryAccess.ArrayType.*
import MemoryAccess.MemoryType.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.MemoryAccess.{ArrayType, LoadAccessKind, MemSpace, MemoryType, PtrKind, StoreAccessKind}
import com.huawei.excelsior.jet.assembler.cbc.isa12.MemoryAccess.StoreAccessKind.*
import com.huawei.excelsior.jet.assembler.cbc.MemExpr.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.IRZ
import xscala.util.MathUtils.{isNBits, isNBitsSigned}
import com.huawei.excelsior.jet.assembler.cbc.*
import com.huawei.excelsior.jet.assembler.cbc.MemExpr.Head.RegPair
import com.huawei.excelsior.jet.assembler.cbc.StackSlot.OffHeapMemory

import scala.collection.mutable.ListBuffer

trait MemoryAccess extends CbcAssembler with NewIsaParts { self: isa12.Assembler =>
  /** bits 7-0 : 11-8 | format    | opc meaning (field cc..cc)
    * ------------------------------------------------------------
    * 1000cccc : cccc |   -----   | Memory access operations (4.5)
    */
  private def genMemExprHead(me: MemExpr): Unit = {
    me.head match {
      case rb: IR if me.body == CbcTypeKind.REC => genMemPtrHead(rb)
      case rb: IR => genMemObjHead(rb)
      case MemExpr.Head.RegPair(rb: IR, rn: IR) => genMemHandle(rb, rn)
      case ts: StackSlot.Typed => genMemTSlot(ts)
      case us: StackSlot.Untyped => genMemUSlot(us)
      case MemExpr.Head.RegImm(rb: IR, offset: Int) => genMemPtr(rb, offset)

      case MemExpr.Head.RecordArray(rb: IR, rn: IR, arrayOrElemSigId: Symbol) =>
        genMemObjHead(rb)
        genIndexArr(checked = false, rn, 0, arrayOrElemSigId)

      case MemExpr.Head.StaticField =>
        me.body match {
          case body: Array[Symbol] =>
            genMemField(mapToFieldRefs(body).head)
          case tk: CbcTypeKind => shouldNotReachHere(tk)
        }
    }
  }

  /** mem.ptr.head rt, rb {       | cip = rb; rb is unmanaged (non-heap) pointer; rt is Mem-scratch register
    *     0100 : 0101 | B3xrrz    | mem.ptr.head rt, rb `{`        | cip =  rb             | *c,d;
    */
  private[assembler] def genMemPtrHead(rb: IR): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_PTR_HEAD)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(0), IRZ))
    emit.seg.putW8(pack8(rb, IRZ))
  }

  /** mem.obj.head rt, rb {       | cip = rb; rb is managed reference to the heap; rt is Mem-scratch register
    *     0100 : 1101 | B3xrrz    | mem.obj.head rt, rb `{`        | cip =  rb             | *c; `rb` should be obj reference (points to managed memory)
    */
  private[assembler] def genMemObjHead(rb: IR): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_OBJ_HEAD)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(1), IRZ))
    emit.seg.putW8(pack8(rb, IRZ))
  }

  /** mem.handle rt, rb, rn {     | cip = handle(ref=rb, offs=rn); rt is Mem-scratch register
    *     0101 : 0101 | B3xrrr    | mem.handle rt, rb, rn `{`      | cip = handle(rb, rn)  | Memexpr head from mut-handle
    */
  private[assembler] def genMemHandle(rb: IR, rn: IR): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_HANDLE)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(0), IRZ))
    emit.seg.putW8(pack8(rb, rn))
  }

  /** mem.tslot rt, #ts {         | cip = &tslot[#ts]; set cip to address of (unresolved) typed slot; rt is Mem-scratch register
    *     0111 : 0001 | B2xr+I    | mem.tslot rt, #ts `{`          | cip = &tslot[#ts]     | Memexpr head for (unresolved) typed frame slot
    */
  private[assembler] def genMemTSlot(ts: StackSlot.Typed): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_TSLOT)
    emit.seg.putW8(pack8(MemoryAccess.opx(0x1), IRZ))
    emit.ts(ts)
  }

  /** mem.uslot rt, #us {         | cip = &uslot[#ts]; set cip to address of (unresolved) untyped slot; rt is Mem-scratch register
    *     1111 : 1101 | B2xr+I    | mem.uslot rt, #us `{`          | cip = &uslot[#us]     | Memexpr head for untyped frame slot
    */
  private[assembler] def genMemUSlot(us: StackSlot.Untyped): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_USLOT)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(1), IRZ))
    emit.us(us)
  }

  /** mem.ptr rt, rb, offs {      | cip = (rb + offs) as unmanaged pointer; rt is Mem-scratch register
    *     1111 : 0101 | B4xrri12  | mem.ptr rt, rb, offs `{`       | cip = rb + offs       | *d;
    */
  private[assembler] def genMemPtr(rb: IR, offset: Int): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_PTR)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(0), IRZ))
    assert(isNBits(offset, 12))
    emit.seg.putW16(pack16(rb, offset))
  }

  /** mem.field rb, @fieldID {    | cip = &rb.field; set cip as intra-pointer to static/instance/record field; `irz` is Mem-scratch register
    *                             | for static @fieldID: `rb` is ignored and should be `irz`
    *                             | for record's fields: `rb` is thread-local non-heap pointer
    *                             | for object's fields: `rb` is managed reference
    *     1000 : 0000 | B2xr+I    | mem.field rb, @fieldID `{`     | cip = &rb.field       | Memexpr head for static/instance/record field
    */
  private[assembler] def genMemField(field: Symbol): Unit = {
    emit.seg.putW8(MemoryAccess.MEM_FIELD)
    emit.seg.putW8(pack8(MemoryAccess.opx(0x0), IRZ))
    self.emit.id16(field)
  }

  /** 0000 00nn : ---- | M1+nI     | fieldseq @fid1,... @fidN                      | *a; all @fidK (except maybe last one) are flat fields */
  private def genFieldSeq(ids: Array[FieldReference]): Unit = {
    genFieldSeq(ids.map(_.asInstanceOf[Symbol]))
  }

  private[assembler] def genFieldSeq(ids: Array[Symbol]): Unit = {
    emit.seg.putW8(MemSpace.M1nI.format(ids.length - 1))
    ids foreach(x => emit.id16(x))
  }

  private inline def mapToFieldRefs(arr: Array[Symbol]): Array[FieldReference] = {
    arr.collect { case f: FieldReference => f } ensuring(_.length == arr.length)
  }

  private def genMemExprBody(me: MemExpr, emitFullBody: Boolean): Array[FieldReference] = {
    me.body match {
      case arr: Array[Symbol] =>
        val fieldRefs = mapToFieldRefs(arr)
        val nonGenericFields = ListBuffer.empty[FieldReference]
        var prevField: Option[FieldReference] = None
        for (s <- fieldRefs) {
          s match {
            case s: FieldReference if !s.isGeneric && !prevField.exists(_.isGeneric) =>
              nonGenericFields += s
              prevField = Some(s)
            case s: FieldReference =>
              if (nonGenericFields.nonEmpty) {
                genMemExprBody(me.head, nonGenericFields.toArray, emitFullBody)
                nonGenericFields.clear()
              }
              emit.seg.putW8(MemSpace.M1.ftcOrSigField(s.isGenericVLT))
              emit.id16(s.refType)
              emit.id16(s)
              prevField = Some(s)
          }
        }
        if (nonGenericFields.nonEmpty) {
          genMemExprBody(me.head, nonGenericFields.toArray, emitFullBody)
        } else {
          Array(fieldRefs.last)
        }
      case _ =>
        shouldNotReachHere(me)
    }
  }

  private def genMemExprBody(head: MemExpr.Head, body: Array[FieldReference], emitFullBody: Boolean): Array[FieldReference] = {
    val exactArr = if (head == MemExpr.Head.StaticField) body.tail else body
    val groupedArr = exactArr.grouped(4).toSeq
    if (emitFullBody) {
      groupedArr.foreach(genFieldSeq)
    } else {
      assert(groupedArr.nonEmpty)
      groupedArr.dropRight(1).foreach(genFieldSeq)
    }
    groupedArr.lastOption.getOrElse(Array.empty)
  }

  /** 0000 0100 : 0fnn | M2xr+nI   | ld.fieldseq rd, @fid1,... @fidN `}`           | *a; all @fidK (except last one) are flat fields
    * 0000 0100 : 1fnn | M2xr+nI   | st.fieldseq rv, @fid1,... @fidN `}`           | *a; all @fidK (except last one) are flat fields
    */
  private def genFieldChainLdSt(isStore: Boolean, rx: Register, meBodyTail: Array[FieldReference]): Unit = {
    genFieldChainLdSt(isStore, rx, meBodyTail.map(_.asInstanceOf[Symbol]))
  }

  private[assembler] def genFieldChainLdSt(isStore: Boolean, rx: Register, meBodyTail: Array[Symbol]): Unit = {
    emit.seg.putW8(MemSpace.M2xr.format(0x4))
    val st = if (isStore) 1 else 0
    val f = if (rx.isFReg) 1 else 0
    assert(0 < meBodyTail.length && meBodyTail.length <= 4)
    val nn = meBodyTail.length - 1
    emit.seg.putW8(pack8(MemSpace.M2xr.opxLdStFieldSeq(st, f, nn), rx))
    meBodyTail foreach(x => emit.id16(x))
  }

  /** 0000 0110 : 1010 | M2xr+I+I  | ld.vst ird, #ohm, #ftc `}`                    |
      0000 0110 : 1011 | M2xr+I    | st.vst irv, #ftc `}`                          |
    */
  private[assembler] def genLdStVSTField(isStore: Boolean, rx: Register, ohm: OffHeapMemory, ftcSig: Symbol): Unit = {
    emit.seg.putW8(MemSpace.M2xr.format(0x6))
    emit.seg.putW8(pack8(MemSpace.M2xr.opxLdStVSTField(store = isStore), rx))
    if (ohm != null) {
      assert(!isStore)
      emit.seg.putW16(ohm.idx)
    }
    emit.id16(ftcSig)
  }

  /** 0000 0110 : 100i | M2xr+i24  | index.arr{.checked} irx|irz, #inc, @sig_id    | *a
    */
  private[assembler] def genIndexArr(checked: Boolean, rx: Register, inc: Int, sigId: Symbol): Unit = {
    emit.seg.putW8(MemSpace.M2xr.format(0x6))
    val low4BytesOfSecondByte = MemSpace.M2xr.opxIndexArr(checked)
    emit.seg.putW8(pack8(low4BytesOfSecondByte, rx))
    emit.seg.putW8(s8(inc))
    emit.id16(sigId)
  }

  /** 0000 0111 : LLNN | M2xt+iL   | st.imm.NN iL `}`                              | *a; LL <= NN */
  private[assembler] def genStImm(width: Width, imm: Long): Unit = {
    emit.seg.putW8(MemSpace.ST_IMM)
    val nn = width.opc
    val immWidth = Seq(W8, W16, W32).find(w => isNBitsSigned(imm, w.nbits)).getOrElse(W64)
    val ll = immWidth.opc.ensuring(_ <= nn)
    emit.seg.putW8(MemSpace.M2xt.opxStImm(ll, nn, 0)) // TODO: utilize rotation
    immWidth match {
      case W8  => emit.seg.putW8(imm.toInt & 0xFF)
      case W16 => emit.seg.putW16(imm.toInt & 0xFFFF)
      case W32 => emit.seg.putW32(imm.toInt)
      case W64 => emit.seg.putW64(imm)
    }
  }

  /** 100f : rrrr | B2rr+I    | ld.field rd, rb, @fieldID      | rd = rb.field         | *f; load from static/instance/record field
    * 101f : rrrr | B2rr+I    | st.field rs, rb, @fieldID      | rb.field = rs         | *g; store to static/instance/record field
    */
  private[assembler] def genLdStField(isStore: Boolean, rs: Register, rb: IR, fs: Symbol): Unit = {
    val opc = (isStore, rs.isFReg) match {
      case (false, false) => MemoryAccess.LD_FIELD
      case (false, true) => MemoryAccess.LD_FIELD_FP
      case (true, false) => MemoryAccess.ST_FIELD
      case (true, true) => MemoryAccess.ST_FIELD_FP
    }
    emit.seg.putW8(opc)
    emit.seg.putW8(pack8(rs, rb))
    emit.id16(fs)
  }

  /** 1101 : llll | B4xrri12  | ld.ptr.X rd, rb, offs          | rd = *(rb + offs)     | *e; raw load from unmanaged memory
    * 1111 : 0sss | B4xrri12  | st.ptr.X rs, rb, offs          | *(rb + offs) = rs     | *e; raw store to unmanaged memory
    */
  private[assembler] def genLdStReg(isStore: Boolean, rx: Register, rb: IR, offset: Int, cbcTypeKind: CbcTypeKind): Unit = {
    // TODO: offs = unsigned(i12|i16) * S; S = MW/8 = access width in bytes
    assert(isNBits(offset, 12))
    if (isStore) {
      emit.seg.putW8(MemoryAccess.ST_PTR)
      emit.seg.putW8(pack8(StoreAccessKind.from(cbcTypeKind).opx(0), rx))
      emit.seg.putW16(pack16(rb, offset))
    } else {
      emit.seg.putW8(MemoryAccess.LD_PTR)
      emit.seg.putW8(pack8(LoadAccessKind.from(cbcTypeKind).ldk, rx))
      emit.seg.putW16(pack16(rb, offset))
    }
  }

  /** 0000 0101 : llll | M2xr      | ld.X ird/frd `}`                              | *a; `llll` is load access kind (see 4.5.1)
      0000 0110 : 0sss | M2xr      | st.X irv/frv `}`                              | *a; `sss` is store access kind (see 4.5.1)
    */
  private[assembler] def genLdStReg(isStore: Boolean, rx: Register, tk: CbcTypeKind): Unit = {
    if (isStore) {
      emit.seg.putW8(MemSpace.ST_X)
      emit.seg.putW8(pack8(StoreAccessKind.from(tk).opx(0), rx))
    } else {
      emit.seg.putW8(MemSpace.LD_X)
      emit.seg.putW8(pack8(LoadAccessKind.from(tk).ldk, rx))
    }
  }

  /** 1110 : llll | B2xr+I    | ld.uslot.X rd, #us             | rd = uslot[#us]       | load from untyped frame slot
    * 1111 : 1sss | B2xr+I    | st.uslot.X rs, #us             | uslot[#us] = rs       | store to untyped frame slot
    */
  private[assembler] def genLdStUSlot(isStore: Boolean, rx: Register, us: StackSlot.Untyped, cbcTypeKind: CbcTypeKind): Unit = {
    if (isStore) {
      emit.seg.putW8(MemoryAccess.ST_USLOT)
      emit.seg.putW8(pack8(StoreAccessKind.from(cbcTypeKind).opx(1), rx))
      emit.us(us)
    } else {
      emit.seg.putW8(MemoryAccess.LD_USLOT)
      emit.seg.putW8(pack8(LoadAccessKind.from(cbcTypeKind).ldk, rx))
      emit.us(us)
    }
  }

  /** 0110 : 0000 | B2xr+II   | ld.uslot.vst rd, #us, #ftc     | rd = uslot[#us]       | load from untyped frame slot that contains holder
    * 0110 : 0001 | B2xr+II   | st.uslot.vst rs, #us, #ftc     | rs = uslot[#us]       | store the holder to untyped frame slot
    */
  private[assembler] def genLdStUSlotVST(isStore: Boolean, rx: Register, us: StackSlot.Untyped, ftcSig: Symbol): Unit = {
    if (isStore) {
      emit.seg.putW8(MemoryAccess.ST_USLOT_VST)
      emit.seg.putW8(pack8(0x1, rx))
    } else {
      emit.seg.putW8(MemoryAccess.LD_USLOT_VST)
      emit.seg.putW8(pack8(0x0, rx))
    }
    emit.us(us)
    emit.id16(ftcSig)
  }

  /** 0110 : 0010 | B4xrri12+I | ld.ptr.vst rd, rb, offs, #ftc  | rd = *(rb + offs)     | *e; raw load holder data from unmanaged memory
    */
  private[assembler] def genLdPtrVST(rd: Register, rb: IR, offset: Int, ftcSig: Symbol): Unit = {
    emit.seg.putW8(MemoryAccess.LD_PTR_VST)
    emit.seg.putW8(pack8(0x2, rd))
    emit.seg.putW16(pack16(rb, offset))
    emit.id16(ftcSig)
  }

  protected def genGenericMemExprLdSt(isStore: Boolean, rx: Register, me: MemExpr, ohm: OffHeapMemory = null): Unit = {
    assert(me.isGeneric)
    assert(me.body.hasFieldChain)

    (me.head, me.body) match {
      case (us: StackSlot.Untyped, Array(ftcSig: Symbol)) =>
        genLdStUSlotVST(isStore, rx, us, ftcSig)
      case (MemExpr.Head.RegImm(rb, offset), Array(ftcSig: Symbol)) =>
        assert(!isStore)
        genLdPtrVST(rx, rb, offset, ftcSig)
      case _ =>
        genMemExprHead(me)
        val tail = genMemExprBody(me, emitFullBody = true)
        tail.last match {
          case s: FieldReference if s.isGenericVST =>
            genLdStVSTField(isStore, rx, ohm, s.fieldType)
          case s: FieldReference =>
            assert(!(isStore && s.isGenericVST), s"unexpected load.vst $me")
            genLdStReg(isStore, rx, s.tk)
        }
    }
  }

  protected def genMemExprLdSt(isStore: Boolean, rx: Register, me: MemExpr): Unit = {
    assert(!me.isGeneric)
    if (me.body.hasFieldChain) {
      if ((me.head == MemExpr.Head.StaticField || me.head.isInstanceOf[IR]) && me.body.length == 1) {
        val rb = me.head match {
          case r: IR => r
          case _ => IRZ
        }
        genLdStField(isStore, rx, rb, mapToFieldRefs(me.body.asInstanceOf[Array[Symbol]]).head)
      } else {
        genMemExprHead(me)
        val meBodyTail = genMemExprBody(me, emitFullBody = false)
        genFieldChainLdSt(isStore, rx, meBodyTail)
      }
    } else {
      val cbcTypeKind = me.body.asInstanceOf[CbcTypeKind]
      me.head match {
        case rb: IR =>
          genLdStReg(isStore, rx, rb, 0, cbcTypeKind)
        case MemExpr.Head.RegImm(rb, offset) =>
          genLdStReg(isStore, rx, rb, offset, cbcTypeKind)
        case us: StackSlot.Untyped =>
          genLdStUSlot(isStore, rx, us, cbcTypeKind)
        case x => shouldNotReachHere(s"unexpected head type without body: $x")
      }
    }
  }

  protected def genMemExprStImm(me: MemExpr, imm: Long, width: Width): Unit = {
    genMemExprHead(me)
    if (me.body.hasFieldChain) {
      genMemExprBody(me, emitFullBody = true)
    }
    genStImm(width, imm)
  }

  private def canBeCopyRecSecondArg(me: MemExpr): Boolean = {
    // TODO: support copy rec for mem expr with a long chain
    me.head match {
      case _: IR => me.body.length <= 4
      case Head.StaticField => me.body.hasFieldChain && me.body.length <= 4
      case _: StackSlot.Typed => me.body.length <= 3
      case _: RegPair => !me.body.hasFieldChain
      case _ => false
    }
  }

  protected def genCopyRec(dst: MemExpr, src: MemExpr, sigId: Symbol): Unit = {
    if (canBeCopyRecSecondArg(src)) {
      genCopyRecImpl(dst, src, firstIsDst = true, sigId)
    } else if (canBeCopyRecSecondArg(dst)) {
      genCopyRecImpl(src, dst, firstIsDst = false, sigId)
    } else {
      shouldNotReachHere(s"cannot copy mem exprs: $dst <- $src")
    }
  }

  private def genCopyRecImpl(first: MemExpr, second: MemExpr, firstIsDst: Boolean, sigId: Symbol): Unit = {
    genMemExprHead(first)
    if (first.body.hasFieldChain) {
      genMemExprBody(first, emitFullBody = true)
    }
    genMemExprCopyRec(second, firstIsDst, sigId)
  }

  private def genMemExprCopyRec(second: MemExpr, firstIsDst: Boolean, sigId: Symbol): Unit = {
    val d = if (firstIsDst) 0 /* .from */ else 1 /* .to */
    second.head match {
      case rb: IR if second.body == CbcTypeKind.REC =>
        assert(!second.body.hasFieldChain)
        genCopyRecHandle(IRZ, rb, firstIsDst, sigId)

      case rb: IR =>
        genCopyRecFieldSeq(rb, firstIsDst, second.body.asInstanceOf[Array[Symbol]])

      case MemExpr.Head.StaticField =>
        genCopyRecFieldSeq(IRZ, firstIsDst, second.body.asInstanceOf[Array[Symbol]])

      case ts: StackSlot.Typed =>
        val fieldChain = if (second.body.hasFieldChain) second.body.asInstanceOf[Array[Symbol]] else Array.empty[Symbol]
        genCopyRecTSlot(ts, firstIsDst, fieldChain)

      case MemExpr.Head.RegPair(rb: IR, rn: IR) =>
        assert(!second.body.hasFieldChain)
        genCopyRecHandle(rb, rn, firstIsDst, sigId)

      case x => shouldNotReachHere(s"unexpected memexpr.head for copy rec: $x")
    }
  }

  /** 0000 1011 : 0nnd | M2xr+nI   | copy.rec.D.fieldseq rb, @fid1,... @fidN `}`   | *a; */
  private[assembler] def genCopyRecFieldSeq(rb: IR, firstIsDst: Boolean, sigs: Array[Symbol]): Unit = {
    assert(1 <= sigs.length && sigs.length <= 4)
    val d = if (firstIsDst) 0 /* .from */ else 1 /* .to */
    emit.seg.putW8(MemSpace.COPY_REC_D_FIELD_SEQ)
    emit.seg.putW8(pack8(MemSpace.M2xr.opxCopyRec(0, sigs.length - 1, d), rb))
    sigs.foreach(x => emit.id16(x))
  }

  /** 0000 1011 : 1nnd | M2xr+nI   | copy.rec.D.tslot #ts (, @fseq} `}`            | *a; rb == irz */
  private[assembler] def genCopyRecTSlot(ts: StackSlot.Typed, firstIsDst: Boolean, sigs: Array[Symbol]): Unit = {
    assert(0 <= sigs.length && sigs.length <= 3)
    val d = if (firstIsDst) 0 /* .from */ else 1 /* .to */
    emit.seg.putW8(MemSpace.COPY_REC_D_TSLOT)
    emit.seg.putW8(pack8(MemSpace.M2xr.opxCopyRec(1, sigs.length /* including ts */ , d), IRZ))
    emit.ts(ts)
    sigs.foreach(x => emit.id16(x))
  }

  /** 0000 110d : rrrr | M4rrI     | copy.rec.D.handle rx, ry, @sig_id `}`         | *a,c;
    * c. `copy.rec.D.handle` aliases:
    *    - copy.rec.D.ptr rb, @sig_id => copy.rec.D.handle irz, rb, @sig_id
    * `.D` = if (d=0) then `.from` else `.to`
    */
  private[assembler] def genCopyRecHandle(rb: IR, rn: IR, firstIsDst: Boolean, sigId: Symbol): Unit = {
    emit.seg.putW8(if (firstIsDst) MemSpace.COPY_REC_D_HANDLE_FROM else MemSpace.COPY_REC_D_HANDLE_TO)
    emit.seg.putW8(pack8(rb, rn))
    emit.id16(sigId)
  }

  /** 00ia : llll | B3xrrr    | ld.arr.AC.X rd, rb, rn         | rd = rb[rn]           | *a,b; load of (non-record) array element */
  protected[assembler] def genLdArr(rd: Register, rb: IR, rn: IR, indexCheck: Boolean, arrayType: ArrayType, loadAccessKind: LoadAccessKind): Unit = {
    val opc = (indexCheck, arrayType) match {
      case (false, Raw) => MemoryAccess.LD_ARR_RAW
      case (false, Java) => MemoryAccess.LD_ARR_JAVA
      case (true, Raw) => MemoryAccess.LD_ARR_AIC_RAW
      case (true, Java) => MemoryAccess.LD_ARR_AIC_JAVA
    }
    emit.seg.putW8(opc)
    emit.seg.putW8(pack8(loadAccessKind.ldk, rd))
    emit.seg.putW8(pack8(rb, rn))
  }

  /** 000a : 0011 | B3xrrz    | len.arr.A rd, rb               | rd = rb.length        | *a; get array length */
  protected[assembler] def genLenArr(rd: Register, rb: IR, arrayType: ArrayType): Unit = {
    val opc = arrayType match {
      case Raw  => MemoryAccess.LEN_ARR_RAW
      case Java => MemoryAccess.LEN_ARR_JAVA
    }
    emit.seg.putW8(opc)
    emit.seg.putW8(pack8(LoadAccessKind.SPECIAL.ordinal, rd))
    emit.seg.putW8(pack8(rb, IRZ))
  }

  /** 001a : 0011 | B3xzrr    | check.idx.A rb, rn             | checkindex(rb, rn)    | *a; */
  protected[assembler] def genAIC(rb: Register, rn: IR, arrayType: ArrayType): Unit = {
    val opc = arrayType match {
      case Raw  => MemoryAccess.CHK_IDX_ARR_RAW
      case Java => MemoryAccess.CHK_IDX_ARR_JAVA
    }
    emit.seg.putW8(opc)
    emit.seg.putW8(pack8(LoadAccessKind.SPECIAL.ordinal, IRZ))
    emit.seg.putW8(pack8(rb, rn))
  }

  protected def genLdArrRecord(rd: IR, rb: IR, rn: IR, sig_id: Symbol): Unit = {
    val me = MemExpr(MemExpr.Head.RecordArray(rb, rn, sig_id), CbcTypeKind.REC)
    genMemExprHead(me)
    genAdrPtr(rd)
  }

  /** 010a : isss | B3xrrr    | st.arr.AC.X rs, rb, rn         | rb[rn] = rs|irz       | *a,b; store to (non-record) array element */
  protected[assembler] def genStArr(rs: Register, rb: IR, rn: IR, indexCheck: Boolean, arrayType: ArrayType, storeAccessKind: StoreAccessKind): Unit = {
    val opc = arrayType match {
      case Raw => MemoryAccess.ST_ARR_RAW
      case Java => MemoryAccess.ST_ARR_JAVA
    }
    val i1 = if (indexCheck) 1 else 0
    emit.seg.putW8(opc)
    emit.seg.putW8(pack8(storeAccessKind.opx(i1), rs))
    emit.seg.putW8(pack8(rb, rn))
  }

  /** 0101 : 1101 | B3xrrz    | check.astore.java rs, rb       | check T(rs)<:T(rb[.]) | Array store check for java covariant array of reference */
  protected[assembler] def genArrStJava(rs: Register, rb: IR, arrayType: ArrayType): Unit = {
    emit.seg.putW8(MemoryAccess.CHK_ARR_ST_JAVA)
    emit.seg.putW8(pack8(StoreAccessKind.SPECIAL.opx(1), rs))
    emit.seg.putW8(pack8(rb, IRZ))
  }

  type AdrMem = StackSlot.Typed | StackSlot.Untyped | Symbol

  protected[assembler] def genLdStackRec(dst: IR, mem: StackSlot.Typed): Unit = {
    val me = MemExpr(mem, CbcTypeKind.REC) // TODO: choose another opcode
    genMemExprHead(me)
    genAdrPtr(dst)
  }

  /** 1110 : 0011 | B2xr+I    | adr.uslot rd, #us              | rd = &uslot[#us]      | address of untyped frame slot
    * 1100 : rrrr | B2rr+I    | adr.field rd, rb, @fieldID     | rd = &rb.field        | address of static/record field
    * 0111 : 0000 | B2xr+I    | adr.tslot rd, #ts              | rd = &tslot[#ts]      | get address of (unresolved) typed frame slot
    * 0111 : 0000 | B2xr+I    | prepare.record irz #ts         | ir1 = &tslot[#ts]     | get address of (unresolved) record typed frame slot, prepare record for consequent call
    */
  protected[assembler] def genAdr(dst: IR, mem: AdrMem): Unit = {
    mem match {
      case us: StackSlot.Untyped =>
        emit.seg.putW8(MemoryAccess.ADR_USLOT)
        emit.seg.putW8(pack8(0x3, dst))
        emit.us(us)
      case s: Symbol =>
        emit.seg.putW8(MemoryAccess.ADR_FIELD)
        emit.seg.putW8(pack8(dst, IRZ))
        emit.id16(s)
      case ts: StackSlot.Typed =>
        emit.seg.putW8(MemoryAccess.ADR_TSLOT)
        emit.seg.putW8(pack8(0x0, dst))
        emit.ts(ts)
    }
  }

  protected def genAdr(dst: IR, me: MemExpr): Unit = {
    genMemExprHead(me)
    if (me.body.hasFieldChain) {
      genMemExprBody(me, emitFullBody = true)
    }
    genAdrPtr(dst)
  }

  /** 0000 0101 : 0011 | M2xr      | adr.ptr ird `}`                               | get address of unmanaged field/array element */
  private[assembler] def genAdrPtr(dst: IR): Unit = {
    emit.seg.putW8(MemSpace.ADR_PTR)
    emit.seg.putW8(pack8(LoadAccessKind.SPECIAL.ldk, dst))
  }

  /** 0000 10gg : rrrr | M2rr      | makehandle.?G irx, iry `}`                    | `gg` != 11 */
  private[assembler] def genMakeHandle(dstHost: IR, dstOffset: IR, makeHandleKind: Int): Unit = {
    emit.seg.putW8(makeHandleKind)
    emit.seg.putW8(pack8(dstHost, dstOffset))
  }

  /** 1001m : rrrr | B2rr      | mem.?.head rv, rb `{`          | cip =  rb           | Memexpr head from reg; (m=1) <=> managed memory */
  protected[assembler] def genMakeHandle(dstHost: IR, dstOffset: IR, r: IR): Unit = {
    genMemPtrHead(r)
    genMakeHandle(dstHost, dstOffset, MemSpace.MAKE_HANDLE_LOCAL)
  }

  protected def genMakeHandle(dstHost: IR, dstOffset: IR, me: MemExpr): Unit = {
    genMemExprHead(me)
    if (me.body.hasFieldChain) {
      genMemExprBody(me, emitFullBody = true)
    }

    val makeHandleKind = me.head match {
      case MemExpr.Head.StaticField => MemSpace.MAKE_HANDLE_GLOBAL
      case _: StackSlot.Typed => MemSpace.MAKE_HANDLE_LOCAL
      case _ => MemSpace.MAKE_HANDLE_OBJ
    }
    genMakeHandle(dstHost, dstOffset, makeHandleKind)
  }
}

object MemoryAccess {
  inline def FormatBits: Int = 0x8
  inline def FormatFreeBits: Int = 4
  inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

  inline def format(mop5: Int): Int = e5(ByteMask) | s5(mop5)
  inline def opx(opx4: Int): Int = s4(opx4)

  inline def opcLdArrRaw(): Int      = e4(ByteMask) | 0x0
  inline def opcLdArrJava(): Int     = e4(ByteMask) | 0x1
  inline def opcLdArrRawAIC(): Int   = e4(ByteMask) | 0x2
  inline def opcLdArrJavaAIC(): Int  = e4(ByteMask) | 0x3
  inline def opcStArrRaw(): Int      = e4(ByteMask) | 0x4
  inline def opcStArrJava(): Int     = e4(ByteMask) | 0x5
  inline def opcLdStUSlotVST(): Int  = e4(ByteMask) | 0x6
  inline def opcAdrTSlot(): Int      = e4(ByteMask) | 0x7
  inline def opcLdField(): Int       = e4(ByteMask) | 0x8
  inline def opcLdFieldFP(): Int     = e4(ByteMask) | 0x9
  inline def opcStField(): Int       = e4(ByteMask) | 0xA
  inline def opcStFieldFP(): Int     = e4(ByteMask) | 0xB
  inline def opcAdrField(): Int      = e4(ByteMask) | 0xC
  inline def opcLdPtr(): Int         = e4(ByteMask) | 0xD
  inline def opcLdUSlot(): Int       = e4(ByteMask) | 0xE
  inline def opcStPtr(): Int         = e4(ByteMask) | 0xF

  /** Memexpr head from reg */
  object B2rr {
    inline def FormatBits: Int = 0x9
    inline def FormatFreeBits: Int = 1
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(mt: MemoryType): Int = MemoryAccess.format(e1(ByteMask) | s1(mt.opc))
  }

  /** Array element load/store */
  object B3xrrr {
    inline def opcLdArr(i1: Int, a: ArrayType): Int = MemoryAccess.format(p(0x3, freeBits = 2) | p(s1(i1), freeBits = 1) | s1(a.opc))
    inline def opcStArr(a: ArrayType): Int = MemoryAccess.format(p(0x8, freeBits = 1) | s1(a.opc))
  }

  /** Symbolic/unresolved load/store */
  object B4rri16 {
    inline def FormatBits: Int = 0x3
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(mop2: Int, f: Int): Int = MemoryAccess.format(e3(ByteMask) | p(mop2, freeBits = 1) | s1(f))
  }

  /** No base-reg load/store [local/global] */
  object B4xri16 {
    inline def format(mop5: Int): Int = MemoryAccess.format(mop5)
  }

  /** Raw/resolved load/store */
  object B4xrri12 {
    inline def FormatBits: Int = 0x0
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    inline def format(mop2: Int, mt: MemoryType): Int = MemoryAccess.format(e3(ByteMask) | p(mop2, freeBits = 1) | s1(mt.opc))
  }

  /** Raw/resolved load/store */
  object B5xrrki16 {
    inline def format(mop5: Int): Int = MemoryAccess.format(mop5)
  }

  enum MemoryType {
    case Unmanaged
    case Managed

    def opc: Int = ordinal
  }

  enum ArrayType {
    case Raw
    case Java

    def opc: Int = ordinal
  }

  enum LoadAccessKind {
                    // ldk  | MW* | extension | dst,w  | remarks
                    //-------------------------------------------
    case LD_U8      // 0000 |  8  | zeroext32 | ir*,32 |
    case LD_U16     // 0001 | 16  | zeroext32 | ir*,32 |
    case LD_32      // 0010 | 32  |    no     | ir*,32 |
    case SPECIAL    // 0011 | --  |   ---     | ir*,64 | get address/offset of the object's/record's field/array element
    case LD_S8      // 0100 |  8  | signext32 | ir*,32 |
    case LD_S16     // 0101 | 16  | signext32 | ir*,32 |
    case LD_F32     // 0110 | 32  |    no     | fr*,32 |
    case LD_F64     // 0111 | 64  |    no     | fr*,64 |
    case LD_U8TO64  // 1000 |  8  | zeroext64 | ir*,64 |
    case LD_U16TO64 // 1001 | 16  | zeroext64 | ir*,64 |
    case LD_U32     // 1010 | 32  | zeroext64 | ir*,64 |
    case LD_64      // 1011 | 64  |    no     | ir*,64 | used to load pointer values
    case LD_S8TO64  // 1100 |  8  | signext64 | ir*,64 |
    case LD_S16TO64 // 1101 | 16  | signext64 | ir*,64 |
    case LD_S32     // 1110 | 32  | signext64 | ir*,64 |
    case LD_REF     // 1111 | 64  |    no     | ir*,64 | load traced objref

    def ldk: Int = ordinal
  }

  object LoadAccessKind {
    def apply(accessWidth: Width, signExt: Boolean, fp: Boolean, resultWidth: Width): LoadAccessKind = {
      (accessWidth, signExt, fp, resultWidth) match {
        case (W8,  false, false, W32) => LD_U8
        case (W16, false, false, W32) => LD_U16
        case (W32, _,     false, W32) => LD_32
                                      // SPECIAL
        case (W8,  true,  false, W32) => LD_S8
        case (W16, true,  false, W32) => LD_S16
        case (W32, _,     true,  W32) => LD_F32
        case (W64, _,     true,  W64) => LD_F64
        case (W8,  false, false, W64) => LD_U8TO64
        case (W16, false, false, W64) => LD_U16TO64
        case (W32, false, false, W64) => LD_U32
        case (W64, _,     false, W64) => LD_64
        case (W8,  true,  false, W64) => LD_S8TO64
        case (W16, true,  false, W64) => LD_S16TO64
        case (W32, true,  false, W64) => LD_S32
                                      // LD_REF
        case x => shouldNotReachHere(s"Denied load access kind: $x")
      }
    }

    // TODO: replace CbcTypeKind usages with LoadAccessKind
    def from(cbcTypeKind: CbcTypeKind): LoadAccessKind = {
      import CbcTypeKind.*

      cbcTypeKind match {
        case I8               => LoadAccessKind.LD_S8
        case U8               => LoadAccessKind.LD_U8
        case F16 | I16        => LoadAccessKind.LD_S16
        case U16              => LoadAccessKind.LD_U16
        case CHAR | I32 | U32 => LoadAccessKind.LD_32
        case I64 | U64 | REC  => LoadAccessKind.LD_64
        case REF | NREF       => LoadAccessKind.LD_REF
        case F32              => LoadAccessKind.LD_F32
        case F64              => LoadAccessKind.LD_F64
        case x => shouldNotReachHere(s"unexpected cbc type kind for LoadAccessKind: $x")
      }
    }
  }

  enum StoreAccessKind {
                 // stk | MW* |  src    | remarks
                 //-------------------------------
    case ST_8    // 000 |  8  | ir*/imm |
    case ST_16   // 001 | 16  | ir*/imm |
    case ST_32   // 010 | 32  | ir*/imm |
    case ST_64   // 011 | 64  | ir*/imm | used to store pointer values
    case ST_REF  // 100 | 64  |   ir*   | store traced objref; (src == irz) means null reference
    case SPECIAL // 101 | --  |   ir*   | set memexpr head and switch to Mem opcode space
    case ST_F32  // 110 | 32  |   fr*   |
    case ST_F64  // 111 | 64  |   fr*   |


    def stk: Int = ordinal

    def opx(highBit: Int): Int = {
      p(highBit, freeBits = 3) | s3(stk)
    }
  }

  object StoreAccessKind {
    def apply(accessWidth: Width, fp: Boolean): StoreAccessKind = {
      (accessWidth, fp) match {
        case (W8,  false) => ST_8
        case (W16, false) => ST_16
        case (W32, false) => ST_32
        case (W64, false) => ST_64
                          // ST_REF
                          // SPECIAL
        case (W32, true)  => ST_F32
        case (W64, true)  => ST_F64

        case x => shouldNotReachHere(s"Denied store access kind: $x")
      }
    }

    // TODO: replace CbcTypeKind usages with StoreAccessKind
    def from(cbcTypeKind: CbcTypeKind): StoreAccessKind = {
      import CbcTypeKind.*

      cbcTypeKind match {
        case I8 | U8          => StoreAccessKind.ST_8
        case F16 | I16 | U16  => StoreAccessKind.ST_16
        case CHAR | I32 | U32 => StoreAccessKind.ST_32
        case I64 | U64 | REC  => StoreAccessKind.ST_64
        case REF | NREF       => StoreAccessKind.ST_REF
        case F32              => StoreAccessKind.ST_F32
        case F64              => StoreAccessKind.ST_F64
        case x => shouldNotReachHere(s"unexpected cbc type kind for StoreAccessKind: $x")
      }
    }
  }

  enum PtrKind {
    case Local  // 00
    case Obj    // 01
    case Global // 10

    def gg: Int = ordinal
  }

  // MemoryAccess opcode list                                 3-0 : 11-8 | format
  val LD_ARR_RAW         = opcLdArrRaw()                  // 0000 : llll | B3xrrr
  val LD_ARR_JAVA        = opcLdArrJava()                 // 0001 : llll | B3xrrr
  val LD_ARR_AIC_RAW     = opcLdArrRawAIC()               // 0010 : llll | B3xrrr
  val LD_ARR_AIC_JAVA    = opcLdArrJavaAIC()              // 0011 : llll | B3xrrr
  val LEN_ARR_RAW        = opcLdArrRaw()      // alias    // 0000 : 0011 | B3xrrz
  val LEN_ARR_JAVA       = opcLdArrJava()     // alias    // 0001 : 0011 | B3xrrz
  val CHK_IDX_ARR_RAW    = opcLdArrRawAIC()   // alias    // 0010 : 0011 | B3xzrr
  val CHK_IDX_ARR_JAVA   = opcLdArrJavaAIC()  // alias    // 0011 : 0011 | B3xzrr
  val ST_ARR_RAW         = opcStArrRaw()                  // 0100 : isss | B3xrrr
  val ST_ARR_JAVA        = opcStArrJava()                 // 0101 : isss | B3xrrr
  val MEM_PTR_HEAD       = opcStArrRaw()      // alias    // 0100 : 0101 | B3xrrz
  val MEM_OBJ_HEAD       = opcStArrRaw()      // alias    // 0100 : 1101 | B3xrrz
  val MEM_HANDLE         = opcStArrJava()     // alias    // 0101 : 0101 | B3xrrr
  val CHK_ARR_ST_JAVA    = opcStArrJava()     // alias    // 0101 : 1101 | B3xrrz
  val LD_USLOT_VST       = opcLdStUSlotVST()              // 0110 : 0000 | B2xr+II
  val ST_USLOT_VST       = opcLdStUSlotVST()  // alias    // 0110 : 0001 | B2xr+II
  val LD_PTR_VST         = opcLdStUSlotVST()  // alias    // 0110 : 0010 | B4xrri12+I
  val ADR_TSLOT          = opcAdrTSlot()                  // 0111 : 0000 | B2xr+I
  val MEM_TSLOT          = opcAdrTSlot()      // alias    // 0111 : 0001 | B2xr+I
  val OFFS_FIELD         = opcAdrTSlot()      // alias    // 0111 : 0010 | B2xr+I
  val LD_FIELD           = opcLdField()                   // 1000 : rrrr | B2rr+I
  val MEM_FIELD          = opcLdField()       // alias    // 1000 : 0000 | B2xr+I
  val LD_FIELD_FP        = opcLdFieldFP()                 // 1001 : rrrr | B2rr+I
  val ST_FIELD           = opcStField()                   // 1010 : rrrr | B2rr+I
  val ST_FIELD_FP        = opcStFieldFP()                 // 1011 : rrrr | B2rr+I
  val ADR_FIELD          = opcAdrField()                  // 1100 : rrrr | B2rr+I
  val LD_PTR             = opcLdPtr()                     // 1101 : llll | B4xrri12
  val LD_USLOT           = opcLdUSlot()                   // 1110 : llll | B2xr+I
  val ADR_USLOT          = opcLdUSlot()       // alias    // 1110 : 0011 | B2xr+I
  val ST_PTR             = opcStPtr()                     // 1111 : 0sss | B4xrri12
  val MEM_PTR            = opcStPtr()         // alias    // 1111 : 0101 | B4xrri12
  val ST_USLOT           = opcStPtr()         // alias    // 1111 : 1sss | B2xr+I
  val MEM_USLOT          = opcStPtr()         // alias    // 1111 : 1101 | B2xr+I

  object MemSpace {
    inline def opcFieldSeq(nn: Int): Int          = M1nI.format(nn)
    inline def opcLdFieldSeq(): Int               = e4(p(0x0, 4)) | 0x4
    inline def opcLd(): Int                       = e4(p(0x0, 4)) | 0x5
    inline def opcSt(): Int                       = e4(p(0x0, 4)) | 0x6
    inline def opcStImm(): Int                    = e4(p(0x0, 4)) | 0x7
    inline def opcMakeHandleG(gg: Int): Int       = e4(p(0x0, 4)) | e2(0x8) | s2(gg)
    inline def opcCopyRecDHandle(d: Int): Int     = e4(p(0x0, 4)) | e1(0xC) | s1(d)
    inline def opcREX_I(i: Int): Int              = e4(p(0x0, 4)) | e1(0xE) | s1(i)

    inline def opcFtcField(): Int                 = e4(p(0x1, 4)) | 0x0
    inline def opcSigField(): Int                 = e4(p(0x1, 4)) | 0x1
    inline def opcCopyVstDHandle(d: Int): Int     = e4(p(0x1, 4)) | e1(0x2) | s1(d)
    inline def opcLdVst(d: Int): Int     = e4(p(0x1, 4)) | 0x4

    inline def unresolved(mop4: Int): Int = p(s4(0x0), freeBits = 4) | s4(mop4)
    inline def ug(mop4: Int): Int = p(s4(0x1), freeBits = 4) | s4(mop4)

    object M1 {
      inline def ftcOrSigField(isVariableType: Boolean): Int = if isVariableType then MemSpace.ug(0x0) else MemSpace.ug(0x1)
    }

    object M1nI {
      inline def format(nn: Int): Int = MemSpace.unresolved(p(s2(0x0), freeBits = 2) | s2(nn))
    }

    object M2xr {
      val opcCopyRec = format(0xb)

      inline def format(mop4: Int): Int = MemSpace.unresolved(mop4)
      inline def opxLdStFieldSeq(st: Int, f: Int, nn: Int) = p(s1(st), freeBits = 3) | p(s1(f), freeBits = 2) | s2(nn)
      inline def opxCopyRec(op: Int, nn: Int, d: Int) = p(s1(op), freeBits = 3) | p(s2(nn), freeBits = 1) | s1(d)
      inline def opxIndexArr(checked: Boolean) = p(0x4, freeBits = 1) | s1(if checked then 0x1 else 0x0)
      inline def opxLdStVSTField(store: Boolean): Int = p(s1(0x1), freeBits = 3) | p(s1(0x0), freeBits = 2) | p(s1(0x1), freeBits = 1) | s1(if store then 0x1 else 0x0)
    }

    object M2rr {
      inline def opcMakeHandle(ptrKind: PtrKind): Int = MemSpace.unresolved(p(s2(0x2), freeBits = 2) | s2(ptrKind.gg))
    }

    object M2xt {
      val opcStImm = MemSpace.unresolved(0x7)
      inline def opxStImm(ll: Int, nn: Int, rot4: Int) = p(s4(rot4), freeBits = 4) | p(s2(ll), freeBits = 2) | s2(nn)
    }

    object M4rrI {
      inline def format(mop3: Int, d: Int): Int = MemSpace.unresolved(p(s3(mop3), freeBits = 1) | s1(d))
    }

    // MemorySpace opcode list
    val FIELD_SEQ_1             = MemSpace.opcFieldSeq(0)
    val FIELD_SEQ_2             = MemSpace.opcFieldSeq(1)
    val FIELD_SEQ_3             = MemSpace.opcFieldSeq(2)
    val FIELD_SEQ_4             = MemSpace.opcFieldSeq(3)
    val LD_FIELD_SEQ            = MemSpace.opcLdFieldSeq()
    val ST_FIELD_SEQ            = MemSpace.opcLdFieldSeq()        // alias
    val LD_X                    = MemSpace.opcLd()
    val ADR_PTR                 = MemSpace.opcLd()                // alias
    val ST_X                    = MemSpace.opcSt()
    val OFFS_OBJ                = MemSpace.opcSt()                // alias
    val IDX_ARR                 = MemSpace.opcSt()                // alias
    val LD_VST                  = MemSpace.opcSt()                // alias
    val ST_VST                  = MemSpace.opcSt()                // alias
    val ST_IMM                  = MemSpace.opcStImm()
    val MAKE_HANDLE_LOCAL       = MemSpace.opcMakeHandleG(0)
    val MAKE_HANDLE_OBJ         = MemSpace.opcMakeHandleG(1)
    val MAKE_HANDLE_GLOBAL      = MemSpace.opcMakeHandleG(2)
    val COPY_REC_D_FIELD_SEQ    = MemSpace.opcMakeHandleG(3)
    val COPY_REC_D_TSLOT        = MemSpace.opcMakeHandleG(3)      // alias
    val COPY_REC_D_HANDLE_FROM  = MemSpace.opcCopyRecDHandle(0)
    val COPY_REC_D_HANDLE_TO    = MemSpace.opcCopyRecDHandle(1)
    val REX_RX                  = MemSpace.opcREX_I(0)
    val REX_RY                  = MemSpace.opcREX_I(1)
    val FTC_FIELD               = MemSpace.opcFtcField()
    val SIG_FIELD               = MemSpace.opcSigField()
    val COPY_VST_D_HANDLE_FROM  = MemSpace.opcCopyVstDHandle(0)
    val COPY_VST_D_HANDLE_TO    = MemSpace.opcCopyVstDHandle(1)
    val COPY_VST_FROM           = MemSpace.opcCopyVstDHandle(0)
    val COPY_VST_TO             = MemSpace.opcCopyVstDHandle(1)
  }
}
