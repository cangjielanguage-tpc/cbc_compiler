/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.Bits.*
import com.huawei.excelsior.jet.assembler.cbc.FormatExtension.*
import com.huawei.excelsior.jet.assembler.cbc.MemExpr.*
import com.huawei.excelsior.jet.assembler.{AsmEmitter, Literal, Symbol, Width}
import xscala.util.MathUtils.{isNBits, signExtend}

import scala.collection.mutable.ArrayBuffer


/** FormatExtension (FExt) is an optional extension for bytecode instructions,
  * that extends or modifies their arguments or results by decoding additional information from "FExt" VM field.
  *
  * Extension is a part of instruction format, so extension loading always precedes modified instruction
  * for easier decoding loop, and can't be used apart from operation.
  *
  * There are two special instruction forms, that load extension:
  *  - Short one-byte form ([[fextYLd4Opcode]]) only use-case is affecting calculation of destination register
  *    for result of binary operation.
  *  - Long multi-byte form ([[fextXLdOpcode]] and [[fextYLdOpcode]]) can be used to pass full FExt payload,
  *    which content is defined by succeeding operation.
  */
object FormatExtension {
  private val fextYLd4Opcode = 0xB7
  private val fextYLdOpcode = 0xC6
  private val fextXLdOpcode = 0xCE

  /** Must be filled according to specification in isa.yaml. */
  private lazy val immFields = Seq(
    (4, 0xF),  // to command format
    (4, 0xF),  // to ix4
    (8, 0xFF), // b1
    (8, 0xFF), // b2
    (8, 0xFF), // b3
    (8, 0xFF), // b4
    (8, 0xFF), // b5
    (8, 0xFF), // b6
    (8, 0xFF)  // b7
  )

  /** `d` and `v` are destination and left argument registers of 3-address FExt operation. */
  private def encodeDst(d: Register, v: Register): Int = (d.idx ^ v.idx) ensuring (isNBits(_, 4))

  def encodeImm(imm: Long): (Long, Int) = {
    assert (immFields.map(_._1).sum == 64)

    var immLeft = imm
    var offset = 0L
    var result = 0L
    for ((bits, mask) <- immFields) {
      val payload = immLeft & mask
      result ^= (payload << offset)
      immLeft -= signExtend(payload, bits)
      immLeft >>= bits
      offset += bits
    }

    (result & ~0xFL, (result & 0xF).toInt)
  }

  def decodeImm(fextState: Long, imm4: Int): Long = {
    val encoded = (fextState & ~0xFL) | (imm4 & 0xF)

    var offset = 0L
    var result = 0L
    for ((bits, mask) <- immFields) {
      val payload = (encoded >> offset) & mask
      result += (signExtend(payload, bits) << offset)
      offset += bits
    }

    result
  }
}

trait FormatExtension { self: Assembler with AsmEmitter.WithLiterals =>

  def fextMovArgs(x: Any, y: Any): Unit = {
    Seq(
      (x, fextXLdOpcode, 0 /* reserved */),
      (y, fextYLdOpcode, 0 /* 2-address instruction */)
    ) foreach { (arg, fextldOpcode, low4) =>
      arg match {
        case me: MemExpr =>
          assert(!me.body.hasFieldChain || me.body.length > 0)
          assert(me.outlined)
          emitOutlinedMemExpr(me, fextldOpcode, low4)
        case _ =>
      }
    }
  }

  /** Calculates fext.dst and fext.imm part and returns `imm4` to be encoded in cbc command.
    * @param d   - destination register of associated with this FExt prefix binary operation, used to calculate fext.dst.
    * @param l   - left register of associated with this FExt prefix binary operation, used to calculate fext.dst.
    * @param imm - right operand of associated with this FExt prefix binary operation, any long to be encoded.
    * Returns 4 lowest bits of encoded `imm`.
    */
  def fextImmDst(d: Register, l: Register, imm: Long): Long = {
    if (isImm4(imm)) {
      fextDst(d, l)
      imm & 0xF
    } else {
      val (fextImm, imm4) = encodeImm(imm)
      emitFextYLd(fextImm, encodeDst(d, l))
      imm4
    }
  }

  /** Calculates fext.imm, emit fextLd instruction, and returns 4 lowest bits of encoded `imm`. */
  def fextImm(imm: Long): Int = {
    val (fextImm, imm4) = encodeImm(imm)
    emitFextYLd(fextImm, dst = 0)
    imm4
  }

  /** Calculates fext.dst and emit fextDst instruction if necessary. */
  def fextDst(d: Register, v: Register): Unit = {
    val dst = encodeDst(d, v)
    if (dst != 0) {
      emitFextYLd4(dst)
    }
  }

  def fextOp(w: Width, op32: Int, op64: Int, d: Register, l: Register, r: Register | Long): Unit = r match {
    case r: Register =>
      fextDst(d, l)
      emit.op(w, op32, op64)
      emit.r4_r4(l, r)

    case imm: Long =>
      val imm4 = fextImmDst(d, l, imm)
      emit.op(w, op32, op64)
      emit.r4_u4(l, imm4)
  }

  private def emitOutlinedMemExpr(me: MemExpr, fextldOpcode: Int, low4: Int): Unit = {
    val memExprBodyLiteral = new Literal(alignment = 2) {
      override def emit(): Unit = {
        me.head match {
          case ts: StackSlot.Typed => self.emit.ts(ts)
          case us: StackSlot.Untyped => self.emit.us(us)
          case MemExpr.Head.RegImm(reg, offset) => self.emit.reg_offs(reg, offset)
          case MemExpr.Head.RegPair(obj, offset) => self.emit.r4_r4(obj, offset)
          case _ =>
        }

        me.body match {
          case body: Array[Symbol] =>
            val symbols = ArrayBuffer.empty[Symbol]
            val fieldRefs = ArrayBuffer.empty[Symbol]
            for (symbol <- body) {
              (fieldRefs.lastOption, symbol) match {
                case (_, curr: FieldReference) if !curr.isGeneric =>
                  symbols ++= (Some(curr) ++ Option(curr.fieldType))
                  fieldRefs += curr
                case (Some(prev: FieldReference), curr: FieldReference) if prev.isGeneric && curr.isGeneric =>
                  assert(prev.fieldType == curr.refType)
                  symbols ++= (Some(curr) ++ Option(curr.fieldType))
                  fieldRefs += curr
                case (_, curr: FieldReference) =>
                  symbols ++= (Option(curr.refType) ++ Some(curr) ++ Option(curr.fieldType))
                  fieldRefs += curr
                case _ =>
                  symbols += symbol
              }
            }
            segment.putW16(symbols.length ensuring (isNBits(_, 16)))
            for (symbol <- symbols) {
              self.emit.id16(symbol)
            }

          case typeKind: CbcTypeKind =>
            self.emit.tk(typeKind)
        }
      }
    }

    addLiteral(memExprBodyLiteral)
    emit.addFixup(new Fixups.FExtMemExprBodyOffs(fextldOpcode, me.mMode, low4, memExprBodyLiteral, literalsStart))
  }

  /** `imm` is the 60 high bits of fext-encoded imm value, see [[encodeImm]].
    * `dst` is a 4-bit number, result of [[encodeDst]].
    */
  private def emitFextYLd(imm: Long, dst: Long): Unit = {
    assert(isNBits(dst, 4), s"${dst.toHexString}")
    assert(0 == (imm & 0xF), s"${imm.toHexString}")
    val state = imm | dst
    if (state == 0) {
      return
    }

    val bytes = (64 - java.lang.Long.numberOfLeadingZeros(state) + 7) / 8
    var toEncode = state
    emit.op(fextYLdOpcode + bytes - 1)
    for (_ <- 0 until bytes) {
      val imm = (toEncode & 0xFF).toInt
      assert(imm != 0 || toEncode != 0)
      emit.byte(imm)
      toEncode >>= 8
    }

    assert(toEncode == 0 || toEncode == -1)
  }

  /** `dst` is a 4-bit number, result of [[encodeDst]]. */
  private def emitFextYLd4(uimm4: Int): Unit = {
    assert(uimm4 != 0 && isNBits(uimm4, 4))
    emit.op(fextYLd4Opcode + uimm4 - 1)
  }
}
