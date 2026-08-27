/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT.Kind.*
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.fixups.{ControlFixup, Relocation}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.CBC_ID16
import com.huawei.excelsior.jet.assembler.{Fixup, Label, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils.*

/** CBC-specific fixups
  *
  * @author paul
  */
object Fixups {

  trait VariableOffsetFixup(signed: Boolean, allowZeroOffset: Boolean = false) { self: Fixup =>
    protected def targetDistance: Int

    /** Number of bytes to fit current offset to jump target.
      *
      * Note: target distance may vary during variable fixups resizing, but they resolve with fixed size,
      * which may be bigger than number of bytes required to encode target distance, see Fixup.resize.
      */
    private def offsetBytes = {
      assert(isVariable)

      targetDistance match {
        case 0 if allowZeroOffset => 0
        case d if isNBits(signed, d, 8)  => 1
        case d if isNBits(signed, d, 16) => 2
        case d if isNBits(signed, d, 32) => 4
      }
    }

    /** Size of permanent part of fixup in bytes. */
    protected def permanentSize: Int

    override protected[assembler] def expectedSize: Int = permanentSize + offsetBytes

    /** Actually allocated width. */
    protected def offsetWidth = {
      assert(!isVariable)
      Width(size - permanentSize)
    }

    /** Actually resolved offset. */
    protected def offset = {
      assert(!isVariable)
      targetDistance.ensuring(isNBits(signed, _, offsetWidth.nbits))
    }
  }

  abstract sealed class AbstractJump(_target: Label, override val permanentSize: Int)
    extends ControlFixup(_target, true, permanentSize + 1) with VariableOffsetFixup(true)

  /** "condop" is byte long. Table of encodings ("#" means there can be any value from {0, 1}):
    * {{{
    * || n | type  | cc     ||
    * ||---|-------|--------||
    * || # |  000  | iofc   ||
    * || # |  001  | iofi   ||
    * || # |  010  | iofa   ||
    * || # |  011  | cha    ||
    * || # |  10X  | cone   || X is either 0 or 1 and defines whatever cone is open or closed
    * || # |  110  | level  ||
    * || # |  111  | point  ||
   * }}}
    * "n" defines whether we jump or true or false.
    *
    * List of encodings variants:
    *     - General: [b.tt#][condop1|arg][imm16][disp#], where "#" from {8, 16, 32}
    *       "condop" and "arg" are 4-bit values.
    *       "imm16" is either "typeId" for iofc/iofi/iofa/point/cone tests or "level" for level test, "0" for CHA.
    *       "disp#" is "#"-bit value and defines bytecode offset to target.
    *       "arg" defines index of integral register, where object to test is stored.
    */
  object BTT {
    enum Kind {
      case IOFC
      case IOFI
      case IOFA
      case CHA
      case OPEN_CONE
      case CLOSED_CONE
      case LEVEL
      case POINT
    }

    val N_MASK = 0x8

    def condOp(kind: BTT.Kind, negated: Boolean): Long = (if (negated) N_MASK else 0) | (kind match {
      case IOFC => 0x0
      case IOFI => 0x1
      case IOFA => 0x2
      case CHA  => 0x3
      case OPEN_CONE => 0x4
      case CLOSED_CONE => 0x5
      case LEVEL => 0x6
      case POINT => 0x7
    })
  }

  abstract class BTT(val kind: BTT.Kind, val reg: IR, val negated: Boolean, _target: Label) extends AbstractJump(_target, 4) {
    protected def emitImm(converter: Relocation.Converter): Unit

    override def resolve(converter: Relocation.Converter): Unit = {
      segment.setByte(position, (offsetWidth: @unchecked) match {
        case W8 => 0x27
        case W16 => 0x28
        case W32 => 0x29
      })
      segment.setByte(position + 1, Bits.u4_u4(BTT.condOp(kind, negated), reg.idx))
      emitImm(converter)
      segment.setSigned(offsetWidth, position + 4, offset)
    }

    override protected def guts = notImplemented("guts")
  }

  class CHA_BTT(reg: IR, negated: Boolean, _target: Label) extends BTT(CHA, reg, negated, _target) {
    protected def emitImm(converter: Relocation.Converter): Unit = {}
  }

  class Level_BTT(reg: IR, negated: Boolean, val level: Int, _target: Label) extends BTT(LEVEL, reg, negated, _target) {
    protected def emitImm(converter: Relocation.Converter): Unit = {
      segment.setW16(position + 2, level ensuring (isNBits(_, 16)))
    }
  }

  class BTTBySymbol(reg: IR, negated: Boolean, val symbol: Symbol, _target: Label, kind: BTT.Kind) extends BTT(kind, reg, negated, _target) {
    protected final def emitImm(converter: Relocation.Converter): Unit = {
      converter.send(position + 2, CBC_ID16, symbol)
    }
  }

  final class BCC private(op: BranchOp, isImm: Boolean, l: Int, r: Int, width: Width, _target: Label) extends AbstractJump(_target, 3) {
    def this(op: BranchOp, l: IR, r: IR,     width: Width, _target: Label) = this(op, false, l.idx, r.idx, width, _target)
    def this(op: BranchOp, l: FR, r: FR,     width: Width, _target: Label) = this(op, false, l.idx, r.idx, width, _target)
    def this(op: BranchOp, l: IR, imm: Int,  width: Width, _target: Label) = this(op, true,  l.idx, imm,   width, _target)
    require(isNBits(l, 4), s"$l")
    require(isNBits(r, 4), s"$r")

    override def resolve(converter: Relocation.Converter): Unit = {
      val (condOp, swap) = FExtBCC.condOp(op, isImm, width)
      assert(!swap || !isImm, s"$op $condOp")
      val arg = {
        val (low, high) = if (swap) (r, l) else (l, r)
        Bits.u4_u4(low, high)
      }

      val seg = segment
      val pos = position

      val opcode: Int = (offsetWidth: @unchecked) match {
        case W8 => 0x01
        case W16 => 0x02
        case W32 => 0x03
      }

      seg.setByte(pos, opcode)
      seg.setByte(pos + 1, condOp)
      seg.setByte(pos + 2, arg)
      seg.setSigned(offsetWidth, pos + 3, offset)
    }

    override protected def guts = Fixup.seq(op, isImm, l, r, width, _target)
  }

  /** Implementation of unconditional jump CBC instructions. */
  private[cbc] final class Jump (_target: Label) extends AbstractJump(_target, 1) {
    override def resolve(converter: Relocation.Converter): Unit =
      Bits.jump(segment, position, offsetWidth, offset)

    override protected def guts = Fixup.seq(target)
  }

  final class InstanceOfBranch(imm8: Int, val symbol: Symbol, _target: Label) extends AbstractJump(_target, 4) {

    assert(symbol != null)
    assert(isNBits(imm8, 8), imm8)

    override def resolve(converter: Relocation.Converter): Unit = {
      val seg = segment
      val pos = position

      // D: [8, 16, 32]
      // format: op_iro_arg_4_id_16_disp_D
      seg.setByte(pos, 0x29 + Bits.jmpOp(offsetWidth))
      seg.setByte(pos + 1, imm8)
      seg.setW16(pos + 2, 0)
      seg.setSigned(offsetWidth, pos + 4, offset)

      converter.send(pos + 2, CBC_ID16, symbol)
    }

    override protected def guts = Fixup.seq(imm8, symbol, _target)
  }

  final class FExtMemExprBodyOffs(opcode: Int, mMode: Int, low4: Int, target: Label, literalsStart: Label)
      extends Fixup(true, 2) with VariableOffsetFixup(false, allowZeroOffset = true) {

    assert(isNBits(opcode, 8))
    assert(isNBits(mMode, 4))
    assert(isNBits(low4, 4))

    override protected def targetDistance = {
      assert(target.segment == literalsStart.segment)
      target.position - literalsStart.position
    }

    override val permanentSize = 1 /* fext.ld opcode */ + 1 /* (mMode | low4) byte */

    override def resolve(converter: Relocation.Converter): Unit = {
      segment.setByte(position, opcode + offsetWidth.nbytes)
      segment.setByte(position + 1, (mMode << 4) | low4)
      if (offsetWidth != W0) {
        segment.setUnsigned(offsetWidth, position + 2, offset)
      }
    }

    override protected def guts = Fixup.seq(opcode, mMode, low4, target)
  }
}
