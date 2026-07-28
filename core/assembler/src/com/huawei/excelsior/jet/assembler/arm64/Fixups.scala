/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.Fixup
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.arm64.immediates.ShiftedImm12
import com.huawei.excelsior.jet.assembler.arm64.immediates.ShiftedImm16
import com.huawei.excelsior.jet.assembler.fixups.*
import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.AsmError.require
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.arm64.Enums.AddSubOp.ADD
import com.huawei.excelsior.jet.assembler.arm64.Enums.LogicalOp.ANDS
import com.huawei.excelsior.jet.assembler.arm64.Bits.getZR
import com.huawei.excelsior.jet.assembler.arm64.Bits.noSP
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.*
import xscala.util.MathUtils.*

/** ARM64-specific fixups
  *
  * @author conwor
  * @author orangebyte256
  * @author ijorch
  * @author paul
  */
private[arm64] object Fixups {

  private val INSTR_SIZE = 4 // instruction size in bytes

  trait Utils { this: Fixup =>
    def emitAt(idx: Int, instr: Int): Unit =
      this.segment.setW32(this.position + idx * INSTR_SIZE, instr)

    def relocAt(idx: Int, kind: RelocationKind, target: Symbol, converter: Relocation.Converter): Unit =
      converter.send(this.position + idx * INSTR_SIZE, kind, target)

    def emitAt(idx: Int, instr: Int, kind: RelocationKind, target: Symbol, converter: Relocation.Converter): Unit = {
      emitAt(idx, instr)
      relocAt(idx, kind, target, converter)
    }

    def emit(instr: Int): Unit = emitAt(0, instr)

    def emit2(instrA: Int, instrB: Int): Unit = {
      emitAt(0, instrA)
      emitAt(1, instrB)
    }
  }

  abstract class FixedSizeControlFixup(_target: Symbol)
    extends ControlFixup(_target, false, INSTR_SIZE) with Utils {

    override protected[assembler] def expectedSize = shouldNotCallThis()
  }

  /** Implementation of `B symbol` and `BL symbol` assembler instructions. */
  class Jump(_target: Symbol, link: Boolean) extends FixedSizeControlFixup(_target) {
    override protected def guts = Fixup.seq(target, link)

    override def resolve(converter: Relocation.Converter): Unit = {
      if (isLocal) {
        emit(Bits.b_bl(targetDistance, link))
      } else {
        emit(Bits.b_bl(0, link))
        relocAt(0, ARM64_B_BL_IMM, target, converter)
      }
    }
  }

  /** Implementation of `B{c} label` assembler instruction. */
  class Branch(_target: Label, cond: CC) extends FixedSizeControlFixup(_target) {
    override protected def guts = Fixup.seq(target, cond)

    override def resolve(converter: Relocation.Converter): Unit = emit(Bits.b_cond(cond, targetDistance))
  }

  /** Implementation of `cbz rt, symbol` and `cbnz rt, symbol` assembler instructions. */
  class CompareBranch(nz: Boolean, reg: IRegister, _target: Label) extends FixedSizeControlFixup(_target) {
    Bits.cb_z(nz, reg, 0) // check args

    override protected def guts = Fixup.seq(target, nz, reg)

    override def resolve(converter: Relocation.Converter): Unit = emit(Bits.cb_z(nz, reg, targetDistance))
  }

  /** Implementation of `tbz rt, symbol` and `tbnz rt, symbol` assembler instructions. */
  class TestBranch(nz: Boolean, reg: IRegister, imm: Int, _target: Label)
    extends ControlFixup(_target, true, INSTR_SIZE) with Utils {

    Bits.tb_z(nz, reg, imm, 0) // check args

    override protected def guts = Fixup.seq(target, nz, reg, imm)

    override protected[assembler] def expectedSize = {
      assert(isAligned(targetDistance, INSTR_SIZE))
      INSTR_SIZE * (if (isNBitsSigned(targetDistance, 16)) 1 else 2)
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      if (size == 4) {
        emit(Bits.tb_z(nz, reg, imm, targetDistance))
      } else {
        emit2(
          Bits.logical(ANDS, getZR(reg), reg, 1L << imm),
          Bits.b_cond(if (nz) CC.NE else CC.EQ, targetDistance - INSTR_SIZE)
        )
      }
    }
  }

  /** Implementation of `ldr rt, label` assembler instruction. */
  class LdrLiteral(reg: Register, target: Label) extends FixedSizeFixup(INSTR_SIZE) with Utils {
    getBits(0) // check args

    protected def getBits(offset: Int) = Bits.ldrLiteral(reg, offset)

    override protected def guts = Fixup.seq(target, reg)

    override def resolve(converter: Relocation.Converter): Unit = {
      assert(target.segment == segment)
      emit(getBits(target.position - position))
    }
  }

  /** Implementation of `ldrsw rt, label` assembler instruction. */
  class LdrswLiteral(reg: IRegister.X, _target: Label) extends LdrLiteral(reg, _target) {
    override protected def getBits(offset: Int) =
      Bits.ldrswLiteral(reg, offset)
  }

  /** Implementation of getting PC-relative 64-bit address via adrp and add instructions. */
  class MovAddr(reg: IRegister.X, target: Symbol)
    extends FixedSizeFixup(INSTR_SIZE * (if (target.isInstanceOf[Label]) 1 else 2)) with Utils {

    require(noSP(reg), "bad arguments for ADR Rd, label")

    override protected def guts = Fixup.seq(target, reg)

    override def resolve(converter: Relocation.Converter): Unit = target match {
      case label: Label =>
        assert(label.segment == segment)
        emit(Bits.adr(reg, label.position - position))
      case _ =>
        emitAt(0, Bits.adrp(reg, 0),
                ARM64_ADRP_IMM, target, converter)
        emitAt(1, Bits.addSub(ADD, reg, reg, ShiftedImm12.encode(0)),
                ARM64_ADD_IMM_LO12, target, converter)
    }
  }

  /** Implementation of getting 32-bit offset in segment via two move instructions. */
  class MovOffs32InMethod(reg: IRegister, target: Label)
    extends FixedSizeFixup(INSTR_SIZE * 2) with Utils {

    require(noSP(reg), "bad arguments for MOVZ/MOVK Rd, imm16, shift")

    override protected def guts = Fixup.seq(target, reg)

    override def resolve(converter: Relocation.Converter): Unit = {
      val offset = target.position
      assert(isNBits(offset, 32))
      emit2(
        Bits.movz(reg, ShiftedImm16.encode(bits(offset, 0, 15), 0, W32)),
        Bits.movk(reg, ShiftedImm16.encode(bits(offset, 16, 31), 16, W32))
      )
    }
  }

  class CodeAlignment(_align: Int) extends Alignment(_align) with Utils {
    override protected def resolveImpl(): Unit = {
      assert(isAligned(position, INSTR_SIZE))
      assert(isAligned(size, INSTR_SIZE))
      val count = size / INSTR_SIZE
      for (i <- 0 until count) {
        emitAt(i, Bits.nop)
      }
    }
  }
}
