/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.BytecodeReferenceSymbol
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{CC, Width}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Assembler.{Opcode, low4, scut4}
import com.huawei.excelsior.jet.assembler.fixups.{ControlFixup, Relocation}
import com.huawei.excelsior.jet.assembler.{Fixup, Label}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.io.LEB128Encoder
import xscala.util.MathUtils

/**
  * Fixups for CBC bytecode.
  */
object Fixups {

  type Reg = IR | FR

  object Jump {
    def apply(target: Label, wide: Boolean): Fixup = new Jump(target, wide)
  }

  object Bcc {
    def apply(op: BranchOp, l: Reg, r: Reg, width: Width, _target: Label, wide: Boolean): Fixup = {
      val (cc, swap) = CondConversions.normalize(op)
      val (lhs, rhs) = if (swap) (r, l) else (l, r)

      def generic(opc: Opcode): Fixup = new BccGeneric(opc, cc, lhs, rhs, _target, wide)
      def specialized(opc: Opcode): Fixup = new BccSpecialized(opc, lhs, rhs, _target, wide)

      import CC.*
      import Opcode.*
      (cc, width) match {
        case (EQ, W32)  => specialized(Bcc32Eq)
        case (NE, W32)  => specialized(Bcc32Ne)
        case (LT, W32)  => specialized(Bcc32Lt)
        case (GE, W32)  => specialized(Bcc32Ge)
        case (ULT, W32) => specialized(Bcc32Ult)
        case (UGE, W32) => specialized(Bcc32Uge)
        case (EQ, W64)  => specialized(Bcc64Eq)
        case (NE, W64)  => specialized(Bcc64Ne)
        case (LT, W64)  => specialized(Bcc64Lt)
        case (GE, W64)  => specialized(Bcc64Ge)
        case (ULT, W64) => specialized(Bcc64Ult)
        case (UGE, W64) => specialized(Bcc64Uge)
        case (REQ, W64) => specialized(BccReq)
        case (RNE, W64) => specialized(BccRne)
        case (_, W32) => generic(Bcc32)
        case (_, W64) => generic(Bcc64)
        case _ => shouldNotReachHere(s"Width $width must be $W32 or $W64")
      }
    }
  }

  object BccImm {
    def apply(op: BranchOp, l: IR, r: Long, width: Width, target: Label, wide: Boolean): Fixup = {
      val (cc, imm) = CondConversions.normalizeImm(op, r, width)

      val opcode = (width: @unchecked) match {
        case W32 => Opcode.BccImm32
        case W64 => Opcode.BccImm64
      }
      new BccImm(opcode, cc, l, imm, target, wide)
    }
  }

  private final class Jump(target: Label, wide: Boolean) extends AbstractJump(target, 3) {
    override def expectedSize = targetDistance match {
      case x if !wide && MathUtils.isNBitsSigned(x - 3, 16) => 3
      case _ => 6
    }

    override def process(converter: Relocation.Converter) = size match {
      case 3 =>
        stream
          .opc8(Opcode.Jump)
          .write16(offset)
      case 6 =>
        stream
          .opc8(Opcode.WidePrefix)
          .opc8(Opcode.Jump)
          .write32(offset) // TODO: maybe 24?
    }
  }

  private final class BccImm(opcode: Opcode, cc: CC, l: IR, imm: Long, _target: Label, wide: Boolean)
    extends AbstractJump(_target, 5) {

    private def immediateSize = LEB128Encoder.calcSizeSLEB128(imm)
    private val defaultSize = 4 + immediateSize
    private val wideSize = 7 + immediateSize

    override def expectedSize: Int = targetDistance match {
      case x if !wide && MathUtils.isNBitsSigned(x - defaultSize, 16) => defaultSize
      case _ => wideSize
    }

    override def process(converter: Relocation.Converter) = {
      if (expectedSize == defaultSize) {
        stream
          .opc8(opcode)
          .bits(_.w4(cc).w4(l))
          .sleb(imm)
          .write16(offset)
      } else {
        assert(expectedSize == wideSize)
        stream
          .opc8(Opcode.WidePrefix)
          .opc8(opcode)
          .bits(_.w4(cc).w4(l))
          .sleb(imm)
          .write32(offset)
      }
    }
  }

  private final class BccSpecialized(opcode: Opcode, lhs: Reg, rhs: Reg, _target: Label, wide: Boolean)
    extends AbstractJump(_target, 4) {

    override def expectedSize: Int = targetDistance match {
      case x if !wide && MathUtils.isNBitsSigned(x - 4, 16) => 4
      case _ => 7
    }

    override def process(converter: Relocation.Converter) = size match {
      case 4 =>
        stream
          .opc8(opcode)
          .bits(_.w4(lhs).w4(rhs))
          .write16(offset)
      case 7 =>
        stream
          .opc8(Opcode.WidePrefix)
          .opc8(opcode)
          .bits(_.w4(lhs).w4(rhs))
          .write32(offset) // TODO: maybe 24?
    }
  }

  private final class BccGeneric(opcode: Opcode, cc: CC, lhs: IR | FR, rhs: IR | FR, _target: Label, wide: Boolean)
    extends AbstractJump(_target, 5) {

    override def expectedSize: Int = targetDistance match {
      case x if !wide && MathUtils.isNBitsSigned(x - 5, 20) => 5
      case _ => 8
    }

    override def process(converter: Relocation.Converter) = size match {
      case 5 =>
        stream
          .opc8(opcode)
          .bits(_.w4(cc).w4(lhs))
          .bits(_.w4(rhs).w4(low4(offset)))
          .write16(scut4(offset))
      case 8 =>
        stream
          .opc8(Opcode.WidePrefix)
          .opc8(opcode)
          .bits(_.w4(cc).w4(lhs))
          .bits(_.w4(rhs).w4(low4(offset)))
          .write32(scut4(offset)) // 36 bits TODO: maybe 28?
    }
  }

  private sealed abstract class AbstractJump(_target: Label, minSize: Int) extends ControlFixup(_target, true, minSize) {
    def offset = targetDistance - size
    def stream = InteriorByteStream(segment, position)

    def process(converter: Relocation.Converter): ByteStream

    override def resolve(converter: Relocation.Converter): Unit = {
      process(converter).asInstanceOf[InteriorByteStream]
        .ensuring(_.pos == (position + size))
    }
  }

  object Reference {
    def apply(target: BytecodeReferenceSymbol): Fixup = new Reference(target)
  }

  final class Reference(val target: BytecodeReferenceSymbol)
    extends Fixup(true, 1) {
    private var id: Int = -1

    override def expectedSize: Int = LEB128Encoder.calcSizeULEB128(id)

    private def stream = InteriorByteStream(segment, position)

    override def resolve(converter: Relocation.Converter): Unit = {
      assert(id != -1)
      stream
        .uleb(id)
    }

    def setId(id: Int): Unit = {
      assert(this.id == -1)
      this.id = id
    }
  }

}
