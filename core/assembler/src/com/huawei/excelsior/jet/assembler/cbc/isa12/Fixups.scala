/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT
import com.huawei.excelsior.jet.assembler.cbc.Assembler.normalizeImm
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.ImmEXT.N.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.ConditionalBranch.{B2xri16dM, B2xri8d8, C1dM}
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl as SOC
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl.Jump
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl.Jump.K.K8
import com.huawei.excelsior.jet.assembler.cbc.{Bits, FExtBCC, Assembler as OldAssembler}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.CBC_ID16
import com.huawei.excelsior.jet.assembler.fixups.{ControlFixup, Relocation}
import com.huawei.excelsior.jet.assembler.util.Overflows
import com.huawei.excelsior.jet.assembler.{AsmType, Fixup, Label, Symbol, Width}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils
import xscala.util.MathUtils.*


/** ISA12-specific fixups
  *
  * @author julian
  */
object Fixups {
  /**
    * Actual fixup size is in range of [minSize, maxSize].
    *
    * @param minSize       - including offset
    * @param maxSize       - including offset
    * */
  abstract class AbstractJump(_target: Label, val minSize: Int, val maxSize: Int)
    extends ControlFixup(_target, true, minSize) {

    override protected[assembler] final def expectedSize: Int = {
      assert(isVariable)
      val expectedSize = expectedSize0
      assert(minSize <= expectedSize && expectedSize <= maxSize)
      expectedSize
    }

    protected def expectedSize0: Int

    /** Actually resolved offset. */
    protected def offset: Int = {
      assert(!isVariable)
      (targetDistance - size).ensuring(_ < targetDistance)
    }
  }

  final class Jump(_target: Label) extends AbstractJump(_target, minSize = 2, maxSize = 5) {

    override protected def expectedSize0: Int = {
      (targetDistance: @unchecked) match {
        case x if isNBitsSigned(x - 2, 8)  => 2 // B1+d8
        case x if isNBitsSigned(x - 3, 16) => 3 // B1+d16
        case x                             => 5 // B1+d32
      }
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      (size: @unchecked) match {
        case 2 =>
          segment.setByte(position, Jump.format(Jump.K.K8))
          segment.setByte(position + 1, bits(offset, 0, 7))
        case 3 =>
          segment.setByte(position, Jump.format(Jump.K.K16))
          segment.setW16(position + 1, bits(offset, 0, 15))
        case 5 =>
          segment.setByte(position, Jump.format(Jump.K.K32))
          segment.setW32(position + 1, offset)
      }
    }
  }

  final class BCC private(op: BranchOp, l: Int, r: Int, width: Width, _target: Label)
    extends AbstractJump(_target, minSize = 3, maxSize = 8) {

    import ConditionalBranch.{B2rrd8, B3xrrdT}
    import Assembler.Width as ISA12Width

    def this(op: BranchOp, l: IR, r: IR, width: Width, _target: Label) = this(op, l.idx, r.idx, width, _target)
    def this(op: BranchOp, l: FR, r: FR, width: Width, _target: Label) = this(op, l.idx, r.idx, width, _target)

    val (cc, swap) = {
      val (condOp, swap, _) = FExtBCC.normalize(op)
      (Assembler.CC.from(condOp), swap)
    }

    override protected def expectedSize0: Int = {
      (targetDistance: @unchecked) match {
        case x if isNBitsSigned(x - 3, 8) && cc.page == 0 => 3 // B2rrd8_irl_irr_d8
        case x if isNBitsSigned(x - 4, 12)                => 4 // B3xrrdT8_irl_irr_d4_d8
        case x if isNBitsSigned(x - 5, 20)                => 5 // B3xrrdT16_irl_irr_d4_d16
        case x if isNBitsSigned(x - 8, 32)                => 8 // B3xrr_irl_irr_04_C1d32_imm32
      }
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      val isaWidth = ISA12Width(width)

      val seg = segment
      val pos = position

      (size: @unchecked) match {
        case 3 =>
          val arg = {
            val (low, high) = if (swap) (r, l) else (l, r)
            Bits.u4_u4(low, high)
          }
          seg.setByte(pos,     B2rrd8.format(cc, isaWidth))
          seg.setByte(pos + 1, arg)
          seg.setByte(pos + 2, bits(offset, 0, 7))

        case 4 =>
          val (imm4, imm8) = (bits(offset, 0, 3), bits(offset, 4, 11))
          seg.setByte(pos,     B3xrrdT.BranchIf.format(B3xrrdT.T.T8, cc.page))
          seg.setByte(pos + 1, B3xrrdT.BranchIf.secondByte(cc.opcWithoutPage(isaWidth), if (swap) r else l))
          seg.setByte(pos + 2, B3xrrdT.BranchIf.thirdByte(if (swap) l else r, imm4))
          seg.setByte(pos + 3, imm8)

        case 5 =>
          seg.setByte(pos,     B3xrrdT.BranchIf.format(B3xrrdT.T.T16, cc.page))
          seg.setByte(pos + 1, B3xrrdT.BranchIf.secondByte(cc.opcWithoutPage(isaWidth), if (swap) r else l))
          seg.setByte(pos + 2, B3xrrdT.BranchIf.thirdByte(if (swap) l else r, bits(offset, 0, 3)))
          seg.setW16 (pos + 3, bits(offset, 4, 19))

        case 8 =>
          seg.setByte(pos,     B3xrrdT.BranchIfContinue.format(cc.page))
          seg.setByte(pos + 1, B3xrrdT.BranchIfContinue.secondByte(cc.opcWithoutPage(isaWidth), if (swap) r else l))
          seg.setByte(pos + 2, B3xrrdT.BranchIfContinue.thirdByte(if (swap) l else r))
          seg.setByte(pos + 3, C1dM.bcc(negated = false, C1dM.M.M32))
          seg.setW32 (pos + 4, offset)
      }
    }

    override protected def guts = Fixup.seq(op, l, r, width, _target)
  }

  final class BCCImm(op: BranchOp, l: IR, r: Long, width: Width, _target: Label) extends AbstractJump(_target, minSize = 4, maxSize = 16) {

    import Assembler.Width as ISA12Width

    val (cc, imm) = {
      val (normalizedOp, normalizedImm) = normalizeImm(op, r, width)
      (Assembler.CC.from(normalizedOp), normalizedImm)
    }

    val low16signBit = if op.isSigned then bit(imm, 15) else 0
    val immext = getImmext((imm >> 16) + low16signBit)
    val immextSize = immext.map(_.genSize).getOrElse(0)

    override protected def expectedSize0: Int = {
      (targetDistance, imm) match {
        case _ if isNBitsSigned(targetDistance - 4, 8)  && MathUtils.isNBits(op.isSigned, imm, 8)  => 4
        case _ if isNBitsSigned(targetDistance - 6, 16) => 6 + immextSize
        case _ if isNBitsSigned(targetDistance - 9, 32) => 9 + immextSize
      }
    }

    private def setImmExt(ie: ImmEXT, pos: Int): Int = {
      segment.setW8(pos, ImmEXT.calculateOPCode(ie))

      ie.n match {
        case N8  => segment.setW8(pos + 1, ie.value.toInt)
        case N16 => segment.setW16(pos + 1, ie.value.toInt)
        case N32 => segment.setW32(pos + 1, ie.value.toInt)
        case N48 => segment.setW32(pos + 1, ie.value.toInt); segment.setW16(pos + 5, (ie.value >> 32).toInt)
      }

      pos + ie.genSize
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      val isaWidth = ISA12Width(width)

      val seg = segment
      var pos = position

      (size: @unchecked) match {
        case 4 =>
          seg.setByte(pos,     B2xri8d8.format(cc.page))
          seg.setByte(pos + 1, B2xri8d8.secondByte(cc, isaWidth, l))
          seg.setByte(pos + 2, bits(imm.toInt, 0, 7))
          seg.setByte(pos + 3, bits(offset, 0, 7))

        case x if x == 6 + immextSize =>
          if (immext.isDefined) {
            pos = setImmExt(immext.get, pos)
          }
          seg.setByte(pos,     B2xri16dM.BranchIf.format(cc.page))
          seg.setByte(pos + 1, B2xri16dM.BranchIf.secondByte(cc, isaWidth, l))
          seg.setW16 (pos + 2, bits(imm.toInt, 0, 15))
          seg.setW16 (pos + 4, bits(offset, 0, 15))

        case x if x == 9 + immextSize =>
          if (immext.isDefined) {
            pos = setImmExt(immext.get, pos)
          }
          seg.setByte(pos,     B2xri16dM.BranchIfContinue.format(cc.page))
          seg.setByte(pos + 1, B2xri16dM.BranchIfContinue.secondByte(cc, isaWidth, l))
          seg.setW16 (pos + 2, bits(imm.toInt, 0, 15))
          seg.setW16 (pos + 4, C1dM.bcc(negated = false, C1dM.M.M32))
          seg.setW32 (pos + 5, offset)
      }
    }

    override protected def guts = Fixup.seq(op, l, r, width, _target)
  }

  final class BCHA(arg: IR, negated: Boolean, _target: Label) extends AbstractJump(_target, minSize = 4, maxSize = 7) {
    override protected def expectedSize0: Int = {
      (targetDistance: @unchecked) match {
        case x if isNBitsSigned(x - 4, 16) => 4
        case x if isNBitsSigned(x - 7, 32) => 7
      }
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      val seg = segment
      val pos = position
      size match {
        case 4 =>
          val op = if negated then SOC.B2xrI.Opc1001.BranchNotChaTest else SOC.B2xrI.Opc1001.BranchChaTest
          seg.setByte(pos,     SOC.format(SOC.B2xrI.Opc1001.BranchChaTest.opc))
          seg.setByte(pos + 1, pack8(op.opx, arg))
          seg.setW16 (pos + 2, bits(offset, 0, 15))

        case 7 =>
          val op = if negated then SOC.B2xr.Opc0100.CmpNotChaTest else SOC.B2xr.Opc0100.CmpChaTest
          seg.setByte(pos,     SOC.format(SOC.B2xr.Opc0100.CmpChaTest.opc))
          seg.setByte(pos + 1, pack8(op.opx, arg))
          seg.setByte(pos + 2, C1dM.bcc(negated = false, C1dM.M.M32))
          seg.setW32 (pos + 3, offset)
      }
    }
  }

  abstract class BTT(val kind: BTT.Kind, l: IR, negated: Boolean, _target: Label) extends AbstractJump(_target, minSize = 6, maxSize = 9) {
    protected def emitImm16(pos: Int, converter: Relocation.Converter): Unit

    override protected def expectedSize0: Int = {
      (targetDistance: @unchecked) match {
        case x if isNBitsSigned(x - 6, 16) => 6
        case x if isNBitsSigned(x - 9, 32) => 9
      }
    }

    override def resolve(converter: Relocation.Converter): Unit = {
      val seg = segment
      val pos = position

      val tpe = kind match {
        case BTT.Kind.IOFC => B2xri16dM.TT.Iof
        case BTT.Kind.IOFI => B2xri16dM.TT.Iof
        case BTT.Kind.IOFA => B2xri16dM.TT.Iof
        case BTT.Kind.CHA => shouldNotReachHere("Should be generated using BCHA")
        case BTT.Kind.OPEN_CONE => B2xri16dM.TT.OpenCone
        case BTT.Kind.CLOSED_CONE => B2xri16dM.TT.ClosedCone
        case BTT.Kind.LEVEL => B2xri16dM.TT.LevelTest
        case BTT.Kind.POINT => B2xri16dM.TT.PointTest
      }

      (size: @unchecked) match {
        case 6 =>
          seg.setByte(pos,     B2xri16dM.BranchTT.format(tpe))
          seg.setByte(pos + 1, B2xri16dM.BranchTT.secondByte(tpe, negated, l))
          emitImm16  (pos + 2, converter)
          seg.setW16 (pos + 4, bits(offset, 0, 15))

        case 9 =>
          seg.setByte(pos,     B2xri16dM.BranchTTContinue.format(tpe))
          seg.setByte(pos + 1, B2xri16dM.BranchTTContinue.secondByte(tpe, negated, l))
          emitImm16  (pos + 2, converter)
          seg.setByte(pos + 4, C1dM.bcc(negated = false, C1dM.M.M32))
          seg.setW32 (pos + 5, offset)
      }
    }

    override protected def guts = Fixup.seq(kind, l, negated, _target)
  }

  final class BTTLevel(l: IR, level: Int, negated: Boolean, _target: Label) extends BTT(BTT.Kind.LEVEL, l, negated, _target) {
    override protected def emitImm16(pos: Int, converter: Relocation.Converter): Unit = {
      segment.setW16(pos, level.ensuring(MathUtils.isNBits(_, 16)))
    }
    override protected def guts: Array[Any] = Fixup.seq(kind, l, level, negated, _target)
  }

  final class BTTBySymbol(kind: BTT.Kind, l: IR, val symbol: Symbol, negated: Boolean, _target: Label) extends BTT(kind, l, negated, _target) {
    override protected def emitImm16(pos: Int, converter: Relocation.Converter): Unit = {
      converter.send(pos, CBC_ID16, symbol)
    }

    override protected def guts: Array[Any] = Fixup.seq(kind, l, symbol, negated, _target)
  }

}
