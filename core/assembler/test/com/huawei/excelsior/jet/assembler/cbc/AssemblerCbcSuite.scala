/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.{AssemblerToolbox, Label, Width as AsmWidth}
import com.huawei.excelsior.jet.assembler.AssemblerToolbox.ResultParseFormat
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.K.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.B3xrrt4iK.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Sign.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler
import com.huawei.excelsior.jet.assembler.cbc.isa12.Fixups.BTT
import com.huawei.excelsior.jet.assembler.cbc.Register.FR.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.*
import com.huawei.excelsior.jet.assembler.cbc.Assembler as OldAssembler
import com.huawei.excelsior.jet.assembler.cbc.isa12.ConditionalBranch.{B2xri16dM, B2xri8d8, B3xrrdT}
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.codeemitter.BranchOp
import org.scalatest.funsuite.AnyFunSuite
import xscala.util.MathUtils
import xscala.util.MathUtils.{bits, clearBit, isNBits, isNBitsSigned, signExtend}

import scala.runtime.stdLibPatches.Predef.assert
import scala.util.Random


class AssemblerCbcSuite extends AnyFunSuite with AssemblerToolbox[Assembler] {

  var asm: Assembler = _
  override def createEmitter() = {
    asm = new Assembler()
    asm.setUp()
    asm
  }

  override val resultParseFormat = ResultParseFormat.CBC

  private val regs = Seq(IR7, IR1, IR12, IR13)
  private val defaultWidths: Seq[AsmWidth] = Seq(W8, W16, W32, W64)

  private val baseOpSpace = 0x99
  private val defaultOpSpace = 0xFF

  val seed = 1216391901
  val random = Random(seed)

  test(s"AssemblerCbcSuite random seed: $seed") {}

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Checked tests

  locally {
    import Assembler.Checked.*

    for (op <- Seq(Add, Sub, Mul, Div); sign <- Seq(Signed, Unsigned); width <- defaultWidths) {
      val signWord = sign.toString
      val signBit = sign.opc

      for (dst <- regs; l <- regs; r <- regs) {
        test(s"$signWord $op $width $dst $l $r") {
          (op, sign == Signed) match {
            case (Add, true) => asm.cadd(dst, l, r, width)
            case (Sub, true) => asm.csub(dst, l, r, width)
            case (Mul, true) => asm.cmul(dst, l, r, width)
            case (Div, true) => asm.cdiv(dst, l, r, width)
            case (Add, false) => asm.cuadd(dst, l, r, width)
            case (Sub, false) => asm.cusub(dst, l, r, width)
            case (Mul, false) => asm.cumul(dst, l, r, width)
            case (Div, false) =>
          }

          if (sign == Unsigned && op == Div) {
            // no such instruction
          } else {
            // CHECKED_PREFIX, B3xrrr format, opcode and dst register, left and right registers, switch instruction
            checkFinal(
              0x42 | signBit,
              (dst.idx << 4) | s2(op.opc) << 2 | s2(Width(width).opc),
              r.idx << 4 | l.idx
            )
          }
        }
      }
    }

    val unsignedImm: Seq[Int] = Seq(
      0xF, 0xFF, 0xFFF,
      0x1, 0x10, 0x100,
      0x8, 0x80, 0x800, 0x88, 0x888,
      0x3, 0x33, 0x333
    )

    val signedImm: Seq[Int] = Seq(
      0xFFFFFFFF,
      0xFFFFFFF4,
      0xFFFFFFF0,
      0xFFFFFF40,
      0xFFFFFF44,
      0xFFFFFF00,
      0xFFFFF840,
      0xFFFFF844,
      0xFFFFF800,
    )

    for (op <- Seq(Add, Sub, Mul); width <- defaultWidths) {
      val signWord = Signed
      val signBit = Signed.opc
      for (dst <- regs; l <- regs; r <- signedImm) {
        test(s"$signWord $op $width $dst $l 0x${r.toHexString}") {
          (op: @unchecked) match {
            case Add => asm.caddi(dst, l, r, width)
            case Sub => asm.csubi(dst, l, r, width)
            case Mul => asm.cmuli(dst, l, r, width)
          }

          val isImm4 = MathUtils.isNBitsSigned(r, 4)
          require(isImm4 || MathUtils.isNBitsSigned(r, 12) || isNBits(r, 12))

          if (isImm4) {
            checkFinal(
              e3(0x20) | e1(0x2) | signBit,
              (dst.idx << 4) | s2(op.opc) << 2 | s2(Width(width).opc),
              (r & 0xF) << 4 | l.idx
            )
          } else {
            checkFinal(
              e3(0x28) | e1(0x2) | signBit,
              (dst.idx << 4) | s2(op.opc) << 2 | s2(Width(width).opc),
              ((r & 0xF) << 4) | l.idx,
              (r >>> 4) & 0xFF
            )
          }
        }
      }
    }

    for (op <- Seq(Add, Sub, Mul); width <- defaultWidths) {
      val signWord = Unsigned
      val signBit = Unsigned.opc
      for (dst <- regs; l <- regs; r <- unsignedImm) {
        test(s"$signWord $op $width $dst $l 0x${r.toHexString}") {
          (op: @unchecked) match {
            case Add => asm.cuaddi(dst, l, r, width)
            case Sub => asm.cusubi(dst, l, r, width)
            case Mul => asm.cumuli(dst, l, r, width)
          }

          val isImm4 = isNBits(r, 4)
          require(isImm4 || isNBits(r, 12))

          if (isImm4) {
            checkFinal(
              e3(0x20) | e1(0x2) | signBit,
              (dst.idx << 4) | s2(op.opc) << 2 | s2(Width(width).opc),
              ((r & 0xF) << 4) | l.idx
            )
          } else {
            checkFinal(
              e3(0x28) | e1(0x2) | signBit,
              (dst.idx << 4) | s2(op.opc) << 2 | s2(Width(width).opc),
              ((r & 0xF) & 0xF) << 4 | l.idx,
              (r >>> 4) & 0xFF
            )
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Common tests

  locally {
    import Assembler.Common.*

    for (op <- Seq(Add, Sub, Mul, And, Or, Xor, SDiv, SRem, UDiv, URem, LSR, ASR, LSL); width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val widthBit = Width(width).opcCommon
      val opOrd = op.ordinal

      for (dst <- regs; l <- regs; r <- regs) {
        test(s"common $op $width $dst $l $r") {
          var prohibitB2r = false
          op match {
            case Add => asm.add(width, dst, l, r)
            case Sub => asm.sub(width, dst, l, r)
            case Mul => asm.mul(width, dst, l, r)
            case And => asm.and(width, dst, l, r)
            case Or => asm.or(width, dst, l, r)
            case Xor => asm.xor(width, dst, l, r)
            case SDiv =>
              prohibitB2r = true
              asm.div(width, dst, l, r)
            case SRem =>
              prohibitB2r = true
              asm.rem(width, dst, l, r)
            case UDiv => asm.udiv(width, dst, l, r)
            case URem => asm.urem(width, dst, l, r)
            case LSR => asm.lsr(width, dst, l, r)
            case ASR => asm.asr(width, dst, l, r)
            case LSL => asm.lsl(width, dst, l, r)
          }

          if (dst == l && op.b2rAllowed && !prohibitB2r) {
            // COMMON_PREFIX, B2rr format, opcode, left and right registers, switch instruction
            checkFinal(
              s4(opOrd) << 1 | s1(widthBit),
              r.idx << 4 | s4(l.idx)
            )
          } else {
            // COMMON_PREFIX, B3xrrr format, opcode and dst register, left and right registers, switch instruction
            checkFinal(
              0x40 | opOrd >> 3,
              dst.idx << 4 | s3(opOrd & 0x7) << 1 | s1(widthBit),
              r.idx << 4 | s4(l.idx)
            )
          }
        }
      }
    }

    // TODO: more tests for common operations
  }

  private val fregs = Seq(FR7, FR1, FR12, FR13)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Float tests

  locally {
    import Assembler.FloatOperations.*

    for (op <- Seq(FAdd, FSub, FMul, FDiv); width <- Seq(W32, W64)) {
      for (dst <- fregs; l <- fregs; r <- fregs) {
        test(s"Float $op $width $dst $l $r") {
          (op: @unchecked) match {
            case FAdd => asm.fadd(width, dst, l, r)
            case FSub => asm.fsub(width, dst, l, r)
            case FMul => asm.fmul(width, dst, l, r)
            case FDiv => asm.fdiv(width, dst, l, r)
          }

          checkFinal(
            (0x23 << 1) | s1(Width(width).opcCommon),
            s4(op.opc) | dst.idx << 4,
            r.idx << 4 | l.idx
          )
        }
      }
    }

    import com.huawei.excelsior.jet.assembler.cbc.isa12.FloatImm.*

    test("FImm encoding (7)") {
      assertResult {
        EncodeData(7, K0, 0)
      } {
        encode(7.0f, Width(W32))
      }
    }

    test("FImm encoding (-8)") {
      assertResult {
        EncodeData(8, K0, 0)
      } {
        encode(-8.0f, Width(W32))
      }
    }

    test("FImm encoding (8)") {
      assertResult {
        EncodeData(8, K8, 0)
      } {
        encode(8.0f, Width(W32))
      }
    }

    test("FImm encoding (-2048)") {
      assertResult {
        EncodeData(0, K8, 0x80)
      } {
        encode(-2048.0f, Width(W32))
      }
    }

    test("FImm encoding (-0.0f)") {
      assertResult {
        EncodeData(1, K16, 2)
      } {
        encode(-0.0f, Width(W32))
      }
    }

    test("FImm encoding (0.25d)") {
      assertResult {
        EncodeData(3, K16, 0x3FD)
      } {
        encode(0.25d, Width(W64))
      }
    }

    test("FImm encoding (42f)") {
      assertResult {
        EncodeData(0xA, K8, 0x02)
      } {
        encode(42f, Width(W32))
      }
    }

    test("FImm encoding (-0.25d)") {
      assertResult {
        EncodeData(3, K16, 0xBFD)
      } {
        encode(-0.25d, Width(W64))
      }
    }

    test("FImm encoding (-0.26260877)") {
      assertResult {
        EncodeData(0, K16, 0x74A8, 0xBE86)
      } {
        encode(java.lang.Float.intBitsToFloat(0xBE8674A8), Width(W32))
      }
    }

    test("FImm encoding (1.1945E103)") {
      assertResult {
        EncodeData(0, K16, 0x5555, 0x555555555555L)
      } {
        encode(java.lang.Double.longBitsToDouble(0x5555555555555555L), Width(W64))
      }
    }

    for (op <- Seq(FMov, FNeg, FAbs, FSqrt); width <- Seq(W32, W64)) {
      for (dst1 <- fregs; dst <- fregs; src <- fregs) {
        test(s"Float $op $width $dst1 $dst $src") {
          (op: @unchecked) match {
            case FMov => asm.fmov(dst1, dst, src, width)
            case FNeg => asm.fneg(dst1, dst, src, width)
            case FAbs => asm.fabs(dst1, dst, src, width)
            case FSqrt => asm.fsqrt(dst1, dst, src, width)
          }

          checkFinal(
            (0x23 << 1) | s1(Width(width).opcCommon),
            s4(op.opc) | dst1.idx << 4,
            src.idx << 4 | dst.idx
          )
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Branch op suite

  locally { // d8
    import Assembler.CC.*

    val page0BranchOp = Seq(
      BranchOp.EQ,
      BranchOp.NE,
      BranchOp.LT,
      BranchOp.GE,
      // TODO return these to tests once test system upgraded
//      BranchOp.ULT,
//      BranchOp.UGE,
      BranchOp.REQ,
      BranchOp.RNE,
    )

    val page1BranchOp = Seq(
      BranchOp.FEQ,
      BranchOp.FNE,
      BranchOp.FLT,
      BranchOp.FNLT,
      BranchOp.FGE,
      BranchOp.FNGE,
      BranchOp.TESTZ,
      BranchOp.TESTNZ,
    )

    for (op <- page0BranchOp; l <- regs; r <- regs; width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val offset8Forward = random.nextInt(128).ensuring(isNBitsSigned(_, 8))
      val offset8Backward = (-random.nextInt(126) - 3).ensuring(isNBitsSigned(_, 8))

      val cc = CC.from(op)

      testBranchFixup(
        s"B2rrd8 branch.if forward $op $l $r $width $offset8Forward ${offset8Forward.toHexString}"
      )(
        offset8Forward, instructionSize = 3
      ) { label =>
        asm.bcc(op, l, r, width, label)
      } {
        Seq(
          0x50 | s4(cc.opc(Width(width))),
          p(r.idx, 4) | l.idx,
          offset8Forward & 0xFF,
        )
      }

      testBranchFixup(
        s"B2rrd8 branch.if backward $op $l $r $width $offset8Backward ${offset8Backward.toHexString}"
      )(
        offset8Backward, instructionSize = 3
      ) { label =>
        asm.bcc(op, l, r, width, label)
      } {
        Seq(
          0x50 | s4(cc.opc(Width(width))),
          p(r.idx, 4) | l.idx,
          offset8Backward & 0xFF,
        )
      }
    }

    for (op <- page0BranchOp ++ page1BranchOp; l <- regs; r <- regs; width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val offset12Forward = p(s3(random.nextInt(7) + 1), freeBits = 8) | s8(random.nextInt(256)) // Random positive 12bit value, which doesn't fit in 8 bit

      val offset12Backward = { // Random negative 12bit value, which doesn't fit in 8 bit
        val bits = p(1, freeBits = 11) | p(s3(random.nextInt(7)), 8) | s8(random.nextInt(256))
        signExtend(bits, 12)
      }

      val cc = CC.from(op)

      testBranchFixup(
        s"B3xrrdT8_${cc.page} forward branch.if $op $l $r $width $offset12Forward"
      )(
        offset12Forward, instructionSize = 4
      ) { label =>
        if (op.isFloatingPoint) {
          asm.bcc(op, FR.values(l.idx), FR.values(r.idx), width, label)
        } else {
          asm.bcc(op, l, r, width, label)
        }
      } {
        val (low4, high8) = (offset12Forward & 0xF, (offset12Forward >> 4) & 0xFF)
        Seq(
          e2(0x60)    | p(B3xrrdT.T.T8.opc, 1) | cc.page,
          p(l.idx, 4) | cc.opc(Width(width)) & 0xF,
          p(low4, 4)  | r.idx,
          high8
        )
      }

      testBranchFixup(
        s"B3xrrdT8_${cc.page} backward branch.if $op $l $r $width $offset12Backward"
      )(
        offset12Backward, instructionSize = 4
      ) { label =>
        if (op.isFloatingPoint) {
          asm.bcc(op, FR.values(l.idx), FR.values(r.idx), width, label)
        } else {
          asm.bcc(op, l, r, width, label)
        }
      } {
        val (low4, high8) = (offset12Backward & 0xF, (offset12Backward >> 4) & 0xFF)

        Seq(
          e2(0x60)    | p(B3xrrdT.T.T8.opc, 1) | cc.page,
          p(l.idx, 4) | cc.opc(Width(width)) & 0xF,
          p(low4, 4)  | r.idx,
          high8
        )
      }
    }

    for (op <- page0BranchOp ++ page1BranchOp; l <- regs; r <- regs; width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val offset20Forward = p(s3(random.nextInt(7) + 1), freeBits = 12) | random.nextInt(1 << 12).ensuring(isNBits(_, 12)) // Random positive 16bit value, which doesn't fit in 12 bit

      val offset20Backward = { // Random negative 16bit value, which doesn't fit in 12 bit
        val bits = p(1, freeBits = 15) | p(s3(random.nextInt(7)), 12) | random.nextInt(1 << 12).ensuring(isNBits(_, 12))
        signExtend(bits, 16)
      }

      val cc = CC.from(op)

      testBranchFixup(
        s"B3xrrdT16_${cc.page} forward branch.if $op $l $r $width $offset20Forward"
      )(
        offset20Forward, instructionSize = 5
      ) { label =>
        if (op.isFloatingPoint) {
          asm.bcc(op, FR.values(l.idx), FR.values(r.idx), width, label)
        } else {
          asm.bcc(op, l, r, width, label)
        }
      } {
        val (low4, middle8, high8) = (bits(offset20Forward, 0, 3), bits(offset20Forward, 4, 11), bits(offset20Forward, 12, 19))
        Seq(
          e2(0x60)    | p(B3xrrdT.T.T16.opc, 1) | cc.page,
          p(l.idx, 4) | cc.opc(Width(width)) & 0xF,
          low4 << 4 | r.idx,
          middle8,
          high8
        )
      }

      testBranchFixup(
        s"B3xrrdT16_${cc.page} backward branch.if $op $l $r $width $offset20Backward"
      )(
        offset20Backward, instructionSize = 5
      ) { label =>
        if (op.isFloatingPoint) {
          asm.bcc(op, FR.values(l.idx), FR.values(r.idx), width, label)
        } else {
          asm.bcc(op, l, r, width, label)
        }
      } {
        val (low4, middle8, high8) = (bits(offset20Backward, 0, 3), bits(offset20Backward, 4, 11), bits(offset20Backward, 12, 19))

        Seq(
          e2(0x60)    | p(B3xrrdT.T.T16.opc, 1) | cc.page,
          p(l.idx, 4) | cc.opc(Width(width)) & 0xF,
          low4 << 4 | r.idx,
          middle8,
          high8
        )
      }
    }

    val allBranchOpWithoutFP = (page0BranchOp ++ page1BranchOp).filter(!_.isFloatingPoint)
    val imm8 = (0 until 50).map(_ => random.nextInt().toByte.toInt).toSet.filter(_ != 0) // cmp with 0 produces `bcc reg IRZ`, must be tested elsewhere

    for (op <- allBranchOpWithoutFP; cc = CC.from(op); l <- regs; r <- imm8; width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val offset8Forward = bits(random.nextInt(), 0, 6).ensuring(_ >= 0)
      val offset8Backward = -random.nextInt(125) - 4

      testBranchFixup(
        s"B2xri8d8_${cc.page} forward branch.if $op $l ${r.toHexString} $width $offset8Forward"
      )(
        offset8Forward, instructionSize = 4
      ) { label =>
        asm.bcc(op, l, r, width, label)
      } {
        Seq(
          e1(B2xri8d8.ByteMask) | s1(cc.page),
          p(s4(l.idx), 4) | s4(cc.opc(Width(width)) & 0xF),
          bits(r, 0, 7),
          bits(offset8Forward, 0, 7)
        )
      }

      testBranchFixup(
        s"B2xri8d8_${cc.page} backward branch.if $op $l ${r.toHexString} $width $offset8Backward"
      )(
        offset8Backward, instructionSize = 4
      ) { label =>
        asm.bcc(op, l, r, width, label)
      } {
        Seq(
          e1(B2xri8d8.ByteMask) | s1(cc.page),
          p(s4(l.idx), 4) | s4(cc.opc(Width(width)) & 0xF),
          bits(r, 0, 7),
          bits(offset8Backward, 0, 7)
        )
      }
    }

    val imm16: Seq[Int] = (0 until 50) map { _ => (random.nextInt(255) + 1).toByte.toInt << 8 }
    for (op <- allBranchOpWithoutFP; cc = CC.from(op); l <- regs; r <- imm8; width <- Seq(AsmWidth.W32, AsmWidth.W64)) {
      val offset16Forward = bits(random.nextInt(), 0, 14).ensuring(_ >= 0) | 0x100
      val offset16Backward = ((-random.nextInt(Short.MaxValue.toInt + 1 - 6) - 6) | 0x100) ^ 0x100

      testBranchFixup(
        s"B2xri16d16_${cc.page} forward branch.if $op $l ${r.toHexString} $width $offset16Forward" // TODO write backward bcc test
      )(
        offset16Forward, instructionSize = 6
      ) { label =>
        asm.bcc(op, l, r, width, label)
      } {
        Seq(
          e4(B2xri16dM.ByteMask) | p(2, freeBits = 2) | p(s1(B2xri16dM.M.M16.ordinal), 1) | s1(cc.page),
          p(s4(l.idx), 4) | s4(cc.opc(Width(width)) & 0xF),
          bits(r, 0, 7),
          bits(r, 8, 15),
          bits(offset16Forward, 0, 7),
          bits(offset16Forward, 8, 15)
        )
      }
    }

    // TODO jump tests
  }

  def testBranchFixup(name: String)(offset: Int, instructionSize: Int)(genFixup: Label => Unit)(expectedBytes: => Seq[Any]): Unit = test(name) {
    if (offset < 0) {
      val label = asm.newBoundLabel
      val numberOfZeroes = (-offset - instructionSize).ensuring(_ >= 0, s"$offset $instructionSize")
      asm.putZeroes(numberOfZeroes)
      genFixup(label)
      checkFinal(Seq.fill[Int](numberOfZeroes)(0) ++ expectedBytes: _*)
    } else {
      val label = asm.newLabel
      genFixup(label)
      asm.putZeroes(offset)
      asm.bind(label)
      checkFinal(expectedBytes ++ Seq.fill[Int](offset)(0): _*)
    }
  }
}
