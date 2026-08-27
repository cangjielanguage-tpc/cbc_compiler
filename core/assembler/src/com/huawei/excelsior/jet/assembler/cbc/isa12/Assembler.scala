/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.cbc.Fixups.BTT.Kind.*
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.IRZ
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.SignedImmCompactEncoding.{EncodedImmParts, calculateMemoryCompactImm}
import com.huawei.excelsior.jet.assembler.cbc.StackSlot.OffHeapMemory
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.ImmEXT.N.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.Fixups.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl as SOC
import com.huawei.excelsior.jet.assembler.cbc.{Bits, CbcAssembler, CbcTypeKind, FExtBCC, FieldReference, OpcodePrefix, RawData, Register, StackSlot}
import com.huawei.excelsior.jet.assembler.fixups.{CoverageLocs, Relocation}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{CBC_ID16, CBC_ID32}
import com.huawei.excelsior.jet.assembler.{AsmType, Fixup, Label, Symbol, Width as AsmWidth}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils
import xscala.util.MathUtils.{bitsSigned, isNBitsSigned, rangeMask64, rightNBits32, rightNBits64, signExtend, isNBits as isNBitsUnsigned}

import scala.PartialFunction.condOpt

trait MeaningfulNewIsaParts {
  def movRef(dst: IR, src: IR): Unit
  def fmov (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def fneg (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def fabs (frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def fsqrt(frd2: FR, frd1: FR, frs: FR, w: AsmWidth): Unit
  def ret(v: IR, w: AsmWidth): Unit
  def retRef(v: IR): Unit
  def fret(v: FR, w: AsmWidth): Unit
  def initConstString(ts: StackSlot.Typed, stringId: Symbol): Unit
  def bfx(dst: IR, src: IR, resW: AsmWidth, argW: AsmWidth, sx: Boolean, offset: Int, size: Int): Unit
}

trait NewIsaParts extends MeaningfulNewIsaParts {
  def callDirect(rd: IR, methodId: Symbol): Unit
  def callVirt(rd: IR, methodId: Symbol): Unit
  def callInterf(rd: IR, sig_id: Symbol, methodId: Symbol): Unit
  def aliveReference(data: Symbol): Unit
  def unmovableReference(data: Symbol): Unit
  def aliveRefDifference(data: Symbol): Unit
  def aliveUnmovableDifference(data: Symbol): Unit
  def aliveRefCheck(data: Symbol): Unit
  def movVST(dst: IR, src: IR): Unit
}

// TODO: Merge with forked part
object Assembler {
  val BYTECODE_VERSION: Byte = 1

  object B1 { // op8
    inline def FormatBits: Int = 0x9 // 01001
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)
    inline def format(low3Bits: Int): Int = e3(ByteMask) | s3(low3Bits)
    val Nop = B1.format(0x0) // 0100_1000
  }

  object B2rr { // op8_rx_ry
    inline def FormatBits: Int = 0x0
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)
    inline def format(low4Bits: Int): Int = e4(ByteMask) | s4(low4Bits)
  }

  object B2ri4 { // op8_i4_rx
    inline def FormatBits: Int = 0x1
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)
    inline def format(low4Bits: Int): Int = e4(ByteMask) | s4(low4Bits)
  }

  case class B3xrr_parts(low3BitsOfFormatByte: Int, low4BitsOfSecondByte: Int)

  object B3xrrt4iK { // op5_ccc3_cccc4_rd_rx_imm4
    inline def FormatBits: Int = 0x1
    inline def FormatFreeBits: Int = 5
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)
    inline def format(k: K, low3Bits: Int): Int = e5(ByteMask) | p(s2(k.opc), freeBits = 3) | s3(low3Bits)

    /** For K0 and K8 imm bits length is {0, 8} + 4
      * For K16 and K32 bits length is {16, 32}
      */
    enum K(val bits: Int) {
      case K0 extends K(0)
      case K8 extends K(8)
      case K16 extends K(16)

      def opc: Int = ordinal
    }
  }

  object B3xrrr { // op8_op4_rd_rx_ry
    inline def FormatBits: Int = 0x8
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)
    inline def format(low3Bits: Int): Int = e3(ByteMask) | s3(low3Bits)
  }

  enum OP7A {
    case Common
    case Checked
    case SetIf
    case Float

    def opc: Int = ordinal
  }

  enum Checked {
    case Add
    case Sub
    case Mul
    case Div
    case UAdd
    case USub
    case UMul
    case Pow

    inline def opc: Int = ordinal
    inline def format(width: Width): Int = p(s2(opc), freeBits = 2) | s2(width.opc)
    inline def format(resW: Width, argW: Width): Int = p(s2(opc), freeBits = 2) | p(s1(resW.opcCommon), 1) | s1(argW.opcCommon)
  }

  object Checked {
    val BFX: Checked = Checked.Div

    private[Assembler] def prepareBits(op: Checked, width: Width, sign: Sign) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Checked.opc), freeBits = 1) | s1(sign.opc),
        low4BitsOfSecondByte = op.format(width)
      )
    }

    private[Assembler] def prepareBFX(resW: Width, argW: Width, sign: Sign) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Checked.opc), freeBits = 1) | s1(sign.opc),
        low4BitsOfSecondByte = Checked.BFX.format(resW, argW)
      )
    }
  }

  enum Common {
    case Add // 0b0000
    case Sub // 0b0001, `ext.u` for B2ri4 and B3xrrt+iK
    case Mul // 0b0010
    case And // 0b0011
    case Or // 0b0100
    case Xor // 0b0101
    case SDiv // 0b0110, `mov` for B2r* encodings
    case SRem // 0b0111, `mov.vst` and `mov.ref` for B2rr

    case UDiv // 0b1000
    case URem // 0b1001
    case LSR // 0b1010
    case ASR // 0b1011
    case LSL // 0b1100

    case Pow // 0b1101

    def b2rAllowed: Boolean = this.ordinal >> 3 == 0
  }

  object Common {
    final val Mov = Common.SDiv
    final val MovVst = Common.SRem
    final val MovRef = MovVst

    final val Extend = Common.Sub

    private[Assembler] def prepareBitsForB2Formats(op: Common, width: Width): Int = prepareBitsForB2Formats(op, width.opcCommon)
    private[Assembler] def prepareBitsForB2Formats(op: Common, b1: Int): Int = p(s3(op.ordinal), freeBits = 1) | s1(b1)

    private[Assembler] def prepareBitsForB3Formats(op: Common, sign: Sign): B3xrr_parts = prepareBitsForB3Formats(op, sign.opc)
    private[Assembler] def prepareBitsForB3Formats(op: Common, width: Width): B3xrr_parts = prepareBitsForB3Formats(op, width.opcCommon)

    private[Assembler] def prepareBitsForB3Formats(op: Common, b1: Int): B3xrr_parts = {
      val opCode = op.ordinal
      val page = opCode >>> 3
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Common.opc), freeBits = 1) | s1(page),
        low4BitsOfSecondByte = p(s3(opCode & 0x7), freeBits = 1) | s1(b1)
      )
    }
  }

  enum Sign {
    case Signed
    case Unsigned

    def opc: Int = ordinal
  }

  object Sign {
    inline def apply(signed: Boolean): Sign = if (signed) Sign.Signed else Sign.Unsigned
  }

  enum CC {
    case EQ
    case NE
    case LT
    case GE
    case ULT
    case UGE
    case REQ
    case RNE
    case FEQ
    case FNE
    case FLT
    case FNLT
    case FGE
    case FNGE
    case TESTZ
    case TESTNZ

    def opc(width: Width): Int = p(s4(ordinal), 1) | s1(width.opcCommon)
    def opcWithoutPage(width: Width): Int = opc(width) & 0xF
    def page: Int = s1(ordinal >> 3)
  }

  object CC {
    val TESTBIT: CC = REQ
    val TESTNBIT: CC = RNE

    /** Any case not mentioned below should be transformed in one of mentioned.
      * For example:
      * integral:          x  <= y ~~ y  >= x
      * floating-point:    x !>= y ~~ y !<= x
      * integral constant: x  >= c ~~ x > c - 1 and e.t.c. (edge cases must be optimized with identities to avoid integer overflow)
      */
    def from(op: BranchOp): CC = tryFrom(op).getOrElse {
      shouldNotReachHere(s"Unexpected kind $op")
    }

    def tryFrom(op: BranchOp): Option[CC] = condOpt(op) {
      case BranchOp.EQ => CC.EQ
      case BranchOp.NE => CC.NE
      case BranchOp.LT => CC.LT
      case BranchOp.GE => CC.GE
      case BranchOp.ULT => CC.ULT
      case BranchOp.UGE => CC.UGE

      case BranchOp.REQ => CC.REQ
      case BranchOp.RNE => CC.RNE
      case BranchOp.TESTBIT => CC.TESTBIT
      case BranchOp.TESTNBIT => CC.TESTNBIT

      case BranchOp.FEQ => CC.FEQ
      case BranchOp.FNE => CC.FNE
      case BranchOp.FLT => CC.FLT
      case BranchOp.FNLT => CC.FNLT
      case BranchOp.FGE => CC.FGE
      case BranchOp.FNGE => CC.FNGE

      case BranchOp.TESTZ => CC.TESTZ
      case BranchOp.TESTNZ => CC.TESTNZ
    }
  }

  // TODO as this class more and more resembles assembler.Width, consider replacing it's uses
  enum Width(val nbytes: Int) {
    case W8 extends Width(1)
    case W16 extends Width(2)
    case W32 extends Width(4)
    case W64 extends Width(8)

    def opc: Int = ordinal

    def opcCommon: Int = {
      debugAssert((opc & 0x2) != 0)
      opc - W32.opc
    }

    def nbits: Int = nbytes * 8
  }

  object Width {
    def apply(w: AsmWidth): Width = (w: @unchecked) match {
      case AsmWidth.W8 => Width.W8
      case AsmWidth.W16 => Width.W16
      case AsmWidth.W32 => Width.W32
      case AsmWidth.W64 | AsmWidth.WPTR => Width.W64
    }
  }

  enum LoadAccessKind {
    // ldk  | dst,w  | remarks
    //-------------------------
    case LD_U8 // 0000 | ir*,32 |
    case LD_U16 // 0001 | ir*,32 |
    case LD_32 // 0010 | ir*,32 |
    case SPECIAL // 0011 | ir*,64 | load effective address
    case LD_S8 // 0100 | ir*,32 |
    case LD_S16 // 0101 | ir*,32 |
    case LD_F32 // 0110 | fr*,32 |
    case LD_F64 // 0111 | fr*,64 |
    case LD_REC // 1000 | ir*,64 | load pointer to record
    case LD_UNUSED1 // 1001 | ------ |
    case LD_U32 // 1010 | ir*,64 |
    case LD_64 // 1011 | ir*,64 |
    case LD_UNUSED2 // 1100 | ------ |
    case LD_UNUSED3 // 1101 | ------ |
    case LD_S32 // 1110 | ir*,64 |
    case LD_REF // 1111 | ir*,64 | load traced objref

    def ldk: Int = ordinal
  }

  object LoadAccessKind {
    // TODO: replace CbcTypeKind usages with LoadAccessKind
    def from(cbcTypeKind: CbcTypeKind): LoadAccessKind = {
      import CbcTypeKind.*

      cbcTypeKind match {
        case I8 => LoadAccessKind.LD_S8
        case U8 => LoadAccessKind.LD_U8
        case F16 | I16 => LoadAccessKind.LD_S16
        case U16 => LoadAccessKind.LD_U16
        case CHAR | I32 | U32 => LoadAccessKind.LD_32
        case I64 | U64 | REC => LoadAccessKind.LD_64
        case REF | NREF => LoadAccessKind.LD_REF
        case F32 => LoadAccessKind.LD_F32
        case F64 => LoadAccessKind.LD_F64
        case x => shouldNotReachHere(s"unexpected cbc type kind for LoadAccessKind: $x")
      }
    }
  }

  enum StoreAccessKind {
    // stk | MW* |  src    | remarks
    //-------------------------------
    case ST_8 // 000 |  8  | ir*/imm |
    case ST_16 // 001 | 16  | ir*/imm |
    case ST_32 // 010 | 32  | ir*/imm |
    case ST_64 // 011 | 64  | ir*/imm | used to store pointer values
    case ST_REF // 100 | 64  |   ir*   | store traced objref; (src == irz) means null reference
    case SPECIAL // 101 | --  |   ir*   | set memexpr head and switch to Mem opcode space
    case ST_F32 // 110 | 32  |   fr*   |
    case ST_F64 // 111 | 64  |   fr*   |


    def stk: Int = ordinal

    def opx(highBit: Int): Int = {
      p(highBit, freeBits = 3) | s3(stk)
    }
  }

  object StoreAccessKind {
    def apply(accessWidth: Width, fp: Boolean): StoreAccessKind = {
      (accessWidth, fp) match {
        case (W8, false) => ST_8
        case (W16, false) => ST_16
        case (W32, false) => ST_32
        case (W64, false) => ST_64
        // ST_REF
        // SPECIAL
        case (W32, true) => ST_F32
        case (W64, true) => ST_F64

        case x => shouldNotReachHere(s"Denied store access kind: $x")
      }
    }

    // TODO: replace CbcTypeKind usages with StoreAccessKind
    def from(cbcTypeKind: CbcTypeKind): StoreAccessKind = {
      import CbcTypeKind.*

      cbcTypeKind match {
        case I8 | U8 => StoreAccessKind.ST_8
        case F16 | I16 | U16 => StoreAccessKind.ST_16
        case CHAR | I32 | U32 => StoreAccessKind.ST_32
        case I64 | U64 | REC => StoreAccessKind.ST_64
        case REF | NREF => StoreAccessKind.ST_REF
        case F32 => StoreAccessKind.ST_F32
        case F64 => StoreAccessKind.ST_F64
        case x => shouldNotReachHere(s"unexpected cbc type kind for StoreAccessKind: $x")
      }
    }
  }

  object BFX {
    object Extend {
      def unapply(packed: (Width, Width, Int, Int)): Option[Int] = condOpt(packed) {
        case (resW, argW, 0, size) if size % 8 == 0 && isNBitsUnsigned(size / 8 - 1, 2) =>
          p(s1(resW.opcCommon), 3) | p(s1(argW.opcCommon), 2) | (size / 8 - 1)
      }
    }

    // encoded as shift
    object Shift {
      def unapply(packed: (Width, Width, Int, Int)): Option[(Int, Int)] = condOpt(packed) {
        case (resW, argW, offset, size) if resW == argW && (offset + size) == argW.nbits =>
          (argW.nbytes, offset)
      }
    }

    object B3xrrt4i8 {
      /** Unpack (t4, imm8) */
      def unapply(packed: (Width, Width, Int, Int)): Option[(Int, Int)] = condOpt(packed) {
        case (_, _, offset, size) if isNBitsUnsigned(offset, 6) && isNBitsUnsigned(size, 6) =>
          val packed12 = p(s6(size), freeBits = 6) | s6(offset)
          (packed12 & 0xF, packed12 >>> 4)
      }
    }
  }

  def getImmext(imm: Long): Option[ImmEXT] = if (imm != 0) {
    import ImmEXT.N
    def getSizeAndSign: (ImmEXT.N, Sign) = {
      for (size <- Seq(N8, N16, N32); sign <- Sign.values) {
        if (isNBits(imm << 16, size.nBits + 16, sign)) {
          return (size, sign)
        }
      }
      (N.N48, Sign.Signed)
    }

    val (size, sign) = getSizeAndSign
    Some(ImmEXT(size, sign, imm & rightNBits64(size.nBits)))
  } else None

  case class ImmEXT(n: ImmEXT.N, sign: Sign, value: Long) {
    def decodeImmEXT(w: Width): Long = {
      (if (sign == Sign.Signed) signExtend(value, n.nBits) else value & rightNBits64(n.nBits)) << 16
    }

    def genSize = n.nBits / 8 + 1
  }

  object ImmEXT {
    private val opc: Int = 0xe // 0b01110 should be shifted to left by 3

    enum N(val nBits: Int) {
      case N8 extends N(8)
      case N16 extends N(16)
      case N32 extends N(32)
      case N48 extends N(48)
    }

    def calculateOPCode(imm: ImmEXT) = {
      assert(!(imm.sign == Sign.Unsigned && imm.n == N.N48)) // not encodable, opc 0b01110111 is reserved

      p(opc, 3) | p(imm.sign.opc, 2) | s2(imm.n.ordinal)
    }
  }

  private inline def debugAssert(inline f: Boolean): Unit = {
    inline val debugEnabled = true // it's declared in method to avoid messing with Scala incremental compilation
    inline if (debugEnabled) {
      assert(f)
    }
  }

  enum FloatOperations {
    case FAdd
    case FSub
    case FMul
    case FDiv

    case FMov
    case FNeg
    case FAbs
    case FSqrt

    case Movi2f
    case Movf2i

    inline def opc: Int = ordinal
  }

  object FloatOperations {
    val F32ToF = FAbs // only in B3xrrt+iK format (t should be 0, K should be 0)
    val FToF32 = FSqrt // only in B3xrrt+iK format (t should be 0, K should be 0)

    private[Assembler] def prepareBits(op: FloatOperations, width: Width) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | s1(width.opcCommon),
        low4BitsOfSecondByte = s4(op.opc)
      )
    }

    private[Assembler] def prepareConvertWithIntBits(toInteger: Boolean, isSigned: Boolean, tWidth: AsmWidth, width: AsmWidth) = {
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | s1(Width(width).opcCommon),
        low4BitsOfSecondByte = p(1, 3) | p(Sign(isSigned).opc, 2) | p(Width(tWidth).opcCommon, 1) | (if (toInteger) 1 else 0)
      )
    }

    private[Assembler] def prepareConvertBits(op: FloatOperations, width: AsmWidth) = {
      assert(op == FloatOperations.F32ToF || op == FToF32)
      assert(width == AsmWidth.W16 || width == AsmWidth.W64)

      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.Float.opc), freeBits = 1) | (if (width == AsmWidth.W64) 1 else 0),
        low4BitsOfSecondByte = s4(op.opc)
      )
    }
  }

  object SetIf {
    private[Assembler] def prepareBits(op: CC, width: Width): B3xrr_parts = {
      assert(!(op == CC.REQ || op == CC.RNE) || width == W64)
      val opCode = op.ordinal
      val page = opCode >>> 3
      B3xrr_parts(
        low3BitsOfFormatByte = p(s2(OP7A.SetIf.opc), freeBits = 1) | s1(page),
        low4BitsOfSecondByte = p(s3(opCode & 0x7), freeBits = 1) | s1(width.opcCommon)
      )
    }
  }

  private[isa12] def pack8(r1: Register, r2: Register): Int = pack8(r1.idx, r2.idx)
  private[isa12] def pack8(r1: Register, v2: Int): Int = pack8(r1.idx, v2)
  private[isa12] def pack8(v1: Int, r2: Register): Int = pack8(v1, r2.idx)
  private[isa12] def pack16(r: Register, high12: Int): Int = p(s(high12, 12), freeBits = 4) | s4(r.idx)
  private[isa12] def pack8(low4: Int, high4: Int): Int = p(s4(high4), freeBits = 4) | s4(low4)
  private[isa12] def pack16(low8: Int, high8: Int): Int = p(s8(high8), freeBits = 8) | s8(low8)

  inline def p(value: Int, freeBits: Int): Int = value << freeBits

  /** Checks, that only right n-bits are set. */
  inline def s(value: Int, n: Int): Int = {
    debugAssert((value & rightNBits32(n)) == value)
    value
  }

  inline def s8(value: Int): Int = s(value, 8)
  inline def s7(value: Int): Int = s(value, 7)
  inline def s6(value: Int): Int = s(value, 6)
  inline def s5(value: Int): Int = s(value, 5)
  inline def s4(value: Int): Int = s(value, 4)
  inline def s3(value: Int): Int = s(value, 3)
  inline def s2(value: Int): Int = s(value, 2)
  inline def s1(value: Int): Int = s(value, 1)

  /** Checks, that right n-bits are empty. */
  inline def e(value: Int, n: Int): Int = {
    debugAssert((value & rightNBits32(n)) == 0)
    value
  }

  inline def e8(value: Int): Int = e(value, 8)
  inline def e7(value: Int): Int = e(value, 7)
  inline def e6(value: Int): Int = e(value, 6)
  inline def e5(value: Int): Int = e(value, 5)
  inline def e4(value: Int): Int = e(value, 4)
  inline def e3(value: Int): Int = e(value, 3)
  inline def e2(value: Int): Int = e(value, 2)
  inline def e1(value: Int): Int = e(value, 1)

  def isNBits(v: Long, n: Int, sign: Sign): Boolean = MathUtils.isNBits(sign == Sign.Signed, v, n)
  def isNBits(v: Int, n: Int, sign: Sign): Boolean = MathUtils.isNBits(sign == Sign.Signed, v, n)

  /** See specification "Branch If (FExt BCC)" */
  private def canBeEncodedInBCC(op: BranchOp): Boolean = {
    import BranchOp.*
    op match {
      case EQ | NE |
           LT | GE | // can't do LE and GT
           ULT | UGE | // can't do ULE and UGT
           REQ | RNE |
           FEQ | FNE | FLT | FNLT | FGE | FNGE | // can't do FLE, FNLE and FGT and FNGT
           TESTZ | TESTNZ | TESTBIT => true
      case _ => false // anything, not listed here, must be achievable through swap of arguments or, in case of constant, normalization
    }
  }

  def normalizeImm(op: BranchOp, c: Long, width: AsmWidth): (BranchOp, Long) = {
    if (canBeEncodedInBCC(op)) {
      return (op, c)
    }

    def incrementUnsigned(c: Long): Long = {
      assert(c != rangeMask64(0, width.nbits - 1), s"$c $width")
      width match {
        case AsmWidth.W32 => c.toInt + 1
        case AsmWidth.W64 => c + 1
        case w => notImplemented(s"feel free to implement: $w")
      }
    }

    def incrementSigned(c: Long): Long = {
      assert(c != rangeMask64(0, width.nbits - 2), s"$c $width") // c != MaxSignedValue(width)
      c + 1
    }

    import BranchOp.*
    (op: @unchecked) match {
      case LE => (LT, incrementSigned(c))
      case GT => (GE, incrementSigned(c))
      case ULE => (ULT, incrementUnsigned(c))
      case UGT => (UGE, incrementUnsigned(c))
    }
  }
}
