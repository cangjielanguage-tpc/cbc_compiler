/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

import com.huawei.excelsior.jet.assembler.AsmError.error
import com.huawei.excelsior.jet.assembler.AsmError.require
import com.huawei.excelsior.jet.assembler.Width.{W16, W32, W64, W8}
import com.huawei.excelsior.jet.assembler.amd64.Bits.{FIXED_WIDTH, VEXBits}
import com.huawei.excelsior.jet.assembler.amd64.GPR.{RAX, RSP}
import com.huawei.excelsior.jet.assembler.amd64.Immediate.fitsTo
import com.huawei.excelsior.jet.assembler.amd64.Immediate.smallestSize
import com.huawei.excelsior.jet.assembler.amd64.IntelWidth.*
import com.huawei.excelsior.jet.assembler.amd64.Register16.AX
import com.huawei.excelsior.jet.assembler.amd64.Register32.EAX
import com.huawei.excelsior.jet.assembler.amd64.Register8.*
import com.huawei.excelsior.jet.assembler.{Fixup, Segment, Width}
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import com.huawei.excelsior.jet.assembler.amd64.Bits.check

/** AMD64 instruction bits emitter.
  *
  * @author paul
  * @author cypok
  */
private[amd64] object Bits {
  // fixed operand size marker => no operand size prefixes required/allowed for instruction
  val FIXED_WIDTH = ZERO

  // ***************************************
  //          VEX prefix
  // ***************************************

  object VEXBits {
    // |23    16|15 13|12    8| 7 |6    3| 2 |1  0|
    // | opcode | RXB | opmap | W | vvvv | L | pp |
    // |++++++++| --- | +++++ | + | ---- | + | ++ |

    val bitR = 15
    val bitX = 14
    val bitB = 13

    val mmMask = 0x1f << 8
    val mm0F = 1 << 8
    val mm0F38 = 2 << 8
    val mm0F3A = 3 << 8

    val bitW = 7
    val W0 = 0 << bitW
    val W1 = 1 << bitW

    val bitR_shortForm = 7

    val vvvvPos = 3
    val vvvvMask = 0xf << vvvvPos

    val bitL = 2
    val L128 = 0 << bitL
    val L256 = 1 << bitL
    val LZ = L128

    val ppNO = 0
    val pp66 = 1
    val ppF3 = 2
    val ppF2 = 3
    val ppMask = 3

    val opcodePos = 16
    val opcodeMask = 0xff << opcodePos

    def op(opcode: Int) = opcode << opcodePos

    val instrFields = opcodeMask | mmMask | 1 << bitW | 1 << bitL | ppMask

    def getVEX3B(vexop: Int, R: Int, X: Int, B: Int, vvvv: Int) = {
      val mm = vexop & mmMask
      assert(mm >= mm0F && mm <= mm0F3A)
      (vexop & ~opcodeMask) | R << bitR | X << bitX | B << bitB | vvvv << vvvvPos
    }

    val longFormFields = 1 << bitX | 1 << bitB | 1 << bitW | mmMask

    def fitsToShortForm(vexBits: Int) =
      (vexBits & longFormFields) == (1 << bitX | 1 << bitB | mm0F)
  }

  def check(ok: Boolean): Unit = require(ok, "bad arguments of instruction")
  def AM(r: Register) = r.toAddrMode
}

private[amd64] abstract class Bits(features: Feature*) {
  def seg: Segment
  def addFixup(fixup: Fixup): Unit

  final def supports(feature: Feature) = features contains feature

  private def highBit(r: Register) = if (r != null && r.code >= 8) 1 else 0
  private def low3Bits(r: Register) = r.code & 7

  // Top-level emitters for Assembler

  // smth m/r: ME format, allowed sizes: 1, 2, 4, 8
  def opA1(opcode: Int, opcodeExt: Int, am: AddrMode): Unit = {
    val w = width(am)
    check(w == BYTE || is248(w))
    val szbit = if (w == BYTE) 0 else 1
    formatME(opcode + szbit, opcodeExt, am, w)
  }

  // smth m/r, imm8: ME format, allowed sizes: 2, 4, 8; imm size = 1
  def opA_I8(opcode: Int, opcodeExt: Int, am: AddrMode, imm: Int): Unit = {
    val w = width(am)
    check(is248(w))
    formatME_I8(opcode, opcodeExt, am, w, imm)
  }

  // smth m/r, r: MR format, allowed sizes: 1, 2, 4, 8
  def opR1_A(opcode: Int, r: Register, am: AddrMode): Unit = {
    val w = width(r, am)
    check(w == BYTE || is248(w))
    val szbit = if (w == BYTE) 0 else 1
    formatMR(opcode + szbit, r, am, w)
  }

  // smth r, m/r: MR format, allowed sizes: 2, 4, 8
  def opR_A(opcode: Int, r: Register, am: AddrMode): Unit = {
    val w = width(r, am)
    check(is248(w))
    formatMR(opcode, r, am, w)
  }

  // smth m/r: ME format; operand size default & fixed at address width
  // no operand size prefixes required/allowed
  def opAW(opcode: Int, opcodeExt: Int, am: AddrMode): Unit = {
    check(matchWidth(am, WPTR))
    formatME(opcode, opcodeExt, am, FIXED_WIDTH)
  }

  // smth r: SR format; operand size default & fixed at address width
  def opRW(baseOpcode: Int, reg: GPR): Unit = {
    formatSR(baseOpcode, reg, FIXED_WIDTH)
  }

  // movXx[d] r, m/r: MR format, allowed sizes: dst{2, 4, 8}, src{srcWidth}, dstSize > srcSize
  def movXxR_A(opcode: Int, dst: Register, src: AddrMode): Unit = {
    val wdst = width(dst)
    val wsrc = width(src)
    check(is248(wdst) && wsrc < wdst && wsrc >= BYTE)
    formatMR(opcode, dst, src, wdst)
  }

  def shiftImm(opcodeExt: Int, am: AddrMode, count: Int): Unit = {
    val w = width(am)
    check(w == BYTE || is248(w))
    val szbit = if (w == BYTE) 0 else 1
    if (count == 1) {
      formatME(0xD0 + szbit, opcodeExt, am, w)
    } else {
      formatME_I8(0xC0 + szbit, opcodeExt, am, w, count)
    }
  }

  def shiftCL(opcodeExt: Int, am: AddrMode, count: Register8): Unit = {
    val w = width(am)
    check(w == BYTE || is248(w))
    val szbit = if (w == BYTE) 0 else 1
    check(count == CL)
    formatME(0xD2 + szbit, opcodeExt, am, w)
  }

   def doubleShiftImm(opcode: Int, am: AddrMode, r: Register, count: Int): Unit = {
    val w = width(r, am)
    check(is248(w) && fitsTo(count, BYTE))
    formatMR_I(opcode, r, am, w, count, imm8 = true)
  }

  def doubleShiftCL(opcode: Int, am: AddrMode, r: Register, count: Register8): Unit = {
    val w = width(r, am)
    check(is248(w) && count == CL)
    formatMR(opcode, r, am, w)
  }

  def imulR_A_I(r: Register, am: AddrMode, imm: Int): Unit = {
    val w = width(r, am)
    check(is248(w))
    if (fitsTo(imm, BYTE)) {
      formatMR_I(0x6b, r, am, w, imm, imm8 = true)
    } else {
      formatMR_I(0x69, r, am, w, imm, imm8 = false)
    }
  }

  def testA_I(am: AddrMode, imm: Int): Unit = {
    val opsize = width(am)
    check(opsize == BYTE || is248(opsize))
    if (isAcc(am.asRegister)) {
      formatS(if (opsize == BYTE) 0xa8 else 0xa9, opsize)
      immediate(imm, width(imm, opsize))
    } else {
      formatME_I(if (opsize == BYTE) 0xf6 else 0xf7, 0, am, opsize, imm)
    }
  }

  private def movA_I_impl(am: AddrMode, immSize: Width): Width = {
    val w = width(am)
    checkImmBounds(immSize, w)
    if (am.isRegister && (w != W64 || immSize == W64)) {
      // prefer format A_I for mov r64, imm32: it is shorter by 3 bytes
      formatSR(if (w == BYTE) 0xB0 else 0xB8, am.asRegister, w)
      w // there is the only place where imm64 can be generated
    } else {
      val iw = getImmWidth(immSize, w)
      formatME(if (w == BYTE) 0xc6 else 0xc7, 0, am, w, iw)
      iw
    }
  }

  def movA_I(am: AddrMode, imm: Long): Unit = {
    val iw = movA_I_impl(am, smallestSize(imm))
    immediate(imm, iw)
  }

  def movA_I(am: AddrMode, imm: Immediate): Unit = {
    val iw = movA_I_impl(am, imm.smallestSize)
    immediate(imm, iw)
  }

  private def x80Group_impl(opcodeExt: Int, opAcc: Int, am: AddrMode, immWidth: Width): Unit = {
    val operandSize = width(am)
    check(operandSize == BYTE || is248(operandSize))
    val szbit = if (operandSize == BYTE) 0 else 1
    if (operandSize != BYTE && immWidth == BYTE) {
      formatME(0x83, opcodeExt, am, operandSize, immWidth)
    } else if (isAcc(am.asRegister)) {
      formatS(opAcc + szbit, operandSize)
    } else {
      formatME(0x80 + szbit, opcodeExt, am, operandSize, immWidth)
    }
  }

  def x80Group(opcodeExt: Int, opAcc: Int, am: AddrMode, imm: Int): Unit = {
    val iw = if (fitsTo(imm, BYTE)) BYTE else width(imm, width(am))
    x80Group_impl(opcodeExt, opAcc, am, iw)
    immediate(imm, iw)
  }

  def x80Group(opcodeExt: Int, opAcc: Int, am: AddrMode, imm: Immediate): Unit = {
    val iw = if (imm.fitsTo(BYTE)) BYTE else width(imm, width(am))
    x80Group_impl(opcodeExt, opAcc, am, iw)
    immediate(imm, iw)
  }

  /** temp <- r1; r1 <- r2; r2 <- temp; */
  def xchgR_R(r1: Register, r2: Register): Unit = {
    def isNop = r1 == r2 && r1 == EAX // special handling of "xchg eax, eax" because this is used as "nop"
    val w = width(r1, r2)
    if (is248(w) && (isAcc(r1) || isAcc(r2)) && !isNop) {
      formatSR(0x90, if (isAcc(r1)) r2 else r1, w)
    } else {
      opR1_A(0x86, r1, r2.toAddrMode)
    }
  }

  private def isAcc(r: Register) = (r == AL) || (r == AX) || (r == EAX) || (r == RAX)

  // region Utils to generate parts of instruction binary format

  // ***************************************
  //          prefixes
  // ***************************************

  /** base opcode of REX prefix */
  private val REX_BASE = 0x40

  /** Checks that byte registers used correctly:
    *  - AH, BH, CH, DH not used with REX
    *  - only A*, B*, C*, D* (*=H,L) can be used w/o REX
    *
    *  Returns adjusted REX prefix
    */
  private def checkREXWithByteHighReg(rex: Int, reg: Register): Int = reg match {
    case AH | BH | CH | DH =>
      require(rex == 0, "Cannot use high register in rex instruction")
      0
    case AL | BL | CL | DL => rex
    case _: Register8 if rex == 0 => REX_BASE
    case _ => rex
  }

  private def getREX(operandSize: Width, reg: Register, index: Register, base: Register): Int = {
    val W = if (is8(operandSize)) 1 else 0
    val R = highBit(reg)
    val X = highBit(index)
    val B = highBit(base)
    val bits = W << 3 | R << 2 | X << 1 | B
    var rex = if (bits == 0) 0 else REX_BASE + bits
    rex = checkREXWithByteHighReg(rex, reg)
    rex = checkREXWithByteHighReg(rex, base)
    rex
  }

  private def segPrefix(am: AddrMode): Unit = {
    if (am != null && am.prefix != null) {
      assert(am.prefix.isSegmentOverride)
      seg.putByte(am.prefix.code)
    }
  }

  private def prefixes(operandSize: Width, ssePrefix: Int, reg: Register, index: Register, base: Register): Unit = {
    val rex = getREX(operandSize, reg, index, base)

    if (operandSize == WORD) {
      seg.putByte(Prefix.OP_SIZE.code)
    }
    if (ssePrefix != 0) {
      assert(ssePrefix == 0x66 || ssePrefix == 0xF2 || ssePrefix == 0xF3)
      assert(ssePrefix != 0x66 || operandSize != WORD)
      seg.putByte(ssePrefix)
    }
    if (rex != 0) {
      seg.putByte(rex)
    }
  }

  private def prefixes(operandSize: Width, reg: Register): Unit =
    prefixes(operandSize, 0, null, null, reg) // for SR format, high reg bit should be in B bit

  private def prefixes(ssePrefix: Int, reg: Register, am: AddrMode, operandSize: Width): Unit = {
    assert(am != null)
    segPrefix(am)
    val base = if (am.isRegister) am.asRegister else am.base
    prefixes(operandSize, ssePrefix, reg, am.index, base)
  }

  // ***************************************
  //          VEX prefix
  // ***************************************

  private def vexOp(vexop: Int, reg: Register, am: AddrMode, v: Register): Unit = {
    assert((vexop & ~VEXBits.instrFields) == 0)

    val R = highBit(reg) ^ 1
    val X = highBit(am.index) ^ 1
    val B = highBit(if (am.isRegister) am.asRegister else am.base) ^ 1
    val vvvv = (if (v == null) 0 else v.code) ^ 0xf

    val vex3b = VEXBits.getVEX3B(vexop, R, X, B, vvvv)
    emitBytes(0xC4, vex3b >> 8, vex3b & 0xFF, vexop >> VEXBits.opcodePos)
  }

  // ***************************************
  //          addressing modes
  // ***************************************

  private def compositeByte(high: Int, medium: Int, low: Int): Unit = {
    assert(0 <= high && high < 4)
    assert(0 <= medium && medium < 8)
    assert(0 <= low && low < 8)
    seg.putByte((high << 6) + (medium << 3) + low)
  }

  /** ModR/M byte */
  private def modRMByte(mod: Int, kindOrReg: Int, base3: Int): Unit =
    compositeByte(mod, kindOrReg, base3)

  /** SIB byte */
  private def SIBByte(ss: Int, index3: Int, base3: Int): Unit =
    compositeByte(ss, index3, base3)

  /** 32/64-bit addressing form with ModR/M byte: reg */
  private def modRM_R(kindOrReg: Int, rm: Register): Unit =
    modRMByte(3, kindOrReg, low3Bits(rm)) // mod = 0b11

  /** 32/64-bit addressing form with ModR/M byte and optional SIB byte
    *  - [base + scale*index + disp]
    *  - NOTE index != esp, scale IN (1,2,4,8)
    *
    * @return width of disp field that should be generated
    */
  private def modRM_M(kindOrReg: Int, base: Register, scale: Width, index: Register, disp: Int, needFixup: Boolean): Width = {
    assert(0 <= kindOrReg && kindOrReg < 8)

    val useSIB = 4 // ModRM:rm field
    val noBase = 5 // ModRM:rm or SIB:base field
    val noIndex = 4 // SIB:index field

    if (needFixup && (base != null || index != null)) {
      error("Cannot use base/scaled registers in RIP-relative addressing")
    }

    val (mod, dispWidth) = {
      if (base == null) {
        (0, DWORD)
      } else if (needFixup) {
        (2, DWORD)
      } else if (disp == 0 && low3Bits(base) != noBase) {
        (0, null) // TODO: check uses
      } else if (fitsTo(disp, BYTE)) {
        (1, BYTE)
      } else {
        (2, DWORD)
      }
    }

    val base3 = if (base == null) noBase else low3Bits(base)

    if (index == null) {
      // absolute addressing in 64-bit mode: use ModRM w. SIB encoding
      val abs64 = (base == null) && !needFixup

      if (abs64 || base3 == useSIB) {
        modRMByte(mod, kindOrReg, useSIB)
        SIBByte(0, noIndex, base3)
      } else {
        modRMByte(mod, kindOrReg, base3)
      }
    } else {
      require(index.code != RSP.code, "Invalid effective address")
      modRMByte(mod, kindOrReg, useSIB)

      val ss = adjustWidth(scale).log2bytes ensuring (_ <= 3)
      SIBByte(ss, low3Bits(index), base3)
    }

    dispWidth
  }

  private def genDisp(disp: Int, dispWidth: Width): Unit = (dispWidth: @unchecked) match {
    case null => // do nothing
    case W8 => seg.putByte(disp & 0xff)
    case W32 => seg.putW32(disp)
  }

  /** 32/64-bit addressing form */
  private def modrm(kindOrReg: Int, am: AddrMode, immWidth: Width): Unit = {
    val kr = kindOrReg & 7 // drop high reg bit
    if (am.isRegister) {
      modRM_R(kr, am.asRegister)
    } else {
      val needFixup = am.symbol != null
      val dispWidth = modRM_M(kr, am.base, am.scale, am.index, am.disp, needFixup)
      if (needFixup) {
        val kind = RelocationKind.OFFS32
        assert(dispWidth == kind.width)
        val addend = am.disp - (kind.width.nbytes + immWidth.nbytes)
        addFixup(new Relocation(kind, am.symbol, addend))
      } else {
        genDisp(am.disp, dispWidth)
      }
    }
  }

  private def modrm(r: Register, am: AddrMode, immWidth: Width): Unit =
    modrm(r.code, am, immWidth)

  // ***************************************
  //          instruction formats
  // ***************************************

  private def opcode(opcode: Int): Unit = {
    assert(opcode >= 0)
    if (opcode > 0xff) {
      val escape = opcode >>> 8
      assert(escape == 0x0f || escape == 0x0f3a || escape == 0x0f38)
      if (escape > 0xff) {
        seg.putByte((escape >>> 8) & 0xff)
      }
      seg.putByte(escape & 0xff)
    }
    seg.putByte(opcode & 0xff)
  }

  private def immediate(imm: Long, width: Width): Unit = {
    checkImmBounds(smallestSize(imm), width)
    (width: @unchecked) match {
      case W8  => seg.putW8((0xff & imm).toInt)
      case W16 => seg.putW16((0xffff & imm).toInt)
      case W32 => seg.putW32(imm.toInt)
      case W64 => seg.putW64(imm)
    }
  }

  private def immediate(imm: Immediate, width: Width): Unit = imm match {
    case imm: Immediate.Relocated =>
      assert(width == imm.size)
      addFixup(imm.relocation)
    case imm: Immediate.Value =>
      immediate(imm.value, width)
  }

  // endregion

  // B format: plain sequence of bytes
  final def emitByte(b1: Int): Unit = {
    seg.putByte(b1)
  }

  // B format: plain sequence of bytes
  final def emitBytes(b1: Int, b2: Int): Unit = {
    seg.putByte(b1)
    seg.putByte(b2)
  }

  // B format: plain sequence of bytes
  final def emitBytes(b1: Int, b2: Int, b3: Int): Unit = {
    seg.putByte(b1)
    seg.putByte(b2)
    seg.putByte(b3)
  }

  // B format: plain sequence of bytes
  final def emitBytes(bytes: Int*): Unit = seg.putBytes(bytes: _*)

  // B_I format: opcode, imm
  final def formatB_I(opc: Int, imm: Int, immWidth: Width): Unit = {
    emitByte(opc)
    assert(immWidth != QWORD)
    immediate(imm, immWidth)
  }

  // B_I format: opcode, imm
  final def formatB_I(opc: Int, imm: Immediate, immWidth: Width): Unit = {
    emitByte(opc)
    assert(immWidth != QWORD)
    immediate(imm, immWidth)
  }

  // S format: prefixes, opcode
  final def formatS(opc: Int, operandSize: Width): Unit = {
    assert(opc >= 0)
    prefixes(operandSize, null)
    opcode(opc)
  }

  // SR format: prefixes, opcode + reg
  final def formatSR(opcBase: Int, reg: Register, operandSize: Width): Unit = {
    assert(opcBase >= 0)
    prefixes(operandSize, reg)
    opcode(opcBase + low3Bits(reg))
  }

  // Fsmth st(i): B+fr format
  def formatFR(byte1: Int, byte2: Int, fr: FPURegister): Unit = {
    emitBytes(byte1, byte2 + fr.code)
  }

  // MR format: prefixes, opcode, modrm/reg, [sib], [disp], [imm]
  private def formatMR_impl(ssePrefix: Int, opc: Int, r: Register, am: AddrMode,
                            operandSize: Width, immWidth: Width): Unit = {
    assert(opc >= 0)
    prefixes(ssePrefix, r, am, operandSize)
    opcode(opc)
    modrm(r, am, immWidth)
  }

  // MR with mandratory/simd prefix
  final def formatMR_SSE(ssePrefix: Int, opc: Int, r: Register, am: AddrMode, operandSize: Width): Unit = {
    formatMR_impl(ssePrefix, opc, r, am, operandSize, ZERO)
  }

  // MR without imm
  final def formatMR(opc: Int, r: Register, am: AddrMode, operandSize: Width): Unit = {
    formatMR_impl(0, opc, r, am, operandSize, ZERO)
  }

  // MR with imm
  final def formatMR_I(opc: Int, r: Register, am: AddrMode, operandSize: Width, imm: Int, imm8: Boolean): Unit = {
    val iw = width(imm, if (imm8) BYTE else operandSize)
    formatMR_impl(0, opc, r, am, operandSize, iw)
    immediate(imm, iw)
  }

  // ME format: prefixes, opcode, modrm/ext, [sib], [disp], [imm]
  private def formatME(opc: Int, opcExt: Int, am: AddrMode, operandSize: Width, immWidth: Width): Unit = {
    assert(opc >= 0)
    assert(opcExt >= 0 && opcExt <= 7)
    prefixes(0, null, am, operandSize)
    opcode(opc)
    modrm(opcExt, am, immWidth)
  }

  // ME without imm
  final def formatME(opc: Int, opcExt: Int, am: AddrMode, operandSize: Width): Unit = {
    formatME(opc, opcExt, am, operandSize, ZERO)
  }

  // ME with imm
  final def formatME_I(opc: Int, opcExt: Int, am: AddrMode, operandSize: Width, imm: Int): Unit = {
    val iw = width(imm, operandSize)
    formatME(opc, opcExt, am, operandSize, iw)
    immediate(imm, iw)
  }

  // ME with imm8
  final def formatME_I8(opc: Int, opcExt: Int, am: AddrMode, operandSize: Width, imm: Int): Unit = {
    formatME(opc, opcExt, am, operandSize, BYTE)
    immediate(imm, BYTE)
  }

  // VEX format: VEX prefix, opcode, modrm/reg, [sib], [disp], [imm]
  private def formatVEX(vexopc: Int, r: Register, am: AddrMode, v: Register, immWidth: Width): Unit = {
    segPrefix(am)
    vexOp(vexopc, r, am, v)
    modrm(r, am, immWidth)
  }

  // VEX without imm
  final def formatVEX(vexopc: Int, r: Register, am: AddrMode, v: Register): Unit = {
    formatVEX(vexopc, r, am, v, ZERO)
  }

  final def formatVEX(vexopc: Int, r: Register, am: AddrMode): Unit = {
    formatVEX(vexopc, r, am, null, ZERO)
  }

  // VEX with imm8
  final def formatVEX_I8(vexopc: Int, r: Register, am: AddrMode, imm8: Int): Unit = {
    formatVEX(vexopc, r, am, null, BYTE)
    immediate(imm8, BYTE)
  }

  // Utils

  private def checkImmBounds(immSize: Width, bounds: Width): Unit = {
    assert(bounds == BYTE || is248(bounds))
    if (immSize > bounds) error(s"$bounds value exceeds bounds")
  }

  private def getImmWidth(immSize: Width, operandSize: Width) = {
    // for 64-bit instructions, imm operand still has 32 bits
    val bounds = if (is8(operandSize)) DWORD else operandSize
    checkImmBounds(immSize, bounds)
    bounds
  }

  final def width(imm: Long, operandSize: Width) = getImmWidth(smallestSize(imm), operandSize)
  final def width(imm: Immediate, operandSize: Width) = getImmWidth(imm.smallestSize, operandSize)

  final def adjustWidth(w: Width) = if (w == WPTR) QWORD else w

  final def width(r: Register): Width = adjustWidth(r.width)

  final def width(am: AddrMode): Width = {
    val w = am.width
    require(w != NO_WIDTH, "Operation size is not specified")
    adjustWidth(am.width)
  }

  final def matchWidth(am: AddrMode, w: Width) = {
    val wa = am.width
    wa == NO_WIDTH || adjustWidth(wa) == adjustWidth(w)
  }

  final def sameWidth(am: AddrMode, w: Width) = width(am) == adjustWidth(w)

  final def width(r1: Register, r2: Register): Width = {
    val w = width(r1)
    require(w == width(r2), "Mismatch in operand size")
    w
  }

  final def width(r: Register, am: AddrMode): Width = {
    val wr = width(r)
    require(matchWidth(am, wr), "Mismatch in operand size")
    wr
  }

  final def width(r1: Register, r2: Register, am: AddrMode): Width = {
    val w = width(r1)
    require(w == width(r2, am), "Mismatch in operand size")
    w
  }
}
