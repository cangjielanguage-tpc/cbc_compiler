/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.{CbcAssembler, Register}
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl as SOC
import com.huawei.excelsior.jet.assembler.cbc.isa12.SymbolicObjectControl.*
import xscala.util.MathUtils

/** isa-12.txt: chapter 3, "Base space opcode map"
  * 1010cccc : cccc |   -----      | Symbolic/object/control operations (4.6)
  */
trait SymbolicObjectControl { self: Assembler =>
  def genB2xr(op: B2xr, r: Register): Unit = {
    assert(!op.lastRegisterIsZero)
    genB2xr_Impl(op, r)
  }

  def genB2xz(op: B2xr): Unit = {
    assert(op.lastRegisterIsZero)
    genB2xr_Impl(op, IR.IRZ)
  }

  private def genB2xr_Impl(op: B2xr, r: Register): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, r))
  }

  def genB3xrrr(op: SOC.B3xrrr, rd: IR, rx: IR, ry: IR): Unit = {
    assert(!op.lastRegisterIsZero)
    genB3xrrr_Impl(op, rd, rx, ry)
  }

  def genB3xrrz(op: SOC.B3xrrr, rd: IR, rx: IR): Unit = {
    assert(op.lastRegisterIsZero)
    genB3xrrr_Impl(op, rd, rx, IR.IRZ)
  }

  private def genB3xrrr_Impl(op: SOC.B3xrrr, rd: IR, rx: IR, ry: IR): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rd))
    emit.seg.putW8(pack8(rx, ry))
  }

  def genB2xrII(op: SOC.B2xrII, rx: IR, id1: Symbol, id2: Symbol): Unit = {
    assert(!op.lastRegisterIsZero)
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rx))
    emit.id16(id1)
    emit.id16(id2)
  }

  def genB2xzI16I32(op: SOC.B2xrII, i16: Int, id: Symbol): Unit = {
    assert(op.lastRegisterIsZero)
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, IR.IRZ))
    emit.seg.putW16(i16)
    emit.id32(id) // TODO-ISA12: replace with id16
  }

  def genB2xrII(op: SOC.B2xrII, rx: IR, uimm16: Int, id: Symbol): Unit = {
    assert(MathUtils.isNBits(uimm16, 16))
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rx))
    emit.seg.putW16(uimm16)
    emit.id16(id)
  }

  def genB2xrI16(op: SOC.B2xrI, rx: IR, id: Symbol): Unit = {
    assert(!op.lastRegisterIsZero)
    genB2xrI16_Impl(op, rx, id)
  }

  def genB2xzI16(op: SOC.B2xrI, id: Symbol): Unit = {
    assert(op.lastRegisterIsZero)
    genB2xrI16_Impl(op, IR.IRZ, id)
  }

  def genB2xzI16(op: SOC.B2xrI, i16: Int): Unit = {
    assert(op.lastRegisterIsZero)
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, IR.IRZ))
    seg.putW16(i16)
  }

  private def genB2xrI16_Impl(op: SOC.B2xrI, rx: IR, id: Symbol): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rx))
    emit.id16(id)
  }

  def genB2xrI32(op: SOC.B2xrI, rx: IR, id: Symbol): Unit = {
    assert(!op.lastRegisterIsZero)
    genB2xrI32_Impl(op, rx, id)
  }

  def genB2xzI32(op: SOC.B2xrI, id: Symbol): Unit = {
    assert(op.lastRegisterIsZero)
    genB2xrI32_Impl(op, IR.IRZ, id)
  }

  private def genB2xrI32_Impl(op: SOC.B2xrI, rx: IR, id: Symbol): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rx))
    emit.id32(id) // TODO-ISA12: replace with id16
  }

  def genB3xrrrI(op: SOC.B3xrrrI, rd: IR, rx: IR, ry: IR, id: Symbol): Unit = {
    assert(!op.lastRegisterIsZero)
    genB3xrrrI_Impl(op, rd, rx, ry, id)
  }

  def genB3xrrzI(op: SOC.B3xrrrI, rd: IR, rx: IR, id: Symbol): Unit = {
    assert(op.lastRegisterIsZero)
    genB3xrrrI_Impl(op, rd, rx, IR.IRZ, id)
  }

  def genB3xrrzI(op: SOC.B3xrrrI, rd: IR, rx: IR, i16: Int): Unit = {
    assert(op.lastRegisterIsZero)
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rd))
    emit.seg.putW8(pack8(rx, IR.IRZ))
    emit.seg.putW16(i16)
  }

  private def genB3xrrrI_Impl(op: SOC.B3xrrrI, rd: IR, rx: IR, ry: IR, id: Symbol): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rd))
    emit.seg.putW8(pack8(rx, ry))
    emit.id16(id)
  }

  def genB3xrrzII(op: SOC.B3xrrzII, rd: IR, rx: IR, id1: Symbol, id2: Symbol): Unit = {
    emit.seg.putW8(SOC.format(op.opc))
    emit.seg.putW8(pack8(op.opx, rd))
    emit.seg.putW8(pack8(rx, IR.IRZ))
    emit.id16(id1)
    emit.id16(id2)
  }
}

object SymbolicObjectControl {
  inline def FormatBits = 0xA
  inline def FormatFreeBits = 4
  inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

  inline def format(opc: Int): Int = e4(ByteMask) | s4(opc)

  object Jump { // B1_d{8/16/32}
    /** Offset bits length */
    enum K {
      case K8
      case K16
      case K32
      def opc: Int = ordinal + 1
    }

    inline def format(k: K): Int = SymbolicObjectControl.format(s2(k.opc))
  }

  trait B2xr {
    def opx: Int
    def opc: Int
    def lastRegisterIsZero: Boolean
  }

  object B2xr {
    enum Opc0100(val opx: Int, val lastRegisterIsZero: Boolean = false) extends B2xr {
      override def opc: Int = 0x4 // 0100

      case CmpChaTest     extends Opc0100(0x4) // 0100
      case CmpNotChaTest  extends Opc0100(0x5) // 0101
      case Throw          extends Opc0100(0x6) // 0110
      case Catch          extends Opc0100(0x7) // 0111
      case Ret32          extends Opc0100(0x8) // 1000
      case Ret64          extends Opc0100(0x9) // 1001
      case Fret32         extends Opc0100(0xA) // 1010
      case Fret64         extends Opc0100(0xB) // 1011
      case CheckDivZero32 extends Opc0100(0xC) // 1100
      case CheckDivZero64 extends Opc0100(0xD) // 1101
      case CheckNull      extends Opc0100(0xE) // 1110
    }

    enum Opc0101(val opx: Int, val lastRegisterIsZero: Boolean = true) extends B2xr {
      override def opc: Int = 0x5 // 0101

      case Gcpoint        extends Opc0101(0x0)        // 0000
      case Covinc         extends Opc0101(0x1)        // 0001
      case BeginUnmovable extends Opc0101(0x2, false) // 0010
      case EndUnmovable   extends Opc0101(0x3, false) // 0011
    }
  }

  enum B3xrrr(val opx: Int, val lastRegisterIsZero: Boolean = true) {
    inline def opc: Int = 0x6 // 0110

    case EopPack         extends B3xrrr(0x0, false) // 0000
    case EopToPlain      extends B3xrrr(0x1)        // 0001
    case EopGetRich      extends B3xrrr(0x2)        // 0010
    case Evacuate        extends B3xrrr(0x3)        // 0011
    case LoadTypeInfoObj extends B3xrrr(0x4)        // 0100
  }

  enum B2xrII(val opx: Int, val lastRegisterIsZero: Boolean = false) {
    inline def opc: Int = 0x7 // 0111

    case CallGtdSig      extends B2xrII(0x0)       // 0000
    case CallGtdFtc      extends B2xrII(0x1)       // 0001
    case CallAtcSig      extends B2xrII(0x2)       // 0010
    case CallAtcFtc      extends B2xrII(0x3)       // 0011
    case CallConstraint  extends B2xrII(0x4)       // 0100
    case CallInterf      extends B2xrII(0x5)       // 0101
    case EopRichConst    extends B2xrII(0x6)       // 0110
    case InitStrConst    extends B2xrII(0x7, true) // 0111

    // NOTICE: this is unspecified operation
    case CallInterfConst extends B2xrII(0xF) // 1111
  }

  trait B2xrI {
    def opx: Int
    def opc: Int
    def lastRegisterIsZero: Boolean
  }

  object B2xrI {
    enum Opc1000(val opx: Int, val lastRegisterIsZero: Boolean = false) extends B2xrI {
      override def opc: Int = 0x8 // 1000

      case CallDirect      extends Opc1000(0x0) // 0000
      case CallVirt        extends Opc1000(0x1) // 0001
      case CallInterfPlain extends Opc1000(0x2) // 0010
      case CallInterfRich  extends Opc1000(0x3) // 0011
      case CallInterfEop   extends Opc1000(0x4) // 0100
      case CallIndirect    extends Opc1000(0x5) // 0101
    }

    enum Opc1001(val opx: Int, val lastRegisterIsZero: Boolean = false) extends B2xrI {
      override def opc: Int = 0x9 // 1001

      case BranchRichIof    extends Opc1001(0x0) // 0000
      case BranchNotRichIof extends Opc1001(0x1) // 0001
      case BranchRichEop    extends Opc1001(0x2) // 0010
      case BranchNotRichEop extends Opc1001(0x3) // 0011
      case BranchChaTest    extends Opc1001(0x4) // 0100
      case BranchNotChaTest extends Opc1001(0x5) // 0101
      case AdrOhm           extends Opc1001(0x6) // 0110
      case AdrData          extends Opc1001(0x7) // 0111
      case CfuncPtr         extends Opc1001(0x8) // 1000
      case CfuncWrap        extends Opc1001(0x9) // 1001
      case InitStrConst     extends Opc1001(0xA) // 1010
      case JavaStrConst     extends Opc1001(0xB) // 1011
      case ArrfillData      extends Opc1001(0xC) // 1100
      case Singleton        extends Opc1001(0xD) // 1101
      case Checkcast        extends Opc1001(0xE) // 1110
    }

    enum Opc1010(val opx: Int, val lastRegisterIsZero: Boolean = true) extends B2xrI {
      override def opc: Int = 0xA // 1010

      case PkgInit            extends Opc1010(0x0) // 0000
      case PkgInitCheck       extends Opc1010(0x1) // 0001
      case JavaClinit         extends Opc1010(0x2) // 0010
      case InitObj            extends Opc1010(0x3) // 0011
      case LoadTypeInfoSig    extends Opc1010(0x4, false) // 0100
      case LoadTypeInfoFTC    extends Opc1010(0x5, false) // 0101
      case TypeVarIsRef       extends Opc1010(0x6, false) // 0110

      case AliveRef           extends Opc1010(0x8) // 1000
      case AliveUnmovable     extends Opc1010(0x9) // 1001
      case AliveRefDiff       extends Opc1010(0xA) // 1010
      case AliveUnmovableDiff extends Opc1010(0xB) // 1011
      case AliveRefCheck      extends Opc1010(0xC) // 1100
      case Zerorefs           extends Opc1010(0xD) // 1101
    }

    enum Opc1011(val opx: Int, val lastRegisterIsZero: Boolean = false) extends B2xrI {
      override def opc: Int = 0xB // 1011

      case Newobj    extends Opc1011(0x0) // 0000
      case NewobjVst extends Opc1011(0x1) // 0001
    }

    private[SymbolicObjectControl] def secondByte(opx: Int, r: IR): Int = Assembler.pack8(opx, r)
  }

  trait B3xrrrI {
    def opx: Int
    def opc: Int
    def lastRegisterIsZero: Boolean
  }

  object B3xrrrI {
    enum Opc1100(val opx: Int, val lastRegisterIsZero: Boolean = true) extends B3xrrrI {
      override def opc: Int = 0xC // 1100

      case Newarr       extends Opc1100(0x0)        // 0000
      case NewarrVst    extends Opc1100(0x1)        // 0001
      case NewarrNoInit extends Opc1100(0x2)        // 0010
      case Newarrfill   extends Opc1100(0x3, false) // 0011
    }

    enum Opc1101(val opx: Int, val lastRegisterIsZero: Boolean = true) extends B3xrrrI {
      override def opc: Int = 0xD // 1101

      case Iof           extends Opc1101(0x2)        // 0010
      case RichIof       extends Opc1101(0x3)        // 0011
      case WeakCast      extends Opc1101(0x4)        // 0100
      case RichCheckcast extends Opc1101(0x5)        // 0101
      case EopMake       extends Opc1101(0x6)        // 0110
      case EopPack       extends Opc1101(0x7)        // 0111
      case CopyVst       extends Opc1101(0x8, false) // 1000
    }
  }

  enum B3xrrzII(val opx: Int) {
    inline def opc: Int = 0xE // 1110

    case EopPack         extends B3xrrzII(0x2) // 0010
    case NewarrfillConst extends B3xrrzII(0x3) // 0011
    case CopyVst         extends B3xrrzII(0x4) // 0100
  }
}
