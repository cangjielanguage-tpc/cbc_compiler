/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12

import com.huawei.excelsior.jet.assembler.cbc.Bits
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.*
import com.huawei.excelsior.jet.assembler.cbc.isa12.ConditionalBranch.B2xri16dM.BranchIf.ByteMask

object ConditionalBranch {
  object B2rrd8 { // op8_rx_ry_d8
    inline def FormatBits: Int = 0x5
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    def format(cc: CC, width: Width): Int = ByteMask | s4(cc.opc(width))
  }

  object B3xrrdT { // op12_rx_ry_t4_dT
    inline def FormatBits: Int = 0xC
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    object BranchIf {
      def format(t: T, page: Int): Int = ByteMask | p(t.opc, freeBits = 1) | s1(page)
      def secondByte(ccBits: Int, l: Int): Int = pack8(s4(ccBits), l)
      def thirdByte(r: Int, imm4: Int): Int = pack8(r, imm4)
    }

    object BranchIfContinue {
      def format(page: Int): Int = ByteMask | p(T.T0.opc, freeBits = 1) | s1(page)
      def secondByte(ccBits4: Int, l4: Int): Int = pack8(s4(ccBits4), s4(l4))
      def thirdByte(r4: Int): Int = pack8(s4(r4), 0)
    }

    enum T {
      case T8
      case T16
      case T0

      def opc = ordinal
    }
  }

  object B2xri8d8 { // op12_rx_i8_d8
    inline def FormatBits = 0x33
    inline def FormatFreeBits = 1
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    def format(page: Int): Int = ByteMask | s1(page)
    def secondByte(cc: CC, width: Width, r: IR): Int = p(s4(r.idx), freeBits = 4) | cc.opcWithoutPage(width)
  }

  object B2xri16dM { // op12 suffix
    inline def FormatBits: Int = 0x6
    inline def FormatFreeBits: Int = 4
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    private def format(low4: Int) = ByteMask | s4(low4)

    object BranchIf { // op12_rx_imm16_dM
      inline def FormatBits: Int = 0x2
      inline def FormatFreeBits: Int = 2
      inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

      def format(page: Int): Int = B2xri16dM.format(e2(ByteMask) | p(s1(M.M16.opc), freeBits = 1) | s1(page))
      def secondByte(cc: CC, width: Width, r: IR): Int = p(s4(r.idx), freeBits = 4) | cc.opcWithoutPage(width)
    }

    object BranchIfContinue {
      inline def FormatBits: Int = 0x2
      inline def FormatFreeBits: Int = 2
      inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

      def format(page: Int): Int = B2xri16dM.format(e2(ByteMask) | p(s1(M.M0.opc), freeBits = 1) | s1(page))
      def secondByte(cc: CC, width: Width, r: IR): Int = p(s4(r.idx), freeBits = 4) | cc.opcWithoutPage(width)
    }

    object BranchTT {
      inline def FormatBits: Int = 0x3
      inline def FormatFreeBits: Int = 2
      inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

      def format(tt: TT): Int = B2xri16dM.format(ByteMask | p(s1(M.M16.opc), freeBits = 1) | s1(tt.page))
      def secondByte(tt: TT, isNegated: Boolean, r: IR): Int = p(s4(r.idx), freeBits = 4) | s4(tt.opc(isNegated))
    }

    object BranchTTContinue {
      inline def FormatBits: Int = 0x3
      inline def FormatFreeBits: Int = 2
      inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

      def format(tt: TT): Int = B2xri16dM.format(ByteMask | p(s1(M.M0.opc), freeBits = 1) | s1(tt.page))
      def secondByte(tt: TT, isNegated: Boolean, r: IR): Int = p(s4(r.idx), freeBits = 4) | s4(tt.opc(isNegated))
    }

    /*
    4.4.5. Type test operations & instructions
      opc  |  type test op     |  i16       | remarks
    ----------------------------------------------------------------
     0000N | [not.]open.cone   | @sig       | open cone test
     0001N | [not.]closed.cone | @sig       | closed cone test
     0010N | [not.]point.test  | @sig       | point type test
     0011N | [not.]iof         | @sig       | is instance of class/interface/java array
     0100N | [not.]level.test  | level: u16 | type level test
     01... |    (reserved)     |            |
     1.... |    (patched / reserved)        |
     */
    enum TT {
      case OpenCone
      case ClosedCone
      case PointTest
      case Iof
      case LevelTest
      def page: Int = 0
      def opc(isNegated: Boolean): Int = p(s3(ordinal), freeBits = 1) | (if (isNegated) 1 else 0)
    }

    enum M {
      case M0
      case M16
      def opc = ordinal
    }
  }

  object C1dM { // op8_imm{8, 16, 32}
    inline def FormatBits: Int = 0x17
    inline def FormatFreeBits: Int = 3
    inline def ByteMask: Int = p(FormatBits, FormatFreeBits)

    def format(negated: Boolean, m: M): Int = ByteMask | p(s2(m.opc), 1) | s1(if negated then 1 else 0)

    enum M {
      case M8
      case M16
      case M32
      def opc: Int = ordinal
    }

    def bcc(negated: Boolean, m: M): Int = format(negated, m)
  }
}
