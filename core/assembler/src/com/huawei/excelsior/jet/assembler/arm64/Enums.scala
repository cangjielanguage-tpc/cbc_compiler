/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.Width.W16
import com.huawei.excelsior.jet.assembler.Width.W32
import com.huawei.excelsior.jet.assembler.Width.W8
import com.huawei.excelsior.jet.assembler.Width

/** Collection of various enumerations used in ARM64 operations. */
object Enums {

  enum AddSubOp {
    case ADD, ADDS, SUB, SUBS

    def op = ordinal >> 1
    def S = ordinal & 1
  }

  enum LogicalOp {
    case AND, ORR, EOR, ANDS

    def opc = ordinal

    def name(N: Boolean) = this match {
      case _ if !N => toString
      case AND => "BIC"
      case ORR => "ORN"
      case EOR => "EON"
      case ANDS => "BICS"
    }
  }

  enum SelectOp {
    case CSEL, CSINV, CSINC, CSNEG

    def op = ordinal & 1
    def op2 = ordinal >> 1
  }

  enum MemOp {
    case LD, LDSX, ST, PRFM

    def name(loc: String, width: Width) = {
      val suffix = if (width == W8) "B"
        else if (width == W16) "H"
        else if (width == W32 && this == LDSX) "W" else ""

      this match {
        case LD   => s"LD$loc$suffix"
        case LDSX => s"LD${loc}S$suffix"
        case ST   => s"ST$loc$suffix"
        case _    => toString
      }
    }
  }

  enum MemOpX {
    case LDXR, STXR,
         LDAXR, STLXR,
         LDAR, STLR

    def exclusive = ordinal < 4
    def ordered = ordinal >= 2
    def isLoad = (ordinal & 1) == 0
  }

  enum FP2Op {
    case FMUL, FDIV,
         FADD, FSUB,
         FMAX, FMIN,
         FMAXNM, FMINNM,
         FNMUL

    def opcode = ordinal
  }
}
