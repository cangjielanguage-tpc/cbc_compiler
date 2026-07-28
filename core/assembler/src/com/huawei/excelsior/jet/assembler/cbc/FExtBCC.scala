/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.codeemitter.BranchOp
import xscala.util.MathUtils.{isNBits, isNBitsSigned}

/** List of encodings variants:
  *       - General form: [b.cc][condop][a|b][disp#]
  *         [opcode] is 1-byte long opcode
  *         [condop] is 1-byte long and has following layout:
  *         ||  CC2  | I |  CC1  | N | W ||
  *         || 7   5 | 4 | 3   2 | 1 | 0 ||
  *         [condop] determines type of operands (CC2, CC1), width of operands (W, 0 for 32-bit and 1 for 64),
  *         condition of operation (CC2, CC1, N stands for "negated"), with or without immediate (I, when set then b is imm4).
  *         [a|b] is 1-byte long, where both "a" and "b" occupies 4 bit each, where "b" can be either register number or imm4
  *         according to condop.I value, in later case full imm value is loaded using FExt (see Format Extension chapter).
  *         [disp#], where "#" is in {8, 16, 32}, depending on "#" occupies either 1, 2 or 4 bytes and is signed integral offset
  *         from current instruction.
  *       - Short forms:
  *         - [b.cond.z][rx|condop1][disp8] - jump if (rx cond zero)
  *           [b.cond.z] is 1-byte long opcode.
  *           [rx|condop1] is 1-byte long, where both "rx" and "condop1" occupies 4 bit each.
  *           "condop" for this short form: condop = 0x10 | zeroExt8(condop1, 4), basically condop.CC2 = 0, condop.I = 1 and imm4 is 0.
  *           [disp8] - 1-byte long signed integral offset.
  *
  * Table of "condop" encodings, "#" means there can be any value from {0, 1}:
  * ||  CC2  |  I  |  CC1 |  N  |  W  |             cc               ||
  * ||  000  |  #  |  00  |  #  |  #  | N == 0 ? EQ : NE             ||
  * ||  000  |  #  |  01  |  #  |  #  | N == 0 ? LT : GE,            ||
  * ||  000  |  #  |  10  |  #  |  #  | N == 0 ? ULT : UGE,          ||
  * ||  000  |  #  |  11  |  #  |  1  | N == 0 ? REQ : RNE,          ||
  * ||  001  |  #  |  00  |  #  |  #  | N == 0 ? TESTZ : TESTNZ      ||
  * ||  001  |  #  |  01  |  #  |  #  | reserved                     ||
  * ||  001  |  0  |  1#  |  #  |  #  | reserved                     ||
  * ||  001  |  1  |  1#  |  #  |  #  | N == 0 ? TESTBIT : TESTNBIT  ||
  * ||  010  |  0  |  00  |  #  |  #  | N == 0 ? FEQ : FNE           ||
  * ||  010  |  0  |  01  |  #  |  #  | N == 0 ? FLT : FNLT          ||
  * ||  010  |  0  |  10  |  #  |  #  | N == 0 ? FGE : FNGE          ||
  * ||  010  |  0  |  11  |  #  |  #  | reserved                     ||
  * Any condop value not mentioned above is also reserved.
  *
  * Any condition, not mentioned in table above, can be achieved by swapping arguments (in case I is 0) or else by
  * adding 1 to imm (a <= b => a < b + 1 if b != Int.MaxValue, else should be optimized during compilation).
  *
  * Used for both SCC and BCC.
  */
object FExtBCC {
  // TODO: this code use CodeEmitter BranchOp, which is wrong. Assembler should use own CC.

  import com.huawei.excelsior.jet.codeemitter.BranchOp.*

  // [opcode][condOp][a|b][disp8/16/32]
  // [condOp] is [CC2|IMM|CC1| N | W ]
  //             [7 5| 4 |3 2| 1 | 0 ]

  private def CC2(op: BranchOp): Int = {
    val res = op match {
      case EQ | NE |
           LT | GE | // Integral
           ULT | UGE | // unsigned
           REQ | RNE => 0x0 // reference

      case TESTZ | TESTNZ | // tests
           TESTBIT => 0x1

      case FEQ | FNE | // floating
           FLT | FNLT |
           FGE | FNGE => 0x2

      case _ => notImplemented(s"$op")
    }
    res.ensuring(isNBits(_, 3), s"${res.toHexString}")
  }

  private def CC1(op: BranchOp): Int = {
    val res = op match {
      case EQ | NE | FEQ | FNE | TESTZ | TESTNZ => 0x0
      case LT | GE | FLT | FNLT => 0x1
      case ULT | UGE | FGE | FNGE => 0x2
      case REQ | RNE | TESTBIT => 0x3
      case _ => notImplemented(s"$op")
    }
    res.ensuring(isNBits(_, 2), s"${res.toHexString}")
  }

  private def W(width: Width): Int = (width: @unchecked) match {
    case W32 => 0
    case W64 | WPTR => 1
  }

  private def I(isImm: Boolean): Int = if (isImm) 1 else 0

  private def N(op: BranchOp): (Int, Boolean) = {
    // swap arguments in case this operation isn't supported
    val (isNeg: Boolean, swap: Boolean) = op match {
      case EQ | NE => (op == NE, false)
      case LT | GE => (op == GE, false)
      case GT | LE => (op == LE, true)
      case ULT | UGE => (op == UGE, false)
      case UGT | ULE => (op == ULE, true)
      case REQ | RNE => (op == RNE, false)
      case FEQ | FNE => (op == FNE, false)
      case FLT | FNLT => (op == FNLT, false)
      case FGT | FNGT => (op == FNGT, true)
      case FGE | FNGE => (op == FNGE, false)
      case FLE | FNLE => (op == FNLE, true)
      case TESTZ | TESTNZ => (op == TESTNZ, false)
      case TESTBIT | TESTNBIT => (op == TESTNBIT, false)
    }

    (if (isNeg) 1 else 0, swap)
  }

  def normalize(op: BranchOp): (BranchOp, Boolean, Int) = {
    val (n, swap) = N(op)
    val cop = if (swap) op.swap else op
    (cop, swap, n)
  }

  def condOp(op: BranchOp, isImm: Boolean, width: Width): (Int, Boolean) = {
    assert(!isImm || !op.isFloatingPoint, s"$op")
    val (cop, swap, n) = normalize(op)
    assert(!swap || !isImm, s"Can't swap register and imm4: $op")
    ((CC2(cop) << 5 | I(isImm) << 4 | CC1(cop) << 2 | n << 1 | W(width)) & 0xFF, swap)
  }
}
