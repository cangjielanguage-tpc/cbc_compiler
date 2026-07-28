/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere

/** Enumerates supported branch operations. */
enum BranchOp {
  // TODO: cleanup during global CodeEmitter refactoring
  //  This enum is polluted with CBC-specific condition codes (which is fun because we do not have CodeEmitterCBC)
  //  and used in API between CodeGeneratorCBC and CBC Assembler which is wrong.

  // TODO: separate to BranchOp:CMP/TEST and BranchCC:EQ/NE/...
  //  Currently this is a mess of this two concepts and CodeGenerator is complicated because of it.

  case
    // basic
    EQ, NE, GE, GT, LT, LE,

    // reference
    REQ, RNE,

    // unsigned
    UGE, UGT, ULT, ULE,

    // test
    TESTZ, TESTNZ, TESTBIT, TESTNBIT,

    // floating-point
    FEQ, FNE,
    FLT, FNLT,
    FGT, FNGT, // achieved by swapping arguments of FLT/FNLT
    FGE, FNGE,
    FLE, FNLE  // achieved by swapping arguments of FGE/FNGE

  /** Returns whether this operation is test branch, i.e. TESTZ or TESTNZ. */
  def isTest = this == TESTZ || this == TESTNZ

  def isTestBit = this == TESTBIT || this == TESTNBIT

  /** Return the branch operation to be used for swapped arguments, satisfying following property:
    * {{{
    *   For each X and Y: (X op Y) == (Y op.swap() X)
    * }}}
    */
  def swap = this match {
    case EQ => EQ
    case NE => NE
    case GE => LE
    case GT => LT
    case LT => GT
    case LE => GE

    case REQ => REQ
    case RNE => RNE

    case UGE => ULE
    case UGT => ULT
    case ULT => UGT
    case ULE => UGE

    case TESTZ | TESTNZ => this
    case TESTBIT | TESTNBIT => shouldNotReachHere(s"$this is not commutative")

    case FEQ => FEQ
    case FNE => FNE
    case FLT => FGT
    case FGT => FLT
    case FNLT => FNGT
    case FNGT => FNLT
    case FGE => FLE
    case FLE => FGE
    case FNGE => FNLE
    case FNLE => FNGE
  }

  def isFloatingPoint = this match {
    case FEQ | FNE | FLT | FNLT | FGE | FNGE | FGT | FNGT | FLE | FNLE => true
    case _ => false
  }

  def isIntegral = this match {
    case EQ | NE |
         LT | GE | GT | LE |
         ULT | UGE | UGT | ULE |
         TESTZ | TESTNZ | TESTBIT | TESTNBIT => true
    case _ => false
  }

  def isReference = this match {
    case REQ | RNE => true
    case _ => false
  }

  def isSigned = this match {
    case ULT | ULE | UGT | UGE => false
    case _ => true
  }
}
