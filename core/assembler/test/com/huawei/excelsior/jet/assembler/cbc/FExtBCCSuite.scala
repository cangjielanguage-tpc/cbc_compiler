/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.assembler.cbc.FExtBCC
import com.huawei.excelsior.jet.codeemitter.BranchOp
import com.huawei.excelsior.jet.codeemitter.BranchOp.*
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.ClassTag

class FExtBCCSuite extends AnyFunSuite {
  def buildOp(CC2: Int, isImm: Boolean, CC1: Int, isNeg: Boolean, is64: Boolean): Int = {
    val I = if (isImm) 0x1 else 0x0
    val N = if (isNeg) 0x1 else 0x0
    val W = if (is64)  0x1 else 0x0
    (CC2 << 5) | I << 4 | (CC1 << 2) | N << 1 | W
  }

  test("opcodes") {
    for ((op, cc2, isImm, cc1, isNeg, is64) <- Seq[(BranchOp, Int, Boolean, Int, Boolean, Boolean)](
      //op  CC2  isImm CC1  isNeg  is64
      (REQ, 0x0, true, 0x3, false, true),
      (RNE, 0x0, true, 0x3, true,  true),

      (REQ, 0x0, false, 0x3, false, true),
      (RNE, 0x0, false, 0x3, true,  true),

      (EQ,  0x0, true, 0x0, false, true),
      (NE,  0x0, true, 0x0, true,  true),
      (EQ,  0x0, true, 0x0, false, false),
      (NE,  0x0, true, 0x0, true,  false),

      (EQ,  0x0, false, 0x0, false, true),
      (NE,  0x0, false, 0x0, true,  true),
      (EQ,  0x0, false, 0x0, false, false),
      (NE,  0x0, false, 0x0, true,  false),

      (LT,  0x0, true, 0x1, false, true),
      (GE,  0x0, true, 0x1, true,  true),
      (LT,  0x0, true, 0x1, false, false),
      (GE,  0x0, true, 0x1, true,  false),

      (LT,  0x0, false, 0x1, false, true),
      (GE,  0x0, false, 0x1, true,  true),
      (LT,  0x0, false, 0x1, false, false),
      (GE,  0x0, false, 0x1, true,  false),

      (ULT,  0x0, true, 0x2, false, true),
      (UGE,  0x0, true, 0x2, true,  true),
      (ULT,  0x0, true, 0x2, false, false),
      (UGE,  0x0, true, 0x2, true,  false),

      (ULT,  0x0, false, 0x2, false, true),
      (UGE,  0x0, false, 0x2, true,  true),
      (ULT,  0x0, false, 0x2, false, false),
      (UGE,  0x0, false, 0x2, true,  false),

      (FEQ,  0x2, false, 0x0, false, true),
      (FNE,  0x2, false, 0x0, true,  true),
      (FEQ,  0x2, false, 0x0, false, false),
      (FNE,  0x2, false, 0x0, true,  false),

      (FLT,   0x2, false, 0x1, false, true),
      (FNLT,  0x2, false, 0x1, true,  true),
      (FLT,   0x2, false, 0x1, false, false),
      (FNLT,  0x2, false, 0x1, true,  false),

      (FGE,   0x2, false, 0x2, false, true),
      (FNGE,  0x2, false, 0x2, true,  true),
      (FGE,   0x2, false, 0x2, false, false),
      (FNGE,  0x2, false, 0x2, true,  false),

      (TESTZ,   0x1, true, 0x0, false, true),
      (TESTNZ,  0x1, true, 0x0, true,  true),
      (TESTZ,   0x1, true, 0x0, false, false),
      (TESTNZ,  0x1, true, 0x0, true,  false),

      (TESTZ,   0x1, false, 0x0, false, true),
      (TESTNZ,  0x1, false, 0x0, true,  true),
      (TESTZ,   0x1, false, 0x0, false, false),
      (TESTNZ,  0x1, false, 0x0, true,  false),

      (TESTBIT, 0x1, true,  0x3, false, true),
      (TESTBIT, 0x1, true,  0x3, false, false),
      (TESTBIT, 0x1, false, 0x3, false, true),
      (TESTBIT, 0x1, false, 0x3, false, false),
    )) {
      val w = if (is64) W64 else W32
      val CC = cc2 << 2 | cc1
      val (condbyte, swap) = FExtBCC.condOp(op, isImm, w)
      val clue = s"($op, $cc2, $isImm, $cc1, $isNeg, $is64)"
      assertResult(false, clue)(swap)
      assertResult(buildOp(cc2, isImm, cc1, isNeg, is64), clue)(condbyte)
    }
  }

  test("unsupported") {
    for ((op, cc2, isImm, cc1, isNeg, is64) <- Seq[(BranchOp, Int, Boolean, Int, Boolean, Boolean)](
      //op  CC2  isImm CC1  isNeg  is64
      (GT,  0x0, false, 0x1, false, true),
      (LE,  0x0, false, 0x1, true,  true),
      (GT,  0x0, false, 0x1, false, false),
      (LE,  0x0, false, 0x1, true,  false),

      (UGT,  0x0, false, 0x2, false, true),
      (ULE,  0x0, false, 0x2, true,  true),
      (UGT,  0x0, false, 0x2, false, false),
      (ULE,  0x0, false, 0x2, true,  false),

      (FGT,   0x2, false, 0x1, false, true),
      (FNGT,  0x2, false, 0x1, true,  true),
      (FGT,   0x2, false, 0x1, false, false),
      (FNGT,  0x2, false, 0x1, true,  false),

      (FLE,   0x2, false, 0x2, false, true),
      (FNLE,  0x2, false, 0x2, true,  true),
      (FLE,   0x2, false, 0x2, false, false),
      (FNLE,  0x2, false, 0x2, true,  false),
    )) {
      val w = if (is64) W64 else W32
      val (condbyte, swap) = FExtBCC.condOp(op, isImm, w)
      val clue = s"($op, $cc2, $isImm, $cc1, $isNeg, $is64)"
      assertResult(true, clue)(swap)
      assertResult(buildOp(cc2, isImm, cc1, isNeg, is64), clue)(condbyte)
    }
  }

  test("can't swap imm with reg") {
    for ((op, cc2, isImm, cc1, isNeg, is64) <- Seq[(BranchOp, Int, Boolean, Int, Boolean, Boolean)](
      //op  CC2  isImm CC1  isNeg  is64
      (GT,  0x0, true, 0x1, false, true),
      (LE,  0x0, true, 0x1, true,  true),
      (GT,  0x0, true, 0x1, false, false),
      (LE,  0x0, true, 0x1, true,  false),

      (UGT,  0x0, true, 0x2, false, true),
      (ULE,  0x0, true, 0x2, true,  true),
      (UGT,  0x0, true, 0x2, false, false),
      (ULE,  0x0, true, 0x2, true,  false),
    )) {
      val w = if (is64) W64 else W32
      assertThrows(FExtBCC.condOp(op, isImm, w))(ClassTag(classOf[AssertionError]))
    }
  }
}
