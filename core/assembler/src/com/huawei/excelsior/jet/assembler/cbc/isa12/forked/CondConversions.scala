/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc.isa12.forked

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.{CC, Width}
import com.huawei.excelsior.jet.assembler.cbc.isa12.Assembler.Width.{W32, W64}
import com.huawei.excelsior.jet.codeemitter.BranchOp
import com.huawei.excelsior.jet.codeemitter.BranchOp.{FGT, FLE, FNGT, FNLE, GT, LE, UGT, ULE}
import xscala.util.MathUtils

object CondConversions {
  /**
    * Encodes [[BranchOp]] for bcc/scc as ISA12 constant [[CC]].
    * To do that, operands could be swapped.
    */
  def normalize(op: BranchOp): (CC, Boolean) = {
    CC.tryFrom(op) match {
      case Some(cc) => (cc, false)
      case None => (CC.from(op.swap), true)
    }
  }

  /**
    * Encodes [[BranchOp]] for bcci/scci as ISA12 constant [[CC]].
    * To do that, immediate could be adjusted.
    */
  def normalizeImm(op: BranchOp, c: Long, width: Width): (CC, Long) = {
    CC.tryFrom(op) match {
      case Some(cc) => (cc, c)
      case _ =>
        def incrementUnsigned(c: Long): Long = {
          assert(c != MathUtils.rangeMask64(0, width.nbits - 1), s"$c $width")
          width match {
            case W32 => c.toInt + 1
            case W64 => c + 1
            case w => notImplemented(s"feel free to implement: $w")
          }
        }

        def incrementSigned(c: Long): Long = {
          assert(c != MathUtils.rangeMask64(0, width.nbits - 2), s"$c $width") // c != MaxSignedValue(width)
          c + 1
        }

        import BranchOp.*
        (op: @unchecked) match {
          case LE => (CC.LT, incrementSigned(c))
          case GT => (CC.GE, incrementSigned(c))
          case ULE => (CC.ULT, incrementUnsigned(c))
          case UGT => (CC.UGE, incrementUnsigned(c))
        }
    }
  }

}