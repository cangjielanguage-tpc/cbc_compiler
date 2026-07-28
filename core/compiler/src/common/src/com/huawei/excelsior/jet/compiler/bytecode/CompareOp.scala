/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.codeemitter.BranchOp

/** Compare-and-conditional-branch bytecode operations
  *
  * @author paul
  */
enum CompareOp {
  case EQ, NE, GE, GT, LT, LE

  def toBranchOp = this match {
    case EQ => BranchOp.EQ
    case NE => BranchOp.NE
    case GE => BranchOp.GE
    case GT => BranchOp.GT
    case LT => BranchOp.LT
    case LE => BranchOp.LE
  }
}
