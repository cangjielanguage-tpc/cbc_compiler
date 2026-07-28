/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import xscala.util.MathUtils

trait FlagProducersPreparation { self: Universe with BackEnd =>

  /** Rematerialize all flag producers and group them with their single uses. Thus we do not need to track flag
    * register on platforms where it exists (x86, ARM) and can generate efficient instructions for platforms with
    * `branchIf` concept (ARM, CBC).
    */
  protected def rematerializeFlagProducers(): Unit = {
    for (fp <- all[FlagProducer] if fp.hasValueUses && !fp.hasGroup) {
      fp match {
        case fp: FloatingNode => Node.rematerializeCompletely(fp) foreach { fp =>
          fp.singleValueUse match {
            case use: CondVal => use.attachToGroup(fp, Group.AttachReason.COND_VAL_RESULT)
            case use: If      => fp.attachToGroup(use, Group.AttachReason.COND_BRANCH_ARG)
          }
        }
        case _ => shouldNotReachHere()
      }
    }
  }

  protected def convertAndToTest(allowConds: Set[Condition]): Unit = {
    for (cmp <- all[Cmp].toList) {
      (cmp.l, cmp.r) match {
        // convert Cmp(And(x, y), 0) to node Test(x, y) if `And` has no uses except `Cmp`
        case (and: And, IntegralConst(0)) if and.uses.size == 1 && allowConds.contains(cmp.op) =>
          replaceTransitively(cmp, Test(cmp.keyType, cmp.op)(and.l, and.r))

        case (bfx @ BitFieldExtract(offset, size, false, arg), IntegralConst(0)) if bfx.tpe == arg.tpe && addrOrIntType(bfx.tpe) &&
          bfx.isGroupRoot && bfx.uses.size == 1 && allowConds.contains(cmp.op) =>
          assert(offset > 0)
          replaceTransitively(cmp, Test(cmp.keyType, cmp.op)(arg, IntegralConst(cmp.keyType)(MathUtils.rightNBits64(size) << offset)))

        case _ =>
      }
    }
  }

  protected def recombineFlagProducers(): Unit
}
