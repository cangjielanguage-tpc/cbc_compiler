/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.Worklist

import scala.PartialFunction.cond

/** This transformation attempts to reconstruct boolean types from integers.
  *
  * This is done by lifting int-to-boolean conversion nodes over ''guaranteed boolean'' phies,
  * thus transforming them into boolean ones.
  *
  * Phi is considered ''guaranteed boolean'' if its arguments are either
  *   - constants 0 or 1, or
  *   - boolean-to-int conversions, or
  *   - ''guaranteed boolean'' phies.
  *
  * @author liontiger
  */
trait BooleanReconstruction { self: Universe =>

  def reconstructBooleanTypes(): Boolean = {
    var changed = false

    for (cmp <- all[Cmp]) cmp match {
      case ZeroComparison(phi: Phi) =>

        // TODO: support more cases based on value-range analysis when it is implemented
        def guaranteedBoolean(n: Node) = cond(n) {
          case IConst(0 | 1) => true
          case _: CondVal => true
          case _: Phi => true
        }

        def collectGuaranteedClojure(worklist: Worklist[Phi]): Boolean = {
          for (phi <- worklist.accumulate) {
            // Note that here we traverse only through phies, and stop at first non-phi arg.
            phi.args foreach {
              case x: Phi => worklist += x
              case x => if (!guaranteedBoolean(x)) return false
            }
          }
          true
        }

        val phiClojure = Worklist(phi)
        if (collectGuaranteedClojure(phiClojure)) {
          bulkReplace {
            // Transform collected int phies into boolean ones.
            for (x <- phiClojure.iterator if x.isCommitted) {
              assert(x.tpe == IntType)
              replaceTransitively(x, CondVal(Phi(ConditionType)(x.block +: (x.argsSeq map (NonZero(_))): _*)))
            }
          }
          changed = true
        }

      case _ =>
    }

    changed
  }
}
