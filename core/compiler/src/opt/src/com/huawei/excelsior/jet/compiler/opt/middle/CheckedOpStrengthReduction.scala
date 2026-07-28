/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import scala.PartialFunction.cond
import scala.annotation.nowarn

/** This optimization replaces spinal CheckedOp, which acts as increment (or decrement),
  * with unchecked operations, if CheckedOp is under condition, which guarantees it can't overflow at run-time.
  *
  * General idea:
  * {{{
  * if (a < b) {
  *   // increment of `a` can't overflow here
  *   a += 1
  *   // decrement of `b` can't overflow here
  *   b -= 1
  * }
  * }}}
  *
  * Other cases:
  * {{{
  * if (a <= b) {
  *   // can't guarantee no-overflow here
  * } else {
  *   // a > b
  *   a -= 1
  *   b += 1
  * }
  * }}}
  *
  * {{{
  * if (a > b) {
  *   a -= 1
  *   b += 1
  * }
  * }}}
  *
  * {{{
  * if (a >= b) {
  *   // ...
  * } else {
  *   a += 1
  *   b -= 1
  * }
  * }}}
  *
  * This optimization is workaround for JET-15017, must be replaced once ContextTypes is able to optimize such cases.
  *
  * @author julian
  */
@nowarn("msg=match may not be exhaustive")
trait CheckedOpStrengthReduction { self: Universe =>
  import CheckedOp.Kind.{ADD, SUB}

  def optimizeRedundantCheckedOp(): Boolean = {
    var changed = false

    def cmpAllowsToReplaceCheckedOp(cmp: Cmp, point: ControlNode, index: Node, isAdd: Boolean): Boolean = {
      def hasCheck(cond: Boolean) = collect[If](cmp.valueUses) exists {
        _.exit(cond) dominates point
      }

      val Cmp(cc, l, r) = cmp

      val swapOp = (l, r) match {
        case (`index`, _) => !isAdd
        case (_, `index`) => isAdd
      }

      (cc, swapOp) match {
        case (Condition.LT, false) => hasCheck(true)
        case (Condition.GE, false) => hasCheck(false)
        case (Condition.GT, true) => hasCheck(true)
        case (Condition.LE, true) => hasCheck(false)
        case _ => false
      }
    }

    for (op @ CheckedOp(ADD | SUB, index, IntegralConst(1)) <- all[CheckedOp]) {
      val isOpGrowLimited = index.valueUses.exists {
        case cmp: Cmp => cmpAllowsToReplaceCheckedOp(cmp, op, index, op.kind == ADD)
        case _ => false
      }

      if (isOpGrowLimited) {
        CheckedOp.replaceWithUncheckedCopy(op)
        changed = true
      }
    }

    changed
  }
}
