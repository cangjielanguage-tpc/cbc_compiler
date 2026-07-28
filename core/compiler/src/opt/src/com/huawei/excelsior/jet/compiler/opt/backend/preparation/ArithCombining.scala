/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.Worklist
import xscala.util.MathUtils

trait ArithCombining { self: Universe with BackEnd =>

  /** After middle there are no integral Subs in IR, they all are converted to Add and Neg operations,
    * where Negs are moved maximum upward for better optimizations of Adds (for more details look at
    * Identities#combinedAddSubNegIdentity).
    *
    * This step transforms arithmetic operations:
    *   (-x) + (-y)   =>  -(x + y)
    *   (-x) + y      =>  y - x
    *   x + (-y)      =>  x - y
    *
    * After this transformation some of operations may become dead and later will be cleaned by DCE.
    */
  private[preparation] def siftNegsDown(): Unit = {
    assert(!identityEnabled) // Otherwise these transformations are worthless

    val wl = Worklist.empty[Add]
    def populate(xs: Iterator[Node]): Unit = xs foreach {
      case add: Add if !(wl contains add) =>
        populate(add.valueArgs)
        wl += add
      case _ =>
    }
    populate(all[Add] filter { _.tpe.isIntegralType }) // populate worklist with candidates in def-before-use order

    for (add <- wl.drain) add match {
      case Add(Neg(x), Neg(y)) => add replaceBy Neg(add.tpe)(Add(x, y))
      case Add(Neg(x), y) => add replaceBy Sub(y, x)
      case Add(x, Neg(y)) => add replaceBy Sub(x, y)
      case _ =>
    }
  }

  private[preparation] def convertAddToLea(): Unit = {
    if (!arithOperationsCombiningInLeaHasImpact) return

    val adds = Worklist.from(all[Add] filter { add => addrOrIntType(add.tpe)})
    for (add @ Lea.ArithPattern(lea) <- adds.drain) {
      add.replaceBy(lea)

      // TODO: investigate an impact of this rematerialization
      lea match {
        case Lea.AnyWithBase(_: ExecEnv, _) => Node.rematerializeCompletely(lea)
        case _ =>
      }
    }
  }

  /** Restructure Lea arguments for better code generation if needed. */
  protected def normalizeLea(): Unit = {}
}
