/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.EliminateEquivalentPhies
import com.huawei.excelsior.jet.util.{DisjointSet, Worklist}

trait EquivalentPhiesElimination { this: Universe =>

  /** Finds pairs of equivalent phi functions and replaces one by another eliminating duplication.
    *
    * Two phi functions are called equivalent if their corresponding non-SSA variables
    * have the same assignments of the same values at the same points.
    */
  def eliminateEquivalentPhies(): Boolean = {
    if (!env.enabled(EliminateEquivalentPhies)) {
      return false
    }

    // This algorithm has quadratic time, could be optimized if needed.
    var changed = false
    for {
      b <- all[Block]
      Seq(x, y) <- b.phies.toList combinations 2
      if x.isCommitted && y.isCommitted
      if x.tpe == y.tpe // fast-path rejection
    } {
      changed |= tryToReplacePhiByPhi(x, y)
    }
    changed
  }

  private def tryToReplacePhiByPhi(xInitial: Phi, yInitial: Phi): Boolean = {
    // Disjoint set here is used to separate all equivalent phies into disjoint classes.
    // Then each class will be replaced by single representative of that class.
    val phies = DisjointSet.empty[Phi]
    val worklist = Worklist.empty[(Phi, Phi)]

    def union(a: Phi, b: Phi): Unit = {
      if (!phies.equiv(a, b)) {
        phies.union(a, b)
        worklist += ((a, b))
      }
    }

    // Starting with initial equivalent phies.
    union(xInitial, yInitial)

    for {
      (x, y) <- worklist.accumulate
      (a, b) <- x.args.iterator zip y.args
    } { (a, b) match {
      case _ if a == b =>
        // good, no need to go deeper
      case (a: Phi, b: Phi) if a.block == b.block =>
        // potentially new equivalent phies, check them
        union(a, b)
      case _ =>
        // different args, stop the process
        return false
    }}

    bulkReplace {
      // Replace all uses of copy phi by the root one.
      for {
        copy <- phies.iterator
        root = phies.find(copy)
        if root != copy
      } {
        replaceTransitively(copy, root)
      }
    }
    true
  }

}
