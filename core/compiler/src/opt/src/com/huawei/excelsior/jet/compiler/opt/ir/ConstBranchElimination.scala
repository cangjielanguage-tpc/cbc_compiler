/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.util.graph.ordering.TopSort

/**
 * Eliminate branches with constant condition.
 *
 * @author cypok
 */
trait ConstBranchElimination { self: Universe =>

  /**
   * Eliminate all branches with constant condition in CFG.
   *
   * @return `true` if any branch was eliminated
   */
  def eliminateConstBranches(): Boolean = {
    var eliminated = false
    for (b <- cfg.topSort.order) {
      b.blockEnd match {
        case br: Branch =>
          if (tryEliminateConstBranch(br)) {
            eliminated = true
          }
        case _ =>
      }
    }
    eliminated
  }

  /** Eliminate given Branch if it is constant. */
  def tryEliminateConstBranch(branch: Branch): Boolean = {
    branch.constExit match {
      case Some(out) => replaceByGoto(out); true
      case None => false
    }
  }

}
