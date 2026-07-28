/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.SwitchAggregation
import com.huawei.excelsior.jet.compiler.options.NumOption.SwitchDiamondGlueCodeSizeLimit
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.util.ScalaCollections.*

/** Aggregates consecutive branches with the same integer selector into switch with corresponding cases:
  *
  * {{{
  *      if (x == A)
  *          f|  \t
  *           |   X                switch (x)
  *           |                    A  B  C  (default)
  *      if (x == B)   ---->      /   |   \     \
  *          f|  \t              X    Y    Z     W
  *           |   Y
  *           |
  *      if (x == C)
  *           |  \t
  *           W   Z
  * }}}
  *
  * Currently only empty branch blocks are supported (without any glue code or controlled node dependencies).
  * TODO: extend this optimization to support limited glue code using xi-transformations
  *
  * Originally introduced for optimization of Grinder/Chess benchmark in Cangjie.
  *
  * @author liontiger
  */
trait SwitchAggregation { self: Universe =>

  private def log = Log(Kind.SwitchAggregation)

  def aggregateSwitches(): Boolean = {
    if (!env.enabled(SwitchAggregation) || currentPhase >= CompilerPhase.Lowering) {
      return false
    }

    log.inSession("switch aggregation", codeUnit) {

      case class IfCandidate(branch: If, selector: Node, num: Int, caseExit: If.Exit, nonCaseExit: If.Exit) {
        def aggregatesWith(that: IfCandidate): Boolean =
          // `that` branch directly follows non-case exit
          this.nonCaseExit.target == that.branch.block &&
            // Spine of `that` branch is empty and no controlled or memory nodes depend on its block
            uniqueValue(that.branch.block.uses ++ that.branch.block.paramNodes).contains(that.branch)
      }

      object Candidate {
        def unapply(branch: If): Option[IfCandidate] = IfEq.Commutative.condOpt(branch) {
          case (x, IConst(c), constExit, nonConstExit) if branch.block.reachable =>
            IfCandidate(branch, x, c, constExit, nonConstExit)
        }
      }

      case class SwitchAggregate(branches: Seq[IfCandidate]) {
        def top = branches.head
        def bot = branches.last
        val cases = branches.map(_.num)
      }

      def collectAggregates(branches: Seq[IfCandidate]): Seq[SwitchAggregate] = {
        val topDownBranches = branches sortBy { b => cfg.dominators.depth(b.branch.block) }
        aggregate(topDownBranches)(_ aggregatesWith _).map(SwitchAggregate.apply)
      }

      val candidates = all[If] collect { case Candidate(c) => (c.selector, c) }
      if (candidates.isEmpty) {
        return false
      }

      val candidatesBySelector = toMultiMap(candidates)

      var changed = false
      for {
        (selector, branches) <- candidatesBySelector
        if branches.size > 1
        aggregate <- collectAggregates(branches)
        if aggregate.branches.size > 1
        if collectDuplicates(aggregate.cases).isEmpty
      } {
        val top = aggregate.top.branch
        val cases = aggregate.cases
        val switch = withPos(top) { Switch(cases)(top.inCtrl, top.inMemory, selector) }

        Block.addEdgeWithTemplate(switch.defaultExit, aggregate.bot.nonCaseExit.outEdge)
        for ((exit, branch) <- switch.caseExits zip aggregate.branches) {
          Block.addEdgeWithTemplate(exit, branch.caseExit.outEdge)
        }

        top.makeUsesUnreachable()
        decommit(top)

        log(s"- aggregated ${switch.name}", top)

        changed = true
      }

      changed
    }
  }
}
