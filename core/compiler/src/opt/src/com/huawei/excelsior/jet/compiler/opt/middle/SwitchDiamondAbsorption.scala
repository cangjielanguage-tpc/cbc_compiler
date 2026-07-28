/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.options.NumOption.SwitchDiamondGlueCodeSizeLimit
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.compiler.options.BoolOption.SwitchDiamondAbsorption
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy

import scala.PartialFunction.condOpt
import scala.collection.mutable.ArrayBuffer

/** Absorbs constant-checking half-diamond into immediately following switch:
  *
  * {{{
  *      if (x == C)                 switch (x)
  *          f|  \t                  A   B   C
  *           |   W     ---->       /    |    \
  *           |  /                 X     Y     W
  *       switch (x)                           |
  *       A   B   C                            Z
  *      /    |    \
  *     X     Y     Z
  * }}}
  *
  * Note that the switch block must not have any side-effects, otherwise the transformation is incorrect.
  * However, we allow a limited amount of data-flow glue-code without side-effects in the switch block.
  * This glue code will be duplicated and placed after the absorbed part of the half-diamond.
  *
  * Originally introduced for optimization of Grinder/Chess benchmark.
  *
  * @author liontiger
  */
trait SwitchDiamondAbsorption extends XiTransform with Scales { self: Universe =>

  private lazy val switchBlockSizeLimit = env.valueOf(SwitchDiamondGlueCodeSizeLimit)

  def absorbSwitchDiamonds(): Boolean = {
    if (!XiTransform.enabled(SwitchDiamondAbsorption)) {
      return false
    }

    XiTransform.log.inSession("switch diamond absorption", codeUnit) {

      object IfConst {
        def unapply(branch: If) = IfEq.Commutative.condOpt(branch) {
          case (x, IConst(c), constExit, nonConstExit) => (x, c, constExit, nonConstExit)
        }
      }

      /** Matches following CFG pattern with additional heuristics applied:
        * {{{
        *   if (x == C)
        *        |  \
        *        |   G
        *        |  /
        *    switch (x)
        *    A   B   C
        *   /    |    \
        * }}}
        */
      object AbsorbableSwitchDiamond {
        def unapply(switch: Switch): Option[If] = {
          val switchBlock = switch.block

          def emptySpine = switchBlock.spine forall {
            case _: Marker => true
            case _ => false
          }

          if (!emptySpine || switchBlock.unreachable) {
            return None
          }

          switchBlock.idom match {
            case ifConst @ IfConst(x, c, _, falseExit)
              if falseExit.target == switchBlock &&
                switch.selector == x && (switch.cases contains c) =>
              Some(ifConst)

            case _ => None
          }
        }
      }

      val candidatesBeforeGCM = all[Switch] collect { case x @ AbsorbableSwitchDiamond(_) => x }
      if (candidatesBeforeGCM.isEmpty) {
        return false
      }

      withIncrementalGCM {
        def weight(switch: Switch) = sumBy(Block.collectNodes(switch.block)) {
          case _: Switch | _: Phi => 0.0
          case n => nodeWeight(n)
        }

        val candidatesAfterGCM = candidatesBeforeGCM filter (weight(_) <= switchBlockSizeLimit)
        if (candidatesAfterGCM.isEmpty) {
          return false
        }

        val diamonds = ArrayBuffer.empty[(Switch, If)]
        xiTransformAndPostProcess { scheduler =>
          for (switch @ AbsorbableSwitchDiamond(ifConst @ IfConst(_, c, _, falseExit)) <- candidatesAfterGCM) {
            diamonds += ((switch, ifConst))

            XiTransform.log(s"- absorbed if (x == $c) by switch (x)", ifConst)
            stats.count(StatsKind.XiTransformations, "switch diamonds absorbed", ifConst)

            // The copied switch will be replacing `ifConst` at the top.
            scheduler.extract(switch.block, falseExit.outEdge)
          }
        } { (xi, _) =>
          for ((switch, IfConst(_, c, trueExit, falseExit)) <- diamonds) {
            val copiedExit = xi.copyOf(switch).outCtrl(c)

            // Redirect copied switch exit to `ifConst` true block.
            makeUnreachable(copiedExit.outEdge)
            Block.addEdgeWithTemplate(copiedExit, trueExit.outEdge)

            // Eliminate top branch by forwarding it to copied switch block.
            replaceByGoto(falseExit)

            // Eliminate old switch by forwarding it to constant case.
            replaceByGoto(switch.outCtrl(c))
          }
        }

        true
      }
    }
  }
}
