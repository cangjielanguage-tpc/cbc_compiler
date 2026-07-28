/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.options.BoolOption.DiamondMerge
import com.huawei.excelsior.jet.compiler.options.NumOption.DiamondCrossroadBodySizeLimit
import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

trait DiamondMerge extends XiTransform with Scales { self: Universe =>

  private lazy val crossroadBodySizeLimit = env.valueOf(DiamondCrossroadBodySizeLimit)

  /** Dominated diamonds optimization merges two subsequent diamonds with compatible tests.
    *
    * The goal is to remove unnecessary branches between warm paths by merging them into a single one
    * without blowing up code size.
    */
  def optimizeDiamonds(): Boolean = {
    if (!XiTransform.enabled(DiamondMerge)) {
      return false
    }

    /** Matches following CFG pattern:
      * {{{
      *  diamond
      *    / \
      *  ..   ..
      *    \ /
      * crossroad
      *    / \
      *  ..   ..
      * }}}
      */
    object Crossroad {
      def unapply(crossroad: If): Option[(Node, If, Seq[Edge])] = (crossroad, crossroad.block) match {
        case (If(crossroadTest), crossroadBlock: BBlock) if crossroadBlock.reachable =>
          crossroadBlock.idom match {
            case diamond: If =>
              def dominatedEdge(e: Edge, exit: If.Exit) = exit dominates e.source.asInstanceOf[ControlNode]
              val (trueEdges, falseEdges) = crossroadBlock.inEdges.toIndexedSeq.partition(dominatedEdge(_, diamond.trueExit))

              if (falseEdges.forall(dominatedEdge(_, diamond.falseExit))) {
                Some(crossroadTest, diamond, trueEdges)
              } else {
                None
              }

            case _ => None
          }
        case _ => None
      }
    }

    object DiamondMergeCandidate {
      def unapply(crossroad: If): Option[(If, PredicateConstructor, Seq[Edge], If.Exit)] = crossroad match {
        case Crossroad(crossroadTest, diamond @ If(diamondTest), trueEdges) =>
          import PredicateConstructor._
          (crossroadTest, diamondTest) match {
            case (_, _) if crossroadTest == diamondTest =>
              Some(diamond, atom(diamondTest), trueEdges, crossroad.trueExit)

            case (Not(`diamondTest`), _) | (_, Not(`crossroadTest`)) =>
              Some(diamond, atom(diamondTest), trueEdges, crossroad.falseExit)

            case (TauTest(crossroadGuard, crossroadInfo, crossroadObj), TauTest(diamondGuard, diamondInfo, diamondObj)) =>

              def objCheck = cond(crossroadObj) {
                case `diamondObj` => true
                case phi: Phi if phi.block == crossroad.block =>
                  // Optimization of chain calls, e.g. x.foo().bar().
                  ScalaCollections.uniqueValue(trueEdges.map(phi.phiArg)) contains diamondObj
              }

              val diamondObjType = nodeTypeAt(diamondObj, diamond)
              diamondGuard.intersectWith(crossroadGuard, diamondObjType) collect {
                case guard if objCheck =>
                  (diamond, tauTestUnchecked(guard, TauInfo.Unknown, diamondObj), trueEdges, crossroad.trueExit)
              }

            case _ => None
          }
        case _ => None
      }
    }

    val candidatesBeforeGCM = all[If] collect { case x @ DiamondMergeCandidate(_, _, _, _) => x }
    if (candidatesBeforeGCM.isEmpty) {
      return false
    }

    withIncrementalGCM {
      // Weighting heuristic can be applied only after GCM is done.
      def weight(crossroad: If): Double = {
        ScalaCollections.sumBy(Block.collectNodes(crossroad.block)) {
          case _: TauTest | _: Branch => 0.0
          case n => nodeWeight(n)
        }
      }
      val candidatesAfterGCM = candidatesBeforeGCM filter (weight(_) <= crossroadBodySizeLimit)
      if (candidatesAfterGCM.isEmpty) {
        return false
      }

      XiTransform.log.inSession("diamonds merge", codeUnit) {

        // Tau tests must be updated in the reversed top-sort order.
        // Otherwise a sequence of mergeable diamonds may end up with the wrong tau test at the top
        // (see "smart merge" unit-test).
        val sortedCandidates = candidatesAfterGCM.toArray.sortWith((x, y) => cfg.topSort.reverse.lt(x.block, y.block))

        val mergeExits = ArrayBuffer.empty[If.Exit]
        xiTransformAndPostProcess { scheduler =>
          for (crossroad @ DiamondMergeCandidate(diamond, pred, mergeEdges, mergeExit) <- sortedCandidates) {
            // JET-13980: optimization doesn't work correctly when it marks two diamonds,
            // connected by a critical edge. We don't need check pred's edges because the candidates
            // are sorted in reverse order.
            for {
              edge <- crossroad.succBlockEdges.toSeq
              if scheduler.shouldCopy(edge.target.block)
            } {
              splitCriticalEdge(edge)
            }

            val originalTest = diamond.selector

            // Update diamond's test.
            val Seq(`diamond`) = replaceByPredicate(diamond, pred)
            val mergedTest = diamond.selector

            XiTransform.log(s"- merged (${originalTest.name}, ${crossroad.selector.name}) -> ${mergedTest.name}", diamond)
            stats.count(StatsKind.XiTransformations, "diamonds optimized", diamond)

            mergeExits += mergeExit

            scheduler.extract(crossroad.block, mergeEdges: _*)
          }
        } { (xi, _) =>
          for (mergeExit <- mergeExits) {
            val copiedExit = xi.copyOf(mergeExit)
            replaceByGoto(copiedExit)
            replaceByGoto(mergeExit.otherExit)
          }
        }

        true
      }
    }
  }
}
