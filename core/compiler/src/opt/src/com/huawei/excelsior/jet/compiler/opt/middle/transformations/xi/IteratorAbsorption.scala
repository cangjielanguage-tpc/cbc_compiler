/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.{Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile
import com.huawei.excelsior.jet.compiler.options.BoolOption.IteratorAbsorption
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Closure

import scala.reflect.ClassTag

/** Absorbs tau-test uses of New[iterator-like] into a specialized path, with the end goal of iterator explosion.
  *
  * @author liontiger
  */
trait IteratorAbsorption extends XiTransform { self: Universe =>

  def absorbIterators(): Boolean = {
    if (!XiTransform.enabled(IteratorAbsorption)) {
      return false
    }

    // Note: absorption analysis requires complete SSA, in order to correctly collect all branches in value uses (see JET-12882).
    if (!profile.isPGOHost || areVarsPresent) {
      return false
    }

    withIncrementalGCM {

      case class Candidate(branches: Seq[Branch], newOp: AnyNewClass) {

        /** Contains blocks to be copied into specialized path.
          *
          * Example with `branches = (b1, b2)`:
          * {{{
          *        ...
          *       /   \
          *      ..   newOp
          *       \   /  \
          *         mp   halt
          *         |
          *        glue
          *       /   \
          *      b1   b2
          *     /  \ /  \
          *    ..  ...  ..
          * }}}
          *
          * Resulting sets:
          * {{{
          *   mergePoint    = mp
          *   newOpDownPath = (newOp, mp, halt)
          *   subGraph      = (mp, glue, b1, b2)
          *   newOpUpPath   = (newOp, mp)
          *   blocks        = (newOp, mp, glue, b1, b2)
          * }}}
          *
          */
        lazy val blocks = {
          var mergePoint: Block = null
          val newOpDownPath = Closure(newOp.block) { b =>
            if (branches forall (b dominates _)) {
              if (mergePoint == null) {
                mergePoint = b
              } else {
                assert(mergePoint == b)
              }
              Seq()
            } else {
              // Avoid backward edges.
              b.succBlocks filterNot (_ dominates newOp.block)
            }
          }

          if (mergePoint != null) {
            val subGraph = versioningSubGraph(mergePoint.outCtrl +: branches: _*)
            assert(!(subGraph contains newOp.block))

            val newOpUpPath = Closure(mergePoint)(_.predBlocks filter newOpDownPath)
            subGraph ++ newOpUpPath

          } else {
            Set.empty[Block]
          }
        }
      }

      def collectBranchUses(n: AnyNewClass): Iterator[If] = {
        def deepValueUses[N <: Node : ClassTag](n: Node): Iterator[N] = {
          collect[N](Closure(n.valueUses) {
            case x @ (_: Phi | _: EOPConvert) => x.valueUses
            case _ => Seq()
          })
        }

        deepValueUses[TauTest](n) flatMap deepValueUses[If] filterNot (n dominates _)
      }

      lazy val loops = cfg.loops

      // Avoid absorption of branches outside of the loop into it.
      def noBranchesInOuterLoops(n: AnyNewClass, branches: Seq[If]): Boolean = {
        val loop = loops loopOf n.block
        // New iterator is not in any loop, or
        loop == null ||
          // all branches are in the same loop (maybe in the inner one).
          (branches forall (loop.body contains _.block))
      }

      val candidates = for {
        n <- all[AnyNewClass] if typeProvider.isIteratorLike(n.allocType.symType)
        branches = collectBranchUses(n).toSeq if branches.nonEmpty && noBranchesInOuterLoops(n, branches)
        candidate = Candidate(branches, n) if candidate.blocks.nonEmpty
      } yield candidate

      if (candidates.isEmpty) {
        return false
      }

      XiTransform.log.inSession("iterator absorption", codeUnit) {
        val scheduledBlocks = Sets[Block].newQSet
        xiTransform { scheduler =>
          for (candidate <- candidates if !(candidate.blocks exists scheduledBlocks)) {
            val goto = Block.splitAfter(candidate.newOp, keepControlled = true)
            val extractionPoint = goto.target
            val extractionPath = candidate.blocks diff Set(goto.block)
            scheduledBlocks ++= extractionPath union Set(extractionPoint)

            scheduler.extract(extractionPoint, goto.targetEdge)
            extractionPath foreach (scheduler.extract(_))
            stats.count(StatsKind.XiTransformations, s"iterators absorbed", candidate.newOp)
            XiTransform.log("- iterator absorbed", candidate.newOp)
          }
        }

        true
      }
    }
  }
}
