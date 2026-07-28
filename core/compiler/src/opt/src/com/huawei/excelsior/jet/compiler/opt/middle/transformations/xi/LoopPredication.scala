/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.options.BoolOption.LoopPredication
import com.huawei.excelsior.jet.compiler.options.NumOption.LoopPredicationGlueCodeSizeLimit
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.util.ScalaCollections._
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}

import scala.PartialFunction.condOpt

/** Eliminates dominating invariant throwing checks in loops by inserting explicit predicate check before the loop.
  *
  * The failed predicate case will mimic the first iteration of the loop and throw corresponding exception instead of the check.
  * Note that only a limited amount of glue code in the failed case is allowed.
  *
  * Example transformation:
  * {{{
  *                                       A
  *                                       |
  *                                 __ x != null __
  *        A    ____               |t   ____      f|
  *        |   |    |              |   |    |      |
  *       _V___V_   |             _V___V_   |      V
  *      |       |  |            |       |  |     pre
  *      |  pre  |  |            |  pre  |  |      |
  *      |   |   |  |            |   |   |  |   throw(NPE)
  *      | NC(x) |  |   ----->   |   |   |  |
  *      |   |   |  |            |   |   |  |
  *      | post  |__|            | post  |__|
  *      |_______|               |_______|
  *          |                       |
  *          V                       V
  *          B                       B
  * }}}
  *
  * This optimization enables loop-invariant code motion.
  *
  * Originally introduced for optimization of Grinder/Chess benchmark.
  *
  * @author liontiger
  */
trait LoopPredication extends XiTransform with Scales { self: Universe =>

  private lazy val glueCodeSizeLimit = env.valueOf(LoopPredicationGlueCodeSizeLimit)

  def predicateLoops(collectFailStats: Boolean = false): Boolean = {
    if (!XiTransform.enabled(LoopPredication)) {
      return false
    }

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    //////////////////////////////////////////////////

    lazy val ts = cfg.topSort

    object PredicationCandidate {
      def unapply(loop: Loop[Block]): Option[PredicationTarget] = withIncrementalGCM {
        val backExits = loopBackwardEdges(loop).map(_.source.asInstanceOf[ControlNode]).toSeq
        def dominatesBackEdges(b: Block) = backExits forall b.dominates

        def loopInvariant(n: Node) = n.block strictDominates loop.header

        val loopSpine = loop.body.toSeq filter dominatesBackEdges sortBy ts.number
        val targetOpt = loopSpine.iterator flatMap (_.spine) collectFirst {
          case n @ PredicationNode(pred) if n.valueArgs forall loopInvariant =>
            PredicationTarget(loop, n, pred)
        }

        targetOpt match {
          case Some(target) if target.glueCodeWeight <= glueCodeSizeLimit && loop.kind != LoopKind.IRREDUCIBLE && !loop.header.isInstanceOf[XBlock] =>
              Some(target)

          case Some(target) =>
            if (collectFailStats) {
              target.logFail()
            }
            None

          case None => None
        }
      }
    }

    object PredicationNode {
      def unapply(n: ThrowingPureCheck): Option[PredicateConstructor] = {
        if (!n.trusted) {
          import PredicateConstructor._
          condOpt(n) {
            case n: NullCheck => nonNull(n.obj)
            // TODO: consider supporting CheckCast and others
          }
        } else {
          None
        }
      }
    }

    case class PredicationTarget(loop: Loop[Block], check: ThrowingPureCheck, pred: PredicateConstructor) {
      lazy val glueCodeWeight = {
        def discardedAfterPredication(n: Node): Boolean = n match {
          case _: Phi => true
          case n: ControlNode => check dominates n
          case n => (check dominates upperPoint(n)) || (n.valueUses forall discardedAfterPredication)
        }
        nonInvariantLoopWeight(loop, discardedAfterPredication)
      }

      def logFail(): Unit = log(success = false)

      def logSuccess(): Unit = log(success = true)

      private def log(success: Boolean): Unit = {
        val predicated = if (success) "predicated" else "not predicated"
        if (loop.kind == LoopKind.IRREDUCIBLE) {
          assert(!success)
          stats.count(StatsKind.XiTransformations, s"loops $predicated: ${LoopKind.IRREDUCIBLE}", loop.header)
          if (XiTransform.log.isEnabled) {
            XiTransform.log(s"- loop $predicated (${LoopKind.IRREDUCIBLE})", loop.header)
          }
        } else {
          stats.count(StatsKind.XiTransformations, s"loops $predicated", loop.header)
          if (XiTransform.log.isEnabled) {
            XiTransform.log(s"- loop $predicated", loop.header)
            XiTransform.log(s"  with glue code weight: $glueCodeWeight")
            XiTransform.log(s"  with check: ${check.name}", check)
          }
        }
      }
    }

    //////////////////////////////////////////////////

    withIncrementalGCM {
      XiTransform.log.inSession("loop predication", codeUnit) {
        val allTargets = loops.iterator collect { case PredicationCandidate(x) => x }

        // Collect only innermost targets to guarantee that they do not overlap (needed for xiTransform).
        // Note that other targets will still be optimized on the following optimization iterations.
        val targets = minimalElements(allTargets)(partialOrderingBy(_.loop isInnerOf _.loop))
        if (targets.isEmpty) {
          return false
        }

        xiTransformAndPostProcess { scheduler =>
          for (target <- targets) {
            val (_, _, failBlock) = scheduler.version(target.pred, target.loop)
            failBlock.markAsCold()
            target.logSuccess()
          }
        } { (xi, _) =>
          for (target <- targets) {
            val failCheck = target.check
            val passCheck = xi.copyOf(failCheck)
            replaceCheckByThrow(failCheck)
            strikeOut(passCheck)
          }
        }

        true
      }
    }
  }
}
