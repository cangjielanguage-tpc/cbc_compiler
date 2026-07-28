/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.util.graph.Loop

import scala.PartialFunction.cond

/** Elimination of counted loops without side-effects and value uses.
  *
  * Currently only single-block and double-block loops are supported. TODO: support more complex cases if needed
  *
  * @author liontiger
  */
trait UselessLoopElimination extends CountedLoopsRecognizer with XiTransform { self: Universe =>

  private object SimpleLoop {
    def unapply(loop: Loop[Block]): Option[Edge] = {
      // skip loops with complicated block structure
      if (loop.body.size > 2) return None

      // skip loops with non-empty spine (except for nodes with no side-effects)
      if (!(loop.body forall (_.spine forall noSideEffects))) return None

      // skip loops with multiple exit edges
      singleton(loopExitEdges(loop))
    }
  }

  private def noSideEffects(n: Node) = cond(n) {
    case _: Marker | _: RawValueRangeFilter => true
  }

  def eliminateUselessLoops(): Boolean = {

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    def uselessBody(loop: Loop[Block]): Boolean = {
      val loopNodes = loop.body flatMap Block.collectNodes
      loopNodes forall (_.valueUses forall loopNodes)
    }

    var changed = false

    withIncrementalGCM {
      for (loop @ SimpleLoop(exitEdge) <- detectCountedLoops(loops) if uselessBody(loop)) {
        // We found useless loop and we have to preserve useful Controlled nodes, for which inCtrl is this loop's block.
        // This can be achieved through combination of gcm, which sets block for every node, and narrowing of inCtrl
        // of every node to it's block scope. See method description for more.
        eliminateCrossBlockInCtrlUses(loop.body)
        if (!changed) {
          // Note: The loop may not have value uses, but still have memory uses.
          //       Simply rearranging control edges in this case will result in memory uses from unreachable code,
          //       so we should eliminate memory uses between blocks here.
          eliminateCrossBlockMemoryEdges()
        }
        val forwardEdges = loopEnterEdges(loop).toSeq
        val preds = forwardEdges map (_.source)
        makeUnreachable(forwardEdges)
        Block.addEdgesWithTemplate(preds, exitEdge)
        // UCE will handle the rest
        changed = true
      }
    }

    changed
  }

  def evaluateUselessLoops(): Boolean = {

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    def evaluate(loop: Loop[Block]): Boolean = {
      (loop.body forall (_.points forall (_.controlUses forall (_.isInstanceOf[PinnedNode])))) &&
        (singleton(findInductiveVariables(loop)) match {
          case Some(iv) if singleton(loop.header.phies) contains iv.index =>
            val loopNodes = loop.body flatMap Block.collectNodes
            val loopValues = Closure[Node](loop.body flatMap (_.points))(_.valueArgs filter loopNodes)
            val resultEdges = loopValues flatMap (_.valueOutEdges) filterNot (e => loopValues(e.target))
            if (resultEdges.nonEmpty && (resultEdges forall (_.source == iv.index))) {

              def eval(endValue: Node): Boolean = {
                for (edge <- resultEdges) {
                  edge.source = endValue
                }
                true
              }

              // TODO: support more
              (iv.cond, iv.step) match {
                case (Condition.NE, _) => eval(iv.limit)
                case _ => false
              }

            } else {
              false
            }

          case _ => false
        })
    }

    var changed = false

    withIncrementalGCM {
      for (loop @ SimpleLoop(_) <- detectCountedLoops(loops)) {
        changed |= evaluate(loop)
      }
    }

    changed
  }

  private object ZeroLoop {

    private def emptyValueRange(variable: InductiveVariable): Boolean =
      calcValueRangeOfInductiveVariable(variable) exists (_.isEmpty)

    def unapply(loop: Loop[Block]): Option[Seq[If.Exit]] = {
      val exits = findInductiveVariables(loop).collect {
        case variable if emptyValueRange(variable) => variable.continueEdge.otherExit
      }.toSeq

      Some(exits)
    }
  }

  /**
    * Eliminate all for loops that iterate zero times.
    *
    * @return `true` if any satisfying loop was eliminated
    */
  def eliminateZeroLoops(): Boolean = {

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    var changed = false

    for (loop @ ZeroLoop(exits) <- detectCountedLoops(loops) if exits.nonEmpty) {
      exits foreach replaceByGoto
      changed = true
      stats.count(StatsKind.ZeroLoopElimination, "eliminate zero loop", loop.header)
    }

    changed
  }

}
