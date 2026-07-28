/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.escape.EscapeAnalysis
import com.huawei.excelsior.jet.compiler.options.BoolOption.OptimizeWriteBarriers
import com.huawei.excelsior.jet.compiler.util.{Log, Sets}
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.util.graph.BiGraph
import com.huawei.excelsior.jet.util.{Closure, Worklist}

import scala.annotation.nowarn

/** Optimizes out redundant write barriers: e.g. the ones that will never be triggered.
  *
  * @author cypok
  * @author afilatov
  * @author arxdukalis
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait EscapeWriteBarriersOptimization extends EscapeAnalysis {
  self: Universe =>

  private val log = Log(Kind.WriteBarriersOpt)

  def strikeOutBarrier(iwb: EscapeWriteBarrier.Instance): Unit = {
    if (Env.isWorkMode) {
      replaceByCode(iwb) {
        VerificationInstanceWriteBarrier(iwb.receiver, iwb.value)
        iwb.value // replace value uses of write barrier with its value
      }
    } else {
      iwb.replaceValueUsesBy(iwb.value)
      strikeOut(iwb)
    }
  }

  private object ReadFrom {
    def unapply(n: GetMemoryOperation): Option[Node] = n match {
      case n: GetInstanceFieldOperation => Some(n.obj)
      case n: ArrayGetOperation => Some(n.array)
      case _ => None
    }
  }

  private sealed trait OptimizationDecision
  private case class Yes(reason: String) extends OptimizationDecision
  private object No extends OptimizationDecision

  def optimizeWriteBarriers(): Boolean = {
    if (!env.enabled(OptimizeWriteBarriers)) {
      return false
    }

    /** Checks that `sink` will never be triggered in the following "Swap" pattern:
      *
      * {{{
      *         source = Read(x)
      *                |
      *                V
      *            _________
      *           |         |
      *           |    A    | <- - - - - -
      *           |_________|             |
      *                |
      *                V                  |
      *
      *   sink = WriteBarrier(x, source)  |   transition `B -> A`
      *                                           is optional
      *                |                  |
      *                V
      *            _________              |
      *           |         |
      *           |    B    |             |
      *           |_________|
      *                |_ _ _ _ _ _ _ _ _ |
      *                |
      *                V
      * }}}
      *
      * It will be satisfied in one of the following cases:
      *  - `B -> A` doesn't exist, node graph `A` contains no write barriers and method calls;
      *  - `B -> A` exists, both `A` and `B` contain no write barriers and method calls.
      */
    def swapBarrierIsIdempotent(source: Node, sink: UpperPoint) = {
      val sourcePoint = upperPoint(source)
      val nodes = BiGraph.anchorSubGraph(spinalCFG, sourcePoint, sink)

      nodes.forall {
        case `sink` | `sourcePoint` => true
        case _: WriteBarrier | _: Call => false
        case _ => true
      }
    }

    /** Eliminates write barriers with Phi as value by inserting new barriers
      * right after each argument of Phi and removing the original ones.
      *
      * Transformation will be performed only if it won't increase the number of write barriers in program,
      * namely, if all but perhaps one of the barriers inserted after the arguments of Phi can be removed
      * with other optimizations.
      */
    def liftBarriers(): Boolean = {

      /** Nearest [[UpperPoint]] following the given node. */
      def upperPointAfter(v: Node) = {
        lowerPoint(v) match {
          case blockEnd: BlockEnd => blockEnd.inCtrl
          case n: SpinalNode => n
          case x => shouldNotReachHere(x)
        }
      }

      /** Phi args that are not [[NoValue]]. */
      def actualPhiArgs(phi: Phi) = phi.argsSeq.filterNot(_.isInstanceOf[NoValue])

      def canBeLifted(wb: EscapeWriteBarrier.Instance, phi: Phi): Boolean = {

        def hasPhiCyclesRootedAt(root: Phi): Boolean = {
          Phi.transitivePhiArgs(root).contains(phi)
        }

        /** Since new barrier will be inserted right after Phi argument,
          * it's required for the receiver to be available at this point.
          */
        def receiverDominatesPhiArgs = {
          val receiverPoint = upperPoint(wb.receiver)
          actualPhiArgs(phi).forall(receiverPoint strictDominates upperPoint(_).outCtrl)
        }

        def noEscapePointsBetweenPhiArgsAndBarrier = {
          val nodes = Sets[ControlNode].newQSet
          nodes += wb
          Closure.collect(nodes, actualPhiArgs(phi).map(lowerPoint))(spinalCFG.succs)

          nodes.forall {
            case `wb` => true
            case _: WriteBarrier | _: Call => false
            case _ => true
          }
        }

        !hasPhiCyclesRootedAt(phi) && receiverDominatesPhiArgs && noEscapePointsBetweenPhiArgsAndBarrier
      }

      def hasReasonToLift(wb: EscapeWriteBarrier.Instance, phi: Phi) = {
        val toBeInserted = actualPhiArgs(phi).length

        val receiver = wb.receiver
        val unoptimizedNodes = actualPhiArgs(phi).filter { v =>
          shouldOptimize(upperPointAfter(v), receiver, v) == No
        }

        unoptimizedNodes match {
          case Seq() => true
          case Seq(n) if toBeInserted > 1 => !cfg.loops.isInLoop(n.block)
          case _ => false
        }
      }

      val worklist = Worklist.from(all[EscapeWriteBarrier.Instance])
      if (worklist.isEmpty) {
        return false
      }

      var changed = false

      for (wb @ EscapeWriteBarrier.Instance(receiver, phi: Phi) <- worklist.accumulate if canBeLifted(wb, phi) && hasReasonToLift(wb, phi)) {
        if (!changed) {
          eliminateCrossBlockMemoryEdges()
        }

        val lifted = actualPhiArgs(phi).map { arg =>
          val argPoint = upperPoint(arg)

          val insertionPoint = if (arg.block.hasXHandlers) {
            Block.splitBefore(argPoint.outCtrl)
            Block.splitAfter(argPoint).target
          } else argPoint

          insertCode(insertionPoint, wb, useDefaultHandler = true, wb) {
            EscapeWriteBarrier.Instance(receiver, arg)
          }
        }
        worklist ++= lifted

        log(s"- lift $wb to ${lifted.length} places", wb)
        if (log.isEnabled) {
          for (lwb @ EscapeWriteBarrier.Instance(receiver, value) <- lifted) {
            shouldOptimize(lwb, receiver, value) match {
              case Yes(reason) =>
                log(s"  - remove $lwb because $reason", lwb.outCtrl)
              case No =>
            }
          }
        }

        strikeOutBarrier(wb)
        changed = true
      }

      changed
    }

    def shouldOptimize(location: UpperPoint, receiver: Node, value: Node) = (receiver, value) match {
      case (Null(), _) =>
        // No code is generated for barriers with null receiver or value,
        // but it's useful to remove them here, e.g. for better work of barrier lifting.
        Yes("receiver is null")

      case (_, Null()) =>
        Yes("value is null")

      case (r, v) if r == v =>
        Yes("receiver coincides with value")

      case (_: AnyNewStackAllocated, _) =>
        Yes("receiver is stack allocated")

      case (receiver, value @ ReadFrom(host)) if receiver == host && swapBarrierIsIdempotent(value, location) =>
        Yes("value is obtained by dereferencing the receiver")

      case _ => No
    }

    var changed = false

    log.inSession(s"removing write barriers in $codeUnit") {
      withIncrementalGCM {

        changed |= liftBarriers()

        for (wb @ EscapeWriteBarrier.Instance(receiver, value) <- all[EscapeWriteBarrier.Instance]) {
          shouldOptimize(wb, receiver, value) match {
            case Yes(reason) =>
              log(s"- remove $wb because $reason", wb)
              strikeOutBarrier(wb)
              changed = true

            case No =>
            // nothing to do
          }
        }
      }
    }

    changed
  }
}
