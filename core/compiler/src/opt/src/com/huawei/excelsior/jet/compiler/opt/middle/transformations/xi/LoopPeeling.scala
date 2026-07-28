/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.options.BoolOption.{LoopPeeling, RedundantLoadElimination}
import com.huawei.excelsior.jet.compiler.options.NumOption.{PeelableLoopImpactPercentThreshold, PeelableLoopMaxDepth, PeelableLoopMinDepth}
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.{IdempotentOperationsOptimizer, MemoryOptimizations}
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.nowarn

/** Loop peeling optimization.
  *
  * Loop peeling transformation prepends to a loop its first iteration (peels the loop).
  *
  * This transformation is beneficial in multiple aspects:
  *
  *   - peeling of a loop with a single back edge creates loop pre-header (useful for RLE);
  *   - peeling of a loop with idempotent or read-memory operations dominating all back edges
  *     essentially eliminates these operations from the loop (leaving them only in peeled iteration);
  *   - continuous peeling may be the way to completely unroll a loop (even if it is not counted).
  *
  * @author liontiger
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait LoopPeeling extends XiTransform with MemoryOptimizations with IdempotentOperationsOptimizer { self: Universe with Scales =>

  private val impactRatioThreshold = env.valueOf(PeelableLoopImpactPercentThreshold).toDouble / 100.0
  private val minDepth = env.valueOf(PeelableLoopMinDepth)
  private val maxDepth = env.valueOf(PeelableLoopMaxDepth)

  private def peelableLoop(loop: Loop[Block]) = loop.kind != LoopKind.IRREDUCIBLE && loop.header.isInstanceOf[BBlock]

  /** Performs peeling of all outermost loops for nightmare mode testing. */
  def peelAllOuterLoops(): Boolean = {
    val loops = cfg.loops.iterator.filter(l => peelableLoop(l) && l.isOutermost)
    if (loops.isEmpty) {
      return false
    }

    XiTransform.log.inSession("peel outer loops", codeUnit) {
      xiTransform { scheduler =>
        for (loop <- loops) {
          scheduler.peel(loop)
          stats.count(StatsKind.XiTransformations, "outer loops peeled", loop.header)
          XiTransform.log("- loop peeled", loop.header)
        }
      }

      dbgPrinter.debugNodes("All graph after outer loop peeling")

      true
    }
  }

  /** Performs impact-driven loop peeling.
    *
    * This is done in two phases:
    *
    *   1. Loops with high impact ratio are marked for peeling using [[LoopPeelingMarker]];
    *   2. Marked loops are grouped by depth and iteratively peeled from innermost loops to outermost ones.
    *
    * Note: markers are needed in order to safely find impact loops during iterative peeling,
    *       because loop structure changes after each group's peeling iteration.
    *
    * Impact ratio of a loop represents estimated profit from peeling and is calculated as `impact / weight`, where
    *   `impact` is a sum of all impact values of given loop's nodes (see [[PeelingCandidate]]);
    *   `weight` is a weight of given loop without nodes that will become invariant after peeling.
    *
    */
  def peelLoops(collectFailStats: Boolean = false): Boolean = {
    if (!XiTransform.enabled(LoopPeeling)) {
      return false
    }

    val loops = cfg.loops
    if (loops.isEmpty) {
      return false
    }

    val memoryOptimizer = new MemoryOptimizer

    withIncrementalGCM {
      XiTransform.log.inSession("peel loops", codeUnit) {
        // Note: maxDepth <= loops.size
        val loopMarkersByDepth = Array.fill[List[(LoopPeelingMarker, LogFunc)]](loops.seq.size + 1)(Nil)

        val cold = findColdBlocks()

        // mark loops with high impact ratio
        for {
          loop <- loops.iterator if !cold(loop.header) && peelableLoop(loop)
          depth = loop.depth if minDepth <= depth && depth <= maxDepth
        } {
          val candidate = new PeelingCandidate(loop, memoryOptimizer)
          val impact = candidate.impact
          val weight = candidate.weight
          if (impact > 0 && impact >= weight * impactRatioThreshold) {
            val marker = insertCodeAfter(loop.header) { LoopPeelingMarker() }
            loopMarkersByDepth(loop.depth) = (marker, candidate.logSuccessFunc) :: loopMarkersByDepth(loop.depth)
          } else if (collectFailStats) {
            candidate.logFail()
          }
        }

        // peel marked loops from innermost to outermost ones
        var changed = false
        for (markers <- loopMarkersByDepth.reverseIterator if markers.nonEmpty) {
          xiTransform { scheduler =>
            val loops = cfg.loops
            for ((marker, log) <- markers) {
              val loop = loops.loopOf(marker.block)
              strikeOut(marker)
              scheduler.peel(loop)
              log()
            }
          }
          changed = true
        }

        changed
      }
    }
  }

  private type LogFunc = () => Unit

  private class PeelingCandidate(loop: Loop[Block], memoryOptimizer: MemoryOptimizer) {

    private lazy val impactNodes = Maps[Node].newQMap(
      for (b <- loop.body.iterator; n @ ImpactNode(nodeImpact) <- b.spine) yield n -> nodeImpact
    )

    lazy val impact: Int = impactNodes.valuesIterator.sum

    lazy val weight: Double = nonInvariantLoopWeight(loop, peeledLoopInvariant)

    def logFail(): Unit = log(success = false)

    def logSuccessFunc: LogFunc = () => log(success = true)

    private def log(success: Boolean): Unit = {
      val peeled = if (success) "peeled" else "not peeled"
      stats.count(StatsKind.XiTransformations, s"loops $peeled", loop.header)
      if (XiTransform.log.isEnabled) {
        XiTransform.log(s"- loop $peeled", loop.header)
        XiTransform.log(s"  with impact ratio: $impact / $weight (${impact / weight})")
        for ((n, i) <- impactNodes) {
          XiTransform.log(s"  with impact node: ${n.name} [impact $i]", n)
        }
      }
    }

    /** Node that will have a positive impact after peeling.
      *
      * Impact values were experimentally chosen during performance audit.
      */
    private object ImpactNode {
      def unapply(n: Node): Option[Int] = n match {
        case n: PureCheck if n.trusted => None
        case n @ IdempotentInvariant() => condOpt(n) {
          case _: AbstractNullCheck | _: CheckCast | _: ThinCheckCast | _: ArrayIndexCheck | _: ArrayStoreCheck => 20
          case _: DivisorCheck | _: GetFlatThinCheck => 5
          case _: Clinit | _: PackageInit | _: PackageInitCheck => 15
          case _ => shouldNotReachHere(s"unexpected Idempotent node: $n")
        }
        case _ => None
      }
    }

    /** Idempotent node that will be eliminated as idempotent to the peeled one.
      *
      * Note: requires [[com.huawei.excelsior.jet.compiler.opt.middle.IdempotentOperationsOptimizer IOO]] enabled.
      */
    private object IdempotentInvariant {
      def unapply(n: Idempotent): Boolean =
        enabledIOO &&
          dominatesBackEdges(n.outCtrl) &&
          (n.idempotentValueArgs forall peeledLoopInvariant)
    }

    /** Get memory operation that will be replaced by peeled dominating one.
      *
      * Note: requires [[MemoryOptimizations.optimizeMemoryReads RLE]] enabled.
      */
    private object GetMemoryInvariant {
      def unapply(n: GetMemoryOperation): Boolean =
        env.enabled(RedundantLoadElimination) &&
          dominatesBackEdges(n.inCtrl) &&
          memoryOptimizer.canMemoryReadBeMovedOutOfLoop(n, loop) &&
          cond(n) {
            case n: GetInstanceFieldOperation => canBeMovedByContextTypes(n, n.obj) && peeledLoopInvariant(n.obj)
            case _: GetStatic => true
            case n: ArrayElementOperation => canBeMovedByContextTypes(n, n.array) && peeledLoopInvariant(n.array) && peeledLoopInvariant(n.idx)
          }
    }

    /** Returns true for loop invariant nodes and for nodes that will become invariant after peeling. */
    private def peeledLoopInvariant(n: Node): Boolean = n match {
      case n: ArrayLength if canBeMovedByContextTypes(n, n.array) => peeledLoopInvariant(n.array)
      case n: GetConstField if canBeMovedByContextTypes(n, n.obj) => peeledLoopInvariant(n.obj) && dominatesBackEdges(n.inCtrl)
      case GetMemoryInvariant() => true
      case IdempotentInvariant() => true
      case _ => withIncrementalGCM {
        n.block strictDominates loop.header
      }
    }

    // workaround for lack of proper types in records in cangjie
    // TODO: remove it when records are parsed from metadata
    private def canBeMovedByContextTypes(n: ControlledNode, obj: Node) = cond(obj.tpe) {
      case ThinType | TRefType => !nodeTypeAt(obj, n.inCtrl).mayBeNull
    }

    /** Returns true for nodes that dominate all loop back edges, and thus will dominate whole loop after peeling. */
    private def dominatesBackEdges(n: ControlNode) = dominatesLoopBackwardEdges(n, loop)
  }
}
