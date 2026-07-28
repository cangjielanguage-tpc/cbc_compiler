/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind.LiveRangesOptimization
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.OptimizeAllLiveRanges
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.util.Worklist

/** Live ranges optimization by pulling up spinal nodes without value results.
  *
  * If node doesn't have value result, it is profitable to generate it as early as possible, because it's generation
  * could release some resources (occupied by node's arguments) but will not occupy any new resources. Nodes without
  * value result should have control result (otherwise they are meaningless in our IR), thus they are spinal nodes.
  * This optimization transforms block spines pulling up spinal nodes.
  *
  * Consider the following example:
  * {{{
  *   l0 = new Foo
  *   l1 = new Foo
  *   ...
  *   lN = new Foo
  *   arr = new Foo[N+1]
  *                         <--- at this point we have RP at least N+2
  *   arr[0] = l0
  *   arr[1] = l1
  *   ...
  *   arr[N] = lN
  *    }}}
  *
  * This optimization transforms such IR, decreasing RP in all points, to:
  *   {{{
  *   arr = new Foo[N+1]
  *   l0 = new Foo
  *   arr[0] = l0
  *   l1 = new Foo
  *   arr[1] = l1
  *   ...
  *   lN = new Foo
  *   arr[N] = lN
  * }}}
  *
  * Currently this optimization is implemented under strong restrictions, essentially for the one case described above.
  * Feel free to improve it.
  *
  * @author conwor
  */
trait LiveRangesOptimization { self: Universe =>
  private lazy val optimizeAllLiveRanges = env.enabled(OptimizeAllLiveRanges)

  /** Returns true iff pulling up `node` may decrease some live ranges and will guarantee not increase any other one.
    * Such transformation may decrease register pressure at some points, thus improve register allocation.
    */
  private def couldImproveRPByPullingUp(node: SpinalNode): Boolean =
    node.valueArgs.nonEmpty && !node.hasValueUses

  /** Returns true iff `node` has no value uses and we could analyze it and pull up. */
  private def hasDirectImpactToPullUp(node: SpinalNode): Boolean = couldImproveRPByPullingUp(node) && (node match {
    case _: ArrayPutOperation => true
    case _ => optimizeAllLiveRanges
  })

  /** Returns true iff `node` could be obstacle for pulling up some other node which has direct impact for pulling up. */
  private def hasSecondaryImpactToPullUp(node: SpinalNode): Boolean = node match {
    case AnyNewArray(_, Seq(IntegralConst(_))) => true
    case _ => false
  }

  /** Returns true iff `node` has dependencies from `upper` directly in value arguments. */
  private def dependentDirectly(node: Node, upper: UpperPoint): Boolean =
    node.valueArgs contains upper

  /** Returns true iff `node` has dependencies from `upper` through some other floating nodes. */
  private def dependentIndirectly(node: Node, upper: UpperPoint): Boolean = {
    requireIncrementalGCM()
    node.valueArgs.exists {
      case arg: FloatingNode if upper dominates arg.upperPoint => true
      case _ => false
    }
  }

  /** Returns true iff `node` has dependencies from `upper` by any edges except direct control and memory edges. */
  private def dependent(node: Node, upper: UpperPoint): Boolean =
    dependentDirectly(node, upper) || dependentIndirectly(node, upper)

  private def canTryToPullUpperWithDependentNode(node: SpinalNode, upper: UpperPoint): Boolean = {
    if (!dependentIndirectly(node, upper)) {
      // There are no dependencies between `node` and `upper` except direct value edge, so we could pull up `upper`
      // and `node` only, there is no need to pull anything else.
      true

    } else if (collect[ControlledNode](upper.pinnedNodes).nonEmpty) {
      // There is some controlled node `n` pinned to `upper`. It could be used in `node` by value edges (we
      // conservatively suppose that it is, feel free to improve optimization). In this case we could not pull
      // up `upper` and `node` nodes, because we should also pull up `n`, but we could not be sure, that it is not
      // controlled-dependent from `upper.inCtrl`.
      false

    } else {
      true
    }
  }

  /** Returns None, if `node` could be pulled up above `upper`. Returns Some((`reason`, `couldTryToPullUpper`)) if `node`
    * could not be pulled up above `upper` because of `reason`. In this case `couldTryToPullUpper` is true iff we can try
    * to pull `upper` with `node` up.
    */
  private def pullUpObstacle(node: SpinalNode, upper: UpperPoint): Option[(String, Boolean)] = (node, upper) match {
    case (_, _: Block) =>
      Some(("moving in CFG is not implemented yet", false))

    case (_, _) if node.hasXHandler =>
      Some(("node has exception handler", false))

    case (_: MemoryNode, upper: MemoryNode) if upper.memoryDependentFloatingNodes.nonEmpty =>
      // To pull up `node` above `upper` we should prove that memory for `upper`'s memory-dependent nodes would not be spoiled by `node`.
      // TODO: feel free to implement it.
      Some(("memory anti-dependency", false))

    case (_, _) if dependent(node, upper) =>
      Some(("value dependency", canTryToPullUpperWithDependentNode(node, upper)))

    case (AnyNewArray(t, _), upper: SpinalNode) =>
      val base = t.getArrayBase
      if (base.isPrimitive || (isClassClinitedAt(asClassType(base), upper) == isClassClinitedAt(asClassType(base), upper.inCtrl))) {
        None
      } else {
        Some(("context types dependency", true))
      }

    case (_: ArrayPutOperation, _: PutField | _: AnyNew) => None

    case (_, _) => Some(("not implemented yet", true))
  }

  /** Returns None, if the whole `column` could be pulled up above `upper`.
    * Returns Some((`node`, `reason`, `couldTryToPullUpper`)) if `node` from `column` could not be pulled up above
    * `upper` because of `reason`. In this case `couldTryToPullUpper` is true iff we can try to pull `upper` with
    * the whole `column` up.
    */
  private def pullUpObstacle(column: List[SpinalNode], upper: UpperPoint): Option[(Node, String, Boolean)] = {
    var result: Option[(Node, String, Boolean)] = None
    for (x <- column) {
      pullUpObstacle(x, upper) match {
        case Some((r, couldTryToPullUpper)) =>
          if (!couldTryToPullUpper) {
            return Some((x, r, false))
          } else if (result.isEmpty) {
            result = Some((x, r, true))
          }
        case _ =>
      }
    }
    result
  }

  def optimizeLiveRanges(): Boolean = {
    if (isO1Compiled || !env.enabled(BoolOption.LiveRangesOptimization)) return false

    def stat(msg: String): Unit = stats.count(LiveRangesOptimization, msg)
    def name(n: Node): String = n.simpleName

    val toPull = Worklist.from(all[SpinalNode] filter hasDirectImpactToPullUp)

    def pullUp(node: SpinalNode): Int = {
      def stat(msg: String): Unit = stats.count(LiveRangesOptimization, msg, node)

      var steps = 0
      var point = node
      var column = List(node)
      var stopReason: String = null

      while (stopReason == null) {
        point.inCtrl match {
          case upper: SpinalNode if toPull.contains(upper) =>
            toPull -= upper
            pullUp(upper)

          case upper => pullUpObstacle(column, upper) match {
            case None =>
              point = upper.asInstanceOf[SpinalNode]
              steps += 1

            case Some((x, reason, couldTryToPullUpper)) =>
              upper match {
                case upper: SpinalNode if couldTryToPullUpper && hasSecondaryImpactToPullUp(upper) && pullUpObstacle(upper +: column, upper.inCtrl).isEmpty =>
                  column = upper +: column
                  point = upper.inCtrl.asInstanceOf[SpinalNode]
                  steps += 1

                case _ =>
                  // Feel free to implement third impact and so on, if you want to.
                  stopReason = s"pulling up ${name(node)} stopped. ${name(x)} could not be pulled above ${name(upper)} because of $reason"
              }
          }
        }
      }

      if (steps > 0) {
        stat(s"success pull of ${name(node)} to $steps steps")
        for (node <- column) {
          val clone = insertCodeBefore(point, useDefaultHandler = true) { Node.clone(node) }
          strikeOutWithValueUses(node, clone)
        }
      }
      stat(stopReason)
      steps
    }

    if (toPull.nonEmpty) {
      withIncrementalGCM {
        var cases = 0
        var sumSteps = 0
        for (node <- toPull.drain) {
          val steps = pullUp(node)
          if (steps > 0) {
            cases += 1
            sumSteps += steps
          }
        }
        if (sumSteps > 0) {
          stat(s"$sumSteps sum steps for $cases cases of pull up in ${rootMethod.getFullName}")
          dbgPrinter.debugNodes("All graph after live ranges optimized")
          return true
        }
      }
    }

    false
  }
}