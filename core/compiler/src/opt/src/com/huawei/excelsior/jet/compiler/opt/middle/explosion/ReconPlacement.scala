/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.explosion

import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.ObjectBiGraph
import com.huawei.excelsior.jet.util.graph.analysis.DataFlowAnalysis
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.Set

/** Finds positions in CFG where object reconstruction should be placed.
  *
  * @see [[PreExplosion]]
  * @author haitaka
  */
trait ReconPlacement {
  self: Universe =>

  def liveBlocks(n: SpinalNode) = withIncrementalGCM {
    Closure(
      Iterator(n.block) ++ n.valueUses map (_.block)
    ) { _.predBlocks filter n.strictDominates }
  }

  def findReconPoints(explosive: SpinalNode, refUses: Set[ControlNode]) = {
    val refUsesBlocks = refUses map (_.block)

    val lb = liveBlocks(explosive)
    assert(lb forall (b => !b.hasXHandlers && !b.isInstanceOf[XBlock]))

    def canBeLiveAt(n: ControlNode) = lb contains n.block

    // CFG subgraph containing only live blocks
    val aliveCFG = cfg.withStart(explosive.block) filter canBeLiveAt

    def spineAfterExplosive = explosive.block.spineForward.dropWhile(_ != explosive)

    object ReconAnalysis extends DataFlowAnalysis[Block](aliveCFG) {

      /** Referenced < Explosive < Unreachable */
      sealed abstract class State
      object State {
        case object Unreachable extends State
        case object Explosive extends State
        case object Referenced extends State

        @scala.annotation.tailrec
        def meet(s1: State, s2: State): State = (s1, s2) match {
          case _ if s1 == s2 => s1
          case (Unreachable, _) => s2
          case (Referenced, _) => Referenced
          case _ => meet(s2, s1)
        }

        def meet(states: IterableOnce[State]): State = states.iterator.fold(Unreachable)(meet)
      }

      override protected def init: State = State.Unreachable

      override protected def join(outputStates: IterableOnce[State]) = State.meet(outputStates)

      override protected def trans(b: Block, inputState: State) = {
        if (b == explosive.block) {
          if (refUsesBlocks contains b) {
            // explosive.block may contain phi ref-use above the explosive node
            // this analysis should ignore such uses
            if (spineAfterExplosive exists refUses) State.Referenced else State.Explosive
          } else {
            State.Explosive
          }
        } else {
          if (refUsesBlocks contains b) State.Referenced else inputState
        }
      }
    }

    import ReconAnalysis.State

    if (env.enabled(BoolOption.LogReconPlacement)) {
      dbgPrinter.debugGraphs(s"recon placement for $explosive", printNodesGraph = false,
        info = DGIProvider { b =>
          if (canBeLiveAt(b)) {
            (ReconAnalysis.in(b), ReconAnalysis.out(b)) match {
              case (State.Unreachable, State.Unreachable) => DGI("unreachable", "yellow")
              case (State.Unreachable, State.Explosive) => DGI("-> expl", "lightgreen")
              case (State.Unreachable, State.Referenced) => DGI("expl -> ref", "orange")
              case (State.Explosive, State.Explosive) => DGI("expl", "green")
              case (State.Explosive, State.Referenced) => DGI("expl -> ref", "darkred")
              case (State.Referenced, State.Referenced) => DGI("ref", "red")
              case x => shouldNotReachHere(x)
            }
          } else null
        }
      )
    }

    def firstRefUseEdge(it: Iterator[SpinalNode]) = (it find refUses map(_.inCtrlEdge)).get
    if (ReconAnalysis.out(explosive.block) == State.Referenced) {
      // special case: there can be no successors
      Set(firstRefUseEdge(spineAfterExplosive))
    } else {
      def edgesBetween(a: Block, b: Block) = a.succBlockEdges filter (_.target == b)
      def findReconEdges(from: Block, to: Block) = {
        if (to == explosive.block && (refUses contains explosive.block)) {
          // explosive.block contains Phi ref-use
          // we have ignored it during analysis to keep markup clean
          // but now we need to handle this situation and place reconstruction on incoming edges
          // TODO In ideal case the block should always be split just before the explosive, so such situation would never happen.
          edgesBetween(from, to)
        } else {
          (ReconAnalysis.out(from), ReconAnalysis.in(to), ReconAnalysis.out(to)) match {
            case (State.Explosive, State.Referenced, _) =>
              // block is a merge point of different states
              edgesBetween(from, to)
            case (State.Explosive, State.Explosive, State.Referenced) if refUses contains to =>
              // block itself is a ref-use
              edgesBetween(from, to)
            case (State.Explosive, State.Explosive, State.Referenced) =>
              // ref-use somewhere in the block spine
              Iterator.single(firstRefUseEdge(to.spineForward))
            case _ => Iterator.empty
          }
        }
      }

      val reconEdges = for {
        from <- aliveCFG.topSort.order.iterator
        to <- aliveCFG.succs(from)
        edge <- findReconEdges(from, to)
      } yield edge

      reconEdges.toSet
    }
  }
}

