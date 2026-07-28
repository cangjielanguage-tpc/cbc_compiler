/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.cfg

import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.UAI
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, MachineDescription}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.{LoopKind, Loops}

/** Part of UAI, responsible for CFG knots processing. There are two types of knots:
  *   1) Branch-point - where two or more UAI states should be merged into one
  *   2) Phi-point, where one UAI state should be splitted to several
  *
  * Also there is one bad knot subtype - branch-point in throwable node.
  *
  * There are some preparing steps before BGCM (critical edges elimination, including
  * exceptional edges, loops normalization), providing good regular state of CFG:
  *   1) There are no one edge from branch-point to phi-point directly
  *   2) Each loop header is phi-point
  *   3) XBlock may not be loop header
  *   4) Each loop enter (forward edge into loop header) is goto edge into phi-point
  *   5) Each loop exit is an edge coming from branch-point and not into phi-point
  *   6) Each loop exit going to outer loop (may be not immediately)
  *   7) Each reducible loop enter coming from immediately outer loop
  *   8) Each backward branch of each loop is goto edge (coming not from branch-point)
  *
  * These restrictions dramatically simplify CFG knots processing.
  *
  * @author conwor
  */
trait Knots { self: Universe with BackEnd =>

  trait CFGKnots { self: UpwardAI =>

    ///////////////////////////////////////////////////////////////////////////////////////////////
    //// Branch points

    /** Helper function to merge states at branch-point and from exceptional handler.
      * Move all nodes from `fromBlocks` states to `to` state and insert required spill hints.
      *
      * TODO: rewrite this code and extract spill decisions to SpillHeuristics trait
      * */
    private def mergeImpl(to: State, base: Block, fromBlocks: Seq[Block], isExceptionMerge: Boolean): Unit = {

      def mergeOne(from: State, coldToHot: Boolean): Unit = {

        // TODO: try to rewrite this code in sets calculation style

        for (node <- from.allNodes) {
          if (to.live(node)) {
            if (to.inRegister(node) == from.inRegister(node)) {
              // nothing to do, node has the same LRS bits

            } else if (coldToHot) {
              // nothing to do, no reason to affect normal code with cold LRS bits

            } else if (to.loop.isOutsider(node)) {
              // synchronize outsider node spill fromBlock one of execution path.
              to.moveToStorage(node)

            } else if (isExceptionMerge) {
              // moving node to registers required to insert spill at control continuation, which is complicated now. TODO: solve this problem

            } else if (from.inRegister(node)) {
              // try to occupy register for node in `to` state
              to.appendNode(node, tryToRegisters = true)

            } else {
              // leave node on registers
            }
          } else {
            to.appendNode(node, tryToRegisters = from.inRegister(node), isExceptionMerge)
          }
        }
      }

      /** Inserts Spill hints for all nodes, which are in storage at `fromBlock` block start
        * and in registers in merged `to` state. */
      def insertSpillHints(fromBlock: Block): Unit = {
        val from = states(fromBlock)
        for (node <- from.allNodes) {
          assert(to.live(node))
          if (to.inRegister(node) && from.inStorage(node)) {
            assert(blockStatus(fromBlock) == BlockStatus.FULLY_INTERPRETED)
            dag.insertHintAtAlreadyGeneratedBlockStart(fromBlock, node, BulldozerHint.spill)
          }
        }
      }

      // 1. Merge nodes fromBlock all successors
      for (fromBlock <- fromBlocks) {
        mergeOne(states(fromBlock), coldToHot = gcm.cold(fromBlock) && !to.cold)
      }

      // 2. Insert required spill hints
      fromBlocks foreach insertSpillHints
      insertSpillHints(base)
    }

    /** Creates state for branch-`point`. Assume that all successors have already been interpreted till the end.
      * Task is to merge their states, including all live nodes from all of them. We cannot simply union
      * register sets, because resulting state may have more nodes on registers than file limit. Also some
      * nodes may have different LRS bits, so we should select resulting bits.
      *
      * To create state we select `base` successor, which will be used as reference, clone its state and append
      * nodes from other states, possibly changing their LRS bits. `Base` selected by two consistently applied rules:
      *   1) It should have same temperature as `point`
      *   2) It should be in the same loop as `point`
      * Both this predicates filter out non empty subsets of `point` successor (this may be proven based on
      * assumable restrictions of CFG). If this predicates summary filter out empty subset, then only first
      * predicate used (open problem, rare case).
      *
      * This selection corresponds to `base` as a base in case of LRS bits difference.
      * */
    def createStateForBranchPoint(point: Block): Unit = {
      assert(!states.contains(point))

      val pointLoop = gcm.loops.loopOf(point)
      val isPointCold = gcm.cold(point)

      // 1. Select base successor and clone its state.
      val sameTemperature = (point.succBlocks filter { gcm.cold(_) == isPointCold }).toList
      val baseSucc = sameTemperature find { gcm.loops.loopOf(_) == pointLoop } getOrElse
        sameTemperature.head // Use only first predicate. TODO: investigate it
      val state = new State(states(baseSucc).local.copy(), loopLRSs(pointLoop), cold = isPointCold)

      // 2. Merge remaining successors states.
      val toMerge = point.succBlocks filterNot { _ == baseSucc}
      mergeImpl(state, baseSucc, toMerge.toSeq, isExceptionMerge = false)

      // 3. Finally check limit and save resulting state for `point`.
      state.checkLimit()
      states(point) = state
    }

    /** Update `state`, merging into it state from `point` xHandler, is one exists. Note, that exceptional edge
      * may be loop continuation. Anyway, `base` successor is `state`'s block itself. */
    def mergeWithHandler(state: State, point: SpinalNode): Unit = {
      mergeImpl(state, point.block, Seq(point.xHandler), isExceptionMerge = true)
    }


    ///////////////////////////////////////////////////////////////////////////////////////////////
    //// Phi points

    /** Replaces phi-functions in `state` to their arguments coming by `controlInput`. Also normalize these
      * arguments (replaces constants and already live arguments to PhiArgCopy nodes). Normalization requires
      * for accurate resources handling, because with normalized phi-arguments we look at real busy resources
      * amount picture. */
    def replacePhiToArgs(state: State, controlInput: Edge): Unit = {
      val block = controlInput.target.block

      val phiesInRegisters = Sets[Node].newImmSet(block.phies filter state.inRegister)
      block.phies foreach state.remove

      for (phi <- block.phies) {
        val edge = phi.phiInput(controlInput)
        val arg = edge.source

        val newArg = if (nodeOnReadOnlyResource(arg) || state.live(arg)) {
          dag.insertCopyForPhiArgument(edge)
        } else {
          arg
        }

        if (phiesInRegisters(phi)) {
          state.moveToRegisters(newArg)
        } else {
          state.moveToStorage(newArg)
        }

        dag.processPhiArg(newArg)
      }
    }

    /** Creates states for all predecessors of already interpreted till the end phi-`point`. Base task is to
      * replace phi-functions to normalized phi-args. Additional task is to generate loop requirements, if
      * `point` is a reducible loop header.
      *
      * TODO: collect requirements for all phi-points, including inside loop ones
      * */
    def createStatesForPhiPointPredecessors(point: Block): Unit = {
      assert(blockStatus(point) == BlockStatus.FULLY_INTERPRETED)
      assert(point.isInstanceOf[BBlock])

      val pointLoop = gcm.loops.loopOf(point)
      val pointState = states(point)

      for (predEdge @ Edge(source, _) <- point.inEdges; pred = source.block) {
        if (states.contains(pred)) {
          // Backward branch of loop.
          assert(pointLoop.header == point)
          assert(blockStatus(pred) == BlockStatus.FULLY_INTERPRETED)

        } else {
          val predLoop = gcm.loops.loopOf(pred)
          if (predLoop == pointLoop) {
            // `point` and `pred` are in the same loop - simple phi replacing.
            val state = new State(pointState.local.copy(), pointState.loop, gcm.cold(pred))
            replacePhiToArgs(state, predEdge)

            state.checkLimit()
            states(pred) = state

          } else {
            // `point` is an enter inside its loop, `pred` is forward branch source.
            assert(pointLoop.isInnerOf(predLoop))

            // 1. Drop required amount of loops
            val droppedLoopsAmount = Loops.depth(pointLoop) - Loops.depth(predLoop)
            val (outerLoopLRS, droppedLoops) = pointState.loop.dropInnerLoops(droppedLoopsAmount)
            val state = new State(pointState.local.copy(), outerLoopLRS, gcm.cold(pred))

            // 2. Replace phi to arguments
            replacePhiToArgs(state, predEdge)

            // 3. Create requirements for dropped loops in predecessor.
            // TODO: describe (or solve) the problem about irreducible loops
            if (droppedLoops.nonEmpty && (droppedLoops forall { _.loop.kind == LoopKind.REDUCIBLE })) {
              assert(pred dominates point) // All reducible loops are normalzied before BGCM
              assert(droppedLoops.size == 1) // Single enter for normalized reducible loop should be exactly from outer loop block
              createRequirementsForDroppedLoop(pred, pointState)
            }

            state.checkLimit()
            states(pred) = state
          }
        }
      }
    }

  }
}
