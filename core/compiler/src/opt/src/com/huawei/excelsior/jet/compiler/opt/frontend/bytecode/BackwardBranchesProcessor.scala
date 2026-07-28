/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.bytecode

import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import scala.collection.mutable.ListBuffer

/**
 * This class process constructed CFG by replacing pattern:
 *
 *   block `x` with (p1, p2, ..., pn, q1, q2, ..., qm) predecessors &
 *   - RPO(pi) < RPO(x) &     // p1, ..., pn are forward predecessors
 *   - RPO(qi) > RPO(x) &     // q1, ..., qm are backward predecessors
 *   - RPO(qi) < RPO(qi+1)    // q1, ..., qm are sorted
 *
 *   where RPO is reverse post order (top sort)
 *
 * to
 *
 *   empty block e0 with (p1, p2, ..., pn) predecessors with original order
 *
 *   r1, ..., rk empty blocks with predecessors:
 *     r1 - (q1, ..., qr1)
 *     r2 - (qr1+1, ..., qr2)
 *     ...
 *     rk - (qrk-1+1, ..., qm)
 *   with rule that all predecessors, collected in each ri block does not dominate each other.
 *   Next r-block created for exit, which dominates all predecessors, collected in previous r-block.
 *
 *   empty block e1 with (e0, rk) predecessors
 *   empty block e2 with (e1, rk-1) predecessors
 *   ...
 *   block x with (ek-1, r1) predecessors
 *
 * This process is required for efficient bytecode decompilation, where bytecode has cycles.
 * Transformation with r-blocks required for more natural loops recognition.
 *
 * @author conwor
 */
trait BackwardBranchesProcessor { self: Universe =>

  /** Process blocks, suitable to defined pattern, and iterable processing them. */
  def processBackwardBranches(): Unit = {
    if (!cfg.hasBackwardEdges) {
      return
    }

    val ts = cfg.topSort

    // Blocks with backward predecessors.
    val worklist = ts.order collect { case bb: BBlock if bb.predBlocks exists { x => ts.contains(x) && ts.gteq(x, bb) } => bb }

    // For each block `x` in the worklist:
    // - create chain of empty blocks, each linked with one backward predecessor of `x`
    // - all forward predecessors connected to first block in chain
    // - last element of chain is `x`
    for (x: Block <- worklist) {

      /** Creates block node with given inputs and returns its goto. */
      def makeBlockWithGoto(inputs: Node*): BlockEnd = withPos(x) {
        val block = BBlock(inputs: _*)
        Goto(block, block)
      }

      /** Collects inputs in block and return goto from this block or returns input if it is single. */
      def collectInputs(inputs: collection.Seq[Node]): Node = inputs.size match {
        case 1 => inputs.head
        case _ => makeBlockWithGoto(inputs.toSeq: _*)
      }

      val (forwards, backsUnsorted) = x.inputs partition { (n: Node) => !ts.contains(n.block) || ts.lt(n.block, x) }

      // Levels are predecessors, collected for r-blocks.
      // For cycle [dw(0 -> (1 || 2))] there would be one level (1, 2)
      // For cycle [dw(dw(0 -> 1) -> 2)] there would be two levels (1) and (2)
      var levels = List[ListBuffer[ControlNode]]()
      for (back <- backsUnsorted.sortBy { _.block }(ts.reverse)) {
        if (levels.isEmpty || (levels.head forall { exit => back.block strictDominates exit.block })) {
          levels = ListBuffer(back) :: levels
        } else {
          levels.head += back
        }
      }
      stats.count(StatsKind.LoopType, "with " + levels.count(_.size > 1) + " diamond exits")

      val backs = levels map collectInputs

      val e1 = (backs.init foldLeft x) { (b: Block, backEdge: Node) =>
        // create block ei and two edges (ei -> b) and (backedge -> ei); return ei
        val goto = makeBlockWithGoto()
        b.replaceArgs(goto, backEdge)
        goto.block
      }

      e1.replaceArgs(collectInputs(forwards), backs.last)
    }
  }

}