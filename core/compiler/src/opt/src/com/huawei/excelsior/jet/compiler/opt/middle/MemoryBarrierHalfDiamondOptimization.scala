/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait MemoryBarrierHalfDiamondOptimization { self: Universe =>

  private object HalfDiamond {
    // If given `If` is half-diamond (with one path empty), then return `If`.exit to this empty path
    def unapply(iff: If): Option[If.Exit] = {
      (iff.trueBlock.blockEnd, iff.falseBlock.blockEnd) match {
        case (x: Goto, _) if x.target == iff.falseBlock && iff.falseBlock.phies.isEmpty => // if trueBlock.exit leads to falseBlock, then `false` path is empty
          Some(iff.falseExit)
        case (_, y: Goto) if y.target == iff.trueBlock && iff.trueBlock.phies.isEmpty => // if falseBlock.exit leads to trueBlock, then `true` path is empty
          Some(iff.trueExit)
        case _ =>
          None
      }
    }
  }

  /** Replaces [[HalfDiamondWithNopBarrier]] with MemBarrier, if shouldEliminate(barrier) is True. */
  def memoryBarrierDiamondElimination(shouldEliminate: MemBarrier => Boolean): Unit = {
    /**
      * {{{
      *           If
      *         /    \
      *        |      BBlock
      *        |      MemBarrier
      *        |      Goto
      *         \    /
      *         Block
      * }}}
      */
    object HalfDiamondWithNopBarrier {
      def unapply(mem: MemBarrier): Option[If.Exit] = {
        if (!shouldEliminate(mem)) {
          return None
        }

        val block = mem.block
        block.points.toSeq.reverse match {
          case Seq(`block`, `mem`, _: Goto) => mem.block.inputs match {
            case Seq(x: If.Exit) => x.owner match {
              case HalfDiamond(targetExit) => Some(targetExit)
              case _ => None
            }
            case _ => None
          }
          case _ => None
        }
      }
    }

    for (case mem @ HalfDiamondWithNopBarrier(exit) <- all[MemBarrier]) {
      insertCodeBefore(exit.owner) {
        MemBarrier(mem.kinds)()
      }
      replaceByGoto(exit)
    }
  }
}
