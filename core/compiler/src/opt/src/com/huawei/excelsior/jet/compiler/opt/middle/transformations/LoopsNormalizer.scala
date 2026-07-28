/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations

import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind, Loops}

/** Loop normalization utilities.
  *
  * A loop is considered normalized iff it satisfies all of the following conditions:
  *   - it is reducible;
  *   - it has exactly one forward edge and exactly one backward edge ''in that particular order''.
  *
  * Note: normalized loop may have no [[com.huawei.excelsior.jet.compiler.opt.ir.Toolbox.isPreHeaderOf pre-header]]
  *       in case of critical forward edge.
  *
  * @author cypok
  * @author liontiger
  */
trait LoopsNormalizer { this: Universe =>

  /** Attempts to normalize all loops. Returns true if IR was modified. */
  def normalizeAllLoops(): Boolean = normalizeLoops(cfg.loops.iterator)

  /** Attempts to normalize given loops. Returns true if IR was modified. */
  def normalizeLoops(loops: Iterator[Loop[Block]]): Boolean = {
    var changed = false
    for (loop <- loops) {
      changed |= normalizeLoop(loop)
    }
    changed
  }

  /** Attempts to normalize given loop. Returns true if IR was modified. */
  def normalizeLoop(loop: Loop[Block]): Boolean = {
    val header = loop.header

    def normalizeWithTwoEdges(): Boolean = {
      val Seq(x, y) = header.inEdges.toList
      if (loop.body contains y.source.block) {
        // x is forward, y is backward => already normalized
        false

      } else {
        // x is backward, y is forward => move x to the end
        Block.addEdgeWithTemplate(x.source, x)
        Block.removeEdge(x)
        true
      }
    }

    header match {
      case _ if loop.kind == LoopKind.IRREDUCIBLE =>
        // Irreducible loop cannot be normalized.
        false

      case _ if header.arity == 2 =>
        normalizeWithTwoEdges()

      case _: XBlock =>
        // Currently we can't normalize loop with XBlock header and arity > 2 (see Block.extractInputEdges).
        // TODO: support it
        false

      case header: BBlock =>
        val (backwardEdges, forwardEdges) = loop.header.inEdges.toList partition (loop.body contains _.source.block)
        assert(backwardEdges.nonEmpty && forwardEdges.nonEmpty)

        if (forwardEdges.size > 1) {
          Loops.addToBody(loop.outer, BBlock.extractInputEdges(header, forwardEdges))
        }

        if (backwardEdges.size > 1) {
          Loops.addToBody(loop, BBlock.extractInputEdges(header, backwardEdges))
        }

        normalizeWithTwoEdges()

        true
    }
  }

  /** Returns true if given `loop` is normalized. */
  def isNormalizedLoop(loop: Loop[Block]): Boolean = {
    loop.kind != LoopKind.IRREDUCIBLE &&
      loop.header.arity == 2 &&
      !(loop.body contains loop.header.inputs.head.block)
  }

  def normalizedLoopHeaderEdges(loop: Loop[Block]): (Edge, Edge) = {
    assert(isNormalizedLoop(loop))
    val Seq(enterEdge, backwardEdge) = loop.header.inEdges.toSeq
    (enterEdge, backwardEdge)
  }

}
