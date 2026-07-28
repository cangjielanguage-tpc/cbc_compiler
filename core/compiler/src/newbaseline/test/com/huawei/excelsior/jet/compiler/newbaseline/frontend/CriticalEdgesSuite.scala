/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.newbaseline.testutils.DSLs.{BlockGraph, BlockGraphBuilderDSL}

/** Tests for critical edges elimination.
  *
  * @see [[com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsSuite]]
  */
class CriticalEdgesSuite extends CompilerSuite with BlockGraphBuilderDSL {

  var splittedEdges: Set[(Block, Block)] = _

  private def eliminate(start: SubGraph): Unit = {
    val BlockGraph(entry, blocks) = makeGraph(start)
    val blocksOriginal = blocks.toSet
    CriticalEdges.eliminate(blocks, entry)

    splittedEdges = (blocks.toSet diff blocksOriginal) map { b =>
      val Seq(src) = b.inputs.toSeq.map(_.block)
      val Seq(dst) = b.end.outputs.toSeq
      checkSplitterPosition(blocks.toSeq, b, src, dst)
      (src, dst)
    }
  }

  private def checkSplitterPosition(blocks: Seq[Block], splitter: Block, src: Block, dst: Block): Unit = {
    splitter.startBC should be (src.endBC)
    splitter.endBC should be (src.endBC)

    def isSplitter(b: Block) = (b.startBC == b.endBC)

    isSplitter(src) should be (false)
    isSplitter(dst) should be (false)
    isSplitter(splitter) should be (true)

    val srcPos = blocks.indexOf(src)
    val splitterPos = blocks.indexOf(splitter)

    // between source block and splitter should be no other normal blocks
    srcPos should be < splitterPos
    for (i <- (srcPos + 1) until splitterPos) {
      isSplitter(blocks(i)) should be (true)
    }
  }

  private def bs(xs: (Int, Int)*) = (xs map { case (x, y) => (b(x), b(y)) }).toSet

  test("critical edges elimination no works") {
    eliminate(0 -> (1 || 2) -> 3)
    splittedEdges should be (empty)
  }

  test("critical edges elimination simple case") {
    eliminate(0 -> ((1 -> 3) || (2 -> (3 || 4))))
    splittedEdges should be (bs((2, 3)))
  }

  test("critical edges elimination self-cycle") {
    eliminate(0 -> wd(1) -> 2)
    splittedEdges should be (bs((1, 1)))
  }

  test("critical edges elimination cycle") {
    eliminate(0 -> dw(1 -> 2) -> 3)
    splittedEdges should be (bs((2, 1)))
  }

  test("critical edges elimination self-cycle-2") {
    eliminate(0 -> dw(1) -> 2)
    splittedEdges should be (bs((1, 1)))
  }

  test("critical edges elimination cycle with break") {
    eliminate(0 -> 1 -> (2 || (3 -> (1 || 4))))
    splittedEdges should be (bs((3, 1)))
  }

  test("critical edges elimination hard case") {
    eliminate(0 -> (dw(1 -> (3 || (4 -> end))) || dw(2 -> ((5 -> 7 -> end) || (6 -> ((7 -> end) || 8))))) -> 9)
    splittedEdges should be (bs((0, 1), (3, 1), (3, 9), (0, 2), (8, 2), (6, 7), (8, 9)))
  }

  test("critical edge in graph entry") {
    eliminate(dw(0 -> 1) -> 2)
    splittedEdges should be (bs((1, 0)))
  }

}

