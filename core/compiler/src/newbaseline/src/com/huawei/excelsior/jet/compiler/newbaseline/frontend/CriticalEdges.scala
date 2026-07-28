/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.frontend

import com.huawei.excelsior.jet.compiler.newbaseline.DEBUG_PRINT

import scala.collection.mutable

object CriticalEdges {

  /** Breaks all critical edges via adding new splitter edges. */
  def eliminate(blocks: mutable.Buffer[Block], entryBlock: Block): Unit = {
    if (blocks.size == 1) {
      assert(blocks.head.end.outputs.size <= 1)
      return
    }

    var hadCriticalEdges = false

    // We iterate with `while` instead of `for` here because `blocks` may be increased
    // during iteration, therefore, `blocks.size` must be recalculated on every step.
    //
    // Iterating with Scala numeric range isn't suitable here since ranges have
    // fixed bounds computed at the time of creation of the range object.
    {
      var blockIdx = 0
      while (blockIdx < blocks.size) {
        val block = blocks(blockIdx)
        val inputs = block.inputs
        val inputsSize = inputs.size +
          (if (block == entryBlock) 1 else 0) +
          (if (block.isHandler) 1 else 0)

        if (inputsSize > 1) {

          for ((inEnd, i) <- inputs.zipWithIndex) {
            val outputs = inEnd.outputs
            if (outputs.size > 1) {
              val j = outputs.indexOf(block)
              assert(j >= 0, "inconsistent control flow")
              // critical edge: inEnd[j] -> block[i]
              splitEdge(blocks, inEnd, j, block, i)
              hadCriticalEdges = true
            }
          }
        }
        blockIdx += 1
      }
    }
    if (DEBUG_PRINT && hadCriticalEdges) {
      println()
      println("Blocks after critical edges elimination:")
      for (b <- blocks) {
        println("  " + b.description)
      }
      println()
    }
  }

  private def splitEdge(blocks: mutable.Buffer[Block], src: Block.End, srcIdx: Int, dst: Block, dstIdx: Int): Unit = {
    val srcBlock = src.block
    val splitter = new Block(srcBlock.endBC, srcBlock.endBC)
    splitter.end = new Block.End(Block.End.Kind.GOTO)

    src.outputs(srcIdx) = splitter
    splitter.inputs += src
    splitter.end.outputs += dst
    dst.inputs(dstIdx) = splitter.end

    // Add splitter after its source to keep good order of blocks for TopSort
    blocks.insert(blocks.indexOf(srcBlock) + 1, splitter)
  }
}
