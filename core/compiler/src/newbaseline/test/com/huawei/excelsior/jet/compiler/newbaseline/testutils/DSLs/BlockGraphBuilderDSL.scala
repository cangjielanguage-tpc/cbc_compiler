/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.testutils.DSLs

import com.huawei.excelsior.jet.compiler.newbaseline.frontend.Block
import com.huawei.excelsior.jet.util.DSLs.GraphBuilderDSL
import org.scalatest.{BeforeAndAfterEach, Suite}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

case class BlockGraph(entry: Block, blocks: ArrayBuffer[Block])

trait BlockGraphBuilderDSL extends GraphBuilderDSL[Block, BlockGraph] with BeforeAndAfterEach { self: Suite =>

  private val blocks = new mutable.HashMap[Int, Block]()

  /** Reset graph builder before new build to suppress interference between builds. */
  override def beforeEach(): Unit = {
    super.beforeEach()
    blocks.clear()
  }

  private val BC_POS_MULTIPLIER = 1000

  protected implicit def int2Block(x: Int): Block = {
    blocks.getOrElseUpdate(x, {
      val b = new Block(x * BC_POS_MULTIPLIER, (x + 1) * BC_POS_MULTIPLIER)
      b.end = new Block.End(null)
      b
    })
  }

  protected def b(x: Int): Block = int2Block(x)

  // It is redundant however scalac does not compile implicit conversions from int to SubGraph
  protected implicit def int2SubGraph(x: Int): SubGraph = node2SubGraph(int2Block(x))

  /** Create simple graph that is backed by list of edges between integers.
    */
  protected def createGraph(startNode: Block, edges: Seq[(Block, Block)]): BlockGraph = {
    edges foreach { case (from, to) =>
      from connectTo to
    }

    val blocks = ArrayBuffer.from((edges flatMap { case (x, y) => Seq(x, y) }).distinct sortBy (_.startBC))
    BlockGraph(startNode, blocks)
  }

}

