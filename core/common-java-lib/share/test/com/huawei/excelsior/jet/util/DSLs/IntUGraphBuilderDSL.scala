/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import com.huawei.excelsior.jet.util.graph.UGraph
import org.scalatest.matchers.should.Matchers

/** DSL for creation an UGraph of Integers.
  *
  * @author conwor
  */
trait IntUGraphBuilderDSL extends Matchers {

  var graph: UGraph[Int] = _

  def checkNeighbours(node: Int)(expected: Int*): Unit = {
    graph.neighboursSet(node) should be (expected.toSet)
    for (e <- expected) {
      graph.adjacent(node, e) should be (true)
    }
  }

  def makeGraph(edges: (Int, Int)*): Unit = {
    graph = new UGraph[Int] {
      override def neighbours(node: Int): Iterator[Int] = (edges collect {
        case (x, `node`) => x
        case (`node`, x) => x
      }).iterator

      override def adjacent(x: Int, y: Int): Boolean = edges exists {
        e => (e._1 == x && e._2 == y) || (e._2 == x && e._1 == y)
      }
    }
  }

}
