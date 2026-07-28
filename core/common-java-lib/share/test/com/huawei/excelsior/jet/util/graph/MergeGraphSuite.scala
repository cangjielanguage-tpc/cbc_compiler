/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntUGraphBuilderDSL
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.MergeGraph

import scala.collection.mutable
import scala.language.implicitConversions

/**
  * Tests for MergeGraph.
  */
class MergeGraphSuite extends CommonSuite with IntUGraphBuilderDSL {

  case class IntVertex(value: Int) extends MergeGraph.Node[IntVertex]
  val int2vMap = mutable.LinkedHashMap.empty[Int, IntVertex]

  implicit def int2vertex(i: Int): IntVertex = int2vMap.getOrElseUpdate(i, {IntVertex(i)})

  var mergeGraph: MergeGraph[IntVertex] = _

  override def makeGraph(edges: (Int, Int)*): Unit = {
    //super.makeGraph(edges: _*)
    mergeGraph = new MergeGraph[IntVertex]
    int2vMap.clear()
    for ((x, y) <- edges) {
      mergeGraph.connect(x, y)
    }
  }

  override def checkNeighbours(node: Int)(expected: Int*): Unit = {
    mergeGraph.neighboursSet(node) should be ((expected map int2vMap).toSet)
    for (e <- expected) {
      mergeGraph.adjacent(node, e) should be (true)
    }
  }

  def checkNeighbours2(node: Int)(expected: Int*): Unit = {
    val n2 = mergeGraph.focused(node) { _.neighbours2.toList }
    n2.toSet.size should be (n2.size)
    n2.toSet should be ((expected map int2vMap).toSet)
    for (e <- expected) {
      mergeGraph.adjacent(node, e) should be (false)
      (mergeGraph.neighboursSet(node) intersect mergeGraph.neighboursSet(e)).nonEmpty should be (true)
    }
  }

  private def checkGroups(groups: Any*): Unit = {
    for (group <- groups) {
      group match {
        case x: Int =>
          mergeGraph.group(x).toSet should be (Set(int2vMap(x)))
          mergeGraph.delegate(x) should be (int2vMap(x))

        case xsUnchecked: Set[_] =>
          val xs = xsUnchecked.asInstanceOf[Set[Int]] // unchecked cast
          xs foreach { x =>
            mergeGraph.group(x).toSet should be (xs map int2vMap)
          }
          ScalaCollections.uniqueValue(xs map { x => mergeGraph.delegate(x) }) match {
            case Some(x) => xs should contain (x.value)
            case None => shouldNotReachHere()
          }

        case _ => shouldNotReachHere(group)
      }
    }
  }

  private def mergeAllNeighbours2(node: Int): Unit = {
    mergeGraph.focused(node) { f => f.neighbours2 foreach f.merge }
  }

  test("diamond") {
    makeGraph((0, 1), (1, 2), (2, 3), (3, 0))

    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0, 2)
    checkNeighbours(2)(1, 3)
    checkNeighbours(3)(0, 2)

    checkNeighbours2(0)(2)
    checkNeighbours2(1)(3)
    checkNeighbours2(2)(0)
    checkNeighbours2(3)(1)

    checkGroups(0, 1, 2, 3)

    mergeGraph.merge(2, 0)

    checkNeighbours(1)(2)
    checkNeighbours(2)(1, 3)
    checkNeighbours(3)(2)

    checkNeighbours2(1)(3)
    checkNeighbours2(2)()
    checkNeighbours2(3)(1)

    checkGroups(Set(0, 2), 1, 3)

    mergeGraph.merge(3, 1)

    checkNeighbours(2)(3)
    checkNeighbours(3)(2)

    checkNeighbours2(2)()
    checkNeighbours2(3)()

    checkGroups(Set(0, 2), Set(1, 3))
  }

  test("hard graph") {
    makeGraph((0, 1), (0, 3), (1, 2), (1, 4), (1, 5), (4, 5), (3, 4))

    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0, 2, 4, 5)
    checkNeighbours(2)(1)
    checkNeighbours(3)(0, 4)
    checkNeighbours(4)(1, 3 ,5)
    checkNeighbours(5)(1, 4)

    checkNeighbours2(0)(2, 4, 5)
    checkNeighbours2(1)(3)
    checkNeighbours2(2)(0, 4, 5)
    checkNeighbours2(3)(5, 1)
    checkNeighbours2(4)(0, 2)
    checkNeighbours2(5)(0, 2, 3)

    checkGroups(0, 1, 2, 3, 4, 5)

    mergeGraph.merge(3, 2)

    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0, 3, 4, 5)
    checkNeighbours(3)(0, 1, 4)
    checkNeighbours(4)(1, 3 ,5)
    checkNeighbours(5)(1, 4)

    checkNeighbours2(0)(4, 5)
    checkNeighbours2(1)()
    checkNeighbours2(3)(5)
    checkNeighbours2(4)(0)
    checkNeighbours2(5)(0, 3)

    checkGroups(0, 1, Set(2, 3), 4, 5)

    mergeGraph.merge(5, 0)

    checkNeighbours(1)(3, 4, 5)
    checkNeighbours(3)(1, 4, 5)
    checkNeighbours(4)(1, 3 ,5)
    checkNeighbours(5)(1, 3, 4)

    checkNeighbours2(1)()
    checkNeighbours2(3)()
    checkNeighbours2(4)()
    checkNeighbours2(5)()

    checkGroups(Set(0, 5), 1, Set(2, 3), 4)
  }

  test("x-wing") {
    makeGraph((0, 1), (0, 2), (0, 3), (0, 4))

    checkNeighbours(0)(1, 2, 3, 4)
    checkNeighbours(1)(0)
    checkNeighbours(2)(0)
    checkNeighbours(3)(0)
    checkNeighbours(4)(0)

    checkNeighbours2(0)()
    checkNeighbours2(1)(2, 3, 4)
    checkNeighbours2(2)(1, 3, 4)
    checkNeighbours2(3)(1, 2, 4)
    checkNeighbours2(4)(1, 2, 3)

    checkGroups(0, 1, 2, 3, 4)

    mergeGraph.merge(1, 2)

    checkNeighbours(0)(1, 3, 4)
    checkNeighbours(1)(0)
    checkNeighbours(3)(0)
    checkNeighbours(4)(0)

    checkNeighbours2(0)()
    checkNeighbours2(1)(3, 4)
    checkNeighbours2(3)(1, 4)
    checkNeighbours2(4)(1, 3)

    checkGroups(0, Set(1, 2), 3, 4)

    mergeGraph.merge(3, 4)

    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0)
    checkNeighbours(3)(0)

    checkNeighbours2(0)()
    checkNeighbours2(1)(3)
    checkNeighbours2(3)(1)

    checkGroups(0, Set(1, 2), Set(3, 4))

    mergeGraph.merge(1, 3)

    checkNeighbours(0)(1)
    checkNeighbours(1)(0)

    checkNeighbours2(0)()
    checkNeighbours2(1)()

    checkGroups(0, Set(1, 2, 3, 4))
  }

  test("n2iterator merge") {
    makeGraph((0, 1), (1, 2), (1, 3), (2, 3), (3, 4), (4, 5), (5, 6))

    checkNeighbours(0)(1)
    checkNeighbours(1)(0, 2, 3)
    checkNeighbours(2)(1, 3)
    checkNeighbours(3)(1, 2, 4)
    checkNeighbours(4)(3, 5)
    checkNeighbours(5)(4, 6)
    checkNeighbours(6)(5)

    checkNeighbours2(0)(2, 3)
    checkNeighbours2(1)(4)
    checkNeighbours2(2)(0, 4)
    checkNeighbours2(3)(0, 5)
    checkNeighbours2(4)(1, 2, 6)
    checkNeighbours2(5)(3)
    checkNeighbours2(6)(4)

    checkGroups(0, 1, 2, 3, 4, 5, 6)

    mergeAllNeighbours2(2)

    checkNeighbours(1)(2, 3)
    checkNeighbours(2)(1, 3, 5)
    checkNeighbours(3)(1, 2)
    checkNeighbours(5)(2)

    checkNeighbours2(1)(5)
    checkNeighbours2(2)()
    checkNeighbours2(3)(5)
    checkNeighbours2(5)(1, 3)

    checkGroups(1, Set(2, 0, 4, 6), 3, 5)

    mergeAllNeighbours2(3)

    checkNeighbours(1)(2, 3)
    checkNeighbours(2)(1, 3)
    checkNeighbours(3)(1, 2)

    checkNeighbours2(1)()
    checkNeighbours2(2)()
    checkNeighbours2(3)()

    checkGroups(1, Set(2, 0, 4, 6), Set(3, 5))
  }

  test("klappspaten") {
    makeGraph((0, 1), (1, 2), (1, 3), (2, 3))

    checkNeighbours(0)(1)
    checkNeighbours(1)(0, 2, 3)
    checkNeighbours(2)(1, 3)
    checkNeighbours(3)(1, 2)

    checkNeighbours2(0)(2, 3)
    checkNeighbours2(1)()
    checkNeighbours2(2)(0)
    checkNeighbours2(3)(0)

    checkGroups(0, 1, 2, 3)

    mergeAllNeighbours2(0)

    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0, 3)
    checkNeighbours(3)(0, 1)

    checkNeighbours2(0)()
    checkNeighbours2(1)()
    checkNeighbours2(3)()

    checkGroups(Set(0, 2), 1, 3)
  }

  test("multi-edges") {
    makeGraph((0, 1), (0, 2), (0, 1), (0, 2), (0, 1))

    checkNeighbours(0)(1, 2)
    checkNeighbours(1)(0)
    checkNeighbours(2)(0)

    checkNeighbours2(1)(2)
    checkNeighbours2(2)(1)

    mergeAllNeighbours2(1)

    checkGroups(Set(1, 2), 0)
  }
}
