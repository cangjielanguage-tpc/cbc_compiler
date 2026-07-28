/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL
import com.huawei.excelsior.jet.util.graph.LoopKind.*
import com.huawei.excelsior.jet.util.graph.ordering.DepthFirstSearch
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind, Loops}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Tests for loops analyzing.
 */
class LoopsRecognizerSuite extends CommonSuite with IntGraphBuilderDSL {

  var loops: Loops[N] = _
  var dfs: DepthFirstSearch[N] = _

  private def findLoopsIn(start: SubGraph): Unit = {
    makeGraph(start)
    dfs = DepthFirstSearch(graph)
    loops = graph.loops
  }

  private def expectLoops(expected: Iterable[Loop[N]]*): Unit = {

    val actualLoops = loops.seq.sortBy(_.header)(dfs).toBuffer
    val expectedLoops = expected.flatten.sortBy(_.header)(dfs).toBuffer

    while (true) {
      (actualLoops.isEmpty, expectedLoops.isEmpty) match {
        case (true, true) => return
        case (true, false) => fail("There are not enough loops. Expected more: " + expectedLoops.mkString(", "))
        case (false, true) => fail("There are some unexpected loops. Not expected: " + actualLoops.mkString(", "))
        case (false, false) =>
      }

      val act = actualLoops.remove(0)
      val exp = expectedLoops.remove(0)

      assert(act.header === exp.header, "Bad header: " + act + " != " + exp)
      assert(act.kind === exp.kind, "Bad kind: " + act + " != " + exp)
      assert(act.body.toSeq.sortBy { x => x } === exp.body.toSeq.sortBy { x => x }, "Bad body: " + act + " != " + exp)

      assert((act.outer != null) === (exp.outer != null), "Bad outer loop: " + act.outer + " != " + exp.outer)
      if (act.outer != null && exp.outer != null) {
        assert(act.outer.header === exp.outer.header, "Bad outer loop's header: " + act.outer + " != " + exp.outer)
      }
    }
  }

  private def loop(kind: LoopKind, header: N, body: N*) = {
    loops(kind, header, body: _*)()
  }

  private def loops(kind: LoopKind, header: N, body: N*)(innerLoopSets: Iterable[Loop[N]]*): Iterable[Loop[N]] = {
    val bodySet = mutable.Set(body: _*)
    bodySet += header
    val outer = new Loop[N](kind, header, bodySet)

    val loopSet = new ArrayBuffer[Loop[N]]
    loopSet += outer

    for (innerLoop <- innerLoopSets.flatten) {
      if (innerLoop.outer == null) {
        innerLoop.outer = outer
      }
      outer.body ++= innerLoop.body
      loopSet += innerLoop
    }

    loopSet
  }

  private def expectDepth(n: N, number: Int): Unit = {
    loops.depth(n) should be (number)
  }

  private def expectInLoop(n: N): Unit = {
    loops.loopOf(n) should (not be (null))
  }

  private def expectNotInLoop(n: N): Unit = {
    loops.loopOf(n) should be (null)
  }

  test("graph without loops") {
    findLoopsIn(0 -> 1 -> 2)
    expectLoops()
  }

  test("graph with one-node loop") {
    findLoopsIn(dw(0))
    expectLoops(loop(SELF, 0))
  }

  test("graph with simple reducible do-while loop") {
    findLoopsIn(0 -> dw(1 -> 2 -> 3) -> 4)
    expectLoops(loop(REDUCIBLE, 1, 2, 3))
  }

  test("graph with simple reducible while-do loop") {
    findLoopsIn(0 -> wd(1 -> 2 -> 3) -> 4)
    expectLoops(loop(REDUCIBLE, 1, 2, 3))
  }

  test("graph with reducible loop at start") {
    findLoopsIn(dw(0 -> 1) -> 2)
    expectLoops(loop(REDUCIBLE, 0, 1))
  }

  test("graph with two loops with one header") {
    findLoopsIn(dw(0 -> (1 || 2)))
    expectLoops(loop(REDUCIBLE, 0, 1, 2))
  }

  test("graph with two loops with one header and one of them is self-loop") {
    findLoopsIn(dw(0 -> (0 || 1)))
    expectLoops(loop(REDUCIBLE, 0, 1))
  }

  test("graph with nested reducible loop") {
    findLoopsIn(0 -> dw(1 -> dw(2 -> 3) -> 4) -> 5)
    expectLoops(loops(REDUCIBLE, 1, 2, 4)(
                  loop(REDUCIBLE, 2, 3)
                ))
  }

  test("graph with irreducible loop with DFS 1") {
    findLoopsIn(0 -> (1 -> 2 || 2 -> 1) )
    dfs.order.toSeq match {
      case Seq(0, 1, 2) => expectLoops(loop(IRREDUCIBLE, 1, 2))
      case Seq(0, 2, 1) => expectLoops(loop(IRREDUCIBLE, 2, 1))
    }
  }

  test("graph with irreducible loop with DFS 2") {
    findLoopsIn(0 -> (2 -> 1 || 1 -> 2) )
    dfs.order.toSeq match {
      case Seq(0, 1, 2) => expectLoops(loop(IRREDUCIBLE, 1, 2))
      case Seq(0, 2, 1) => expectLoops(loop(IRREDUCIBLE, 2, 1))
    }
  }

  test("graph with two pathes to irreducible loop") {
    findLoopsIn(0 -> ((3 -> 4) || (1 -> dw(2 -> 4 -> 5) -> 0)))
    dfs.order.toSeq match {
      case Seq(0, 1, 2, 4, 5, 3) => {
        expectLoops(loops(REDUCIBLE, 0, 1, 2, 3)(
                      loop(IRREDUCIBLE, 2, 4, 5)
                    ))
      }
      case Seq(0, 3, 4, 5, 2, 1) => {
        expectLoops(loops(REDUCIBLE, 0, 3, 4, 1)(
                      loop(IRREDUCIBLE, 4, 2, 5)
                    ))
      }
    }
  }

  test("graph with one nested loop (nesting)") {
    findLoopsIn(0 -> dw(1 -> 2 -> 3) -> 4)
    expectLoops(loop(REDUCIBLE, 1, 2, 3))
    expectDepth(0, 0)
    expectDepth(4, 0)
    expectDepth(1, 1)
    expectDepth(2, 1)
    expectDepth(3, 1)
  }

  test("graph with one nested loop (detecting loops)") {
    findLoopsIn(0 -> dw(1 -> 2 -> 3) -> 4)
    expectNotInLoop(0)
    expectInLoop(1)
    expectInLoop(2)
    expectInLoop(3)
    expectNotInLoop(4)
  }

  test("graph with two nested loops") {
    findLoopsIn(0 -> dw(1 -> 2 -> dw(3 -> 4 -> 5) -> 6) -> 7)
    expectLoops(loops(REDUCIBLE, 1, 2, 3, 6)(
                  loop(REDUCIBLE, 3, 4, 5)
                ))
    expectDepth(0, 0)
    expectDepth(7, 0)
    expectDepth(1, 1)
    expectDepth(2, 1)
    expectDepth(6, 1)
    expectDepth(3, 2)
    expectDepth(4, 2)
    expectDepth(5, 2)
  }

  test("graph with many nested loops") {
    findLoopsIn(dw(0 -> ((dw(1 -> 2) -> 3) || (dw(4 -> dw(5) -> 6) -> dw(7 -> 8)))))
    expectLoops(loops(REDUCIBLE, 0, 1, 4, 7, 3)(
                  loop(REDUCIBLE, 1, 2),
                  loops(REDUCIBLE, 4, 5, 6)(
                    loop(SELF, 5)
                  ),
                  loop(REDUCIBLE, 7, 8)
                ))
    expectDepth(0, 1)
    expectDepth(1, 2)
    expectDepth(2, 2)
    expectDepth(3, 1)
    expectDepth(4, 2)
    expectDepth(5, 3)
    expectDepth(6, 2)
    expectDepth(7, 2)
    expectDepth(8, 2)
  }

  test("loop with parallel backedges") {
    findLoopsIn((0 -> lp(1 -> 2 -> 4, exits(2)) -> 3) |>| (4 -> 1))
    expectLoops(loop(REDUCIBLE, 1, 2, 4))
  }

  test("loop with backpreds from one inner loop body") {
    findLoopsIn(lp(0 -> dw(1 -> 2 -> (4 || 3 -> 4)), exits(3, 4)))
    expectLoops(loops(REDUCIBLE, 0)(
                  loop(REDUCIBLE, 1, 2, 3, 4)))
  }

  test("graph with some unreachable nodes") {
    findLoopsIn((0 -> 1 -> 2) |>| (4 -> 1))
    expectLoops()
    expectNotInLoop(4)
    expectNotInLoop(5)
  }

  test("graph with simple reducible do-while loop and some unreachable nodes") {
    findLoopsIn((0 -> dw(1 -> 2 -> 3) -> 4) |>| (5 -> 2))
    expectLoops(loop(REDUCIBLE, 1, 2, 3))
    expectDepth(0, 0)
    expectDepth(1, 1)
    expectDepth(2, 1)
    expectDepth(3, 1)
    expectDepth(4, 0)
    expectDepth(5, 0)
  }

  test("graph with unreachable loop") {
    findLoopsIn((0 -> 1 -> 2) |>| (dw(3) -> 1))
    expectLoops()
    expectDepth(0, 0)
    expectDepth(1, 0)
    expectDepth(2, 0)
    expectDepth(3, 0)
  }

  test("halt after loop that looks like inside loop") {
    findLoopsIn(0 -> dw(1 -> (!9 || 2)) -> 3)
    expectLoops(loop(REDUCIBLE, 1, 2)) // halt block is not in SCC of loop
  }

  test("infinite loop that looks like inner loop") {
    findLoopsIn(0 -> dw(1 -> (!dw(9) || 2)) -> 3 |>| (9 -> 3))
    expectLoops(
      loop(REDUCIBLE, 1, 2),
      loop(SELF, 9)) // infinite loop is not in SCC of the first loop
  }

}

