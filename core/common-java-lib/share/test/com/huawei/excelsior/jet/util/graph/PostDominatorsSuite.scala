/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL
import com.huawei.excelsior.jet.util.graph.PostDominators
import org.scalactic.source

import scala.language.implicitConversions
import scala.util.chaining.scalaUtilChainingOps

class PostDominatorsSuite extends CommonSuite with IntGraphBuilderDSL {

  private var sink: Int = -1
  private class Sink(x: Int) { def s : Int = { sink = x; x } }
  private implicit def int2sink(x: Int): Sink = new Sink(x)

  private def check(testName: String, start: SubGraph)(action: (PostDominators[N], PostDominators[N]) => Unit)(implicit pos: source.Position): Unit = {
    val s = sink
    sink = -1

    test(testName) {
      makeGraph(start)
      val pd = PostDominators(graph, s)
      val apd = PostDominators.augmented(graph)
      action(pd, apd)
    }
  }

  private def stop = graph.invalidNode

  private case class EqPD(x: PostDominators[N], y: PostDominators[N]) {
    def iPostDom(i: Int) =
      x.iPostDom(i) tap { _ shouldBe y.iPostDom(i) }

    def postDoms(i: Int) =
      (x.postDoms(i).toSeq tap { _ shouldBe (y.postDoms(i).toSeq) }).iterator

    def postDominates(a: Int, b: Int) =
      x.postDominates(a, b) tap { _ shouldBe y.postDominates(a, b) }
  }

  private val undefined = Int.MinValue + 1

  private case class NotEqPD(x: PostDominators[N], y: PostDominators[N]) {
    private def _iPostDom(pd: PostDominators[N], x: N) = try {
      pd.iPostDom(x)
    } catch {
      case _: scala.NoSuchElementException => undefined
    }

    def iPostDom(i: Int) = (_iPostDom(x, i), _iPostDom(y, i))
    def postDoms(i: Int) = (x.postDoms(i), y.postDoms(i))
    def postDominates(a: Int, b: Int) = (x.postDominates(a, b), y.postDominates(a, b))
  }

  private case class BothPDBuilder(x: PostDominators[N]) {
    def && (y: PostDominators[N]) = EqPD(x, y)
  }

  private implicit def pd2BothPDBuilder(pd: PostDominators[N]): BothPDBuilder = BothPDBuilder(pd)
  private implicit def pdTuple2BothPDBuilder(tuple: (PostDominators[N], PostDominators[N])): NotEqPD = NotEqPD(tuple._1, tuple._2)

  check("simple two nodes", 0 -> 1.s) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 1
    (pd && apd).iPostDom(1) shouldBe stop

    (pd && apd).postDoms(0) should beIterator (0, 1)
    (pd && apd).postDoms(1) should beIterator (1)
  }

  check("simple nodes line", 0 -> 1 -> 2 -> 3.s) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 1
    (pd && apd).iPostDom(1) shouldBe 2
    (pd && apd).iPostDom(2) shouldBe 3
    (pd && apd).iPostDom(3) shouldBe stop

    (pd && apd).postDoms(0) should beIterator (0, 1, 2, 3)
    (pd && apd).postDoms(1) should beIterator (1, 2, 3)
    (pd && apd).postDoms(2) should beIterator (2, 3)
    (pd && apd).postDoms(3) should beIterator (3)
  }

  check("diamond", 0 -> (1 || 2) -> 3.s) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 3
    (pd && apd).iPostDom(1) shouldBe 3
    (pd && apd).iPostDom(2) shouldBe 3
    (pd && apd).iPostDom(3) shouldBe stop
  }

  check("several leaves, one selected as sink", 0 -> (1.s || (2 -> 3 -> 4))) { (pd, apd) =>
    (pd, apd).iPostDom(0) shouldBe (1,          stop)
    (pd, apd).iPostDom(2) shouldBe (undefined,  3)
    (pd, apd).iPostDom(3) shouldBe (undefined,  4)
    (pd, apd).iPostDom(4) shouldBe (undefined,  stop)

    (pd && apd).iPostDom(1) shouldBe stop

    (pd, apd).postDominates(2, 3) shouldBe (true, false)

    (pd && apd).postDominates(3, 2) shouldBe true
  }

  check("simple do-while loop", 0 -> dw(1 -> 2) -> 3.s) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 1
    (pd && apd).iPostDom(1) shouldBe 2
    (pd && apd).iPostDom(2) shouldBe 3
    (pd && apd).iPostDom(3) shouldBe stop
  }

  check("simple while-do loop", 0 -> wd(1 -> 2) -> 3.s) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 1
    (pd && apd).iPostDom(1) shouldBe 3
    (pd && apd).iPostDom(2) shouldBe 1
    (pd && apd).iPostDom(3) shouldBe stop
  }

  check("irreducible loop", 0 -> ((dw(1 -> 2 -> 3) -> 4.s) || 2)) { (pd, apd) =>
    (pd && apd).iPostDom(0) shouldBe 2
    (pd && apd).iPostDom(1) shouldBe 2
    (pd && apd).iPostDom(2) shouldBe 3
    (pd && apd).iPostDom(3) shouldBe 4
    (pd && apd).iPostDom(4) shouldBe stop

    (pd && apd).postDominates(3, 0) shouldBe true
    (pd && apd).postDominates(2, 3) shouldBe false
    (pd && apd).postDominates(4, 1) shouldBe true
  }

  check("leaf and endless loop", 0 -> (dw(1 -> 2) || 3.s)) { (pd, apd) =>
    (pd, apd).iPostDom(0) shouldBe (3,          stop)
    (pd, apd).iPostDom(1) shouldBe (undefined,  stop) // endless loop header has edge to stopNode
    (pd, apd).iPostDom(2) shouldBe (undefined,  1)

    (pd && apd).iPostDom(3) shouldBe stop

    (pd, apd).postDominates(3, 0) shouldBe (true, false)
    (pd, apd).postDominates(2, 1) shouldBe (true, false)

    (pd && apd).postDominates(1, 2) shouldBe true
  }

  check("endless loop with diamond inside - augmented", 0 -> dw(1 -> (2 || 3) -> 4)) { (_, apd) =>
    apd.iPostDom(0) shouldBe 1
    apd.iPostDom(1) shouldBe stop // endless loop header has edge to stopNode
    apd.iPostDom(2) shouldBe 4
    apd.iPostDom(3) shouldBe 4
    apd.iPostDom(4) shouldBe 1
  }

  check("endless loop with multiple back edges - augmented", 0 -> dw(1 -> (2 || 3))) { (_, apd) =>
    apd.iPostDom(0) shouldBe 1
    apd.iPostDom(1) shouldBe stop // endless loop header has edge to stopNode
    apd.iPostDom(2) shouldBe 1
    apd.iPostDom(3) shouldBe 1
  }

  check("irreducible endless loop with multiple back edges - augmented", (0 -> dw(1 -> 2 -> 3 -> 4)) |>| (0 -> 2)) { (_, apd) =>
    apd.iPostDom(0) shouldBe 1
    apd.iPostDom(1) shouldBe stop // endless loop header has edge to stopNode
    apd.iPostDom(2) shouldBe 3
    apd.iPostDom(3) shouldBe 4
    apd.iPostDom(4) shouldBe 1
  }
}
