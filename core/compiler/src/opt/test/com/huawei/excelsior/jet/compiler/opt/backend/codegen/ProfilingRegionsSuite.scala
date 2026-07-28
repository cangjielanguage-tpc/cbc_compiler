/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.IRBuilderDSL

import scala.collection.mutable.ArrayBuffer
import scala.language.{implicitConversions, postfixOps}

class ProfilingRegionsSuite extends CompilerSuite with IRBuilderDSL with ProfilingRegions with BackEndAmd64 {

  private var regions: collection.Map[Block, Int] = _

  private def region(blocks: Block*): Unit = {
    val id = regions(blocks.head)
    for (block <- cfg.collectReachableFrom(0)) {
      if (blocks.contains(block)) {
        regions(block) shouldBe id
      } else {
        regions(block) shouldNot be(id)
      }
    }
  }

  private def noRegion(blocks: Block*): Unit = {
    region(blocks: _*)
    regions(blocks.head) shouldBe -1
  }

  private val markers = new ArrayBuffer[Block]()
  private class Marked(x: Int) { def m : Block = { markers += x; x } }
  private implicit def int2marked(x: Int): Marked = new Marked(x)

  private def setGraph(g: SubGraph): Unit = {
    makeCFG(g)
    regions = findProfilingRegions(cfg, markers.toSeq)
    markers.clear()
  }

  test("one node, no markers") {
    setGraph(0)

    noRegion(0)
  }

  test("one node, one marker") {
    setGraph(0.m)

    region(0)
  }

  test("simple line, one marker") {
    setGraph(0 -> 1 -> 2.m -> 3)

    region(0, 1, 2, 3)
  }

  test("simple line, two markers") {
    setGraph(0 -> 1.m -> 2 -> 3.m -> 4)

    region(0, 1, 2, 3, 4)
  }

  test("diamond - no markers") {
    setGraph(0 -> (1 || 2) -> 3)

    noRegion(0, 1, 2, 3)
  }

  test("diamond - marker on top") {
    setGraph(0.m -> (1 || 2) -> 3)

    region(0, 1, 2, 3)
  }

  test("diamond - marker on one side") {
    setGraph(0 -> (1.m || 2) -> 3)

    region(1)
    noRegion(0, 2, 3)
  }

  test("diamond - marker on bottom") {
    setGraph(0 -> (1 || 2) -> 3.m)

    region(0, 1, 2, 3)
  }

  test("diamond - markers on top and on one side") {
    setGraph(0.m -> (1.m || 2) -> 3)

    region(0, 2, 3)
    region(1)
  }

  test("diamond - markers on both sides") {
    setGraph(0 -> (1.m || 2.m) -> 3)

    region(1)
    region(2)
    noRegion(0, 3)
  }

  test("diamond - markers on side and bottom") {
    setGraph(0 -> (1 || 2.m) -> 3.m)

    region(2)
    region(0, 1, 3)
  }

  test("diamond - markers on top and bottom") {
    setGraph(0.m -> (1 || 2) -> 3.m)

    region(0, 1, 2, 3)
  }

  test("loop with markers around it") {
    setGraph(0.m -> 1 -> dw(2 -> 3 -> 4) -> 5 -> 6.m)

    region(0, 1, 5, 6)
    noRegion(2, 3, 4)
  }

  test("loop with marker inside") {
    setGraph(0 -> 1 -> dw(2 -> 3.m -> 4) -> 5 -> 6)

    region(0, 1, 2, 3, 4, 5, 6)
  }

  test("loop with markers around and inside") {
    setGraph(0.m -> 1 -> dw(2 -> 3.m -> 4) -> 5 -> 6.m)

    region(0, 1, 6)
    region(2, 3, 4, 5)
  }

  test("loop with markers around and inside on entry") {
    setGraph(0.m -> 1 -> dw(2.m -> 3 -> 4) -> 5 -> 6.m)

    region(0, 1, 6)
    region(2, 3, 4, 5)
  }

  test("loop with markers around and inside on exit") {
    setGraph(0.m -> 1 -> dw(2 -> 3 -> 4.m) -> 5 -> 6.m)

    region(0, 1, 6)
    region(2, 3, 4, 5)
  }

  test("diamond with loop on one side, marker on top of diamond") {
    setGraph(0.m -> (1 || (2 -> dw(3 -> 4 -> 5) -> 6)) -> 7)

    region(0, 1, 2, 6, 7)
    noRegion(3, 4, 5)
  }

  test("diamond with loop on one side, marker on bottom of diamond") {
    setGraph(0 -> (1 || (2 -> dw(3 -> 4 -> 5) -> 6)) -> 7.m)

    region(0, 1, 2, 6, 7)
    noRegion(3, 4, 5)
  }

  test("diamond with loop on one side, markers on top and bottom of diamond") {
    setGraph(0.m -> (1 || (2 -> dw(3 -> 4 -> 5) -> 6)) -> 7.m)

    region(0, 1, 2, 6, 7)
    noRegion(3, 4, 5)
  }

  test("diamond with loop on one side, markers on top of diamond and inside loop") {
    setGraph(0.m -> (1 || (2 -> dw(3 -> 4.m -> 5) -> 6)) -> 7)

    region(0, 1, 7)
    region(2, 3, 4, 5, 6)
  }

  test("several diamonds, one region") {
    setGraph(0.m -> (1 || 2) -> 3.m -> (4 || 5) -> 6.m -> (7 || 8) -> 9.m)

    region(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("several diamonds, several regions because of loop") {
    setGraph(0.m -> (1 || 2) -> 3.m -> (4 || dw(5)) -> 6.m -> (7 || 8) -> 9.m)

    region(0, 1, 2, 3, 4, 6, 7, 8, 9)
    noRegion(5)
  }

  test("several diamonds, several regions because of mark on diamond side") {
    setGraph(0.m -> (1 || 2) -> 3.m -> (4 || 5.m) -> 6.m -> (7 || 8) -> 9.m)

    region(0, 1, 2, 3, 4, 6, 7, 8, 9)
    region(5)
  }

  test("huge graph") {
    setGraph(0.m -> ((1 -> ((3 -> 7 -> 9.m) || (4.m -> 5 -> 7))) || (2 -> (5 || (6.m -> 8 -> 9)))))

    region(0, 1, 2, 3, 5, 7, 9)
    region(4)
    region(6, 8)
  }
}