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

import scala.collection.mutable

/** Tests for [[LayoutComponent]]. */
class LayoutComponentSuite extends CompilerSuite with IRBuilderDSL with LayoutComponent with BackEndAmd64 {

  private def cold = Int.MinValue

  private val redirectMap = mutable.Map.empty[Block, Block]

  private def checkLayout(expected: Int*): Unit = {
    val (xs, ys) = expected.span(_ != cold)
    val expectedHotBlocks = xs map int2Block
    val expectedColdBlocks = if (ys.isEmpty) Seq() else ys.tail map int2Block
    val expectedBlocks = expectedHotBlocks ++ expectedColdBlocks

    val detectedColdBlocks = findColdBlocks()
    val order = makeLayout(redirectMap).order

    (detectedColdBlocks &~ Set(entryBlock)) should equal(expectedColdBlocks.toSet)
    order should equal(entryBlock +: expectedBlocks)
    redirectMap.clear()
  }

  private def makeCold(bs: Block*): Unit = {
    bs foreach (_.markAsCold())
  }

  private def makeRedirect(pairs: (Block, Block)*): Unit = {
    for ((x, y) <- pairs) redirectMap(x) = y
  }

  test("simple sequence") {
    makeCFG(0 -> 1 -> 2 -> 3)
    checkLayout(0, 1, 2, 3)
  }

  test("simple diamond") {
    makeCFG(0 -> (1 || 2) -> 3)
    checkLayout(0, 1, 2, 3)
  }

  test("simple triangle") {
    makeCFG(0 -> (1 -> 2 || 2))
    checkLayout(0, 1, 2)
  }

  test("simple do-while loop") {
    makeCFG(0 -> dw(1 -> (2 || 3)) -> 4)
    checkLayout(0, 1, 2, 3, 4)
  }

  test("simple while-do loop") {
    makeCFG(0 -> wd(1 -> 2-> (3 || 4) -> 5) -> 6)
    checkLayout(0, 2, 3, 4, 5, 1, 6)
  }

  test("nested loops 1") {
    makeCFG(0 -> wd(1 -> 2-> wd(3 -> 4) -> 5) -> 6)
    checkLayout(0, 2, 4, 3, 5, 1, 6)
  }

  test("nested loops 2") {
    makeCFG(0 -> wd(1 -> 2-> wd(3 -> 4) -> dw(5 -> 6 -> dw(7) -> 8) -> 9) -> 10)
    checkLayout(0, 2, 4, 3, 5, 6, 7, 8, 9, 1, 10)
  }

  test("infinite loop") {
    makeCFG(0 -> !dw(1 -> 2))
    checkLayout(0, 1, 2)
  }

  test("irreducible loop") {
    makeCFG(0 -> ((wd(1 -> 2 -> 3) -> 4) || 2))
    checkLayout(0, 2, 3, 1, 4)
  }

  test("nested irreducible loops") {
    makeCFG(0 -> ((wd(1 -> 2 -> wd(3 -> 4 -> 5) -> 6) -> 7) || 4))
    checkLayout(0, 2, 4, 5, 3, 6, 1, 7)
  }

  test("irreducible loop nested in reducible") {
    makeCFG(0 -> wd(1 -> 2 -> ((wd(3 -> 4 -> 5) -> 6) || 4) -> 7) -> 8)
    checkLayout(0, 2, 4, 5, 3, 6, 7, 1, 8)
  }

  test("two loop exits") {
    makeCFG(0 -> lp(1 -> 2 -> 3 -> 4, exits(1, 3)) -> 5)
    checkLayout(0, 4, 1, 2, 3, 5)
  }

  test("nested loop with two loop exits") {
    makeCFG(1 -> wd(10 -> 17 -> wd(19 -> 26 -> (31 -> (40 || (36 -> !53)) || 58) -> 43) -> 47) -> 50 -> 53)
    //checkLayout(1, 17, 26, 31, 40, 58, 43, 19, 47, 10, 50, 36, 53)
    checkLayout(1, 40, 58, 43, 19, 47, 10, 17, 26, 31, 36, 50, 53)
  }

  test("many loop exits") {
    makeCFG(0 -> dw(1 -> (2 || (3 -> (!4 || 5))) -> 6 -> (7 || (8 -> (9 || !10))) -> 11 -> (12 || (13 -> (!14 || 15))) -> 16) -> 17)
    checkLayout(0, 1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 17, 14, 10, 4)
  }

  test("merging control flow after many loop exits") {
    makeCFG(0 -> dw(1 -> (2 || (3 -> (!4 || 5))) -> 6 -> (7 || (8 -> (9 || 10 -> !18))) -> 11 -> (12 || (13 -> (!14 || 15))) -> 16) -> 17 -> 18)
    checkLayout(0, 1, 2, 3, 5, 6, 7, 8, 9, 11, 12, 13, 15, 16, 17, 14, 10, 18, 4)
  }

  test("siberian simple") {
    makeCFG(0 -> wd(1 -> 2 -> (3 || 4) -> 5) -> 6)
    makeCold(3)
    checkLayout(0, 2, 4, 5, 1, 6, cold, 3)
  }

  test("siberian spreading and loop") {
    makeCFG(0 -> wd(1 -> 2 -> ((3 -> 4-> wd(5 -> 6) -> 7 -> (8 || 9) -> 10) || 11) -> 12) -> 13)
    makeCold(4)
    checkLayout(0, 2, 11, 12, 1, 13, cold, 3, 4, 6, 5, 7, 8, 9, 10)
  }

  test("several siberian regions") {
    makeCFG(0 -> ((1 -> wd(2 -> 3) -> 4) || (5 -> (6 || 7) -> 8 -> ((wd(9 -> 10 -> 11) -> 12) || 13) -> 14)) -> 15)
    makeCold(1, 6, 9)
    checkLayout(0, 5, 7, 8, 13, 14, 15, cold, 1, 3, 2, 4, 6, 10, 11, 9, 12)
  }

  test("several entries into one siberian region") {
    makeCFG(0 -> ((1 -> (2 || 3) -> 4) || (5 -> (3 || 6) -> 7)) -> 8)
    makeCold(1, 3)
    checkLayout(0, 5, 6, 7, 8, cold, 1, 2, 3, 4)
  }

  test("infinite loops and siberian regions") {
    makeCFG(0 -> ((!dw(1 -> (2 || 3) -> 4)) || (5 -> !dw(6 -> 7))))
    makeCold(2, 7)
    checkLayout(0, 1, 3, 4, cold, 2, 5, 6, 7)
  }

  test("siberian region should not spread up to the loop") {
    makeCFG(0 -> ((dw(1 -> 2) -> 3) || 4) -> 5)
    makeCold(3)
    checkLayout(0, 1, 2, 4, 5, cold, 3)
  }

  test("basic block reachable from several siberian region entry points") {
    makeCFG(0 -> ((1 -> (2 || (3 -> (4 || 5)))) || (6 -> (7 || (8 -> (9 || 4))))) -> 10)
    makeCold(3, 8)
    checkLayout(0, 1, 2, 6, 7, 10, cold, 3, 5, 8, 9, 4)
  }

  test("siberian region spanning several loops ") {
    makeCFG(0 -> dw(1 -> 2 -> ((dw(3 -> 4 -> ((5 -> (3 || 8)) || 6) -> 7) -> 8) || 9) -> 10))
    makeCold(5, 8)
    checkLayout(0, 9, 10, 1, 2, 3, 4, 6, 7, cold, 5, 8)
  }

  test("irreducible loop is broken to disjoint regions") {
    makeCFG(0 -> ((200 -> 1700 -> ((1789 -> 35 -> 200) || !1707)) || 35))
    makeCold(1789)
    checkLayout(0, 35, 200, 1700, 1707, cold, 1789)
  }

  test("irreducible loop overlapping siberian region") {
    makeCFG(0 -> ((510 -> 30) || (161 -> 171 -> 121 -> ((601 -> !77) || (612 -> (77 || (30 -> 171)))))))
    makeCold(510, 612)
    checkLayout(0, 161, 171, 121, 601, 77, cold, 510, 612, 30)
  }

  test("irreducible nested loops with entry to inner") {
    makeCFG(0 -> ((30 -> dw(1 -> 2 -> dw(3 -> 4 -> 10 -> 5) -> 6) -> 7) || (20 -> (10 || 2))))
    checkLayout(0, 30, 20, 1, 2, 3, 4, 10, 5, 6, 7)
  }

  test("simple exception handler") {
    makeCFG(0 -> 1 -> ((2 -> 3) || (xb(4) -> 5)) -> 6)
    checkLayout(0, 1, 2, 3, 6, cold, 4, 5)
  }

  test("hot code in Siberia (JET-7676)") {
    makeCFG((1 -> (44 || 35) -> 39) |>| ((35 || 39) -> xb(16) -> 19))
    makeCold(35)
    checkLayout(1, 44, 39, cold, 35, 16, 19)
  }

  test("all code is cold") {
    makeCFG(1 -> xb(13) -> 16)
    makeCold(1)
    checkLayout(cold, 1, 13, 16)
  }

  test("nested loops with many exits 1") { // new is slightly worse
    makeCFG((0 -> wd(1 -> wd(2 -> 3) -> 4 -> 42) -> 6 -> 7) |>| (4 -> 5 -> 7))
    checkLayout(0, 42, 1, 3, 2, 4, 5, 6, 7)
    //checkOldLayout(0, 3, 2, 4, 42, 1, 6, 5, 7)
  }

  test("nested loops with many exits 2") {
    makeCFG((0 -> wd(1 -> wd(2 -> 3) -> 4 -> 42) -> 7) |>| (4 -> 5 -> 7))
    checkLayout(0, 42, 1, 3, 2, 4, 5, 7)
  }

  test("nested loops with many exits 3") {
    makeCFG((0 -> wd(1 -> wd(2 -> 100 -> 3) -> 4 -> 42)) |>| (100 -> 6 -> 7) |>| (4 -> 5 -> 7))
    checkLayout(0, 42, 1, 100, 3, 2, 4, 5, 6, 7)
  }

  test("simple half-diamond") {
    makeCFG(0 -> (1 || 2) -> 3)
    makeRedirect((1, 3))
    checkLayout(0, 2, 3)
  }

  test("tricky irreducible loop") {
    makeCFG(0 -> ((1 -> wd(2 -> 3 -> 4) -> 5) || (6 -> 4)))
    checkLayout(0, 1, 6, 3, 4, 2, 5)
  }

}
