/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import org.scalactic.source

class EquivalentPhiesEliminationSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with EquivalentPhiesElimination {

  def testLocal(name: String)(graph: => SubGraph)(pps: Seq[Seq[String]])(pos: source.Position)(success: Boolean): Unit = {
    test(name) {
      makeCFG(graph)
      eliminateEquivalentPhies()
      for (ps <- pps) {
        val result = ps.toSet map (n(_))
        if (success) {
          result.size shouldBe 1
        } else {
          result.size shouldBe ps.size
        }
      }
    }
  }

  def testPositive(name: String)(graph: => SubGraph)(pps: Seq[String]*)(implicit pos: source.Position): Unit = {
    testLocal(name)(graph)(pps)(pos)(success = true)
  }

  def testNegative(name: String)(graph: => SubGraph)(pps: Seq[String]*)(implicit pos: source.Position): Unit = {
    testLocal(name)(graph)(pps)(pos)(success = false)
  }

  testPositive("general value numbering already works")(
    0@@("a", "b") ->
    (1 || 2) ->
    3@@("p1=phi(a,b)", "p2=phi(a,b)")
  )(Seq("p1", "p2"))

  testPositive("simple loop")(
    0@@("a", "b") ->
    (1 || 2) ->
    dw(3@@("p1=phi(a,b,p1)", "p2=phi(a,b,p2)")) ->
    4
  )(Seq("p1", "p2"))

  testPositive("simple loop with swapped values")(
    0@@("a", "b") ->
    (1 || 2) ->
    dw(3@@("p1=phi(a,b,p2)", "p2=phi(a,b,p1)")) ->
    4
  )(Seq("p1", "p2"))

  testPositive("simple loop with triple cycle")(
    0@@("a", "b") ->
    (1 || 2) ->
    dw(3@@("p1=phi(a,b,p3)", "p2=phi(a,b,p1)", "p3=phi(a,b,p2)")) ->
    4
  )(Seq("p1", "p2", "p3"))

  testPositive("hard loop")(
    0@@"a" ->
    dw(1@@("p11=phi(a,p13)","p21=phi(a,p23)") ->
       (2@@"b" ||
         ((3 || 4@@"c") ->
          5@@("p12=phi(p11,c)","p22=phi(p21,c)"))
       ) ->
       6@@("p13=phi(b,p12)","p23=phi(b,p22)")) ->
    7
  )(Seq("p11", "p21"), Seq("p12", "p22"), Seq("p13", "p23"))

  testPositive("hard loop (regression test for JET-15566")(
    0@@"a" ->
    dw(1@@("p11=phi(a,p13)","p21=phi(a,p23)") ->
       (2@@"b" ||
         ((3 || 4@@"c") ->
          5@@("p12=phi(p21,c)","p22=phi(p11,c)"))
       ) ->
       6@@("p13=phi(b,p12)","p23=phi(b,p22)")) ->
    7
  )(Seq("p11", "p21"), Seq("p12", "p22"), Seq("p13", "p23"))

  testNegative("different args in loop")(
    0@@("a", "b", "c") ->
    (1 || 2) ->
    dw(3@@("p1=phi(a,b,p1)", "p2=phi(a,c,p2)")) ->
    4
  )(Seq("p1", "p2"))

  testNegative("different args order in loop")(
    0@@("a", "b") ->
    (1 || 2) ->
    dw(3@@("p1=phi(a,b,p1)", "p2=phi(b,a,p2)")) ->
    4
  )(Seq("p1", "p2"))

  testNegative("different args after diamond")(
    0@@("a", "b", "c") ->
    (1 || 2) ->
    3@@("p1=phi(a,b)", "p2=phi(a,c)")
  )(Seq("p1", "p2"))

  for (phies <- Seq(
      "p10=phi(z1,z2,p11)",
      "p11=phi(z1,z2,p10)",
      "p20=phi(z1,z2,p20)"
  ).permutations) {

    testPositive(s"regression test for JET-12206 ($phies)")(
      0 -> (1@@"z1" || 2@@"z2") -> dw(3@@@(phies) )
    )(Seq("p10", "p11", "p20"))
  }

}

