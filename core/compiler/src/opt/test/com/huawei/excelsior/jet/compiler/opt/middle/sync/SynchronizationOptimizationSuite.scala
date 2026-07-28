/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.sync

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import org.scalactic.source

class SynchronizationOptimizationSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with SynchronizationOptimization {

  startPhase(CompilerPhase.PostInline)
  
  private var obj: Node = _
  
  override def beforeEach(): Unit = {
    super.beforeEach()
    obj = addObjNode()
  }


  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("reg")({
        case Seq() => SynchronizedRegion(SynchronizedRegion.noRegion())
        case Seq(outer) => SynchronizedRegion(outer)
      }),
      new SimpleAttribute("enter")({ case Seq(region) => MonitorEnter(obj, region) }),
      new SimpleAttribute("exit")({ case Seq(region) => MonitorExit(obj, region) }),
    ) ++ super.parsableAttributes()
  }

  def m(name: String) = n(name).asInstanceOf[MonitorOperation]

  def reg(name: String) = m(name).syncRegion

  test("no slicing - trivial") {
    makeCFG(0@@"r=reg()" -> 1@@"enter(r)" -> (2 || 3) -> 4@@"exit(r)")
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - single enter") {
    makeCFG(0@@"r=reg()" -> 1@@"enter(r)" -> (2@@"exit(r)" || 3@@"exit(r)") -> 4)
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - single exit") {
    makeCFG(0@@"r=reg()" -> 1 -> (2@@"enter(r)" || 3@@"enter(r)") -> 4@@"exit(r)")
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - crossroad") {
    makeCFG(0@@"r=reg()" -> (1@@"enter(r)" || 2@@"enter(r)") -> 3 -> (4@@"exit(r)" || 5@@"exit(r)") -> 6)
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - nested") {
    makeCFG(0@@("or=reg()", "ir=reg(or)") -> 1@@"enter(or)" -> (2@@("enter(ir)", "exit(ir)") || 3) -> 4@@"exit(or)"
      |>| 2 -> xb(5) -> 6 -> (7@@"exit(or)" || wd(8)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - enter xpoint") {
    makeCFG(0@@"r=reg()" -> 1@@"enter(r)" -> (2 || xb(4)@@"enter(r)") -> 3@@"exit(r)")
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe false
  }

  test("no slicing - exit xpoint") {
    makeCFG(0@@"r=reg()" -> 1@@"enter(r)" -> 2 -> 3@@"exit(r)" -> xb(4)@@"exit(r)")
    removeHandlerAnchors() // actually xblock is unreachable after this, because exit does not throw
    sliceSynchronizedRegions() shouldBe false
  }

  case class SlicedRegion(enters: Seq[String], nested: Seq[SlicedRegion], exits: Seq[String])
  object R {
    def apply(enters: String*)(nested: SlicedRegion*)(exits: String*): SlicedRegion = SlicedRegion(enters, nested, exits)
  }

  def checkSliced(regions: SlicedRegion*): Unit = {
    def impl(regions: Seq[SlicedRegion], outer: Option[SynchronizedRegion]): Unit = {
      for (r <- regions) {
        val regNode = reg(r.enters.head)
        regNode.outer shouldBe outer
        for (m <- r.enters ++ r.exits) {
          reg(m) shouldBe regNode
        }
        regNode.enters.toSet shouldBe r.enters.map(m).toSet
        regNode.exits.toSet shouldBe r.exits.map(m).toSet

        impl(r.nested, Some(regNode))
      }
      for (Seq(r1, r2) <- regions.combinations(2)) {
        reg(r1.enters.head) should not be reg(r2.enters.head)
      }
    }

    impl(regions, None)
  }

  test("slicing - parallel trivial") {
    makeCFG(0@@"r=reg()" ->
      (1@@"n1=enter(r)" -> 2@@"x1=exit(r)"
        || 3@@"n2=enter(r)" -> 4@@"x2=exit(r)")
      -> 5)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2")()("x2"),
    )
  }

  test("slicing - parallel single enter") {
    makeCFG(0@@"r=reg()" ->
      (1@@"n1=enter(r)" -> (2@@"x1=exit(r)" || 3@@"x11=exit(r)")
        || 4@@"n2=enter(r)" -> 5@@"x2=exit(r)")
      -> 6)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1", "x11"),
      R("n2")()("x2"),
    )
  }

  test("slicing - parallel single exit") {
    makeCFG(0@@"r=reg()" ->
      (1 -> (2@@"n1=enter(r)" || 3@@"n11=enter(r)") -> 4@@"x1=exit(r)"
        || 5@@"n2=enter(r)" -> 6@@"x2=exit(r)")
      -> 7)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1", "n11")()("x1"),
      R("n2")()("x2"),
    )
  }

  test("slicing - parallel crossroad") {
    makeCFG(0@@"r=reg()" ->
      (1 -> (2@@"n1=enter(r)" || 3@@"n11=enter(r)") -> 4 -> (5@@"x1=exit(r)" || 6@@"x11=exit(r)")
        || 7@@"n2=enter(r)" -> 8@@"x2=exit(r)")
      -> 9)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1", "n11")()("x1", "x11"),
      R("n2")()("x2"),
    )
  }

  test("slicing - parallel endless loop") {
    makeCFG(0@@"r=reg()" ->
      (1@@"n1=enter(r)" -> 2@@"x1=exit(r)"
        || 3@@"n2=enter(r)" -> !wd(4))
      -> 5)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2")()(),
    )
  }

  test("slicing - sequential trivial") {
    makeCFG(0@@"r=reg()" ->
      1@@"n1=enter(r)" -> 2@@"x1=exit(r)" ->
      3@@"n2=enter(r)" -> 4@@"x2=exit(r)"
      -> 5)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2")()("x2"),
    )
  }

  test("slicing - sequential single enter") {
    makeCFG(0@@"r=reg()" ->
      1@@"n1=enter(r)" -> (2@@"x1=exit(r)" || 3@@"x11=exit(r)") ->
      4@@"n2=enter(r)" -> 5@@"x2=exit(r)"
      -> 6)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1", "x11"),
      R("n2")()("x2"),
    )
  }

  test("slicing - sequential single exit") {
    makeCFG(0@@"r=reg()" ->
      1 -> (2@@"n1=enter(r)" || 3@@"n11=enter(r)") -> 4@@"x1=exit(r)" ->
      5@@"n2=enter(r)" -> 6@@"x2=exit(r)"
      -> 7)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1", "n11")()("x1"),
      R("n2")()("x2"),
    )
  }

  test("slicing - sequential crossroad") {
    makeCFG(0@@"r=reg()" ->
      1 -> (2@@"n1=enter(r)" || 3@@"n11=enter(r)") -> 4 -> (5@@"x1=exit(r)" || 6@@"x11=exit(r)") ->
      7@@"n2=enter(r)" -> 8@@"x2=exit(r)"
      -> 9)
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1", "n11")()("x1", "x11"),
      R("n2")()("x2"),
    )
  }

  test("slicing - sequential endless loop") {
    makeCFG(0@@"r=reg()" ->
      1@@"n1=enter(r)" -> 2@@"x1=exit(r)" ->
      3@@"n2=enter(r)" -> !wd(4))
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2")()(),
    )
  }

  test("slicing - nested inner") {
    makeCFG(0@@("or=reg()", "ir=reg(or)", "on=enter(or)") ->
      (1@@"in1=enter(ir)" -> 2@@"ix1=exit(ir)"
        || 3@@"in2=enter(ir)" -> 4@@"ix2=exit(ir)")
      -> 5@@"ox1=exit(or)"
      |>| (1 || 3) -> xb(6) -> 7 -> (8@@"ox2=exit(or)" || wd(9)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    reg("on") shouldBe n("or")
    reg("ox1") shouldBe n("or")
    reg("ox2") shouldBe n("or")
    checkSliced(
      R("on")(
        R("in1")()("ix1"),
        R("in2")()("ix2")
      )("ox1", "ox2")
    )
  }

  test("slicing - nested outer") {
    makeCFG(0@@("or=reg()", "ir=reg(or)") ->
      (1@@"on1=enter(or)" -> 2@@"in=enter(ir)" -> 3@@"ix=exit(ir)" -> 4@@"ox1=exit(or)"
        || 5@@"on2=enter(or)" -> 6 -> 7 -> 8@@"ox2=exit(or)")
      -> 9
      |>| 2 -> xb(10) -> 11 -> (12@@"ox11=exit(or)" || wd(13)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("on1")(
        R("in")()("ix")
      )("ox1", "ox11"),
      R("on2")()("ox2"),
    )
  }

  test("slicing - nested both") {
    makeCFG(0@@("or=reg()", "ir=reg(or)") ->
      (1@@"on1=enter(or)" -> 2@@"in1=enter(ir)" -> 3@@"ix1=exit(ir)" -> 4@@"ox1=exit(or)"
        || 5@@"on2=enter(or)" -> 6@@"in2=enter(ir)" -> 7@@"ix2=exit(ir)" -> 8@@"ox2=exit(or)")
      -> 9
      |>| 2 -> xb(10) -> 11 -> (12@@"ox11=exit(or)" || wd(13))
      |>| 6 -> xb(14) -> 15 -> (16@@"ox22=exit(or)" || wd(17)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("on1")(
        R("in1")()("ix1")
      )("ox1", "ox11"),
      R("on2")(
        R("in2")()("ix2")
      )("ox2", "ox22"),
    )
  }

  test("slicing - nested missing") {
    makeCFG(0@@("or=reg()", "mr=reg(or)", "ir=reg(mr)", "on=enter(or)") ->
      (1@@"in1=enter(ir)" -> 2@@"ix1=exit(ir)"
        || 3@@"in2=enter(ir)" -> 4@@"ix2=exit(ir)")
      -> 5@@"ox1=exit(or)"
      |>| (1 || 3) -> xb(6) -> 7 -> (8@@"ox2=exit(or)" || wd(9)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    reg("on") shouldBe n("or")
    reg("ox1") shouldBe n("or")
    reg("ox2") shouldBe n("or")
    // Note that the middle region `mr` is eliminated
    checkSliced(
      R("on")(
        R("in1")()("ix1"),
        R("in2")()("ix2")
      )("ox1", "ox2")
    )
  }

  test("slicing - nested both missing") {
    makeCFG(0@@("or=reg()", "mr=reg(or)", "ir=reg(mr)") ->
      (1@@"on1=enter(or)" -> 2@@"in1=enter(ir)" -> 3@@"ix1=exit(ir)" -> 4@@"ox1=exit(or)"
        || 5@@"on2=enter(or)" -> 6@@"in2=enter(ir)" -> 7@@"ix2=exit(ir)" -> 8@@"ox2=exit(or)")
      -> 9
      |>| 2 -> xb(10) -> 11 -> (12@@"ox11=exit(or)" || wd(13))
      |>| 6 -> xb(14) -> 15 -> (16@@"ox22=exit(or)" || wd(17)))
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    // Note that the middle region `mr` is eliminated
    checkSliced(
      R("on1")(
        R("in1")()("ix1")
      )("ox1", "ox11"),
      R("on2")(
        R("in2")()("ix2")
      )("ox2", "ox22"),
    )
  }

  test("slicing - sequential enter xpoint") {
    makeCFG(0@@"r=reg()" ->
      1@@"n1=enter(r)" -> 2@@"x1=exit(r)" ->
      3@@("xspinal()", "n2=enter(r)") -> (4 || xb(5)@@"n3=enter(r)") -> 6@@"x2=exit(r)")
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2", "n3")()("x2"),
    )
  }

  test("slicing - sequential exit xpoint") {
    makeCFG(0@@"r=reg()" ->
      1@@"n1=enter(r)" -> 2@@"x1=exit(r)" ->
      3@@"n2=enter(r)" -> 4@@("xspinal()", "x2=exit(r)")
      -> xb(5)@@"x3=exit(r)")
    removeHandlerAnchors()
    sliceSynchronizedRegions() shouldBe true
    checkSliced(
      R("n1")()("x1"),
      R("n2")()("x2", "x3"),
    )
  }

  ////////////////////////
  // Consistency checks

  private def testFail(name: String)(g: => SubGraph)(implicit pos: source.Position): Unit = {
    test(s"inconsistency - $name") {
      makeCFG(g)
      removeHandlerAnchors()
      intercept[AssertionError] {
        checkSynchronizationConsistency()
      }
    }
  }

  private def testOk(name: String)(g: => SubGraph)(implicit pos: source.Position): Unit = {
    test(s"consistency - $name") {
      makeCFG(g)
      removeHandlerAnchors()
      checkSynchronizationConsistency()
    }
  }

  test("consistency checks are scared of vars") {
    makeCFG(0@@"r=reg()" -> 1@@"enter(r)")
    // Currently checks are scared of any vars, even without uses.
    withNewVar(IntType) { (_, _) => }
    checkSynchronizationConsistency()
    // Cleanup vars.
    completeSSA()
  }

  testOk("normal") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"exit(r)"
  }

  testFail("only enter") {
    0@@"r=reg()" -> 1@@"enter(r)"
  }

  testFail("only exit") {
    0@@"r=reg()" -> 1@@"exit(r)"
  }

  testFail("many enters") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"enter(r)" -> 3@@"exit(r)"
  }

  testFail("many exits") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"exit(r)" -> 3@@"exit(r)"
  }

  testFail("many pairs") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"enter(r)" -> 3@@"exit(r)" -> 4@@"exit(r)" |>|
      2 -> xb(5) -> 6 -> (7@@"exit(r)" || wd(8))
  }

  testFail("swapped") {
    0@@"r=reg()" -> 1@@"exit(r)" -> 2@@"enter(r)"
  }

  testFail("path without enter") {
    0@@"r=reg()" -> (1@@"enter(r)" || 2) -> 3@@"exit(r)"
  }

  testFail("path without exit") {
    0@@"r=reg()" -> 1@@"enter(r)" -> (2 || 3@@"exit(r)")
  }

  testOk("unreachable path") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"exit(r)" |>| 3 -> 2
  }

  testFail("enter in loop") {
    0@@"r=reg()" -> wd(1@@"enter(r)") -> 2@@"exit(r)"
  }

  testFail("exit in loop") {
    0@@"r=reg()" -> 1@@"enter(r)" -> wd(2@@"exit(r)") -> 3
  }

  testOk("endless loop") {
    0@@"r=reg()" -> (!wd(1@@"enter(r)" -> 2@@"exit(r)") || 3) -> 4
  }

  testOk("only enter endless loop") {
    0@@"r=reg()" -> 1@@"enter(r)" -> !wd(2)
  }

  testOk("disjoint regions") {
    0@@("r1=reg()", "r2=reg()") -> 1@@"enter(r1)" -> 2@@"exit(r1)" -> 3@@"enter(r2)" -> 4@@"exit(r2)"
  }

  testFail("disjoint regions (inner)") {
    0@@("r1=reg()", "r2=reg()") -> 1@@"enter(r1)" -> 2@@"enter(r2)" -> 3@@"exit(r2)" -> 4@@"exit(r1)" |>|
      2 -> xb(5) -> 6 -> (7@@"exit(r1)" || wd(8))
  }

  testFail("disjoint regions (intersecting)") {
    0@@("r1=reg()", "r2=reg()") -> 1@@"enter(r1)" -> 2@@"enter(r2)" -> 3@@"exit(r1)" -> 4@@"exit(r2)"
  }

  testOk("inner region") {
    0@@("or=reg()", "ir=reg(or)") -> 1@@"enter(or)" -> 2@@"enter(ir)" -> 3@@"exit(ir)" -> 4@@"exit(or)" |>|
      2 -> xb(5) -> 6 -> (7@@"exit(or)" || wd(8))
  }

  testOk("inner region without outer monitors") {
    0@@("or=reg()", "ir=reg(or)") -> 1@@"enter(ir)" -> 2@@"exit(ir)"
  }

  testFail("inner region (swapped)") {
    0@@("or=reg()", "ir=reg(or)") -> 1@@"enter(ir)" -> 2@@"enter(or)" -> 3@@"exit(or)" -> 4@@"exit(ir)" |>|
      2 -> xb(5) -> 6 -> (7@@"exit(ir)" || wd(8))
  }

  testFail("inner region (intersecting)") {
    0@@("or=reg()", "ir=reg(or)") -> 1@@"enter(or)" -> 2@@"enter(ir)" -> 3@@"exit(or)" -> 4@@"exit(ir)" |>|
      2 -> xb(5) -> 6 -> (7@@"exit(or)" || wd(8))
  }

  testOk("around loop") {
    0@@"r=reg()" -> 1@@"enter(r)" -> wd(2 -> 3) -> 4@@"exit(r)"
  }

  testFail("loose xpoint") {
    0@@"r=reg()" -> 1@@"enter(r)" -> 2@@"xspinal()" -> 3@@"exit(r)"
  }

  // Consistency checks
  ////////////////////////
}
