/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.types.Guards.PointGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{ArithNodesDSL, CFGTransformationDSL, GlobalNodesBuilder}

import scala.util.chaining.scalaUtilChainingOps

class IteratorAbsorptionSuite
  extends CompilerSuite
    with GlobalNodesBuilder
    with ArithNodesDSL
    with CFGTransformationDSL
    with IteratorAbsorption {

  override def isPGOHost = true

  startPhase(CompilerPhase.PostInline)

  override def transformation(): Unit = {
    while (completeSSA() | absorbIterators()) { }
  }

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new StringAttribute("point")({ case (name, Seq(obj)) => TypeTest(PointGuard(sym(name)), TauInfo.Unknown)(obj) tap setCondition }),
      new SimpleAttribute("obj")({ case Seq() => addObjNode() }),
    ) ++ super.parsableAttributes()
  }


  test("absorbed single") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"point(IterImpl,phi(x,y))" -> (4 || 5) -> 6)
    after(0 -> (1 -> 30 -> 33 || 2 -> 3) -> (4 || 5) -> 6)
  }

  test("absorbed single weird") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"point(IterImpl,phi(x,y))" -> (4 || 5) -> 6 |>| 1 -> 6)
    after(0 -> (1 -> 30 -> 31 -> 33 || 2 -> 3) -> (4 || 5) -> 6 |>| 30 -> 60 -> 6)
  }

  test("absorbed chain (same obj)") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@("p=phi(x,y)", "point(IterImpl,p)") -> (4 || 5) -> 6@@"point(IterImpl,p)"
      -> (7 || 8) -> 9)
    after(0 -> (1 -> 30 -> 33 -> (44 || 55) -> 66 || 2 -> 3 -> (4 || 5) -> 6)
      -> (7 || 8) -> 9)
  }

  test("absorbed chain (different obj)") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@("p=phi(x,y)", "point(IterImpl,p)") -> (4 || 5) -> 6@@"point(IterImpl,phi(p,y))"
      -> (7 || 8) -> 9)
    after(0 -> (1 -> 30 -> 33 -> (44 || 55) -> 66 || 2 -> 3 -> (4 || 5) -> 6)
      -> (7 || 8) -> 9)
  }

  test("absorbed loop (same obj)") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"t=phi(x,y)" ->
      lp(4@@("p=phi(t,p)", "point(IterImpl,p)") -> (5 || 6) -> 7@@"point(IterImpl,p)" -> (8 || 9) -> 10, exits(5)) -> 11)
    after(0 -> (1 -> 30 -> 33 -> lp(44 -> (55 -> 550 || 66) -> 77 -> (88 || 99) -> 100, exits(55))
      || 2 -> 3 -> lp(4 -> (5 -> 50 || 6) -> 7 -> (8 || 9) -> 10, exits(5))) -> 11)
  }

  test("absorbed loop (different obj)") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"t=phi(x,y)" ->
      lp(4@@("p=phi(t,q)", "point(IterImpl,p)") -> (5 || 6) -> 7@@("q=phi(y,p)", "point(IterImpl,q)") -> (8 || 9) -> 10, exits(5)) -> 11)
    after(0 -> (1 -> 30 -> 33 -> lp(44 -> (55 -> 550 || 66) -> 77 -> (88 || 99) -> 100, exits(55))
      || 2 -> 3 -> lp(4 -> (5 -> 50 || 6) -> 7 -> (8 || 9) -> 10, exits(5))) -> 11)
  }

  test("absorbed multi") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"p=phi(x,y)"
      -> (4@@"point(IterImpl,p)" -> (5 || 6) || 7@@"point(IterImpl,p)" -> (8 || 9)) -> 10)
    after(0 -> (1 -> 30 -> 33 -> (44 -> (5 || 6) || 77 -> (8 || 9)) || 2 -> 3
      -> (4 -> (5 || 6) || 7 -> (8 || 9)) -> 10))
  }

  test("absorbed path") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"p=phi(x,y)"
      -> (4@@"point(IterImpl,p)" -> (5 || 6) || 7) -> 8)
    after(0 -> (1 -> 30 -> 33 -> (44 -> (5 || 6) || 7) || 2 -> 3
      -> (4 -> (5 || 6) || 7) -> 8))
  }

  test("absorbed nested new") {
    before(0@@"y=obj()" -> (1 -> (2@@"x=new(IterImpl)" || 3) -> 4@@"p=phi(x,y)" || 5) -> 6@@"point(IterImpl,phi(p,y))"
      -> (7 || 8) -> 9)
    after(0 -> (1 -> (2 -> 40 -> 44 -> 66 || 3 -> 4 -> 6) || 5 -> 6)
      -> (7 || 8) -> 9)
  }

  test("absorbed single weird merge point in loop") {
    before(0@@"z=obj()" -> wd(1 -> (2@@"x=new(IterImpl)" || 3@@"y=obj()") -> 4@@"p=phi(x,y)" -> (5@@"point(IterImpl,phi(p,z))" -> (6 || 7) -> 8 || 9) -> 10) -> 11
      |>| 1 -> 12 -> 5)
    after(0 -> wd(1 -> (2 -> 40 -> 44 -> !(50 -> 55 -> (6 || 7) || 9) || 3 -> 4 -> (51 -> 5 -> (6 || 7) -> 8 || 9) -> 10)) -> 11
      |>| 1 -> 12 -> 5)
  }


  test("no absorption (already absorbed)") {
    before(0@@("x=new(IterImpl)", "point(IterImpl,x)") -> (1 || 2) -> 3@@"point(IterImpl,x)" -> (4 || 5) -> 6)
    after(0 -> (1 || 2) -> 3 -> (4 || 5) -> 6)
  }

  test("no absorption (no tau-tests)") {
    before(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"use(phi(x,y))" -> (4 || 5) -> 6)
    after(0 -> (1 || 2) -> 3 -> (4 || 5) -> 6)
  }

  test("no absorption (no tau-test uses)") {
    beforeWithPre(0 -> (1@@"x=new(IterImpl)" || 2@@"y=obj()") -> 3@@"point(IterImpl,phi(x,y))" -> (4 || 5) -> 6, {
      3.blockEnd.asInstanceOf[If].selector = False()
    })
    after(0 -> (1 || 2) -> 3 -> (4 || 5) -> 6)
  }

  test("no absorption (nested new in loop)") {
    before(0@@"y=obj()" -> wd(1@@"p=phi(y,x)" -> 2@@"x=new(IterImpl)") -> 3@@"point(IterImpl,p)" -> (4 || 5) -> 6)
    after(0 -> wd(1 -> 2) -> 3 -> (4 || 5) -> 6)
  }

  test("no absorption (branch above new in loop - 1)") {
    before(0@@"y=obj()" -> wd(1@@"p=phi(y,x)" -> 2 -> 3@@"point(IterImpl,p)" -> (4 || 5) -> 6@@"x=new(IterImpl)") -> 7)
    after(0 -> wd(1 -> 2 -> 3 -> (4 || 5) -> 6) -> 7)
  }

  test("no absorption (branch above new in loop - 2)") {
    before(0@@"y=obj()" -> wd(1@@"p=phi(y,q)" -> 2 -> 3@@"point(IterImpl,p)" -> (4@@"x=new(IterImpl)" || 5) -> 6@@"q=phi(x,p)") -> 7)
    after(0 -> wd(1 -> 2 -> 3 -> (4 || 5) -> 6) -> 7)
  }

}
