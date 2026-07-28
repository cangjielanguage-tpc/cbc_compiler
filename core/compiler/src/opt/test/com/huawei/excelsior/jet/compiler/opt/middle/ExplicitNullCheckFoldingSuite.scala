/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.{CompilerSuite, Domain}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethod, FakeMethodType}

class ExplicitNullCheckFoldingSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with CFGTransformationDSL
     with IRTransformationsCollection
     with ExplicitNullCheckFolding {

  private def removeHandlerAnchorsCompletely() = {
    removeHandlerAnchors()
    eliminateUnreachableCode()
  }

  override def transformation(): Unit = {
    removeHandlerAnchorsCompletely()
    while (foldExplicitNullChecks()) { }
  }

  override def makeDebug = false

  val jrThrowNullPointerException = new FakeMethod("JR_ThrowNullPointerException")

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.setRtsProc(RTSProc.JR_ThrowNullPointerException, jrThrowNullPointerException)
  }

  override def parsableAttributes(): Seq[Attribute] = Seq(
    new SimpleAttribute("controlled")({ case Seq(x) => FakeControlledUnary(IntType)(x) }),
    new SimpleAttribute("null"      )({ case Seq() => Null() }),
    new SimpleAttribute("nc"        )({ case Seq(x) => NullCheck(x) }),
    new SimpleAttribute("obj"       )({ case Seq() => addObjNode() }),
    new SimpleAttribute("npe"       )({ case Seq() => ErrorRTSCall(RTSProc.JR_ThrowNullPointerException)() })
  ) ++ super.parsableAttributes()

  test("require non null") {
    before(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@"npe()" || 2@@"y=use(x)") -> 3)
    after(0 -> 1 -> 2 -> 3)
  }

  test("handled require non null") {
    before(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@"npe()" -> xb(2) -> 3@@"y=use(x)" || 3))
    after(0 -> 1 -> (3 || (xb(2) -> 3)))
  }

  test("value from throwing block used in handler") {
    before(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("y=read()", "npe()") -> xb(2)@@"use(y)" -> 3@@"use(x)" || 3))
    // optimization should not be performed in this case
    after(0 -> ((1 -> xb(2) -> 3) || 3))
  }

  test("throwing block with multiple null-test predecessors") {
    before(0@@("x=obj()") -> (1@@"if(cmp(x,null()))" || 2@@"if(cmp(x,null()))") -> (3@@"npe()" -> xb(4) || 5) -> 6)
    after(0 -> (1 -> 2 -> (xb(5) || 6) || 3 -> 4 -> (xb(5) || 6)) -> 7)
  }

  test("throwing block with multiple predecessors") {
    before(0 -> (1@@("x=obj()", "if(cmp(x,null()))") || 2) -> (3@@"npe()" -> xb(4) || 5) -> 6)
    after(0 -> 1 -> 2 -> (3 || xb(4)) -> 5 |>| 0 -> 6 -> (7 -> xb(4) || 3))
  }

  test("replace throwing block by null check") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@"thrw=xspinal()" -> xb(2) || 3) -> 4)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe true
  }

  test("replace throwing block by null check (no cross-block edges)") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("y=read()","add(y,ic(1))","xspinal()","thrw=xspinal()") -> xb(2) || 3) -> 4)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe true
  }

  test("don't replace throwing block by null check (it's used in handler as phi)") {
    makeCFG(0@@("x=obj()", "a=ic(0)", "b=ic(1)", "if(cmp(x,null()))") -> ((1@@("av", "y=read()", "xspinal()", "thrw=xspinal()") -> xb(2)@@("p=phi(av,a,b)", "add(p,y)") -> 3@@"use(x)") || 3))
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe false
  }

  test("don't replace throwing block by null check (handler uses its nodes)") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("v=read()", "thrw=xspinal()") -> xb(2)@@"use(v)" || 3) -> 4)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe false
  }

  test("don't replace throwing block by null check (handler successors use its nodes)") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("y=read()", "thrw=xspinal()") -> xb(2) -> 3 -> (4 || 5@@"z=ic(0)") -> 6@@"phi(y,z)" || 7) -> 8)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe false
  }

  test("don't replace throwing block by null check (cross-block control use in handler)") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("v=spinal()", "thrw=xspinal()") -> xb(2)@@"controlled(v)" || 3) -> 4)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe false
  }

  test("don't replace throwing block by null check (cross-block control use in handler's successor)") {
    makeCFG(0@@("x=obj()", "if(cmp(x,null()))") -> (1@@("v=spinal()", "thrw=xspinal()") -> xb(2) -> 3 -> (4@@"controlled(v)" || 5) -> 6 || 7) -> 8)
    removeHandlerAnchorsCompletely()
    replaceByNullCheck(b(1), n("thrw").asInstanceOf[SpinalNode], n("thrw"), Domain.JAVA) shouldBe false
  }

}
