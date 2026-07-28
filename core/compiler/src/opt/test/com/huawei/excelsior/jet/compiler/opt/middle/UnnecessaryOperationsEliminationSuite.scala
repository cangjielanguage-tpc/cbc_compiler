/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.{CompilerSuite, Domain}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeType

/** Tests for [[UnnecessaryOperationsElimination]] */
class UnnecessaryOperationsEliminationSuite extends CompilerSuite
                                            with GlobalNodesBuilder
                                            with UnnecessaryOperationsElimination {
  override def parsableAttributes() = Seq(
    new SimpleAttribute("cdomain.aj")({ case Seq(v) => ConvertDomain(Domain.AJ)(v) }),
    new SimpleAttribute("cdomain.java")({ case Seq(v) => ConvertDomain(Domain.JAVA)(v) }),
    new SimpleAttribute("throw")({ case Seq(v) => Throw(v) }),
  ) ++ super.parsableAttributes()

  val javaLangThrowable = FakeType.create(classOf[java.lang.Throwable])
  val javaThrowableFoo = makeSymClass("JavaThrowableFoo", javaLangThrowable)

  test("one ConvertDomain use which is throw") {
    makeCFG(xb(1)@@("x=catch()", "y=cdomain.aj(x)", "throw(y)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe n("x")
  }

  test("multiple ConvertDomain uses which are throws") {
    makeCFG(
      xb(0)@@("x=catch()", "y=cdomain.java(x)") -> 1@@"if(cmp(ic(1),ic(2)))" ->
        (2 || 3@@"z=new(A)") ->
        4@@("t=phi(y,z)", "if(cmp(ic(1),ic(3)))") ->
        (5@@"r=new(B)" || 6) ->
        7@@("s=phi(r,t)", "throw(s)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe n("x")
  }

  test("one ConvertDomain use which is not throw") {
    makeCFG(xb(1)@@("x=catch()", "y=cdomain.aj(x)", "use(y)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe a[ConvertDomain]
  }

  test("multiple ConvertDomain uses some of which are not throws") {
    makeCFG(
      xb(0)@@("x=catch()", "y=cdomain.java(x)") -> 1@@"if(cmp(ic(1),ic(2)))" ->
        (2 || 3@@"z=new(A)") ->
        4@@("t=phi(y,z)", "if(cmp(ic(1),ic(3)))") ->
        (5@@"r=new(B)" || 6@@"use(t)") ->
        7@@("s=phi(r,t)", "throw(s)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe a[ConvertDomain]
  }

  test("ConvertDomain with matching exception arg") {
    makeCFG(0@@("x=new(JavaThrowableFoo)", "y=cdomain.java(x)", "use(y)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe n("x")
  }

  test("ConvertDomain with non-matching exception arg") {
    makeCFG(0@@("x=new(JavaThrowableFoo)", "y=cdomain.aj(x)", "use(y)"))
    eliminateUnnecessaryOperations()
    n("y") shouldBe a[ConvertDomain]
  }
}
