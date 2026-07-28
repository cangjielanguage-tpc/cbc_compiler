/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import org.scalatest.matchers.must.Matchers.{be, not}
import transformations.IRTransformationsCollection

/**
 * Tests for DCEComponent
 */
class DCEComponentSuite extends CompilerSuite
                           with GlobalNodesBuilder
                           with DCEComponent
                           with IRTransformationsCollection {

  override def parsableAttributes() = Seq(
    new SimpleAttribute("weak")({ case Seq(x) => RawValueRangeFilter(x, IConst(0), IConst(10)) }),
    new SimpleAttribute("weak2")({ case Seq(x, y) => RawValueRangeFilter(x, y, IConst(10)) }),
    new SimpleAttribute("idx_dummy")({ case Seq() => addInductiveVariable(IConst(0), Condition.LT, IConst(100), IConst(1)) }),
  ) ++ super.parsableAttributes()

  private def succ(b: Block): Block = b.blockEnd.asInstanceOf[Goto].target

  def check(alive: Node*)(dead: Node*): Unit = {
    eliminateDeadCode()
    alive foreach { _ shouldBe Symbol("committed") }
    dead foreach { _ should not be Symbol("committed") }
  }

  test("simple dead code") {
    makeCFG(0@@("x", "y") -> 1@@("ret(x)"))
    check(
      "x")(
      "y")
  }

  test("dead if elimination") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1 || 2) -> 3@@("ret(x)"))
    val branch = 0.blockEnd
    check(
      "x")(
      "y", "c", branch)
    succ(succ(0)) should be (3: Block)
  }

  test("dead if elimination with cold code markers") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1@@("cc1=coldcode()") || 2@@("cc2=coldcode()")) -> 3@@("ret(x)"))
    val branch = 0.blockEnd
    check(
      "x", "cc1")(
      "y", "c", "cc2", branch)
    succ(succ(0)) should be (3: Block)
  }

  test("bad dead if elimination with cold code marker (JET-12122)") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1@@("cc1=coldcode()") || 2) -> 3@@("ret(x)"))
    val branch = 0.blockEnd
    check(
      "x", "cc1")(
      "y", "c", branch)
    succ(succ(0)) should be (3: Block)
  }

  test("dead if and phi-function elimination") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1 || 2) -> 3@@("p=phi(y,x)", "ret(x)"))
    val branch = 0.blockEnd
    check(
      "x")(
      "y", "c", "p", branch)
    succ(succ(0)) should be (3: Block)
  }

  test("undead phi-function") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1 || 2) -> 3@@("p=phi(y,x)", "ret(p)"))
    check(
      "x", "y", "c", "p")(
      )
  }

  test("undead phi-function 2") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1 || 2) -> 3@@("p=phi(y,x)", "ret(p)"))
    EmptyBlocksElimination()

    check(
      "x", "y", "c", "p")(
      )
  }

  test("dead phi-function 2") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1 || 2) -> 3@@("p=phi(y,x)", "ret(x)"))
    EmptyBlocksElimination()
    val branch = 0.blockEnd

    check(
      "x")(
      "y", "c", "p", branch)
  }

  test("control node is undead") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1@@("w=write()") || 2) -> 3@@("p=phi(y,x)", "ret(x)"))
    val branch = 0.blockEnd
    check(
      "x", "y", "c", "w", branch)(
      "p")
  }

  test("controlled node could be dead") {
    makeCFG(0@@("x", "y", "c=cmp(x,y)", "if(c)") -> (1@@("r=read()") || 2) -> 3@@("p=phi(y,x)", "ret(x)"))
    val branch = 0.blockEnd
    check(
      "x")(
      "y", "c", "p", "r", branch)
    succ(succ(0)) should be (3: Block)
  }

  test("a lot of branches") {
    makeCFG(0@@("x", "y", "q", "r", "s", "t", "c1=cmp(x,y)", "if(c1)") -> (1 || (2@@("c2=cmp(q,r)", "if(c2)") -> (3 || 4) -> 5@@("c3=cmp(s,t)", "if(c3)") -> (6 || 7))) -> 8@@("ret(x)"))
    val b1 = 0.blockEnd
    val b2 = 2.blockEnd
    val b3 = 5.blockEnd
    check(
      "x")(
      "y", "q", "r", "s", "t", "c1", "c2", "c3", b1, b2, b3)
  }

  test("control dependent points calculation") {
    makeCFG(0@@("x", "y", "q", "r", "s", "t", "c1=cmp(x,y)", "if(c1)") -> (1 || (2@@("c2=cmp(q,r)", "if(c2)") -> (3 || 4) -> 5@@("c3=cmp(s,t)", "if(c3)") -> (6@@("w=write()") || 7))) -> 8@@("ret(x)"))
    val b1 = 0.blockEnd
    val b2 = 2.blockEnd
    val b3 = 5.blockEnd
    check(
      "x", "y", "c1", "s", "t", "c3", "w", b1, b3)(
      "q", "r", "c2", b2)
  }

  test("simple undead cycle") {
    makeCFG(0@@("x0", "n", "1") -> wd(1@@("p=phi(x0,x)", "c=cmp(p,n)", "if(c)") -> 2@@("x=add(p,1)")) -> 3@@("ret(p)"))
    val branch = 1.blockEnd
    check(
      "x0", "n", "1", "p", "c", "x", branch)(
      )
  }

  test("cycle with dead phi-function") {
    makeCFG(0@@("x0", "n", "1") -> wd(1@@("p=phi(x0,x)", "dp=phi(x0,t)", "c=cmp(p,n)", "if(c)") -> 2@@("x=add(p,1)", "t=add(dp,1)")) -> 3@@("ret(p)"))
    val branch = 1.blockEnd
    check(
      "x0", "n", "1", "p", "c", "x", branch)(
      "dp", "t")
  }

  test("dead code into undead cycle") {
    makeCFG(0@@("x0", "n", "1", "w") -> wd(1@@("p=phi(x0,x)", "c=cmp(p,n)", "if(c)") -> 4@@("dc=cmp(p,w)", "if(dc)") -> (5 || 6) -> 2@@("x=add(p,1)")) -> 3@@("ret(p)"))
    val branch = 1.blockEnd
    val deadBranch = 4.blockEnd
    check(
      "x0", "n", "1", "p", "c", "x", branch)(
      "w", "dc", deadBranch)
  }

  test("bug in functional test subtest") {
    makeCFG(0@@("x", "a", "b", "c") -> 1@@("p=phi(x,y)", "cmp1=cmp(a,b)", "if(cmp1)") -> ((2@@("w", "cmp2=cmp(b,c)", "if(cmp2)") -> (3 || 4) -> 5@@("y=phi(p,w)") -> 1) || 6@@("ret(p)")))
    val branch1 = 1.blockEnd
    val branch2 = 2.blockEnd
    check(
      "x", "a", "b", "c", "p", "cmp1", "w", "cmp2", "y", branch1, branch2)(
      )
  }

  test("cfg with one-node-loop") {
    makeCFG(0@@("x", "y") -> 1@@("c=cmp(x,y)", "if(c)") -> (1 || 2@@("ret(x)")))
    val branch = 1.blockEnd
    check(
      "x", "y", branch)(
      )
  }

  test("a lot of branches with cold code markers") {
    makeCFG(0@@("x", "y", "q", "r", "s", "t", "cc1=coldcode()", "c1=cmp(x,y)", "if(c1)") -> (1 || (2@@("cc2=coldcode()", "c2=cmp(q,r)", "if(c2)", "cc3=coldcode()") -> (3 ||4) -> 5@@("c3=cmp(s,t)", "if(c3)") -> (6 || 7@@("cc4=coldcode()")))) -> 8@@("cc5=coldcode()", "ret(x)"))
    val b1 = 0.blockEnd
    val b2 = 2.blockEnd
    val b3 = 5.blockEnd
    check(
      "x", "cc1", "cc5")(
      "y", "q", "r", "s", "t", "c1", "c2", "c3", "cc2", "cc3", "cc4", b1, b2, b3)
  }

  test("weak node with single argument") {
    makeCFG(0@@("x", "y", "z=weak(y)") -> 1@@("ret(x)"))
    check(
      "x")(
      "y", "z")
  }

  test("node with weak and non-weak use") {
    makeCFG(0@@("x", "y=weak(x)") -> 1@@("ret(x)"))
    check(
      "x", "y")(
      )
  }

  test("node with multiple weak and non-weak uses") {
    makeCFG(0@@("x", "u1=weak(x)", "u2=weak(x)", "u3=weak(x)", "u4=weak(x)") -> 1@@("ret(x)"))
    check(
      "x", "u1", "u2", "u3", "u4")(
      )
  }

  test("node with multiple weak uses (x could be dead)") {
    makeCFG(0@@("v", "x", "y=weak(x)", "z=weak(x)") -> 1@@("ret(v)"))
    check(
      "v")(
      "x", "y", "z")
  }

  test("inductive variable weak capture (in loop)") {
    makeCFG(0 -> wd(1@@("x=idx_dummy()", "y=idx_dummy()", "w=weak(x)")) -> 2@@("ret(y)"))
    check(
      "y")(
      "x", "w")
  }

  test("inductive variable weak capture (out of loop)") {
    makeCFG(0 -> wd(1@@("x=idx_dummy()", "y=idx_dummy()")) -> 2@@("w=weak(x)") -> 3@@("ret(y)"))
    check(
      "y")(
      "x", "w")
  }

  test("inductive variable weak capture (in and out of loop)") {
    makeCFG(0 -> wd(1@@("x=idx_dummy()", "y=idx_dummy()", "w1=weak(x)")) -> 2@@("w2=weak(x)") -> 3@@("ret(y)"))
    check(
      "y")(
      "x", "w1", "w2")
  }

  test("inductive variable weak capture (weak nodes could be alive)") {
    makeCFG(0 -> wd(1@@("x=idx_dummy()", "y=idx_dummy()", "w1=weak(y)")) -> 2@@("w2=weak(y)") -> 3@@("ret(y)"))
    check(
      "y", "w1", "w2")(
      "x")
  }

  test("strong use below weak") {
    makeCFG(0@@("x", "w=weak(x)", "s=use(x)"))
    check(
      "x", "s", "w")(
      )
  }

  test("strong use below indirect weak") {
    makeCFG(0@@("x", "w=weak(x)", "y=add(x,x)", "s=use(y)"))
    check(
      "x", "y", "s", "w")(
      )
  }

  test("weak use below strong") {
    makeCFG(0@@("x", "s=use(x)", "w=weak(x)"))
    check(
      "x", "s", "w")(
      )
  }

  test("liveness-significant argument of weak node makes node and other args live") {
    makeCFG(0@@("x", "y", "w=weak2(x,y)") -> 1@@("ret(x)"))
    check(
      "x", "y", "w")(
      )
  }

  test("weak node dies when its significant argument is dead") {
    makeCFG(0@@("x", "y", "w=weak2(x,y)"))
    check(
      )(
      "x", "y", "w")
  }

  test("indirect weak revival") {
    makeCFG(0@@("x", "y", "z=add(y,y)", "w=weak2(x,z)") -> 1@@("ret(x)"))
    check(
      "x", "y", "z", "w")(
      )
  }

  test("indirect weak elimination") {
    makeCFG(0@@("x", "y", "z=add(y,y)", "w=weak2(x,z)"))
    check(
      )(
      "x", "y", "z", "w")
  }

  test("indirect weak elimination doesn't affect live node") {
    makeCFG(0@@("x", "y", "z=add(y,y)", "w=weak2(x,z)") -> 1@@("ret(y)"))
    check(
      "y")(
      "x", "z", "w")
  }

  test("diamond weak revival (JET-13839)") {
    makeCFG(0@@("x", "y") -> (1@@"w1=weak(x)" || 2@@"w2=weak(y)") -> 3@@("use(x)", "use(y)"))
    check(
      "x", "y", "w1", "w2")(
      )
  }

  test("partial diamond weak revival") {
    makeCFG(0@@("x", "y") -> (1@@"w1=weak(y)" || 2@@"w2=weak(x)") -> 3@@"ret(x)")
    check(
      "x", "w2")(
      "y", "w1")
  }

  test("diamond weak revival via liveness-significant argument") {
    makeCFG(0@@("x", "y") -> (1@@"w1=weak(x)" || 2@@"w2=weak2(x,y)") -> 3@@"ret(x)")
    check(
      "x", "y", "w1", "w2")(
      )
  }

  test("indirect diamond weak revival") {
    makeCFG(0@@("x", "y", "z=add(y,y)") -> (1@@"w1=weak(y)" || 2@@"w2=weak2(x,z)") -> 3@@"ret(x)")
    check(
      "x", "y", "z", "w1", "w2")(
      )
  }

  test("DCE leaves dead Cmp JET-12745") {
    makeCFG((0 -> 8 -> dw(1 -> (2 || !8) -> ((3 -> (4 || (7 -> 4))) || 4) -> 5))  |>|
      0@@("x=read()", "z", "zz")                                                  |>|
      8@@("p3=phi(x,z)")                                                          |>|
      1@@("p1=phi(zz,p2)")                                                        |>|
      3@@("y=read()", "c=cmp(y,p1)")                                              |>|
      4@@"p2=phi(p1,p1,y)"                                                        |>|
      5@@("use(x)"))

    n("z").replaceUsesBy(n("p1"))
    n("zz").replaceUsesBy(n("p3"))

    EmptyBlocksElimination()

    check("x")("p1", "y", "c", "p2")

    checkIRConsistency(CheckLevels.Important)
  }
}
