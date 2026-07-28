/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class LivenessAnalysisSuite extends CompilerSuite with GlobalNodesBuilder with LivenessAnalysis with IRTransformationsCollection {

  private def named(node: Node) = node match {
    case N(_) => true
    case _ => false
  }

  private def check(actual: Iterable[Node], expected: Iterable[Node]): Unit = {
    val actSet = actual.toSet filter named
    val expSet = expected.toSet

    actSet shouldBe expSet
  }

  private def check0(f: Block => collection.Set[Node], expectedByBlock: Seq[Seq[Node]]): Unit = {
    for ((expected, i) <- expectedByBlock.zipWithIndex) {
      check(f(i), expected)
    }
  }

  private def checkLiveIn(expectedByBlock: Seq[Node]*): Unit = {
    check0(liveness.in, expectedByBlock)
  }

  private def checkLiveOut(expectedByBlock: Seq[Node]*): Unit = {
    check0(liveness.out, expectedByBlock)
  }

  private var liveness: CFGLiveness = _

  private def init(): Unit = {
    splitCriticalEdges()
    liveness = calcCFGLiveness()
  }

  private def s(x: Node*): Seq[Node] = x

  test("simple line") {
    makeCFG(0@@"x=pinned()" -> 1@@"y=pinned()" -> 2@@("use(x)", "use(y)"))

    init()

    checkLiveIn(s(), s("x"), s("x", "y"))
    checkLiveOut(s("x"), s("x", "y"), s())
  }

  test("diamond without phi") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> (1@@"use(x)" || 2@@"use(x)") -> 3@@"use(y)")

    init()

    checkLiveIn(s(), s("x", "y"), s("x", "y"), s("y"))
    checkLiveOut(s("x", "y"), s("y"), s("y"), s())
  }

  test("diamond with phi") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> (1 || 2) -> 3@@("p=phi(x,y)"))

    init()

    checkLiveIn(s(), s("x"), s("y"), s("p"))
    checkLiveOut(s("x", "y"), s("x"), s("y"), s())
  }

  test("diamond with phi with it's use") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> (1 || 2) -> 3@@("p=phi(x,y)", "use(p)"))

    init()

    checkLiveIn(s(), s("x"), s("y"), s("p"))
    checkLiveOut(s("x", "y"), s("x"), s("y"), s())
  }

  test("diamond with phi with it's use and use of phi-arg") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> (1 || 2) -> 3@@("p=phi(x,y)", "use(p)") -> 4@@"use(x)")

    init()

    checkLiveIn(s(), s("x"), s("x", "y"), s("x", "p"), s("x"))
    checkLiveOut(s("x", "y"), s("x"), s("y", "x"), s("x"), s())
  }

  test("diamond with phi with it's use in next block and use of phi-arg") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> (1 || 2) -> 3@@("p=phi(x,y)", "use(p)") -> 4@@("use(p)", "use(x)"))

    init()

    checkLiveIn(s(), s("x"), s("x", "y"), s("x", "p"), s("x", "p"))
    checkLiveOut(s("x", "y"), s("x"), s("y", "x"), s("x", "p"), s())
  }

  test("simple cycle without critical edges") {
    makeCFG(0@@"x=pinned()" -> 1@@("p=phi(x,y)", "use(p)") -> 2@@"y=use(x)" -> ((4 -> 1 -> end) || 3@@"use(p)"))

    init()

    checkLiveIn(s(), s("x", "p"), s("x", "p"), s("p"), s("x", "y"))
    checkLiveOut(s("x"), s("x", "p"), s("x", "y", "p"), s(), s("x", "y"))
  }

  val obj = makeSymClass("Object", symObj)

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("new")({
        case Seq() => New(sig(obj))()
      })
    ) ++ super.parsableAttributes()
  }

  test("phi V in x-block") {
    makeCFG(0@@"n=new()" -> (1@@("n1=new()", "xspinal()") || 2@@("n2=new()", "xspinal()")) -> xb(3)@@("p=phi(n,n,n,n1,n,n2)", "use(p)"))

    init()

    checkLiveIn(s(), s("n"), s("n"), s("p"))
    checkLiveOut(s("n"), s(), s(), s())
  }

  test("phi args in x-block") {
    makeCFG(0@@("x=pinned()", "y=pinned()") -> ((1@@("xspinal()", "xspinal()") || 2) -> xb(3)@@("p=phi(x,x,x,y)", "use(p)")) -> 4)
    init()
    checkLiveIn(
      s(),
      s("x", "y"), // block 1: x and y are args to phi(x, _, x, y) by HandlerAnchor, xspinal(), xspinal()
      s("x"), // block 2: x is arg to phi(_, x, _, _) by HandlerAnchor of this block
      s("p"),
      s())
    checkLiveOut(s("x", "y"), s(), s(), s(), s())
  }
}
