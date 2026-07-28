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

class UnmovableAnalysisSuite extends CompilerSuite with GlobalNodesBuilder with UnmovableAnalysis {

  override def parsableAttributes(): Seq[Attribute] = {
    Seq(
      new SimpleAttribute("obj")    ({ case Seq() => Fake(TRefType)() }),
      new SimpleAttribute("begin")  ({ case Seq(x) => BeginLocalUnmovable(x) }),
      new SimpleAttribute("end")    ({ case Seq(x) => EndLocalUnmovable(x) }),

    ) ++ super.parsableAttributes()
  }

  private def phi(args: Node*) = Phi(TRefType)(args: _*)

  private var analysis: LocalUnmovableAnalysis = _

  private def testCFG(start: SubGraph): Unit = {
    makeCFG(start)
    analysis = analyzeLocalUnmovable()
  }

  private def negative(start: SubGraph): Unit = {
    try { testCFG(start); assert(false) } catch {
      case _: Throwable => // passed
    }
  }

  private def check(block: Block)(expected: Node*)(implicit pos: source.Position): Unit = {
    analysis.out(block).values.toSet shouldBe (expected).toSet
  }

  test("simple line") {
    testCFG((0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7)  |>|
      0@@("x=obj()", "y=obj()", "z=obj()")          |>|
      1@@ "bx=begin(x)"                             |>|
      2@@ "by=begin(y)"                             |>|
      3@@ "bz=begin(z)"                             |>|
      4@@ "end(bx)"                                 |>|
      5@@ "end(by)"                                 |>|
      6@@ "end(bz)")

    check(1) ()
    check(2) ("x")
    check(3) ("x", "y")
    check(4) ("x", "y", "z")
    check(5) ("y", "z")
    check(6) ("z")
    check(7) ()
  }

  test("simple phi") {
    testCFG((0 -> (1 || 2) -> 3 -> 4)  |>|
      0@@("x=obj()", "y=obj()")        |>|
      3@@("p=phi(x,y)", "b=begin(p)")  |>|
      4@@ "end(b)")

    check(1) ()
    check(2) ()
    check(3) ()
    check(4) ("p")
  }

  test("several enter-exits on simple line") {
    testCFG((0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7)  |>|
      0@@ "x=obj()"                                 |>|
      1@@ "b1=begin(x)"                             |>|
      2@@ "b2=begin(x)"                             |>|
      3@@ "end(b2)"                                 |>|
      4@@ "b3=begin(x)"                             |>|
      5@@ "end(b3)"                                 |>|
      6@@ "end(b1)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
    check(4) ("x")
    check(5) ("x")
    check(6) ("x")
    check(7) ()
  }

  test("cfg with diamonds") {
    testCFG((0 -> (1 || 2) -> 3 -> (4 || 5) -> 6) |>|
      0@@ "x=obj()"                               |>|
      1@@ "b1=begin(x)"                           |>|
      2@@ "b2=begin(x)"                           |>|
      3@@ "p=phi(b1,b2)"                          |>|
      4@@ "end(p)"                                |>|
      5@@ "end(p)")

    check(1) ()
    check(2) ()
    check(3) ("x")
    check(4) ("x")
    check(5) ("x")
    check(6) ()
  }

  test("multiple begins") {
    testCFG((0 -> 1 -> (2 -> 3 || 4 -> 5) -> 6) |>|
      0@@ "x=obj()"                             |>|
      2@@ "b1=begin(x)"                         |>|
      4@@ "b2=begin(x)"                         |>|
      6@@("p=phi(b1,b2)", "end(p)"))

    check(1) ()
    check(2) ()
    check(3) ("x")
    check(4) ()
    check(5) ("x")
    check(6) ("x")
  }

  test("multiple begins with phi") {
    testCFG((0 -> 1 -> (2 -> 3 || 4 -> 5) -> 6) |>|
      0@@("x=obj()", "y=obj()")                 |>|
      2@@ "b1=begin(x)"                         |>|
      4@@ "b2=begin(y)"                         |>|
      6@@("p=phi(x,y)", "bp=phi(b1,b2)", "use(p)", "end(bp)"))

    check(1) ()
    check(2) ()
    check(3) ("x")
    check(4) ()
    check(5) ("y")
    check(6) ("p")
  }

  test("multiple begins with phi no uses") {
    testCFG((0 -> 1 -> (2 -> 3 || 4 -> 5) -> 6) |>|
      0@@("x=obj()", "y=obj()")                 |>|
      2@@ "b1=begin(x)"                         |>|
      4@@ "b2=begin(y)"                         |>|
      6@@("bp=phi(b1,b2)", "end(bp)"))

    check(1) ()
    check(2) ()
    check(3) ("x")
    check(4) ()
    check(5) ("y")
    check(6) (phi(6, "x", "y"))
  }

  test("multiple ends") {
    testCFG((0 -> 1 -> (2 || 3) -> 4) |>|
      0@@ "x=obj()"                   |>|
      1@@ "b=begin(x)"                |>|
      2@@ "end(b)"                    |>|
      3@@ "end(b)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
    check(4) ()
  }

  test("simple loop") {
    testCFG((0 -> 1 -> dw(2 -> 3) -> 4 -> 5)  |>|
      0@@("x=obj()", "y=obj()")               |>|
      1@@ "bx=begin(x)"                       |>|
      2@@ "by=begin(y)"                       |>|
      3@@ "end(by)"                           |>|
      4@@ "end(bx)")

    check(1) ()
    check(2) ("x")
    check(3) ("x", "y")
    check(4) ("x")
    check(5) ()
  }

  test("tricky loop") {
    testCFG((0 -> 1 -> dw(2 -> 3) -> 4 -> 5)  |>|
      0@@ "x=obj()"                           |>|
      1@@ "b1=begin(x)"                       |>|
      2@@("p=phi(b1,b2)", "end(p)")           |>|
      3@@ "b2=begin(x)"                       |>|
      4@@ "end(p)")

    check(1) ()
    check(2) ("x")
    check(3) ()
    check(4) ("x")
    check(5) ()
  }

  test("JET-13081 loop") {
    testCFG((0 -> 1 -> dw(2 -> (3 -> 4 || 5 -> 6) -> 7) -> 8) |>|
      0@@("x=obj()", "y=obj()")                               |>|
      3@@ "bx=begin(x)"                                       |>|
      5@@ "by=begin(y)"                                       |>|
      7@@("p=phi(x,y)", "bp=phi(bx,by)", "use(p)", "end(bp)"))

    check(1) ()
    check(2) ()
    check(3) ()
    check(4) ("x")
    check(5) ()
    check(6) ("y")
    check(7) ("p")
    check(8) ()
  }

  test("assertion") {
    testCFG((0 -> 1 -> (!2@@"halt()" || 3)) |>|
      0@@ "x=obj()"                         |>|
      1@@ "b=begin(x)"                      |>|
      3@@ "end(b)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
  }

  test("assertion with multiple incoming edges - normal") {
    testCFG((0 -> (1 -> (!2@@"halt()" || 3) || 4 -> (!2 || 5)) -> 6) |>|
      0@@ "x=obj()"                                                  |>|
      1@@ "b1=begin(x)"                                              |>|
      3@@ "end(b1)"                                                  |>|
      4@@ "b2=begin(x)"                                              |>|
      5@@ "end(b2)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
    check(4) ()
    check(5) ("x")
    check(6) ()
  }

  test("assertion with multiple incoming edges - different objects") {
    testCFG((0 -> (1 -> (!2@@"halt()" || 3) || 4 -> (!2 || 5)) -> 6) |>|
      0@@("x=obj()", "y=obj()")                                      |>|
      1@@ "b1=begin(x)"                                              |>|
      3@@ "end(b1)"                                                  |>|
      4@@ "b2=begin(y)"                                              |>|
      5@@ "end(b2)")

    check(1) ()
    check(2) ("x", "y")
    check(3) ("x")
    check(4) ()
    check(5) ("y")
    check(6) ()
  }

  test("assertion with multiple incoming edges - no object") {
    testCFG((0 -> (!2 || 1) -> (!2@@"halt()" || 3)) |>|
      0@@ "x=obj()"                                 |>|
      1@@ "b=begin(x)"                              |>|
      3@@ "end(b)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
  }

  test("exceptions") {
    testCFG((0 -> 1 -> 2 -> 3 -> 4) |>| (2 -> xb(5) -> 4) |>|
      0@@ "x=obj()"                                       |>|
      1@@ "b=begin(x)"                                    |>|
      2@@ "xspinal()"                                     |>|
      3@@ "end(b)"                                        |>|
      5@@ "end(b)")

    check(1) ()
    check(2) ("x")
    check(3) ("x")
    check(4) ()
    check(5) ("x")
  }

  test("incorrect unpaired case - no uses") { // Should not occur in actual code (javac ensures that), but we can handle it anyway
    testCFG((0 -> 1 -> (2 || 3) -> 4 -> 5) |>|
      0@@("x=obj()", "y=obj()")            |>|
      1@@("bx=begin(x)", "by=begin(y)")    |>|
      4@@("p=phi(bx,by)", "end(p)"))

    check(1) ()
    check(2) ("x", "y")
    check(3) ("x", "y")
    check(4) (phi(4, "x", "y"))
    check(5) () // !!!! Incorrect !!!!
                // Actually exactly one object will be unmovable here:
                // either x or y depending on control-flow.
  }

  test("incorrect unpaired case - with uses") { // Should not occur in actual code (javac ensures that), but we can handle it anyway
    testCFG((0 -> 1 -> (2 || 3) -> 4 -> 5) |>|
      0@@("x=obj()", "y=obj()")            |>|
      1@@("bx=begin(x)", "by=begin(y)")    |>|
      4@@("bp=phi(bx,by)", "p=phi(x,y)", "use(p)", "end(bp)"))

    check(1) ()
    check(2) ("x", "y")
    check(3) ("x", "y")
    check(4) ("p")
    check(5) () // !!!! Incorrect !!!!
                // Actually exactly one object will be unmovable here:
                // either x or y depending on control-flow.
  }

  test("no return") {
    testCFG((0 -> 1 -> 2@@"halt()") |>|
      0@@ "x=obj()"                 |>|
      1@@ "begin(x)")

    check(1) ()
    check(2) ("x")
  }

  test("negative begin") {
    negative((0 -> 1) |>|
      0@@ "x=obj()"   |>|
      1@@ "begin(x)")
  }

  test("negative end") {
    negative((0 -> 1) |>|
      0@@ "x=obj()"   |>|
      1@@ "end(x)")
  }

  test("negative diamond begin") {
    negative((0 -> (1 || 2) -> 3) |>|
      0@@ "x=obj()"               |>|
      1@@ "begin(x)"              |>|
      3@@ "end(x)")
  }

  test("negative diamond end") {
    negative((0 -> (1 || 2) -> 3) |>|
      0@@("x=obj()", "begin(x)")  |>|
      1@@ "end(x)")
  }

  test("negative loop begin") {
    negative((0 -> dw(1 -> 2) -> 3) |>|
      0@@ "x=obj()"                 |>|
      1@@ "begin(x)"                |>|
      3@@ "end(x)")
  }

  test("negative loop end") {
    negative((0 -> dw(1 -> 2) -> 3) |>|
      0@@("x=obj()", "begin(x)")    |>|
      2@@ "end(x)")
  }

}
