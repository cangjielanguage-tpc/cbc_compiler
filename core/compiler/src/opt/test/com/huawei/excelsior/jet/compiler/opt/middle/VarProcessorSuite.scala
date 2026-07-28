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
import com.huawei.excelsior.jet.util.ScalaCollections
import org.scalatest.Inside.*

class VarProcessorSuite extends CompilerSuite
                            with GlobalNodesBuilder
                            with VarProcessor {

  test("single var SSA completion") {
    /**    0
      *   / \
      *  1   2
      *  |  /|
      *  | 3 4
      *   \|/
      *    5
      */
    makeCFG(0@@("a", "b", "c") -> (1 || (2 -> (3 || 4))) -> 5)
    val a = "a": Node
    val b = "b": Node
    val c = "c": Node

    splitCriticalEdges()

    var x0: FakeControlledUnary = null
    var x1: FakeControlledUnary = null
    var x2: FakeControlledUnary = null
    var x3: FakeControlledUnary = null
    var x4: FakeControlledUnary = null
    var x5: FakeControlledUnary = null

    withNewVar(IntType) { (assignAt, readAt) =>
      assignAt(0: Block, a)
      assignAt(1: Block, b)
      assignAt(2: Block, c)
      assignAt(3: Block, a)
      assignAt(4: Block, b)

      def useAt(b: Int): FakeControlledUnary = {
        val r = readAt(b.blockEnd.inCtrl)
        addControlledUnaryNode(r, r).asInstanceOf[FakeControlledUnary]
      }

      x0 = useAt(0)
      x1 = useAt(1)
      x2 = useAt(2)
      x3 = useAt(3)
      x4 = useAt(4)
      x5 = useAt(5)
    }

    completeSSA()

    x0.inValue should be (a)
    x1.inValue should be (b)
    x2.inValue should be (c)
    x3.inValue should be (a)
    x4.inValue should be (b)

    inside (x5.inValue) { case Phi(block, a0, a1, a2) =>
      block should be (5: Block)
      (a0, a1, a2) should matchPattern {
        case (`b`, `a`, `b`) =>
      }
    }
  }

  for (eopType <- Seq(EopType.Null, EopType.Plain, EopType.Eop(tI))) {

    // Regression test for EopType.Null hack in AbstractInterpreter.
    test(s"complex var completion with $eopType in loop (JET-15704)") {
      /**    0
        *   / \
        *  1   2
        *   \ /
        *    3<--,
        *   /|\  |
        *  8 4 5 |
        *     \| |
        *      6 |
        *      |/
        *      7
        */
      makeCFG(0 -> (1 || 2) -> wd(3 -> (4 || 5) -> 6 -> 7) -> 8)
      
      val a = addNode(eopType)
      
      splitCriticalEdges()
  
      var x1: FakeControlledUnary = null
      var x2: FakeControlledUnary = null
      var x3: FakeControlledUnary = null
      var x4: FakeControlledUnary = null
      var x6: FakeControlledUnary = null
      var x7: FakeControlledUnary = null
  
      withNewVar(eopType) { (assignAt, readAt) =>
        assignAt(1: Block, Null())
        assignAt(2: Block, NoValue())
        assignAt(4: Block, Null())
        assignAt(7: Block, a)
  
        def useAt(b: Int): FakeControlledUnary = {
          val r = readAt(b.blockEnd.inCtrl)
          addControlledUnaryNode(r, r).asInstanceOf[FakeControlledUnary]
        }
  
        x1 = useAt(1)
        x2 = useAt(2)
        x3 = useAt(3)
        x4 = useAt(4)
        x6 = useAt(6)
        x7 = useAt(7)
      }
  
      completeSSA()
  
      x1.inValue should be (Null())
      x2.inValue should be (NoValue())
  
      val p3 = ScalaCollections.singleElement(3.phies)
      p3.tpe should be (eopType)
      p3.argsSeq should be (Seq(Null(), NoValue(), a))
      x3.inValue should be (p3)
  
      x4.inValue should be (Null())
  
      val p6 = ScalaCollections.singleElement(6.phies)
      p6.tpe should be (eopType)
      p6.argsSeq should be (Seq(Null(), p3))
      x6.inValue should be (p6)
  
      x7.inValue should be (a)
    }
  }

  test("replace uses") {
    /**    0
      *    |
      *    1<--,
      *   / \  |
      *  2   3 |
      *   \ /  |
      *    4---'
      *    |
      *    5
      */
    makeCFG(0@@"a" -> dw(1@@("p1=phi(a,a4)", "a1=controlled(a)") -> (2 || 3) -> 4@@("p4=phi(a,a1)", "a4=controlled(a)")) -> 5@@"ret(a)")
    val a = "a": Node

    splitCriticalEdges()

    replaceAllValueUsesByVar(a)

    completeSSA()

    a.valueUses.toSet should be (Set(n("a1"), n("p1"), n("a4"), n("p4"), Return.unique.get))
  }

  test("replace single phi") {
    /**    0
      *   / \
      *  1   2
      *  |  /|
      *  | 3 4
      *   \|/
      *    5
      */
    makeCFG(0@@("a", "b", "c") -> (1 || (2 -> (3 || 4))) -> 5@@("p=phi(a,b,c)", "ret(p)"))
    val a = "a": Node
    val b = "b": Node
    val c = "c": Node

    replacePhiByVar(("p": Node).asInstanceOf[Phi])

    completeSSA()

    inside (Return.unique.get.inValue) { case Phi(b5, x, y, z) =>
      b5 should be (5: Block)
      (x, y, z) should matchPattern {
        case (`a`, `b`, `c`) =>
      }
    }
  }

  test("replace multiple phies") {
    /**      0
      *      |
      *      1
      *     / \
      *    2   3
      *   / \ /
      *  4   5
      *   \ /
      *    6
      */
    makeCFG(0@@("a", "b", "c") -> ((1 -> 2 -> (4 || 5@@("p5=phi(b,c)"))) |>| (1 -> 3 -> 5)) -> 6@@("p6=phi(a,p5)", "ret(p6)"))
    val a = "a": Node
    val b = "b": Node
    val c = "c": Node

    splitCriticalEdges()

    replacePhiByVar(("p5": Node).asInstanceOf[Phi])
    replacePhiByVar(("p6": Node).asInstanceOf[Phi])

    completeSSA()

    inside (Return.unique.get.inValue) { case Phi(b6, x, Phi(b4, y, z)) =>
      b6 should be (6: Block)
      b4 should be (5: Block)
      (x, y, z) should matchPattern {
        case (`a`, `b`, `c`) =>
      }
    }
  }

  test("replace loop phi") {
    /**    0
      *    |
      *    1<--,
      *   / \  |
      *  2   3 |
      *   \ /  |
      *    4---'
      *    |
      *    5
      */
    makeCFG(0@@("a", "b") -> dw(1@@"p1=phi(a,p4)" -> (2 || 3) -> 4@@"p4=phi(p1,b)") -> 5@@"ret(p4)")
    val a = "a": Node
    val b = "b": Node

    splitCriticalEdges()

    replacePhiByVar(n("p1").asInstanceOf[Phi])
    replacePhiByVar(n("p4").asInstanceOf[Phi])

    completeSSA()

    inside (Return.unique.get.inValue) { case p @ Phi(b4, Phi(b1, x, y), z) =>
      b4 should be (4: Block)
      b1 should be (1: Block)
      // for some reason scalac can't match Phi(...) inside, so both phies are matched before
      (x, y, z) should matchPattern {
        case (`a`, `p`, `b`) =>
      }
    }
  }
}
