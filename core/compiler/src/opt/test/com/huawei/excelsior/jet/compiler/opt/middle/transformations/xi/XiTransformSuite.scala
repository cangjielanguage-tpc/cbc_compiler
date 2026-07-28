/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.middle.DCEComponent
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.Loops

class XiTransformSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with CFGTransformationDSL
     with XiTransform
     with DCEComponent
     with IRTransformationsCollection {

  override def transformation(): Unit = {} // all transformations should be done in beforeWithPre

  override def makeDebug: Boolean = false 

  ////////////////////////////////////
  // extract

  test("extract single block") {
    beforeWithPre(0 -> (1 || 2) -> 3 -> 4, {
      val b3 = b(3)
      xiTransform { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33) -> 4)
  }

  test("extract disconnected blocks") {
    beforeWithPre(0 -> (1 || 2) -> 3 -> (4 || 5) -> 6 -> 7, {
      val b3 = b(3)
      val b6 = b(6)
      xiTransform { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
        scheduler.extract(b6, b6.inEdge(1))
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33) -> (4 -> 6 || 5 -> 66) -> 7)
  }

  test("extract disconnected blocks - 2") {
    beforeWithPre(0 -> (1 || 2) -> 3 -> (4 -> (5 || 6) -> 7 || 8 ) -> 9, {
      val b3 = b(3)
      val b7 = b(7)
      xiTransform { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
        scheduler.extract(b7, b7.inEdge(1))
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33) -> (4 -> (5 -> 7 || 6 -> 77) || 8 ) -> 9)
  }

  test("extract connected blocks") {
    beforeWithPre(0 -> (1 || 2) -> 3 -> (4 -> (5 || 6) -> 7 || 8 ) -> 9, {
      val b3 = b(3)
      xiTransform { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
        scheduler.extract(b(4))
        scheduler.extract(b(5))
        scheduler.extract(b(7))
      }
    })
    after(0 -> (1 -> 3) -> (4 -> (5 || 6) -> 7 || 8 ) -> 9 |>|
      0 -> 2 -> 33 -> (44 -> (55 -> 77 -> 9 || 6) || 8)
    )
  }


  test("extract block without uses") {
    beforeWithPre(0 -> (1 || 2) -> 3@@("w=write()") -> 4, {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(0))
      } { (xi, _) =>
        val w = n("w").asInstanceOf[PutField]
        val cw = xi.copyOf(w)
        (cw.field, cw.obj) shouldBe (w.field, w.obj)
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33) -> 4)
  }

  test("extract block with uses in succs") {
    beforeWithPre(0@@("a") -> (1 || 2) -> 3@@("u=use(a)") -> 4@@("ret(u)"), {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      } { (xi, _) =>
        completeSSA()
        eliminateUnreachableCode()
        eliminateDeadCode()

        val u = n("u").asInstanceOf[FakeUse]
        val cu = ScalaCollections.singleElement(collect[FakeUse](xi.copyOf(b3).uses))
        cu.inValue shouldBe u.inValue

        val b4 = b(4)
        val phi = ScalaCollections.singleElement(b4.phies)
        phi.argsSeq shouldBe Seq(u, cu)
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33)-> 4)
  }

  test("extract block with phi uses in succs") {
    beforeWithPre(0@@("a", "b") -> (1 || 2) -> 3@@("p=phi(a,b)") -> 4@@("ret(p)"), {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      } { (xi, _) =>
        completeSSA()
        eliminateUnreachableCode()
        eliminateDeadCode()
        val b4 = b(4)
        val phi = ScalaCollections.singleElement(b4.phies)
        phi.argsSeq shouldBe (Seq("a", "b") map (n(_)))
      }
    })
    after(0 -> (1 -> 3 || 2 -> 33)-> 4)
  }


  test("extract block without uses in phi succs") {
    beforeWithPre(0@@("a", "b") -> ((1 || 2) -> 3 || 4) -> 5@@("ret(phi(a,b))"), {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      } { (xi, _) =>
        completeSSA()
        val b5 = b(5)
        val phi = ScalaCollections.singleElement(b5.phies)
        phi.argsSeq shouldBe (Seq("a", "b", "a") map (n(_)))
      }
    })
    after(0 -> ((1 -> 3 || 2 -> 33) || 4) -> 5)
  }

  test("extract block with uses in phi succs") {
    beforeWithPre(0@@("a", "b") -> ((1 || 2) -> 3@@("u=use(a)") || 4) -> 5@@("ret(phi(u,b))"), {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      } { (xi, _) =>
        completeSSA()

        val u = n("u").asInstanceOf[FakeUse]
        val cu = ScalaCollections.singleElement(collect[FakeUse](xi.copyOf(b3).uses))
        cu.inValue shouldBe u.inValue

        val b5 = b(5)
        val phi = ScalaCollections.singleElement(b5.phies)
        phi.argsSeq shouldBe Seq(u, n("b"), cu)
      }
    })
    after(0 -> ((1 -> 3 || 2 -> 33) || 4) -> 5)
  }

  test("extract block with phi uses in phi succs") {
    beforeWithPre(0@@("a", "b", "c") -> ((1 || 2) -> 3@@("q=phi(a,c)") || 4) -> 5@@("ret(phi(q,b))"), {
      val b3 = b(3)
      xiTransformAndPostProcess { scheduler =>
        scheduler.extract(b3, b3.inEdge(1))
      } { (xi, _) =>
        completeSSA()
        val b5 = b(5)
        val phi = ScalaCollections.singleElement(b5.phies)
        phi.argsSeq shouldBe (Seq("a", "b", "c") map (n(_)))
      }
    })
    after(0 -> ((1 -> 3 || 2 -> 33) || 4) -> 5)
  }

  // extract
  ////////////////////////////////////

  ////////////////////////////////////
  // peel

  test("peel while-do") {
    beforeWithPre(0 -> wd(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.peel(loop)
      }
    })
    after(0 -> 1 -> 2 -> wd(11 -> 22) -> 3 |>| 1 -> 3)
  }

  test("peel do-while") {
    beforeWithPre(0 -> dw(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.peel(loop)
      }
    })
    after(0 -> 1 -> 2 -> 91 -> lp(11 -> 22 -> 92, exits(22)) -> 3 |>| 2 -> 3)
  }

  test("peel endless loop") {
    beforeWithPre(0 -> !wd(1 -> 2), {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.peel(loop)
      }
    })
    after(0 -> 1 -> 2 -> !wd(11 -> 22))
  }

  // peel
  ////////////////////////////////////

  ////////////////////////////////////
  // version

  private def versionAndCheck(anchors: LowerPoint*)(checks: (XiResult, If) => Unit): Unit = {
    val cond = addSomeConditionNode()
    xiTransformAndPostProcess { scheduler =>
      scheduler.version(PredicateConstructor.atom(cond), anchors: _*)
    } { case (xi, (Seq(branch), _, _)) =>
      // Reduce number of empty blocks to simplify checks.
      EmptyBlocksElimination()

      branch.selector shouldBe cond
      checks(xi, branch)
    }
  }

  test("version single spinal") {
    beforeWithPre(0@@("x=spinal()", "y=spinal()", "z=spinal()"), {
      val y = n("y").asInstanceOf[SpinalNode]
      versionAndCheck(y) { (xi, branch) =>
        branch.block shouldBe b(0)
        n("x").block shouldBe branch.block

        branch.falseBlock.spine.toSet shouldBe Set(y)
        branch.trueBlock.spine.toSet shouldBe Set(xi.copyOf(y))

        n("z").block.predBlocks.toSet shouldBe branch.succBlocks.toSet
      }
    })
    after(0/*x*/ -> (1/*copyOf(y)*/ || 2/*y*/) -> 3/*z*/)
  }

  test("copy single block with parameters") {
    beforeWithPre(0 @@ "x=spinal()" -> xb(1)@@("q=catch()", "u=use(q)"), {
      val x = n("x").asInstanceOf[SpinalNode]
      val u = n("u").asInstanceOf[FakeSpinalUnary]
      val q = n("q").asInstanceOf[Catch]
      versionAndCheck(x,1.blockEnd) { (xi, _) =>
        // Catch should be copied as well as its uses
        ScalaCollections.singleElement(q.uses) shouldBe u

        val newCatch = xi.copyOf(xb(1)).catchNode
        ScalaCollections.singleElement(newCatch.uses) shouldBe xi.copyOf(u)
      }
    })
    after(0 /*test*/ ->
      ( (1 /*x*/         -> 3 /*anchor*/         -> (7 /*ret*/ || xb(5)))
        ||
        (2 /*copyOf(x)*/ -> 4 /*copyOf(anchor)*/ -> (7 /*ret*/ || xb(6))))
    )
  }

  test("version multiple with loop") {
    beforeWithPre(0 -> (1@@("x=spinal()") || (wd(2 -> 3@@("marker=spinal()")) -> 4@@("y=spinal()"))) -> 5, {
      val x = n("x").asInstanceOf[SpinalNode]
      val y = n("y").asInstanceOf[SpinalNode]
      versionAndCheck(x, y) { (_, branch) =>
        branch.block shouldBe b(0)
      }
    })
    after(0 -> (
         (61 -> ((1  -> 80) || (wd(2  -> 3 ) -> 4  -> 90))) ||
        !(62 -> ((11 -> 80) || (wd(22 -> 33) -> 44 -> 90)))
      ) -> 5)
  }

  // version
  ////////////////////////////////////

  ////////////////////////////////////
  // unroll

  test("unroll while-do once") {
    beforeWithPre(0 -> wd(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 1)
      }
    })
    after(0 -> lp(1 -> 2 -> 11 -> 22, exits(1, 11)) -> 3)
  }

  test("unroll while-do twice") {
    beforeWithPre(0 -> wd(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 2)
      }
    })
    after(0 -> lp(1 -> 2 -> 11 -> 22 -> 111 -> 222, exits(1, 11, 111)) -> 3)
  }

  test("unroll do-while once") {
    beforeWithPre(0 -> dw(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 1)
      }
    })
    after(0 -> lp(1 -> 2 -> 91 -> 11 -> 22 -> 92, exits(2, 22)) -> 3)
  }

  test("unroll do-while twice") {
    beforeWithPre(0 -> dw(1 -> 2) -> 3, {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 2)
      }
    })
    after(0 -> lp(1 -> 2 -> 91 -> 11 -> 22 -> 92 -> 111 -> 222 -> 923, exits(2, 22, 222)) -> 3)
  }

  test("unroll endless loop once") {
    beforeWithPre(0 -> !wd(1 -> 2), {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 1)
      }
    })
    after(0 -> !wd(1 -> 2 -> 11 -> 22))
  }

  test("unroll endless loop twice") {
    beforeWithPre(0 -> !wd(1 -> 2), {
      val loop = cfg.loops.seq.head
      loop.header shouldBe b(1)
      xiTransform { scheduler =>
        scheduler.unroll(loop, 2)
      }
    })
    after(0 -> !wd(1 -> 2 -> 11 -> 22 -> 111 -> 222))
  }

  // unroll
  ////////////////////////////////////

  ////////////////////////////////////
  // unsafe copy

  private def gotoEdge(b: Block) = b.blockEnd match {
    case goto: Goto => goto.targetEdge
    case _ => shouldNotReachHere(be)
  }

  test("copy single block") {
    beforeWithPre(0@@"x=spinal()" -> 1@@"y=spinal()" -> 2@@"z=spinal()", {
      val y = n("y").asInstanceOf[SpinalNode]
      xiTransformAndPostProcess { scheduler =>
        scheduler.unsafe.copy(gotoEdge(1), y, 1.blockEnd)
      } { (xi, _) =>
        // Reduce number of empty blocks to simplify checks.
        EmptyBlocksElimination()

        ScalaCollections.singleElement(y.block.succBlocks).spine.toSeq shouldBe Seq(xi.copyOf(y))
      }
    })
    after(0/*x*/ -> 1/*y*/ -> 11/*copyOf(y)*/ -> 2/*z*/)
  }



  test("copy subgraph") {
    beforeWithPre(0 -> 1 -> (2 || (3 -> 4)) -> 5, {
      xiTransform { scheduler =>
        scheduler.unsafe.copy(gotoEdge(4), 2.blockEnd, 4.blockEnd)
      }
    })
    after(0 -> 90 -> 1 -> (2 || (3 -> 4 -> 11 -> (22 || (33 -> 44)))) -> 5)
  }

  test("copy while-do") {
    beforeWithPre(0 -> 1 -> wd(2 -> 3) -> 4 -> 5, {
      xiTransform { scheduler =>
        val preHeader = b(1)
        val singleExit = b(4)
        val header = b(2)
        scheduler.unsafe.copy(gotoEdge(singleExit), preHeader.blockEnd, header.blockEnd, singleExit.blockEnd)
      }
    })
    after(0 -> 90 -> 1 -> wd(2 -> 3) -> 4 -> 11 -> wd(22 -> 33) -> 44 -> 5)
  }

  test("copy do-while") {
    beforeWithPre(0 -> 1 -> dw(2 -> 3) -> 4 -> 5, {
      xiTransform { scheduler =>
        val preHeader = b(1)
        val singleExit = b(4)
        val header = b(2)
        scheduler.unsafe.copy(gotoEdge(singleExit), preHeader.blockEnd, header.blockEnd, singleExit.blockEnd)
      }
    })
    after(0 -> 90 -> 1 -> lp(2 -> 3 -> 91, exits(3)) -> 4 -> 11 -> lp(22 -> 33 -> 92, exits(33)) -> 44 -> 5)
  }

  // unsafe copy
  ////////////////////////////////////

  ////////////////////////////////////
  // unsafe redirect

  test("redirect with copying") {
    beforeWithPre(0 -> (1 || 2) -> 3, {
      xiTransform { scheduler =>
        scheduler.extract(1)
        scheduler.unsafe.redirect(gotoEdge(1), _.copyOf(1))
      }
    })
    after(0 -> (1 -> 11 || 2) -> 3)
  }

  test("redirect without copying") {
    beforeWithPre(0 -> (1 || 2) -> 3, {
      xiTransform { scheduler =>
        scheduler.extract(0) // need to copy something for XiTransform to work
        scheduler.unsafe.redirect(gotoEdge(1), _ => 2)
      }
    })
    after(0 -> (1 -> 2 || 2 -> 3))
  }

  test("redirect without copying (infinite loop)") {
    beforeWithPre(0 -> (1 || 2) -> 3, {
      xiTransform { scheduler =>
        scheduler.extract(0) // need to copy something for XiTransform to work
        scheduler.unsafe.redirect(gotoEdge(1), _ => 1)
      }
    })
    after(0 -> (!wd(1) || 2) -> 3)
  }

  test("redirect misuse") {
    beforeWithPre(0@@("x", "y") -> (1 || 2) -> 3@@"phi(x,y)" -> 4 -> 5, {
      val e = intercept [AssertionError] {
        xiTransform { scheduler =>
          scheduler.extract(0) // need to copy something for XiTransform to work
          scheduler.unsafe.redirect(gotoEdge(4), _ => 3)
        }
      }
      e.getMessage should include ("cannot redirect edges to a block with phies")
    })
    after(0 -> (1 || 2) -> 3 -> 4 -> 5)
  }

  // unsafe redirect
  ////////////////////////////////////

}
