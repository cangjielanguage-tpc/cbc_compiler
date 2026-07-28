/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.codeemitter.BarrierKind.LOAD_LOAD
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.types.Guards.CHABitGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeMethodReference}
import com.huawei.excelsior.jet.util.ScalaCollections._
import com.huawei.excelsior.jet.util.graph.PostDominators
import org.scalatest.Inside.inside

import scala.language.implicitConversions

/**
 * Tests for some node transformations methods (e.g. Block.splitAfter, Blockend.makeUsesUnreachable, ...)
 */
class GeneralNodeTransformationsSuite extends CompilerSuite with GlobalNodesBuilder with CFGTransformationDSL {

  import PredicateConstructor._

  override def transformation(): Unit = {} // all transformations should be done in beforeWithPre

  override def makeDebug: Boolean = false

  startPhase(CompilerPhase.PostInline)

  test("empty block splitting") {
    makeCFG(0)
    val block = b(0)
    val blockEnd = block.blockEnd
    val goto @ Goto(_, succ) = Block.splitAfter(block)

    goto.succBlocks.toSeq should be (Seq(succ))

    block.pointsForward.toSeq shouldBe Seq(block, goto)
    succ.pointsForward.toSeq shouldBe Seq(succ, blockEnd)

    checkIRConsistency(CheckLevels.Important)
  }

  test("block splitting with pinned nodes") {
    makeCFG(0 -> (1@@"x" || 2@@"y") -> 3@@("p1=phi(x,y)", "p2=pinned()", "c=read()"))
    val block = b(3)
    val Goto(_, succ) = Block.splitAfter(block)

    val Seq(p1, p2) = Seq("p1", "p2") map (n(_).asInstanceOf[BlockParamNode])
    val c = n("c").asInstanceOf[HasInControl]
    p1.block shouldBe block
    p2.block shouldBe block
    c.inCtrl shouldBe succ

    checkIRConsistency(CheckLevels.Important)
  }

  private def s(name: String) = n(name).asInstanceOf[SpinalNode]

  test("block with control nodes splitting") {
    makeCFG(0@@(
      "c1=spinal()",
      "c2=spinal()",
      "c3=spinal()",
      "c4=spinal()"
    ))
    val block = b(0)
    val blockEnd = block.blockEnd

    val goto @ Goto(_, succ) = Block.splitAfter(s("c2"))

    block.pointsForward.toSeq shouldBe Seq(block, n("c1"), n("c2"), goto)
    succ.pointsForward.toSeq shouldBe Seq(succ, n("c3"), n("c4"), blockEnd)

    checkIRConsistency(CheckLevels.Important)
  }

  test("block splitting after with controlled") {
    makeCFG(0@@(
      "c1=write()",
      "cc=read()",
      "c2=spinal()"
    ))
    val Goto(_, succ) = Block.splitAfter(s("c1"))
    val cc = n("cc").asInstanceOf[HasInControl with HasInMemory]
    cc.inCtrl shouldBe succ
    cc.inMemory shouldBe n("c1")

    checkIRConsistency(CheckLevels.Important)
  }

  test("block splitting after with controlled with keeping") {
    makeCFG(0@@(
      "c1=write()",
      "cc=read()",
      "c2=spinal()"
    ))
    Block.splitAfter(s("c1"), keepControlled = true)
    val cc = n("cc").asInstanceOf[HasInControl with HasInMemory]
    cc.inCtrl shouldBe n("c1")
    cc.inMemory shouldBe n("c1")

    checkIRConsistency(CheckLevels.Important)
  }

  test("block splitting before with controlled") {
    makeCFG(0@@(
      "c1=write()",
      "cc=read()",
      "c2=spinal()"
    ))
    Block.splitBefore(s("c2"))
    val cc = n("cc").asInstanceOf[HasInControl with HasInMemory]
    cc.inCtrl shouldBe n("c1")
    cc.inMemory shouldBe n("c1")

    checkIRConsistency(CheckLevels.Important)
  }

  test("remove uses of non-usable block end") {
    makeCFG(0)
    val end = b(0).blockEnd

    end.succBlocks should be (empty)
    end.makeUsesUnreachable()
    end.succBlocks should be (empty)
  }

  test("remove uses of block end with 1 use") {
    makeCFG(0 -> 1)
    val end = b(0).blockEnd

    end.succBlocks.toSeq should be (Seq(b(1)))
    end.makeUsesUnreachable()
    end.succBlocks should be (empty)
  }

  test("remove uses of block end with 2 uses in different blocks") {
    makeCFG(0 -> (1 || 2))
    val end = b(0).blockEnd

    end.succBlocks.toSeq should be (Seq(b(1), b(2)))
    end.makeUsesUnreachable()
    end.succBlocks should be (empty)
  }

  test("remove uses of block end with 2 uses in one block") {
    makeCFG((0 -> 1) |>| (0 -> 1))
    val end = b(0).blockEnd

    end.succBlocks.toSeq should be (Seq(b(1), b(1)))
    end.makeUsesUnreachable()
    end.succBlocks should be (empty)
  }

  test("remove uses of block end with many uses") {
    makeCFG((0 -> (1 || 2 || 3)) |>| (0 -> 2) |>| (0 -> 2))
    val end = b(0).blockEnd

    end.succBlocks.toSeq should be (Seq(b(1), b(2), b(3), b(2), b(2)))
    end.makeUsesUnreachable()
    end.succBlocks should be (empty)
  }

  test("extract basic block input edges") {
    makeCFG(0 -> (1@@"a" || 2@@"e" || 3@@"b" || 4@@"f") -> 5@@"phi(a,e,b,f)")

    val newBlock = BBlock.extractInputEdges(b(5), Seq(b(1), b(3)) flatMap (_.blockEnd.outEdges))

    newBlock.predBlocks.toSeq should be (Seq(b(1), b(3)))
    newBlock.xSuccBlocks.toSeq should be (Seq(b(5)))
    b(5).predBlocks.toSeq should be (Seq(b(2), b(4), newBlock))

    val newPhi = singleElement(newBlock.phies)
    newPhi.argsSeq should be (Seq(n("a"), n("b")))
    singleElement(b(5).phies).argsSeq should be (Seq(n("e"), n("f"), newPhi))
  }

  test("extract exception block input edges") {
    beforeWithPost(
      0@@("a", "b", "c", "d", "anchorVal") ->
        (1@@("xspinal()", "xspinal()") || 2@@("xspinal()", "xspinal()")) ->
        xb(3)@@("p=phi(anchorVal,anchorVal,a,b,c,d)", "q=catch()", "up=use(p)", "uq=use(q)") ->
        4
      , {
        removeHandlerAnchors()
        eliminateUnreachableCode()

        val Array(na, nb, nc, nd, np, nq) = "abcdpq"  split ""  map (n(_))
        val Array(up, uq)                 = "up,uq" split "," map (n(_).asInstanceOf[FakeSpinalUnary])

        val oldBlock = xb(3)

        inside (np) { case Phi(block, a, b, c, d) =>
          // for some reason scalac can't match Phi(...) inside, so all phies are matched before
          (block, a, b, c, d) should matchPattern {
            case (`oldBlock`, `na`, `nb`, `nc`, `nd`) =>
          }
        }

        val inEdges = oldBlock.inEdges.toArray
        val newBlock = XBlock.extractInputEdges(oldBlock, Seq(inEdges(0), inEdges(2)))
        val mergeBlock = singleElement(newBlock.succBlocks)

        inside (up.inValue) { case Phi(m, Phi(l, b, d), Phi(r, a, c)) =>
          // for some reason scalac can't match Phi(...) inside, so all phies are matched before
          (m, l, b, d, r, a, c) should matchPattern {
            case (`mergeBlock`, `oldBlock`, `nb`, `nd`, `newBlock`, `na`, `nc`) =>
          }
        }

        inside (uq.inValue) { case Phi(m, x, y) =>
          // for some reason scalac can't match Phi(...) inside, so all phies are matched before
          (m, x, y) should matchPattern {
            case (`mergeBlock`, `nq`, copy: Catch) if copy.block == newBlock =>
          }
        }

    })
    after(0 -> (1 -> 10 -> 11 || 2 -> 20 -> 21) -> xb(3) -> 5 -> 4 |>| (10 || 20) -> xb(33) -> 5)
  }

  test("get pre-header") {
    makeCFG(0 -> dw(1 -> 2))
    val loops = cfg.loops

    getOrCreateLoopPreHeader(loops.loopOf(b(1))) should be (b(0), false)
    isPreHeaderOf(b(0), b(1)) should be (true)
  }

  for ((desc, graph) <- Seq(
    ("multiple forwards", () => 0 -> dw(1 -> dw(2 -> (3 || 4) -> dw(9 -> 5))) -> 6),
    ("critical forward",  () => 0 -> dw(1 -> dw(2 -> (dw(9 -> 3) || 4))) -> 5)
  )) {
    test(s"create pre-header for $desc") {
      makeCFG(graph())
      val loops = cfg.loops

      val header = b(9)
      val loop = loops.loopOf(header) ensuring (_.depth == 3)

      header.predBlocks exists (isPreHeaderOf(_, header)) should be (false)
      val (preHeader, true) = getOrCreateLoopPreHeader(loop)
      isPreHeaderOf(preHeader, header) should be (true)

      for (id <- Seq(1, 2)) {
        val outer = loops.loopOf(b(id)) ensuring (_.depth == id)
        outer.body should contain (preHeader)
      }
    }
  }

  test("get pre-header ignoring exceptional edge") {
    makeCFG(0 -> (dw(1 -> 2) || xb(3)))
    assert(b(0).succBlocks.size == 1 && b(0).xSuccBlocks.size == 2)
    val loops = cfg.loops
    getOrCreateLoopPreHeader(loops.loopOf(b(1))) should be (b(0), false)
  }

  val postExitCases = Seq(
    ("single exit exclusive",    () => 0 -> dw(1 -> 2) -> 8,         () => Some(b(8)), true),
    ("multiple exits exclusive", () => 0 -> dw(1 -> (2 || 3)) -> 8,  () => Some(b(8)), true),

    ("single exit",    () => 0 -> dw(1 -> 2) -> 8 |>| 0 -> 8,        () => Some(b(8)), false),
    ("multiple exits", () => 0 -> dw(1 -> (2 || 3)) -> 8 |>| 0 -> 8, () => Some(b(8)), false),

    ("single exit to outer loop", () => 0 -> wd(1 -> dw(2 -> 3)) -> 8, () => Some(b(1)), false),

    ("multiple exits with extra block", () => 0 -> dw(1 -> 2 -> 3) -> 8 |>| 2 -> 7 -> 8, () => None, false),

    ("single x-exit",          () => 0 -> dw(1 -> 2) -> xb(8) |>| 0 -> xb(8),           () => None, false),
    ("single x-exit and exit", () => 0 -> dw(1 -> 2 -> 3) -> xb(8) |>| 2 -> 7 -> xb(8), () => None, false),
  )

  for ((desc, graph, postExit, _) <- postExitCases) {
    test(s"get post-exit - $desc") {
      makeCFG(graph())
      val loops = cfg.loops
      getLoopPostExit(loops.loopOf(b(2))) should be (postExit())
    }
  }

  for ((desc, graph, postExit, exclusive) <- postExitCases) {
    test(s"get or create exclusive post-exit - $desc") {
      makeCFG(graph())
      val loops = cfg.loops
      val loop = loops.loopOf(b(2))

      getOrCreateExclusiveLoopPostExit(loop) match {
        case Some((exclusivePostExit, created)) =>
          created should not be exclusive
          (loop.header dominates exclusivePostExit) should be (true)
          getLoopPostExit(loop) should contain (exclusivePostExit)
          if (!created) {
            postExit() should contain (exclusivePostExit)
          }

          for (outer <- iterateUntilNull(loop.outer)(_.outer)) {
            outer.body should contain (exclusivePostExit)
          }
        case None =>
          postExit() should be (None)
      }
    }
  }

  test("insert empty diamond") {
    beforeWithPre(0, {
      val cond = addSomeConditionNode()
      val (Seq(branch), _, _) = insertEmptyDiamondBefore(0.blockEnd, atom(cond))
      branch.selector should be (cond)
    })
    after(0 -> (1 || 2) -> 3)
  }

  test("insert empty diamond predicate (!a || b)") {
    beforeWithPre(0, {
      val a = addSomeConditionNode()
      val b = addSomeConditionNode()

      val pred = !atom(a) || atom(b)

      val (Seq(ba, bb), t, f) = insertEmptyDiamondBefore(0.blockEnd, pred)

      ba.selector should be (a)
      ba.trueBlock should be (bb.block)
      ba.falseBlock should be (t)

      bb.selector should be (b)
      bb.trueBlock should be (t)
      bb.falseBlock should be (f)
    })
    after(0 -> (10 || 1) |>| 10 -> (1 || 2) -> 3)
  }

  test("insert empty diamond predicate (a || b || c)") {
    beforeWithPre(0, {
      val a = addSomeConditionNode()
      val b = addSomeConditionNode()
      val c = addSomeConditionNode()

      val pred = atom(a) || atom(b) || atom(c)

      val (Seq(ba, bb, bc), t, f) = insertEmptyDiamondBefore(0.blockEnd, pred)

      ba.selector should be (a)
      ba.trueBlock should be (t)
      ba.falseBlock should be (bb.block)

      bb.selector should be (b)
      bb.trueBlock should be (t)
      bb.falseBlock should be (bc.block)

      bc.selector should be (c)
      bc.trueBlock should be (t)
      bc.falseBlock should be (f)
    })
    after(0 -> (1 || 11) |>| 11 -> (1 || 12) |>| 12 -> (1 || 2) -> 3)
  }

  test("insert empty diamond predicate (a && b && c)") {
    beforeWithPre(0, {
      val a = addSomeConditionNode()
      val b = addSomeConditionNode()
      val c = addSomeConditionNode()

      val pred = atom(a) && atom(b) && atom(c)

      val (Seq(ba, bb, bc), t, f) = insertEmptyDiamondBefore(0.blockEnd, pred)

      ba.selector should be (a)
      ba.trueBlock should be (bb.block)
      ba.falseBlock should be (f)

      bb.selector should be (b)
      bb.trueBlock should be (bc.block)
      bb.falseBlock should be (f)

      bc.selector should be (c)
      bc.trueBlock should be (t)
      bc.falseBlock should be (f)
    })
    after(0 -> (11 || 2) |>| 11 -> (12 || 2) |>| 12 -> (1 || 2) -> 3)
  }

  test("insert empty diamond predicate (a || (b && c))") {
    beforeWithPre(0, {
      val a = addSomeConditionNode()
      val b = addSomeConditionNode()
      val c = addSomeConditionNode()

      val pred = atom(a) || (atom(b) && atom(c))

      val (Seq(ba, bb, bc), t, f) = insertEmptyDiamondBefore(0.blockEnd, pred)

      ba.selector should be (a)
      ba.trueBlock should be (t)
      ba.falseBlock should be (bb.block)

      bb.selector should be (b)
      bb.trueBlock should be (bc.block)
      bb.falseBlock should be (f)

      bc.selector should be (c)
      bc.trueBlock should be (t)
      bc.falseBlock should be (f)
    })
    after(0 -> (1 || 11) |>| 11 -> (12 || 2) |>| 12 -> (1 || 2) -> 3)
  }

  test("insert empty diamond predicate (!(a || b) && (c || d))") {
    beforeWithPre(0, {
      val a = addSomeConditionNode()
      val b = addSomeConditionNode()
      val c = addSomeConditionNode()
      val d = addSomeConditionNode()

      val pred = !(atom(a) || atom(b)) && (atom(c) || atom(d))

      val (Seq(ba, bb, bc, bd), t, f) = insertEmptyDiamondBefore(0.blockEnd, pred)

      ba.selector should be (a)
      ba.trueBlock should be (f)
      ba.falseBlock should be (bb.block)

      bb.selector should be (b)
      bb.trueBlock should be (f)
      bb.falseBlock should be (bc.block)

      bc.selector should be (c)
      bc.trueBlock should be (t)
      bc.falseBlock should be (bd.block)

      bd.selector should be (d)
      bd.trueBlock should be (t)
      bd.falseBlock should be (f)
    })
    after(0 -> (2 || 11) |>| 11 -> (2 || 12) |>| 12 -> (1 || 13) |>| 13 -> (1 || 2) -> 3)
  }

  test("insert empty tau-diamond with null test") {
    beforeWithPre(0, {
      val obj = addObjNode()
      val (Seq(nullBranch, tauBranch), _, _) = insertEmptyDiamondBefore(0.blockEnd, tauTest(CHABitGuard, TauInfo.Unknown, obj))
      nullBranch.selector should matchPattern { case Cmp(Condition.NE, `obj`, Null()) => }
      tauBranch.selector should matchPattern { case TauTest(CHABitGuard, TauInfo.Unknown, `obj`) => }
    })
    after(0 -> (11 || 2) |>| 11 -> (1 || 2) -> 3)
  }

  test("insert code") {
    makeCFG(0 -> 1 -> 2)

    optimizeBlockMemory() ensuring { b(2).blockEnd.inMemory == b(0).memoryAfter }

    val obj1 = addObjNode()
    val obj2 = addObjNode()
    val f1 = new FakeField
    val f2 = new FakeField
    val f3 = new FakeField


    // block -- end
    //    \\
    //    gf1

    val gf1 = insertCodeAfter(b(1)) { GetField(f1)(obj1) }.asInstanceOf[GetField]
    gf1.inCtrl should be (b(1))
    gf1.inMemory should be (b(1).memoryAfter)
    b(1).blockEnd.inCtrl should be (b(1))


    // block -- pf2 -- end
    //    \\
    //    gf1

    val pf2 = insertCodeAfter(b(1)) { PutField(f1)(obj2, Null()) }
    pf2.inCtrl should be (b(1))
    pf2.inMemory should be (b(1).memoryAfter)
    b(1).blockEnd.inCtrl should be (pf2)
    b(1).blockEnd.inMemory should be (pf2)


    // block -- pf2 -- gcp6 -- end
    //    \\
    //    gf1

    val gcp6 = insertCodeBefore(b(1).blockEnd) { GCPoint() }
    gcp6.inCtrl should be (pf2)
    b(1).blockEnd.inCtrl should be (gcp6)


    // block -- pf2 -- gcp6 -- end
    //    \\     \\
    //    gf1    gf3

    val gf3 = insertCodeAfter(pf2) { GetField(f2)(obj2) }.asInstanceOf[GetField]
    gf3.inCtrl should be (pf2)
    gf3.inMemory should be (pf2)


    // block -- pf2 -- gcp6 -- end
    //    \\     \\  \   \
    //    gf1    gf3  -- gf7

    val gf7 = insertCodeAfter(gcp6) { GetField(f2)(obj1) }.asInstanceOf[GetField]
    gf7.inCtrl should be (gcp6)
    gf7.inMemory should be (pf2)
    b(1).blockEnd.inCtrl should be (gcp6)


    // block -- pf2 -- pf5 -- gcp6 -- end
    //    \\     \\        \   \
    //    gf1    gf3        -- gf7

    val pf5 = insertCodeBefore(gcp6) { PutField(f3)(obj1, Null()) }
    pf5.inCtrl should be (pf2)
    pf5.inMemory should be (pf2)

    gf7.inMemory should be (pf5) // GCM placed gf7 below of pf5
    gf3.inMemory should be (pf2) // GCM placed gf3 above of pf5


    // block -- pf2 -- pf4 -- pf5 -- gcp6 -- end
    //    \\     \\               \   \
    //    gf1    gf3               -- gf7

    val pf4 = insertCodeAfter(pf2) { PutField(f3)(obj2, Null()) }
    pf4.inCtrl should be (pf2)
    pf4.inMemory should be (pf2)

    gf3.inMemory should be (pf2) // GCM placed gf3 above of pf4
  }

  test("insert code with memory in diamond") {
    makeCFG(0 -> (1 || 2) -> 3@@"w=write()")

    optimizeBlockMemory()

    n("w").asInstanceOf[HasInMemory].inMemory shouldBe entryMemory
    3.blockEnd.inMemory shouldBe n("w")

    insertCodeAfter(1) { MemBarrier(Set(LOAD_LOAD))() }
  }

  test("insert code with memory before return") {
    makeCFG(0 -> (1 || 2) -> 3)

    optimizeBlockMemory()

    3.blockEnd.inMemory shouldBe entryMemory

    val m1 = insertCodeAfter(0) { MemBarrier(Set(LOAD_LOAD))() }
    val m2 = insertCodeAfter(3) { MemBarrier(Set(LOAD_LOAD))() }
    optimizeBlockMemory()

    m2.inMemory shouldBe m1
    3.blockEnd.inMemory shouldBe m2
  }

  test("insert code with memory before another memory in diamond") {
    makeCFG(0 -> ((1 -> (2 || 3) -> 4@@"w=write()") || 5) -> 6)

    optimizeBlockMemory()

    1.blockEnd.inMemory shouldBe entryMemory
    6.blockEnd.inMemory shouldBe b(6)

    val w = n("w").asInstanceOf[SpinalMemoryNode]

    val m1 = insertCodeAfter(1) { MemBarrier(Set(LOAD_LOAD))() }
    val m2 = insertCodeBefore(w) { MemBarrier(Set(LOAD_LOAD))() }
    optimizeBlockMemory()

    m2.inMemory shouldBe m1
    w.inMemory shouldBe m2
  }

  test("insert code with unreachable memory use") {
    makeCFG(0 -> (1 || 2) -> 3)

    optimizeBlockMemory()

    2.blockEnd.inMemory shouldBe entryMemory
    3.blockEnd.inMemory shouldBe entryMemory
    
    makeUnreachable(2.inEdges)

    2.blockEnd.inMemory shouldBe entryMemory
    3.blockEnd.inMemory shouldBe entryMemory

    val m1 = insertCodeAfter(3) { MemBarrier(Set(LOAD_LOAD))() }

    m1.inMemory shouldBe entryMemory
    3.blockEnd.inMemory shouldBe m1

    val m2 = insertCodeAfter(3) { MemBarrier(Set(LOAD_LOAD))() }

    m2.inMemory shouldBe entryMemory
    m1.inMemory shouldBe m2
    3.blockEnd.inMemory shouldBe m1
  }


  test("insert code throwing") {
    makeCFG(0 -> 1 -> xb(2))
    // Initially throwing node may be inserted only manually.
    val anchors = makeNodes { at =>
      for (b <- Seq(0, 1)) yield {
        at(b)
        Invoke(new FakeMethodReference)()
      }
    }
    anchors(0).xHandlerOption should be (None)
    anchors(1).xHandlerOption should be (Some(xb(2)))

    for (i <- Seq(0, 1)) {
      val nc = insertCodeBefore(anchors(i)) { NullCheck(addObjNode()) }
      nc.canThrow should be (true)
      nc.xHandlerOption should be (anchors(i).xHandlerOption)
    }

    intercept[Throwable] {
      insertCodeAfter(b(0)) { NullCheck(addObjNode()) }
    }
  }

  test("insert code positions") {
    makeCFG(0)

    def makePos(n: Int) = BytecodePosition(10*n + 1, 10*n + 2, 10*n + 3, InlineContext.newRoot(rootMethod))
    val pos1 = makePos(1)
    val pos2 = makePos(2)
    val pos3 = makePos(3)

    b(0).pos = pos1

    val (x, y) = withPos(pos3) {
      insertCodeAfter(b(0)) {
        (GCPoint(), withPos(pos2) { GCPoint() })
      }
    }

    x.pos should be (pos1)
    y.pos should be (pos2)
  }

  test("replace by code with memory") {
    makeCFG(0@@("x=coldcode()", "y=write()"))

    optimizeBlockMemory()

    n("y").asInstanceOf[HasInMemory].inMemory shouldBe entryMemory
    0.blockEnd.inMemory shouldBe n("y")

    val m1 = replaceByCode(n("x").asInstanceOf[SpinalNode]) {
      MemBarrier(Set(LOAD_LOAD))()
    }
    val m2 = replaceByCode(n("y").asInstanceOf[SpinalNode]) {
      MemBarrier(Set(LOAD_LOAD))()
    }

    m1.inMemory shouldBe entryMemory
    m2.inMemory shouldBe m1
    0.blockEnd.inMemory shouldBe m2
  }

  test("replace by code without memory") {
    makeCFG(0 @@ ("x=coldcode()", "y=write()"))

    optimizeBlockMemory()

    n("y").asInstanceOf[HasInMemory].inMemory shouldBe entryMemory
    0.blockEnd.inMemory shouldBe n("y")

    replaceByCode(n("x").asInstanceOf[SpinalNode]) {
      ColdCodeMarker()
    }
    replaceByCode(n("y").asInstanceOf[SpinalNode]) {
      ColdCodeMarker()
    }

    0.blockEnd.inMemory shouldBe entryMemory
  }

  test("replace by code with mixed memory") {
    makeCFG(0 @@ ("x=coldcode()", "y=write()"))

    optimizeBlockMemory()

    n("y").asInstanceOf[HasInMemory].inMemory shouldBe entryMemory
    0.blockEnd.inMemory shouldBe n("y")

    var m1: MemBarrier = null
    replaceByCode(n("x").asInstanceOf[SpinalNode]) {
      m1 = MemBarrier(Set(LOAD_LOAD))()
      ColdCodeMarker()
    }
    var m2: MemBarrier = null
    replaceByCode(n("y").asInstanceOf[SpinalNode]) {
      m2 = MemBarrier(Set(LOAD_LOAD))()
      ColdCodeMarker()
    }

    m1.inMemory shouldBe entryMemory
    m2.inMemory shouldBe m1
    0.blockEnd.inMemory shouldBe m2
  }

  test("IfEq") {
    makeCFG(0@@("x", "y") -> (1 || 2) -> 3)
    makeNodes { at =>
      at(0)
      setCondition(Cmp(IntType, Condition.EQ)(n("x"), n("y")))
    }
    val branch = b(0).blockEnd.asInstanceOf[If]

    // if (x == y) t else f
    branch should matchPattern {
      case IfEq.NonCommutative(N("x"), N("y"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }

    // if (x != y) f else t
    If.invert(branch)
    branch should matchPattern {
      case IfEq.NonCommutative(N("x"), N("y"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }

    // if (x != y) t else f
    If.internal.swapExits(branch)
    branch should matchPattern {
      case IfEq.NonCommutative(N("x"), N("y"), BlockExit(_, B(2)), BlockExit(_, B(1))) =>
    }

    // if (x == y) f else t
    If.invert(branch)
    branch should matchPattern {
      case IfEq.NonCommutative(N("x"), N("y"), BlockExit(_, B(2)), BlockExit(_, B(1))) =>
    }


    branch should     matchPattern { case IfEq.NonCommutative(N("x"), N("y"), _, _) => }
    branch should not matchPattern { case IfEq.NonCommutative(N("y"), N("x"), _, _) => }
    IfEq.Commutative.cond(branch) { case (N("x"), N("y"), _, _) => true } shouldBe true
    IfEq.Commutative.cond(branch) { case (N("y"), N("x"), _, _) => true } shouldBe true
  }

  test("IfInstanceOf") {
    val tpe = symA
    makeCFG(0 -> (1 || 2) -> 3)
    makeNodes { at =>
      at(0)
      n("o") = addObjNode()
      setCondition(Cmp(IntType, Condition.NE)(InstanceOf(sigA)(n("o")), IConst(0)))
    }
    val branch = b(0).blockEnd.asInstanceOf[If]

    // if (o instanceof tpe) t else f
    branch should matchPattern {
      case IfInstanceOf(`tpe`, N("o"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }

    // if (!(o instanceof tpe)) f else t
    If.invert(branch)
    branch should matchPattern {
      case IfInstanceOf(`tpe`, N("o"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }

    // if (!(o instanceof tpe)) t else f
    If.internal.swapExits(branch)
    branch should matchPattern {
      case IfInstanceOf(`tpe`, N("o"), BlockExit(_, B(2)), BlockExit(_, B(1))) =>
    }

    // if (o instanceof tpe) f else t
    If.invert(branch)
    branch should matchPattern {
      case IfInstanceOf(`tpe`, N("o"), BlockExit(_, B(2)), BlockExit(_, B(1))) =>
    }
  }

  test("IfNull") {
    makeCFG(0 -> (1 || 2) -> 3)
    makeNodes { at =>
      at(0)
      n("x") = Fake(TRefType)
      setCondition(Cmp(TRefType, Condition.EQ)(n("x"), Null()))
    }
    val branch = b(0).blockEnd.asInstanceOf[If]

    branch should matchPattern {
      case IfNull(N("x"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }

    makeNodes { at =>
      setCondition(Cmp(TRefType, Condition.EQ)(Null(), n("x")))
    }
    branch should matchPattern {
      case IfNull(N("x"), BlockExit(_, B(1)), BlockExit(_, B(2))) =>
    }
  }

  def checkMemory(memoryBlocks: Block*)(nonMemoryBlocks: Block*)(nodeMemories: (String, Node)*): Unit = {
    for (b <- memoryBlocks) {
      b.redefinesMemory shouldBe true
    }
    for (b <- nonMemoryBlocks) {
      b.redefinesMemory shouldBe false
    }
    for ((x, m) <- nodeMemories) {
      n(x).asInstanceOf[HasInMemory].inMemory shouldBe m
    }
  }

  test("optimize block memory simple") {
    makeCFG(0 -> 1 -> (2@@("r1=read()", "w1=write()") -> 3@@"r2=read()" || 4) -> 5@@"r3=read()")

    withIncrementalGCM { eliminateCrossBlockMemoryEdges() }
    checkMemory(0, 1, 2, 3, 4, 5)()(
      "r1" -> b(2),
      "w1" -> b(2),
      "r2" -> b(3),
      "r3" -> b(5),
    )

    optimizeBlockMemory()
    checkMemory(5)(0, 1, 2, 3, 4)(
      "r1" -> entryMemory,
      "w1" -> entryMemory,
      "r2" -> n("w1"),
      "r3" -> b(5),
    )
  }

  test("optimize block memory in loop with changed memory") {
    makeCFG(0 -> wd(1 -> (2@@("r1=read()", "w1=write()") -> 3@@"r2=read()" || 4) -> 5@@"r3=read()") -> 6@@"r4=read()")

    withIncrementalGCM { eliminateCrossBlockMemoryEdges() }
    checkMemory(0, 1, 2, 3, 4, 5, 6)()(
      "r1" -> b(2),
      "w1" -> b(2),
      "r2" -> b(3),
      "r3" -> b(5),
      "r4" -> b(6),
    )

    optimizeBlockMemory()
    checkMemory(1, 5)(0, 2, 3, 4, 6)(
      "r1" -> b(1),
      "w1" -> b(1),
      "r2" -> n("w1"),
      "r3" -> b(5),
      "r4" -> b(1),
    )
  }

  test("optimize block memory in loop with unchanged memory") {
    makeCFG(0 -> wd(1 -> (2@@("r1=read()") -> 3@@"r2=read()" || 4) -> 5@@"r3=read()") -> 6@@"r4=read()")

    withIncrementalGCM { eliminateCrossBlockMemoryEdges() }
    checkMemory(0, 1, 2, 3, 4, 5, 6)()(
      "r1" -> b(2),
      "r2" -> b(3),
      "r3" -> b(5),
      "r4" -> b(6),
    )

    optimizeBlockMemory()
    checkMemory()(0, 1, 2, 3, 4, 5, 6)(
      "r1" -> entryMemory,
      "r2" -> entryMemory,
      "r3" -> entryMemory,
      "r4" -> entryMemory,
    )
  }

  def checkRems(rems: Iterator[Node])(useBlocks: Seq[Block]*) = {
    rems.map(_.valueUses.map(_.block).toSeq.sortBy(block2Int)).toSeq should contain theSameElementsAs useBlocks
  }

  test("rematerialize completely single use") {
    makeCFG(1@@"x=read()" -> 2@@"use(x)")
    checkRems(Node.rematerializeCompletely(n("x").asInstanceOf[FloatingNode]))(
      Seq(2),
    )
  }

  test("rematerialize completely") {
    makeCFG(1@@"x=read()" -> 2@@"use(x)" -> (3@@("use(x)", "use(x)") || 4@@"use(x)") -> 5@@"use(x)")
    checkRems(Node.rematerializeCompletely(n("x").asInstanceOf[FloatingNode]))(
      Seq(2),
      Seq(3),
      Seq(3),
      Seq(4),
      Seq(5),
    )
  }

  test("rematerialize conditionally") {
    makeCFG(1@@"x=read()" -> 2@@"use(x)" -> (3@@("use(x)", "use(x)") || 4@@"use(x)") -> 5@@"use(x)")
    checkRems(Node.rematerializeConditionally(n("x").asInstanceOf[FloatingNode], _.useBlock == b(3)))(
      Seq(3),
      Seq(3),
      Seq(2, 4, 5),
    )
  }

  val domEquiv: Equiv[Edge] = Equiv.fromFunction { (e1, e2) =>
    cfg.dominators.tryCompare(e1.useBlock, e2.useBlock).nonEmpty
  }

  test("rematerialize dom equiv - 1") {
    makeCFG(1@@"x=read()" -> 2@@"use(x)" -> (3@@("use(x)", "use(x)") || 4@@"use(x)") -> 5@@"use(x)")
    checkRems(Node.rematerialize(n("x").asInstanceOf[FloatingNode], domEquiv))(
      Seq(2, 3, 3, 4, 5),
    )
  }

  test("rematerialize dom equiv - 2") {
    makeCFG(1@@"x=read()" -> 2 -> (3@@("use(x)", "use(x)") || 4@@"use(x)") -> 5@@"use(x)")
    checkRems(Node.rematerialize(n("x").asInstanceOf[FloatingNode], domEquiv))(
      Seq(3, 3),
      Seq(4),
      Seq(5),
    )
  }

  def xp(name: String): XPoint = n(name).asInstanceOf[SpinalNode].xpoint

  test("one xpoint without xhandler consistency check") {
    makeCFG(
      0 @@ "xspinal()"
    )

    checkIRConsistency(CheckLevels.Optional)
  }

  test("one xpoint with xhandler consistency check") {
    makeCFG(
      0 @@ "xspinal()" -> xb(1)
    )

    removeHandlerAnchors()

    checkIRConsistency(CheckLevels.Optional)
  }

  test("multiple xpoints without xhandlers consistency check") {
    makeCFG(
      0 @@ (
        "x=xspinal()",
        "y=xspinal()",
        "z=xspinal()")
    )

    checkIRConsistency(CheckLevels.Optional)
  }

  test("multiple xpoints with the same xhandler consistency check") {
    makeCFG(
      0 @@ (
        "xspinal()",
        "xspinal()") -> xb(1)
    )

    removeHandlerAnchors()

    checkIRConsistency(CheckLevels.Optional)
  }

  test("xpoint with xhandler followed by xpoint without xhandler consistency check") {
    makeCFG(
      0 @@ (
        "x=xspinal()",
        "y=xspinal()") -> xb(1)
    )

    makeUnreachable(xp("y").xEdge)

    removeHandlerAnchors()

    assertThrows[AssertionError] {
      checkIRConsistency(CheckLevels.Optional)
    }
  }

  test("xpoint without xhandler followed by xpoint with xhandler consistency check") {
    makeCFG(
      0 @@ (
        "x=xspinal()",
        "y=xspinal()") -> xb(1)
    )

    makeUnreachable(xp("x").xEdge)

    removeHandlerAnchors()

    assertThrows[AssertionError] {
      checkIRConsistency(CheckLevels.Optional)
    }
  }

  test("2 xpoints with different xhandlers consistency check") {
    makeCFG(
      0 @@ (
        "x=xspinal()",
        "y=xspinal()") -> xb(1) -> xb(2)
    )

    makeUnreachable(xp("y").xEdge)
    xb(2).addArg(xp("y"))

    removeHandlerAnchors()

    assertThrows[AssertionError] {
      checkIRConsistency(CheckLevels.Optional)
    }
  }

  test("3 xpoints (xh1, no xh, xh2) consistency check") {
    makeCFG(
      0 @@ (
        "x=xspinal()",
        "y=xspinal()",
        "z=xspinal()") -> xb(1) -> xb(2)
    )

    makeUnreachable(xp("y").xEdge)

    makeUnreachable(xp("z").xEdge)
    xb(2).addArg(xp("z"))

    removeHandlerAnchors()

    assertThrows[AssertionError] {
      checkIRConsistency(CheckLevels.Optional)
    }
  }
}
