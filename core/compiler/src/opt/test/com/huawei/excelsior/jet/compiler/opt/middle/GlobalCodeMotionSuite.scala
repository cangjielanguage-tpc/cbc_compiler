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
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.util.Maps
import org.scalactic.source

import scala.collection.mutable.ArrayBuffer

/**
 * Tests for GlobalCodeMotion
 */
class GlobalCodeMotionSuite extends CompilerSuite
                               with GlobalNodesBuilder
                               with IRTransformationsCollection {


  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("val")({
        case Seq() => FakeControlled(IntType)()
        case Seq(x) => FakeControlledUnary(IntType)(x)
      }),

      new SimpleAttribute("writeSameField")({
        case Seq(gf: GetField) => PutField(gf.field)(addObjNode(), addNode())
      }),

      new SimpleAttribute("xSpinalNoMemory")({
        case Seq() => FakeSpinalXNoMemory(IntType)()
      })
    ) ++ super.parsableAttributes()
  }

  // GCM

  private def pinned(b: Block, nodes: Node*): Unit = {
    requireGlobalCodeMotion()
    for (n <- nodes) {
      n.block shouldBe b
    }
  }

  private def testGCM(name: String, graph: => SubGraph)(action: => Unit)(implicit pos: source.Position): Unit = {
    test(s"gcm: $name") {
      makeCFG(graph)
      // this test suite requires optimized memory so optimize it explicitly
      optimizeBlockMemory()

      withGCM() {
        action
      }
    }
  }

  testGCM("latest point",
    0@@"x" -> 1 -> 2@@"ret(x)") {
    pinned(2, "x")
  }

  testGCM("dead uses",
    0@@("x", "y") -> 1@@"z=val(x)" -> 2@@"ret(x)") {
    pinned(2, "x")
    pinned(null, "y")
    pinned(null, "z")
  }

  testGCM("diamond graph",
    0@@("x", "y", "a", "b") -> 1@@("c=cmp(x,y)", "if(c)") -> (2 || 3) -> 4@@"p=phi(a,b)" -> 5@@"ret(p)") {
    pinned(1, "x", "y", "c")
    pinned(2, "a")
    pinned(3, "b")
    pinned(4, "p")
  }

  testGCM("read operation moving",
    0@@"r=read()" -> 1 -> 2@@"ret(r)") {
    pinned(2, "r")
  }

  testGCM("read-write anti-dependency",
    0@@"r1=read()" -> 1@@"w=write()" -> 2@@"r2=read()" -> 3@@("r=add(r1,r2)", "ret(r)")) {
    pinned(1, "r1", "w")
    pinned(3, "r2", "r")
  }

  testGCM("read-write anti-dependency on diamond",
    0@@("r=read()", "x", "y") -> 1@@("c=cmp(x,y)", "if(c)") -> (2@@"w=write()" || 3) -> 4 -> 5@@"ret(r)") {
    pinned(1, "r", "x", "y", "c")
    pinned(2, "w")
  }

  testGCM("read-write anti-dependency ignoring",
    0@@("r=read()", "x", "y") -> 1@@("c=cmp(x,y)", "if(c)") -> (2@@"w=write()" || 3@@"q=add(r,y)") -> 4@@"p=phi(y,q)" -> 5@@"ret(p)") {
    pinned(1, "x", "y", "c")
    pinned(2, "w")
    pinned(3, "r", "q")
    pinned(4, "p")
  }

  testGCM("scheduleLate bug",
    0@@("x", "y") -> dw(1@@("p1=phi(x,y)", "p2=phi(x,z)") -> 2@@("a=add(p2,x)", "z=add(p1,a)", "c=cmp(z,x)", "if(c)")) -> 3@@"ret(z)") {
    pinned(0, "x", "y")
    pinned(1, "p1", "p2")
    pinned(2, "a", "z", "c")
  }

  testGCM("memory anti-dependency is a tricky",
    (0@@("x=read()", "xSpinalNoMemory()", "write()") -> 1@@("use(x)")) |>| (0 -> xb(2) -> 1)) {
    pinned(0, "x")
  }

  // Rematerialization

  private case class Rems(nodeName: String, uses: UsesAt*) {
    def node: FloatingNode = n(nodeName).asInstanceOf[FloatingNode]
  }

  private case class UsesAt(block: Int, useBlocks: Int*)

  private def testRems(name: String, graph: => SubGraph)(expectedRems: Rems*)(implicit pos: source.Position): Unit = {
    test(s"rems: $name") {
      makeCFG(graph)
      // this test suite requires optimized memory so optimize it explicitly
      optimizeBlockMemory()

      val engine = new GCMEngine(allowRematerialization = true) {
        override protected def needsRematerialization(n: FloatingNode) = n match {
          case _: FakeControlled => true
          case _ => false
        }
      }

      val rems = Maps[FloatingNode].newQMap[ArrayBuffer[FloatingNode]]

      for (r <- expectedRems) {
        // node itself will be in its rems
        rems(r.node) = ArrayBuffer(r.node)
      }

      def collectRems(n: Node) = n match {
        case n: FloatingNode =>
          val key = rems.keys.find(x => x.proto == n.proto && x.argsSeq == n.argsSeq).get
          rems(key) += n
        case _ =>
      }

      def block2IntNullable(b: Block): Int = if (b == null) -1 else block2Int(b)

      onCommit.withCallback(collectRems) {
        withGCM(engine) {
          for (r <- expectedRems) {
            rems(r.node).toSeq.map(x =>
              UsesAt(block2IntNullable(x.block), x.valueUses.map(b => block2IntNullable(b.block)).toSeq.sorted: _*)
            ) should contain theSameElementsAs r.uses
          }
        }
      }
    }
  }

  testRems("simple diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" || 3@@"use(x)") -> 4)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(3, 3),
    ),
  )

  testRems("simple diamond with dead uses",
    0@@"x=val()" -> 1@@("y=val(x)", "z=val(x)") -> (2@@"use(x)" || 3@@"use(x)") -> 4@@"val(y)")(
    Rems("x",
      UsesAt(0, -1, -1), // y and z are dead
      UsesAt(2, 2),
      UsesAt(3, 3),
    ),
    Rems("y", // y is dead
      UsesAt(-1, -1), // val(y) is dead
    ),
    Rems("z", // z is dead
      UsesAt(-1),
    ),
  )

  testRems("simple half-diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" || 3) -> 4)(
    Rems("x",
      UsesAt(2, 2),
    ),
  )

  testRems("dominated diamond",
    0@@"x=val()" -> 1@@"use(x)" -> (2@@"use(x)" || 3@@"use(x)") -> 4)(
    Rems("x",
      UsesAt(1, 1, 2, 3),
    ),
  )

  testRems("dominated half-diamond",
    0@@"x=val()" -> 1@@"use(x)" -> (2@@"use(x)" || 3) -> 4)(
    Rems("x",
      UsesAt(1, 1, 2),
    ),
  )

  testRems("dominated cold diamond",
    0@@"x=val()" -> 1@@"use(x)" -> (2@@"use(x)" || 3@@("coldcode()", "use(x)")) -> 4)(
    Rems("x",
      UsesAt(1, 1, 2, 3),
    ),
  )

  testRems("post-dominated half-diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" || 3) -> 4@@"use(x)")(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(4, 4),
    ),
  )

  testRems("post-dominated cold diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" || 3@@("coldcode()", "use(x)")) -> 4@@"use(x)")(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(3, 3),
      UsesAt(4, 4),
    ),
  )

  testRems("stacked half-diamonds",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4@@"use(x)" || 5)) -> 6)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(4, 4),
    ),
  )

  testRems("stacked swapped half-diamonds",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4 || 5@@"use(x)")) -> 6)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(5, 5),
    ),
  )

  testRems("stacked post-dominated half-diamonds",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4@@"use(x)" || 5)) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(4, 4),
      UsesAt(6, 6),
    ),
  )

  testRems("stacked half-diamond and diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4@@"use(x)" || 5@@"use(x)")) -> 6)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(4, 4),
      UsesAt(5, 5),
    ),
  )

  testRems("stacked half-diamond and cold diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4@@"use(x)" || 5@@("coldcode()", "use(x)"))) -> 6)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(4, 4),
      UsesAt(5, 5),
    ),
  )

  testRems("stacked half-diamond and cold swapped half-diamond",
    0@@"x=val()" -> 1 -> (2@@"use(x)" -> 4 || 3 -> (4 || 5@@("coldcode()", "use(x)"))) -> 6)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(5, 5),
    ),
  )

  testRems("chain diamond",
    0@@"x=val()" -> 1@@"a=add(x,ic(1))" -> (2@@"use(a)" || 3@@"use(a)") -> 4)(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(3, 3),
    ),
    Rems("a",
      UsesAt(2, 2),
      UsesAt(3, 3),
    ),
  )

  testRems("chain combo diamond",
    0@@"x=val()" -> 1@@"a=add(x,ic(1))" -> (2@@"use(a)" || 3@@"use(x)") -> 4@@"use(a)")(
    Rems("x",
      UsesAt(2, 2),
      UsesAt(3, 3),
      UsesAt(4, 4),
    ),
    Rems("a",
      UsesAt(2, 2),
      UsesAt(4, 4),
    ),
  )

  testRems("chain dominated diamond",
    0@@"x=val()" -> 1@@("use(x)", "a=add(x,ic(1))") -> (2@@"use(a)" || 3@@"use(a)") -> 4)(
    Rems("x",
      UsesAt(1, 1, 2, 3),
    ),
    Rems("a", // TODO: do not rematerialize chain uses if not required
      UsesAt(2, 2),
      UsesAt(3, 3),
    ),
  )

  testRems("after loop invariant",
    0@@"x=val()" -> wd(1 -> 2 -> (3 || 4) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(6, 6),
    ),
  )

  testRems("in loop invariant",
    0@@"x=val()" -> wd(1 -> 2 -> (3@@"use(x)" || 4@@"use(x)") -> 5) -> 6)(
    Rems("x",
      UsesAt(0, 3, 4),
    ),
  )

  testRems("in and after loop invariant",
    0@@"x=val()" -> wd(1 -> 2 -> (3@@"use(x)" || 4) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(0, 3, 6),
    ),
  )

  testRems("after cold loop invariant",
    0@@"x=val()" -> wd(1 -> 2 -> (3 || 4@@("coldcode()", "use(x)")) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(4, 4),
      UsesAt(6, 6),
    ),
  )

  testRems("in and after cold loop invariant",
    0@@"x=val()" -> wd(1 -> 2 -> (3@@"use(x)" || 4@@("coldcode()", "use(x)")) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(0, 3, 4, 6),
    ),
  )

  testRems("after multi-exit loop invariant",
    0@@"x=val()" -> lp(1 -> (2 || !3@@"use(x)") -> 4 -> (5 || !6@@"use(x)"), Set(3, 6)) -> 7)(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(6, 6),
    ),
  )

  testRems("in and after multi-exit loop invariant",
    0@@"x=val()" -> lp(1 -> (2 || !3@@"use(x)") -> 4@@"use(x)" -> (5 || !6@@"use(x)"), Set(3, 6)) -> 7)(
    Rems("x",
      UsesAt(0, 3, 4, 6),
    ),
  )

  testRems("after loop variable",
    0 -> wd(1@@"x=val()" -> 2 -> (3 || 4) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(6, 6),
    ),
  )

  testRems("in loop variable",
    0 -> wd(1@@"x=val()" -> 2 -> (3@@"use(x)" || 4@@"use(x)") -> 5) -> 6)(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(4, 4),
    ),
  )

  testRems("in and after loop variable",
    0 -> wd(1@@"x=val()" -> 2 -> (3@@"use(x)" || 4) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(6, 6),
    ),
  )

  testRems("after cold loop variable",
    0 -> wd(1@@"x=val()" -> 2 -> (3 || 4@@("coldcode()", "use(x)")) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(4, 4),
      UsesAt(6, 6),
    ),
  )

  testRems("in and after cold loop variable",
    0 -> wd(1@@"x=val()" -> 2 -> (3@@"use(x)" || 4@@("coldcode()", "use(x)")) -> 5) -> 6@@"use(x)")(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(4, 4),
      UsesAt(6, 6),
    ),
  )

  testRems("after multi-exit loop variable",
    0 -> lp(1@@"x=val()" -> (2 || !3@@"use(x)") -> 4 -> (5 || !6@@"use(x)"), Set(3, 6)) -> 7)(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(6, 6),
    ),
  )

  testRems("in and after multi-exit loop variable",
    0 -> lp(1@@"x=val()" -> (2 || !3@@"use(x)") -> 4@@"use(x)" -> (5 || !6@@"use(x)"), Set(3, 6)) -> 7)(
    Rems("x",
      UsesAt(3, 3),
      UsesAt(4, 4, 6),
    ),
  )

  // Incremental GCM

  private def unpin(n: Node): Unit = { n match {
    case _: PinnedNode =>
    case n: FloatingNode => n atLowerPoint null
  }}

  private def undoGCM(): Unit = {
    allNodes foreach unpin
  }

  private def checkRawPoints(cases: (Node, Node)*): Unit = {
    for ((n, p) <- cases) {
      withClue(s"$n.rawPoint ==") {
        n match {
          case n: FloatingNode =>
            val point = if (n.pinned) n.upperPoint else null
            point should be (p)
        }
      }
    }
  }

  private def checkPoints(cases: (Node, Node)*): Unit = {
    for ((n, p) <- cases) {
      withClue(s"$n.point ==") {
        n.asInstanceOf[FloatingNode].upperPoint should be (p)
      }
    }
  }

  test("strikeOut during IGCM") {
    makeCFG(0@@("r1=read()") -> 1@@("w1=write()", "r2=read()", "w2=write()") -> 2@@("r=add(r1,r2)", "ret(r)"))
    undoGCM()
    withIncrementalGCM {
      checkPoints(
         ("r1": Node) -> (0: Block)
        ,("r2": Node) -> ("w1": Node)
        ,("r":  Node) -> ("w1": Node)
      )

      strikeOut(("w1": Node).asInstanceOf[SpinalNode])
      // points survived
      checkRawPoints(
         ("r1": Node) -> (0: Block)
        ,("r2": Node) -> (1: Block)
        ,("r":  Node) -> (1: Block)
      )
    }
  }

  test("strikeOut with pinned during IGCM") {
    makeCFG(0@@("r1=read()") -> 1@@("w1=write()", "r2=read()", "w2=write()", "r3=read()") -> 2@@("r=add(r1,r2)", "ret(r)"))
    undoGCM()
    withIncrementalGCM {
      checkPoints(
         ("r1": Node) -> (0: Block)
        ,("r2": Node) -> ("w1": Node)
        ,("r3": Node) -> ("w2": Node)
        ,("r":  Node) -> ("w1": Node)
      )

      strikeOut(("w2": Node).asInstanceOf[SpinalNode])
      // points survived
      checkRawPoints(
         ("r1": Node) -> (0: Block)
        ,("r2": Node) -> ("w1": Node)
        ,("r3": Node) -> ("w1": Node)
        ,("r":  Node) -> ("w1": Node)
      )
    }
  }

  test("nested IGCM") {
    makeCFG(0)
    withIncrementalGCM {
      withIncrementalGCM {
        // should be fine
      }
    }
  }

  test("no IGCM during GCM") {
    makeCFG(0)
    withGCM() {
      an [AssertionError] should be thrownBy withIncrementalGCM { }
    }
  }

  test("no GCM during IGCM") {
    makeCFG(0)
    withIncrementalGCM {
      an [AssertionError] should be thrownBy withGCM() { }
    }
  }

  test("new block creation during IGCM") {
    makeCFG(0@@("x=read()", "y=read()") -> 1 -> 2)
    undoGCM()
    withIncrementalGCM {
      checkPoints(
         ("x": Node) -> (0: Block)
        ,("y": Node) -> (0: Block)
      )

      val v1 = FakeControlled(IntType)(1: Block)
      val z1 = FakeControlledUnary(IntType)(0: Block, v1)
      checkPoints(
         v1 -> (1: Block)
        ,z1 -> (1: Block)
      )

      makeUnreachable((1: Block).inEdges)
      // points survived
      checkRawPoints(
         v1 -> (1: Block)
        ,z1 -> (1: Block)
      )

      val v0 = FakeControlled(IntType)(0: Block)
      val b = BBlock(0.blockEnd)
      val z = FakeControlledUnary(IntType)(b, v0)

      checkRawPoints(
         v0 -> null // Note that nodes are not automatically pinned on commit
        ,v1 -> (1: Block)
        ,z1 -> (1: Block)
        ,z  -> null // Note that nodes are not automatically pinned on commit
      )

      checkPoints(
        v0 -> (0: Block)
      )

      v0.replaceUsesBy("x")

      checkRawPoints(
         v0 -> (0: Block)
        ,v1 -> (1: Block)
        ,z1 -> (1: Block)
        ,z  -> null // Because z wasn't pinned, changing its args doesn't trigger re-pinning
      )

      v1.replaceUsesBy("x")

      checkRawPoints(
         v0 -> (0: Block)
        ,v1 -> (1: Block)
        ,z1 -> (0: Block) // Despite the IR being broken right now, we still can calculate a valid upper point for z1
        ,z  -> null
      )

      b.blockEnd = Goto(b, b)
      1.addArg(b.blockEnd)

      checkPoints(
         v0 -> (0: Block)
        ,v1 -> (1: Block)
        ,z1 -> (0: Block)
        ,z  -> (b: Block)
      )
    }
  }

  test("replace uses by null during IGCM") {
    makeCFG(0)
    undoGCM()
    withIncrementalGCM {

      val a = FakeControlled(IntType)(0: Block)
      val b = FakeControlledUnary(IntType)(0: Block, a)
      val c = FakeControlledUnary(IntType)(0: Block, b)
      val d = FakeControlledUnary(IntType)(0: Block, c)

      checkPoints(
         a -> (0: Block)
        ,b -> (0: Block)
        ,c -> (0: Block)
        ,d -> (0: Block)
      )

      b.replaceValueUsesBy(NoValue())

      checkRawPoints(
         a -> (0: Block)
        ,b -> (0: Block)
        ,c -> null
        ,d -> null
      )
    }
  }

  test("pinEarly consistency during IGCM") {
    makeCFG(0 -> 1 -> 2)
    undoGCM()
    withIncrementalGCM {

      val a = FakeControlled(IntType)(0: Block)
      val b = FakeControlledUnary(IntType)(1: Block, a)
      val c = FakeControlledUnary(IntType)(0: Block, b)
      val d = FakeControlledUnary(IntType)(0: Block, c)

      checkPoints(
         a -> (0: Block)
        ,b -> (1: Block)
        ,c -> (1: Block)
        ,d -> (1: Block)
      )

      b.replaceValueUsesBy(a)

      checkRawPoints(
         a -> (0: Block)
        ,b -> (1: Block)
        ,c -> (0: Block)
        ,d -> (0: Block)
      )
    }
  }

  // Def-use consistency checks

  def testDefUseDominance(name: String)(passAction: => Unit)(failAction: => Unit)(implicit pos: source.Position): Unit = {
    passDefUseDominance(name) {
      passAction
    }
    failDefUseDominance(name) {
      failAction
    }
  }

  def passDefUseDominance(name: String)(action: => Unit)(implicit pos: source.Position): Unit = {
    test(s"def-use dominance pass: $name") {
      action
    }
  }

  def failDefUseDominance(name: String)(action: => Unit)(implicit pos: source.Position): Unit = {
    test(s"def-use dominance fail: $name") {
      intercept[AssertionError](action).getMessage should (include ("should dominate") or include ("should not be used by reachable"))
    }
  }

  testDefUseDominance("linear") {
    makeCFG(0@@("x=read()", "use(x)", "y=read()", "use(y)"))
  } {
    makeCFG(0@@("x=read()", "use(x)", "y=read()", "use(y)"))
    n("x").replaceUsesBy(n("y"))
    checkDefUseDominance()
  }

  testDefUseDominance("diamond") {
    makeCFG(0 -> (1@@"x=pinned()" || 2@@"y=pinned()") -> 3@@"phi(x,y)")
  } {
    makeCFG(0 -> (1@@"x=pinned()" || 2@@"y=pinned()") -> 3@@"phi(y,x)")
  }

  testDefUseDominance("xblock") {
    makeCFG(0@@("anchorVal", "x=read()", "xspinal()", "y=read()", "xspinal()") -> xb(1)@@"phi(anchorVal,x,y)")
  } {
    makeCFG(0@@("anchorVal", "x=read()", "xspinal()", "y=read()", "xspinal()") -> xb(1)@@"phi(anchorVal,y,x)")
  }

  testDefUseDominance("loop") {
    makeCFG(0@@"x" -> dw(1@@"p=phi(x,y)" -> 2@@"y=pinned()") -> 3)
  } {
    makeCFG(0@@"x" -> dw(1@@"p=phi(y,x)" -> 2@@"y=pinned()") -> 3)
  }

  // Note that phi arguments are inverted in this test compared to the other one,
  // because DSL builds the provided template first and then attaches it to the entry block.
  testDefUseDominance("loop with weird DSL quirk") {
    makeCFG(wd(0@@("x", "p=phi(y,x)", "y=add(p,ic(1))", "spinal()")) -> 1)
  } {
    makeCFG(wd(0@@("x", "p=phi(x,y)", "y=add(p,ic(1))", "spinal()")) -> 1)
  }

  testDefUseDominance("loop inductive") {
    makeCFG(0@@("x", "y1=pinned()") -> wd(1 -> 2@@"y2=pinned()") -> 3)
    makeNodes { at =>
      at(1)
      addInductiveVariable("x", Condition.EQ, "y1", IConst(1))
    }
  } {
    makeCFG(0@@("x", "y1=pinned()") -> wd(1 -> 2@@"y2=pinned()") -> 3)
    makeNodes { at =>
      at(1)
      addInductiveVariable("x", Condition.EQ, "y2", IConst(1))
    }
  }

  testDefUseDominance("unreachable def") {
    makeCFG(0@@"x" -> 1@@"p=phi(x,y)" |>| 2@@"y=pinned()" -> 1)
  } {
    makeCFG(0@@"x" -> 1@@"p=phi(y,x)" |>| 2@@"y=pinned()" -> 1)
  }

  passDefUseDominance("unreachable use") {
    makeCFG(0@@"x" -> 1 |>| 2@@"use(x)" -> 1)
  }

  failDefUseDominance("unreachable def and reachable use") {
    makeCFG(0@@"x" -> 1@@"use(x)" |>| 2@@"y=pinned()" -> 1)
    n("x").replaceUsesBy("y")
    checkDefUseDominance()
  }

  failDefUseDominance("def with xpoint") {
    makeCFG(0 -> 1@@"x=xspinal()" -> (xb(2) -> 3@@"use(x)" || 3) -> 4)
    removeHandlerAnchors()
    eliminateUnreachableCode()
    xb(2).inputs shouldBe Seq(n("x").asInstanceOf[SpinalNode].xpoint)
    checkDefUseDominance()
  }

  failDefUseDominance("def with xpoint and tricky control") {
    makeCFG(0 -> 1@@"x=xspinal()" -> (xb(2) -> 3@@"y=read()" || 3) -> 4)
    removeHandlerAnchors()
    eliminateUnreachableCode()
    xb(2).inputs shouldBe Seq(n("x").asInstanceOf[SpinalNode].xpoint)

    n("y").asInstanceOf[ControlledNode].inCtrl = n("x").asInstanceOf[UpperPoint]

    checkDefUseDominance()
  }

  failDefUseDominance("def with xpoint and tricky memory") {
    makeCFG(0 -> 1@@"x=xspinal()" -> (xb(2) -> 3@@"y=read()" || 3) -> 4)
    removeHandlerAnchors()
    eliminateUnreachableCode()
    xb(2).inputs shouldBe Seq(n("x").asInstanceOf[SpinalNode].xpoint)

    n("y").asInstanceOf[HasInMemory].inMemory = n("x").asInstanceOf[MemoryNode]

    checkDefUseDominance()
  }

  passDefUseDominance("unreachable def and use in different blocks") {
    makeCFG(0@@"x" -> 1 |>| 2@@ "use(x)" -> 3@@"y=pinned()")
    n("x").replaceUsesBy("y")
    checkDefUseDominance()
  }

  passDefUseDominance("unreachable def and use in same block") {
    makeCFG(0@@"x" -> 1 |>| 2@@("use(x)", "y=read()", "use(y)") -> 3)
    n("x").replaceUsesBy("y")
    checkDefUseDominance()
  }

  // GCM with memory anti-dependency optimization

  private def lowerPoint(point: Node, nodes: Node*): Unit = {
    requireGlobalCodeMotion()
    for (n <- nodes) {
      n.asInstanceOf[FloatingNode].lowerPoint shouldBe point
    }
  }

  private def testGCMWithMemoryAntiDepOptimization(name: String, graph: => SubGraph)(standard: => Unit)(withMemoryAntiDepOptimization: => Unit)(implicit pos: source.Position): Unit = {
    test(s"gcm with memory anti-dep optimization: $name") {
      makeCFG(graph)
      // this test suite requires optimized memory so optimize it explicitly
      optimizeBlockMemory()

      withGCM() {
        standard
      }

      withGCM(new GCMEngine(optimizeMemoryAntiDependency = true)) {
        withMemoryAntiDepOptimization
      }
    }
  }

  testGCMWithMemoryAntiDepOptimization("positive",
    0@@("x=read()", "y=write()", "r=ret(x)")) {
    lowerPoint("y", "x") } { // standard
    lowerPoint("r", "x") }   // with optimization

  testGCMWithMemoryAntiDepOptimization("negative",
    0@@("x=read()", "y=writeSameField(x)", "r=ret(x)")) {
    lowerPoint("y", "x") } { // standard
    lowerPoint("y", "x") }   // with optimization

  testGCMWithMemoryAntiDepOptimization("cfg processing is not supported yet - 1",
    0@@("x=read()", "y=write()") -> 1@@("r=ret(x)")) {
    lowerPoint("y", "x") } { // standard
    lowerPoint(b(0).blockEnd, "x") } // with optimization

  testGCMWithMemoryAntiDepOptimization("cfg processing is not supported yet - 2",
    0@@("x=read()") -> (1@@("y1=write()") || 2@@("y2=write()")) -> 3@@("r=ret(x)")) {
    lowerPoint(b(0).blockEnd, "x") } { // standard
    lowerPoint(b(0).blockEnd, "x") }   // with optimization
}
