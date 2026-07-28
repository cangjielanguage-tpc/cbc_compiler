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
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.middle.{ContextTypesRecalculation, UCEComponent}
import com.huawei.excelsior.jet.compiler.types.Guards._
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeMethod

class DiamondMergeSuite extends CompilerSuite
                              with GlobalNodesBuilder
                              with CFGTransformationDSL
                              with DiamondMerge
                              with ContextTypesRecalculation
                              with UCEComponent
                              with IRTransformationsCollection {

  startPhase(CompilerPhase.PostInline)

  override def transformation(): Unit = {
    while (optimizeDiamonds()) { }
  }

  override def nodeWeight(n: Node) = 0.0

  override def makeDebug = false

  override def parsableAttributes() = {
    Seq(
      new UnnamedAttribute(() => addObjNode()),
      new SimpleAttribute("call")({ case Seq() => addCall() }),

    ) ++ super.parsableAttributes()
  }

  private def setSelector(block: Block, selector: Node): Unit = {
    block.blockEnd match {
      case branch: If => branch.selector = selector
    }
  }

  lazy val normalMethod = makeSymMethod("foo", tA).setStatic(false)

  def mt = MethodGuard(normalMethod.getMethodReference, normalMethod)

  /** Sequence of guards for `tA` from most specific to the least. */
  private def tAGuards: Seq[Guard] = Seq(
    PointGuard(tA),
    LevelGuard(5),
    MaxClosedConeGuard(tA),
    OpenConeGuard(tA),
    CHABitGuard,
    mt
  )

  /** Here we create 3 diamonds like this:
    *
    *       0 obj, obj0
    *      / \
    *     1   2 topCall
    *      \ /
    *       3 midPhi(obj, topCall)
    *      / \
    *     4   5 midCall
    *      \ /
    *       6 botPhi(midPhi, midCall)
    *      / \
    *     7   8 botCall
    *      \ /
    *       9
    */
  private def diamonds = 0@@("obj", "obj0") -> (1 || 2@@"topCall=call()") ->
    3@@"midPhi=phi(obj,topCall)" -> (4 || 5@@"midCall=call()") ->
    6@@"botPhi=phi(midPhi,midCall)" -> (7 || 8@@"botCall=call()") -> 9

  private def rCHA(): Unit = resetCHA(tIB, tCF, tD)

  private def initDiamonds(tests: Node*): Unit = {
    require(tests.size == 3)
    val blocks: Seq[Block] = Seq(0, 3, 6)
    for ((b, test) <- blocks zip tests) {
      setSelector(b, test)
    }
  }

  private def initDiamonds(guards: Guard*)(objs: String*): Unit = {
    import TypeApproximationBuildingHelperStrict.{c,cn}

    rCHA()

    setNodeType("obj", c(tA))
    val objCone = cn(tObj)
    setNodeType("topCall", objCone)
    setNodeType("midCall", objCone)
    setNodeType("botCall", objCone)

    def expand[T](elems: Seq[T]) = elems match {
      case Seq(elem) => Seq.fill(3)(elem)
      case Seq(_, _, _) => elems
      case _ => shouldNotReachHere()
    }

    val blocks: Seq[Block] = Seq(0, 3, 6)
    for ((b, g, o) <- blocks lazyZip expand(guards) lazyZip expand(objs)) {
      setSelector(b, TauTest(g, TauInfo.Unknown, b, o) atLowerPoint b.outCtrl)
    }
  }

  private def nothing = 0 -> (1 || 2) -> 3 -> (4 || 5) -> 6 -> (7 || 8) -> 9
  private def middle = 0 -> ((1 -> 33 -> 4) || (2 -> 3 -> 5)) -> 6 -> (7 || 8) -> 9
  private def bottom = 0 -> (1 || 2) -> 3 -> ((4 -> 66 -> 7) || (5 -> 6 -> 8)) -> 9
  private def middleAndBottom = 0 -> ((1 -> 33 -> 4 -> 66 -> 7) || (2 -> 3 -> 5 -> 6 -> 8)) -> 9

  test("same guard") {
    rCHA()

    val cases = Seq(
      // simple
       ("obj", "obj",  "obj", middleAndBottom)
      ,("obj", "obj0", "obj", nothing)

      // chain calls
      ,("obj",  "midPhi", "botPhi", middleAndBottom)
      ,("obj0", "obj",    "midPhi", nothing) // don't merge using phi from other block
    )

    for (g <- tAGuards) {
      for ((o1, o2, o3, template) <- cases)
        withClue(s"g = $g, objs = ($o1, $o2, $o3):") {
          beforeWithPre(diamonds, {
            initDiamonds(g)(o1, o2, o3)
          })
          after(template)
        }
    }
  }

  test("smart merge") {
    rCHA()

    val cases = Seq(
       (LevelGuard(6), MaxClosedConeGuard(tB), PointGuard(tIB),        middleAndBottom, Some(0, PointGuard(tIB)))
      ,(LevelGuard(6), LevelGuard(4),          LevelGuard(5),          middleAndBottom, Some(0, LevelGuard(4)))
      ,(CHABitGuard,   LevelGuard(6),          MaxClosedConeGuard(tB), middleAndBottom, Some(0, MaxClosedConeGuard(tB)))
      ,(CHABitGuard,   LevelGuard(6),          OpenConeGuard(tB),      middleAndBottom, Some(0, MaxClosedConeGuard(tB)))
      ,(CHABitGuard,   MaxClosedConeGuard(tC), OpenConeGuard(tB),      middle,          Some(0, MaxClosedConeGuard(tC)))
      ,(CHABitGuard,   MaxClosedConeGuard(tC), MaxClosedConeGuard(tB), middle,          Some(0, MaxClosedConeGuard(tC)))
    )

    for ((g1, g2, g3, template, res) <- cases; templateOnly <- Seq(true, false)) {
      withClue(s"guards = ($g1, $g2, $g3):") {
        before(diamonds, {
          initDiamonds(g1, g2, g3)("obj")
        }, {
          for ((i, g) <- res if !templateOnly) {
            b(i).blockEnd should be (an [If])
            b(i).blockEnd.asInstanceOf[If].selector should matchPattern { case TauTest(`g`, TauInfo.Unknown, _) => }
          }
        })
        after(template)
      }
    }
  }

  test("smart merge: incompatible diamonds") {
    rCHA()

    beforeWithPre(diamonds, {
      initDiamonds(MaxClosedConeGuard(tC), CHABitGuard, MaxClosedConeGuard(tB))("obj")
    })
    after(middle orElse bottom)
  }

  private def testDiamonds(preAction: => Seq[Node])(afterGraph: => SubGraph)(trueBlocks: Block*)(falseBlocks: Block*): Unit = {
    before(diamonds, {
      val nodes = preAction
      initDiamonds(nodes: _*)
    }, {
      val topBranch = 0.blockEnd.asInstanceOf[If]
      for (b <- trueBlocks) {
        withClue(s"true block = $b:") {
          (topBranch.trueExit dominates b) shouldBe true
        }
      }

      for (b <- falseBlocks) {
        withClue(s"false block = $b:") {
          (topBranch.falseExit dominates b) shouldBe true
        }
      }
    })
    after(afterGraph)
  }

  test("same test - 1") {
    testDiamonds {
      val x = addNode(ConditionType)
      Seq(x, x, x)
    } (middleAndBottom) (
      1, 4, 7
    )(
      2, 5, 8
    )
  }

  test("same test - 2") {
    testDiamonds {
      val x = addNode(ConditionType)
      Seq(x, Not(x), x)
    } (middleAndBottom) (
      1, 5, 7
    )(
      2, 4, 8
    )
  }

  test("flipped test - top") {
    testDiamonds {
      val x = addNode(ConditionType)
      val y = addNode(ConditionType)
      Seq(Not(x), x, y)
    } (middle) (
      1, 5
    )(
      2, 4
    )
  }

  test("flipped test - middle") {
    testDiamonds {
      val x = addNode(ConditionType)
      val y = addNode(ConditionType)
      Seq(x, Not(x), y)
    } (middle) (
      1, 5
    )(
      2, 4
    )
  }


  /** Here we create 2 diamonds like this:
    *
    *     0 obj
    *    / \
    *   1   2
    *    \ / \
    *     3   4
    *      \ /
    *       5
    *      / \
    *     6   7
    *  true   false
    */
  private def stackedDiamonds = ((0 -> (1 || 2) -> 3 -> 5) |>| (2 -> 4 -> 5)) -> (6 || 7)

  private def initStackedDiamonds(guard: Guard): Unit = {
    import TypeApproximationBuildingHelperStrict._
    rCHA()

    val obj = addObjNode(c(tA))

    val blocks: Seq[Block] = Seq(0, 5)
    for (b <- blocks) {
      setSelector(b, TauTest(guard, TauInfo.Unknown, b, obj) atLowerPoint b.outCtrl)
    }
  }

  test("same guard (negative)") {
    rCHA()

    for (g <- tAGuards) {
      withClue(s"g = $g:") {
        beforeWithPre(stackedDiamonds, {
          initStackedDiamonds(g)
        })
        after(stackedDiamonds)
      }
    }
  }

  test("JET-13980: incorrect optimization") {
    before(0@@("a", "b", "u", "v", "y=cmp(a,b)", "x=cmp(u,v)", "if(y)") ->
      ((2@@"if(y)" -> (3 || 4) -> 7) || (1@@"if(x)" -> (3 || 4) -> 23@@"if(x)" -> (2 || 7))),
    {
      // Here we need to redirect if's edges into one block to reproduce the sample from JET-13980.
      // In that case, XiCloner will perform incorrect copying.

      // PreAction is needed because DSL doesn't support multi-edges.
      // If DSL would support it, it would look like this: `2@@"if(y)" -> (7 || 7)`.
      makeUnreachable(b(1).succBlockEdges ++ b(2).succBlockEdges)

      b(23).addArgs(b(1).blockEnd.exits)
      b(7).addArgs(b(2).blockEnd.exits)

      eliminateUnreachableCode()
    }, {
      // Without these transformations the final graph would be large.
      BlocksConnectionTransformation()
      eliminateUnreachableCode()
    })

    after(50 || (1001 -> (58 -> 48 -> 1002 || 60 -> 23) || 50) -> 7)
  }
}
