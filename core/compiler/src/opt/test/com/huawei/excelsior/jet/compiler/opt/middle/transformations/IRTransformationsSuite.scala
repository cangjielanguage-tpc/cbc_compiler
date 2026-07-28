/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.ConstBranchElimination
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.{CFGTransformationDSL, GlobalNodesBuilder}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeMethodReference, FakeType}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox
import com.huawei.excelsior.jet.util.ScalaCollections
import org.scalactic.source

/**
  * Tests for IRTransformations.
  *
  * @see [[com.huawei.excelsior.jet.compiler.newbaseline.frontend.CriticalEdgesSuite]]
  */
class IRTransformationsSuite extends CompilerSuite
                                with GlobalNodesBuilder
                                with CFGTransformationDSL
                                with IRTransformationsCollection
                                with ConstBranchElimination
                                with TypesToolbox {

  // for PhiToCondValReplacing
  startPhase(CompilerPhase.PreLowering)


  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("obj")({ case Seq() => addObjNode() }),

    ) ++ super.parsableAttributes()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    tr = null
  }

  def replaceGotoBySwitch(block: Block): Unit = {
    addBlockEnd(block)(Switch(Seq.empty)(_, _, IConst(0)))
  }

  val makeDebug = false

  private var tr: () => Unit = _
  private def setTR[A](f: => A): Unit = {
    tr = () => { f; () }
  }
  def transformation(): Unit = { tr() }

  private def eliminateCriticalEdges(): Unit = { setTR { splitCriticalEdges() } }
  private def emptyBlockElimination(): Unit = { setTR { transform(EmptyBlocksEliminationWithManyPredecessors) } }
  private def emptyBlockEliminationWithSinglePredecessor(): Unit = { setTR { transform(EmptyBlocksElimination) } }
  private def blocksConnection(): Unit = { setTR { transform(BlocksConnectionTransformation) } }
  private def eliminateMultiEdges(): Unit = { setTR { transform(MultiEdgeElimination) } }

  private def allTransformations(): Unit = {
    setTR { transform(EmptyBlocksEliminationWithManyPredecessors, BlocksConnectionTransformation, MultiEdgeElimination, PhiToCondValReplacing) }
  }

  test("critical edges elimination no works") {
    eliminateCriticalEdges()
    before(0 -> (1 || 2) -> 3)
    after (0 -> (1 || 2) -> 3)
  }

  test("critical edges elimination simple case") {
    eliminateCriticalEdges()
    before(0 -> ((1 -> 3) || (2 -> (3 || 4))))
    after (0 -> ((1 -> 3) || (2 -> ((5 -> 3) || 4))))
  }

  test("critical edges elimination self-cycle") {
    eliminateCriticalEdges()
    before(0 -> wd(1) -> 2)
    after (0 -> wd(1 -> 3) -> 2)
  }

  test("critical edges elimination does not change infinite self-cycle") {
    eliminateCriticalEdges()
    before(0 -> wd(1))
    after (0 -> wd(1))
  }

  test("critical edges elimination cycle") {
    eliminateCriticalEdges()
    before(0 -> dw(1 -> 2) -> 3)
    after (0 -> 1 -> 2 -> ((4 -> 1 -> end) || 3))
  }

  test("critical edges elimination self-cycle-2") {
    eliminateCriticalEdges()
    before(0 -> dw(1) -> 2)
    after (0 -> wd(1 -> 3) -> 2)
  }

  test("critical edges elimination cycle with break") {
    eliminateCriticalEdges()
    before(0 -> 1 -> (2 || (3 -> (1 || 4))))
    after (0 -> 1 -> (2 || (3 -> ((5 -> 1) || 4))))
  }

  test("critical edges elimination hard case") {
    eliminateCriticalEdges()
    before(0 -> ((dw(1 -> (3 || (4 -> end)))) || (dw(2 -> ((5 -> 7 -> end) || (6 -> ((7 -> end) || (8))))))) -> 9)
    after (0 -> ((12 -> 1 -> ((3 -> ((10 -> 1 -> end) || 11)) || 4 -> end)) ||
           (13 -> 2 -> ((5 -> 7 -> end) || (6 -> ((16 -> 7 -> end) || (8 -> ((15 -> 2 -> end) || (14)))))))) -> 9)
  }

  test("simple empty block elimination") {
    emptyBlockElimination()
    before(0 -> 1)
    after (0)
  }

  test("line of empty blocks elimination") {
    emptyBlockElimination()
    before(0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9)
    after (0)
  }

  test("empty wings of if diamond elimination") {
    emptyBlockElimination()
    before(0 -> (1 || 2) -> 3)
    after ((0 -> 3) |>| (0 -> 3))
  }

  test("empty block elimination and critical edges elimination fight") {
    for (i <- 0 until 10) {
      beforeEach()
      eliminateCriticalEdges()
      before(0 -> ((1 -> 3) || (2 -> (3 || 4))))
      after (0 -> ((1 -> 3) || (2 -> ((5 -> 3) || 4))))
      beforeEach()
      emptyBlockElimination()
      before(0 -> ((1 -> 3) || (2 -> ((5 -> 3) || 4))))
      after (0 -> (3 || (2 -> (3 || 4))))
    }
  }

  test("attention to control nodes in empty block elimination") {
    emptyBlockElimination()
    beforeWithPre(0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9, {
      addSomeCtrlNode(5)
    })
    after (5 -> 6)
  }

  test("attention to control nodes in empty block elimination - 2") {
    emptyBlockElimination()
    beforeWithPre(0 -> (1 || 2) -> 3, {
      addSomeCtrlNode(1)
    })
    after (0 -> ((1 -> 3) || 3))
  }

  test("empty block with many predecessors") {
    emptyBlockElimination()
    before((0 -> 1 -> 2) |>| (0 -> 1))
    after ((0   ->    2) |>| (0 -> 2))

    emptyBlockEliminationWithSinglePredecessor()
    before((0 -> 1 -> 2) |>| (0 -> 1))
    after ((0 -> 1 -> 2) |>| (0 -> 1))
  }

  test("empty block with many entrances and phies") {
    emptyBlockElimination()
    beforeWithPost(0@@("x", "y", "z") -> (1 || ((2 || 3) -> 4@@("p4=phi(y,z)"))) -> 5@@("p5=phi(x,p4)"), {
      eliminateUnreachableCode()
      n("p5").asInstanceOf[Phi].argsSeq should be (Seq(n("x"), n("y"), n("z")))
    })
    after ((0 -> 5) |>| (0 -> 5) |>| (0 -> 5))
  }

  test("non-empty block with many entrances and phies") {
    emptyBlockElimination()
    before(0@@("x", "y") -> (1 || ((2 || 3) -> 4@@("p=phi(x,y)", "u=use(p)"))) -> 5)
    after ((0 -> 5) |>| (0 -> 4 -> 5) |>| (0 -> 4))
  }

  test("empty block with useful phi") {
    emptyBlockElimination()
    before(0@@("x", "y", "z") -> (1 || 2) -> 3@@("p=phi(x,y)") -> dw(dw(4@@("pp=phi(p,p,z)"))), {
      all[Phi] forall (_.args forall (_ != null)) should be (true)
    }, {
      all[Phi] forall (_.args forall (_ != null)) should be (true)
    })
    after (((0 -> 3) |>| (0 -> 3)) -> dw(dw(4)))
  }

  test("empty block with loop") {
    emptyBlockElimination()
    before(0 -> dw(1))
    after (dw(1))
  }

  test("empty block with switch with one exit") {
    emptyBlockElimination()
    beforeWithPre(0 -> 1, replaceGotoBySwitch(0))
    after        (0 -> 1)
  }

  test("simple blocks connection") {
    blocksConnection()
    before(dw(0) -> 1 -> 2)
    after (dw(0) -> 1)
  }

  test("blocks connection with switch with one exit") {
    blocksConnection()
    beforeWithPre(dw(0) -> 1 -> 2, replaceGotoBySwitch(1))
    after        (dw(0) -> 1 -> 2)
  }

  test("blocks connection") {
    blocksConnection()
    before(dw(0) -> 8 -> (1 || 2) -> 3 -> 4 -> (5 || 6) -> 7)
    after (dw(0) -> 8 -> (1 || 2) -> 3      -> (5 || 6) -> 7)
  }

  test("blocks line with control nodes connection") {
    blocksConnection()
    var x: SpinalNode = null
    var y: SpinalNode = null
    var z: SpinalNode = null
    before(dw(0) -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 -> 9, {
      x = addSomeCtrlNode(2)
      y = addSomeCtrlNode(4)
      z = addSomeCtrlNode(8)
    }, {
      val block: Block = 1
      x.inCtrl should be (block)
      y.inCtrl should be (x)
      z.inCtrl should be (y)
      block.blockEnd.inCtrl should be (z)
    })
    after (dw(0) -> 1)
  }

  test("simple multi-edge elimination") {
    eliminateMultiEdges()
    before((0 -> 1) |>| (0 -> 1))
    after  (0 -> 1)
  }

  test("multi-edge elimination with third edge") {
    eliminateMultiEdges()
    before((0 -> (1 || 2) -> 3) |>| (1 -> 3))
    after  (0 -> (1 || 2) -> 3)
  }

  private def createAndCheckCondVal(): Unit = {
    val cmp = addCondition(0, addNode(), addNode(), Condition.EQ)
    val phi = addPhi(1, IConst(1), IConst(0))
    val phiUse = addNode(1, phi)

    PhiToCondValReplacing()

    phi.isCommitted should be (false)

    val cval = phiUse.arg.asInstanceOf[CondVal]

    if (b(1).inputs.head == b(0).blockEnd.asInstanceOf[If].trueExit) {
      // first input is True branch
      cval.condition should be (cmp)
    } else {
      // first input is False branch
      cval.condition should be (Not(cmp))
    }
  }

  test("replacing phi-function to conditional move") {
    makeCFG((0 -> 1) |>| (0 -> 1))

    val branch = b(0).blockEnd.asInstanceOf[If]
    b(1).replaceArgs(branch.falseExit, branch.trueExit)

    createAndCheckCondVal()
  }

  test("replacing phi-function to conditional move with reversed args") {
    makeCFG((0 -> 1) |>| (0 -> 1))

    val branch = b(0).blockEnd.asInstanceOf[If]
    b(1).replaceArgs(branch.trueExit, branch.falseExit)

    createAndCheckCondVal()
  }

  test("diamond to conditional val") {
    allTransformations()
    before(dw(0@@("a=ic(1)", "b=ic(0)")) -> 1@@("x", "y", "cmp(x,y)") -> (2 || 3) -> 4@@("p=phi(a,b)","ret(p)"), {
    }, {
      val endBlock: Block = 4
      val phiUseArg = endBlock.blockEnd match {
        case ret: Return => ret.inValue
      }
      phiUseArg shouldBe a [CondVal]
    })
    after(dw(0) -> 1)
  }

  test("self loop back edge splitting") {
    setTR { splitInfiniteSelfLoops() }
    before(wd(0))
    after (wd(0 -> 1))
  }

  {
    for (c <- Seq(
      classOf[java.lang.Boolean],
      classOf[java.lang.Byte],
      classOf[java.lang.Short],
      classOf[java.lang.Character],
      classOf[java.lang.Integer],
      classOf[java.lang.Long],
      classOf[java.lang.Float],
      classOf[java.lang.Double],
    )) {
      val fakeType = FakeType.create(c)
      // Pre-init type resolution cache for RTStructs
      env.typesResolution.put(fakeType.getXName, fakeType)
    }

    val eqB = 10
    val neqB = 20
    val refCmpB = 1
    val instOfB = 2
    val valCmpB = 3
    val nonNullCmpB = 4
    def set(xs: Int*): Set[Int] = Set(xs*)
    for ((params @ (boxingType, invert, effectsIn, castedPrimVal, nullCheckObj, shouldChange), pos) <- Seq(
      tp(Java.Lang.Integer, set(), set(), true, false, true),
      tp(Java.Lang.Short, set(), set(), true, false, true),
      tp(Java.Lang.Integer, set(), set(refCmpB), true, false, true),
      tp(Java.Lang.Integer, set(refCmpB, valCmpB), set(), true, false, false),
      tp(Java.Lang.Integer, set(), set(instOfB), true, false, false),
      tp(Java.Lang.Integer, set(refCmpB, valCmpB), set(instOfB), true, false, false),
      tp(Java.Lang.Integer, set(instOfB), set(), true, false, false),
      tp(Java.Lang.Integer, set(), set(instOfB, valCmpB), true, false, false),
      tp(Java.Lang.Integer, set(), set(valCmpB), true, false, false),
      tp(Java.Lang.Float, set(valCmpB), set(), true, false, false),
      tp(Java.Lang.Integer, set(), set(), false, false, true),
      tp(Java.Lang.Short, set(), set(), false, false, false),
      tp(Java.Lang.Character, set(), set(), false, false, false),
      tp(Java.Lang.Boolean, set(), set(), false, false, false),
      tp(Java.Lang.Boolean, set(), set(), true, false, true),
      tp(Java.Lang.Integer, set(), set(), true, true, true),
      tp(Java.Lang.Long, set(), set(), true, true, true),
    )) {
      test(s"boxing comparison $params") {
        if (nullCheckObj) {
          makeCFG(0 -> refCmpB -> (eqB || nonNullCmpB -> (instOfB -> ((valCmpB -> (eqB || neqB)) || neqB) || neqB)))
        } else {
          makeCFG(0 -> refCmpB -> (eqB ||                 instOfB -> ((valCmpB -> (eqB || neqB)) || neqB)))
        }
        makeNodes { at =>
          at(0)
          val primArg = Fake(ValueType(boxingType.kind))
          val o = addObjNode()
          val box = BoxedValue(boxingType)(primArg)

          at(refCmpB)
          b(refCmpB).blockEnd.asInstanceOf[If].selector = Cmp(TRefType, Condition.EQ)(o, box)

          if (nullCheckObj) {
            at(nonNullCmpB)
            b(nonNullCmpB).blockEnd.asInstanceOf[If].selector = Cmp(TRefType, Condition.NE)(o, Null())
          }

          at(instOfB)
          b(instOfB).blockEnd.asInstanceOf[If].selector = Cmp(IntType, Condition.NE)(InstanceOf(sig(boxingType.symType))(o), IConst(0))

          at(valCmpB)
          b(valCmpB).blockEnd.asInstanceOf[If].selector = Cmp(primArg.tpe, Condition.EQ)(GetField(boxingType.value)(o), if (castedPrimVal) box.primitiveValue() else primArg)

          invert foreach (x => x.blockEnd match {
            case i: If => i.selector = Not(i.selector)
          })
          effectsIn foreach { x =>
            at(x)
            DirectCall(new FakeMethodReference(MethodAccessKind.STATIC))()
          }
        }

        val nodesBefore = allNodes.toSet
        transform(BoxingEqualitySimplification) should be(shouldChange)
        if (!shouldChange) {
          allNodes.toSet shouldBe nodesBefore withClue ("\nAdded: " + (allNodes.toSet diff nodesBefore))
        }
        eliminateConstBranches()

        if (shouldChange) {
          b(refCmpB).blockEnd.asInstanceOf[Goto].target should be (b(if (nullCheckObj) nonNullCmpB else instOfB))
        } else {
          b(refCmpB).blockEnd shouldBe an [If]
        }
      }
    }
  }

  def testCCNCSwap(name: String, attrs: String*)(spine: String*)(implicit pos: source.Position): Unit = {
    testCCNCSwapX(name, attrs*)()(spine*)
  }

  def testCCNCSwapX(name: String, attrs: String*)(xattrs: String*)(spine: String*)(implicit pos: source.Position): Unit = {
    testCCNCSwapAndCheck(name, attrs*)(xattrs*)(spine*) {}
  }

  def testCCNCSwapAndCheck(name: String, attrs: String*)(xattrs: String*)(spine: String*)(check: => Unit)(implicit pos: source.Position): Unit = {
    test(s"cc-nc swap: $name") {
      makeCFG(0@@@(attrs) -> xb(1)@@@(xattrs))
      removeHandlerAnchors()
      var newNC: NullCheck = null
      def nameNC(x: Node) = x match {
        case x: NullCheck => newNC = x
        case _ =>
      }
      onCommit.withCallback(nameNC) {
        while (transform(CheckCastNullCheckSwapping)) {}
      }
      if (newNC != null) {
        n("nc'") = newNC
      }
      0.spineForward.toSeq shouldBe spine.map(n.apply)
      0.xHandlers.toSeq shouldBe Seq(xb(1))
      check
    }
  }

  testCCNCSwap("normal", "x=obj()", "cc=cc(A,x)", "nc=nc(x)")(
    "nc'", "cc"
  )

  testCCNCSwap("different obj", "x=obj()", "y=obj()", "cc=cc(A,x)", "nc=nc(y)")(
    "cc", "nc"
  )

  testCCNCSwap("cold marker", "x=obj()", "cc=cc(A,x)", "c=coldcode()", "nc=nc(x)")(
    "cc", "c", "nc"
  )

  testCCNCSwap("multiple cc", "x=obj()", "cc1=cc(A,x)", "cc2=cc(A,x)", "nc=nc(x)")(
    "nc'", "cc1", "cc2"
  )

  testCCNCSwapAndCheck("normal with controlled nodes", "x=obj()", "cc=cc(A,x)", "rcc=read()", "nc=nc(x)", "rnc=read()")()(
    "nc'", "cc"
  ) {
    n("rnc").asInstanceOf[HasInControl].inCtrl shouldBe n("cc")
    n("rcc").asInstanceOf[HasInControl].inCtrl shouldBe n("cc")
  }

  testCCNCSwapX("x-phi args above cc", "anchorVal", "x=obj()", "r1=read()", "r2=read()", "cc=cc(A,x)", "nc=nc(x)")("phi(anchorVal,r1,r2)")(
    "nc'", "cc"
  )

  testCCNCSwapX("x-phi args below cc", "anchorVal", "x=obj()", "rx=read()", "cc=cc(A,x)", "rcc=read()", "nc=nc(x)")("phi(anchorVal,rx,rcc)")(
    "cc", "nc"
  )

  def testNoCCNCSwap(name: String, graph: => SubGraph): Unit = {
    test(s"cc-nc no swap: $name") {
      makeCFG(graph)
      transform(CheckCastNullCheckSwapping) shouldBe false
    }
  }

  testNoCCNCSwap("different block", 0@@("x=obj()", "cc(A,x)") -> 1@@ "nc(x)")
  testNoCCNCSwap("cfg", 0@@("x=obj()", "cc(A,x)") -> (1@@ "nc(x)" || 2))
}
