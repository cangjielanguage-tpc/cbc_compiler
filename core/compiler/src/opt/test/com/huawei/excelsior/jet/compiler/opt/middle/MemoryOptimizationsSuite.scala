/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STRICT_MEM
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.STATIC
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.LoopsNormalizer
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.options.BoolOption.MoveLoadsOutOfLoops
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeType}
import com.huawei.excelsior.jet.util.ScalaCollections
import org.scalatest.Inside.inside
import org.scalatest.Tag

import scala.util.chaining.scalaUtilChainingOps

class MemoryOptimizationsSuite
  extends CompilerSuite
     with GlobalNodesBuilder
     with MemoryOptimizations
     with UCEComponent
     with LoopsNormalizer
     with ContextTypesRecalculation
     with EquivalentPhiesElimination {

  /** Tag for tests which cover unimplemented mutation of value args. */
  object ValueArgsMutation extends Tag("")

  import TypeApproximationBuildingHelperStrict._

  private var coldReloadsEnabled = false
  override def isPGOHost = coldReloadsEnabled

  var fastPath: Boolean = _
  override def allowTransformationFastPath = fastPath

  inline def testOpt(testName: String, testTags: org.scalatest.Tag*)(testFun: => Any /* Assertion */)(implicit pos: org.scalactic.source.Position): Unit = {
    test(testName, testTags: _*)({
      fastPath = true
      testFun
    }, pos)
    test(testName + " (without fast-path)", testTags: _*)({
      fastPath = false
      testFun
    }, pos)
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.enable(MoveLoadsOutOfLoops)
    n("x") = addNode()
    n("y") = addNode()
  }

  val host = makeSymClass("host", symObj).markTurboClinited() // no preparation/clinit checks, please
  val sgArrType = sig(makeSymArray("arr", symByte, 1))

  val hostAppr = cn(ReferenceType(host))

  val Seq(f1, f2, f3) =
    for (i <- 1 to 3)
      yield makeSymField(s"fake$i", symByte, host)

  val Seq(sfUnique, sf1, sf2, sf3) =
    for (i <- 0 to 3)
      yield makeSymField(s"sfake$i", symByte, host).setJavaModifiers(Modifiers(STATIC))

  val sfNonNull = makeSymField(s"sfakeNonNull", host, host).setJavaModifiers(Modifiers(STATIC))

  def ps(field: FakeField)(args: Node*) = args match { case Seq(value) => PutStatic(field)(value) }
  def gs(field: FakeField)(args: Node*) = args match { case Seq() => GetStatic(field) }

  def pf(field: FakeField)(args: Node*) = args match { case Seq(obj, value) => PutField(field)(obj, value) }
  def gf(field: FakeField)(args: Node*) = args match { case Seq(obj) => GetField(field)(obj) }

  val extraAttributes = {
    Seq(
      new SimpleAttribute("obj")({ case Seq() => addPinnedObjNode(hostAppr) }),

      new SimpleAttribute("pa")({ case Seq(arr, idx, value) => ArrayPut(sgArrType)(arr, idx, value) }),
      new SimpleAttribute("ga")({ case Seq(arr, idx) => ArrayGet(sgArrType)(arr, idx) }),

      new Attribute("fill") {
        def handle(ctx: ParsingContext, args: Seq[String]) = args match {
          case Seq(arrS, values @ _*) =>
            Some(ArrayFill(sgArrType, values map java.lang.Long.decode)(parseArgAsNode(ctx, arrS)))
        }
      },

      // simple trick to prevent identity but do not interfere optimizations
      new SimpleAttribute("mem")({ case Seq() => PutStatic(sfUnique)(addNode()) }),

      new SimpleAttribute("barrier")({ case Seq() => MemBarrier(Set(STRICT_MEM))() }),

      new SimpleAttribute("null")({ case Seq() => Null() }),
      new SimpleAttribute("xn")({ case Seq() => addObjNode(pn(ReferenceType(host))) }),
      new SimpleAttribute("psnn")({ case Seq(arg) => PutStatic(sfNonNull)(arg) }),
      new SimpleAttribute("gsnn")({ case Seq() => GetStatic(sfNonNull) tap (setNodeType(_, p(ReferenceType(host)))) }),

    ) ++ (
      for {
        (opName, op) <- Seq(("pf", pf), ("gf", gf)): Seq[(String, FakeField => Seq[Node] => Node)]
        (fieldName, field) <- Seq(("1", f1), ("2", f2), ("3", f3))
      } yield new SimpleAttribute(opName + fieldName)(op(field))
    ) ++ (
      for {
        (opName, op) <- Seq(("ps", ps), ("gs", gs)): Seq[(String, FakeField => Seq[Node] => Node)]
        (fieldName, field) <- Seq(("1", sf1), ("2", sf2), ("3", sf3))
      } yield new SimpleAttribute(opName + fieldName)(op(field))
    )
  }

  override def parsableAttributes() = extraAttributes ++ super.parsableAttributes()


  def check(getNode: Node, changedExpected: Boolean)(checker: Node => Unit): Unit = {
    val get = getNode match {
      case get: GetMemoryOperation => get
      case _ => fail(s"no identity, please: $getNode")
    }

    checkIRConsistency(CheckLevels.Important)

    // Set less conservative control dependencies.
    normalizeAllLoops()
    recalculateContextTypes()

    get.isCommitted should be (true)

    val getFromUse = {
      val deprive = depriveIfNeeded(get)
      val use = FakeUnary(deprive.tpe)(deprive)
      () => use.arg match {
        case Deprive(_, g) => g
        case g => g
      }
    }
    getFromUse() shouldBe get

    val memOpt = new MemoryOptimizer
    val loops = cfg.loops
    val loopsMovingInfo =
      loops.allLoopsOf(gmo(get).inMemory.block).toSeq map { l => (l.header, memOpt.canMemoryReadBeMovedOutOfLoop(gmo(get), l)) }

    val nodesBefore = allNodes.toSet
    val actuallyChanged = optimizeMemoryReads()
    val nodesAfter = allNodes.toSet

    checkIRConsistency(CheckLevels.Important)
    completeSSA()
    checkIRConsistency(CheckLevels.Important)

    checker(getFromUse())

    actuallyChanged shouldBe changedExpected
    if (!actuallyChanged) {
      nodesAfter shouldBe nodesBefore withClue ", dead nodes littering"
    }

    if (env.enabled(MoveLoadsOutOfLoops)) {
      withIncrementalGCM {
        val point = upperPoint(getFromUse())
        for ((header, movable) <- loopsMovingInfo) {
          point strictDominates header shouldBe movable withClue s", should ${if (movable) "" else "not "}be moved out of ${header.id}: "
        }
      }
    }

    checkIRConsistency(CheckLevels.Important)
  }

  private def gmo(n: Node) = n.asInstanceOf[GetMemoryOperation]

  private def rememberOp(original: Node)(action: ((Node => Unit), (Node => Unit)) => Unit): Unit = {
    val name = original.toString
    val proto = original.proto
    val args = original.valueArgs.toSeq
    val mem = gmo(original).inMemory
    action({ another =>
      withClue(s"original: $name, checked: $another\n") {
        another.proto shouldBe proto
        another.valueArgs.toSeq shouldBe args
      }
    }, { another =>
      gmo(another).inMemory shouldBe mem
    })
  }

  def checkEliminationBy(get: Node)(checker: PartialFunction[Node, Unit]): Unit = {
    check(get, changedExpected = true) { r =>
      get should not be Symbol("committed")
      checker should be definedAt (r)
      checker(r)
    }
  }

  def checkEliminationWithReloadsBy(get: Node, shouldCheckNoOptimization: Boolean = true)(checker: PartialFunction[Node, Unit]): Unit = {
    if (shouldCheckNoOptimization) {
      checkNoOptimization(get)
    }

    coldReloadsEnabled = true
    try {
      checkEliminationBy(get)(checker)
    } finally {
      coldReloadsEnabled = false
    }
  }

  def checkMovementTo(get: Node, newMemory: => Node): Unit = {
    rememberOp(get) { (checkSameOp, checkSameMem) =>
      check(get, changedExpected = true) { r =>
        checkSameOp(r)
        gmo(r).inMemory shouldBe newMemory
      }
    }
  }

  def checkMovementToPreHeaderOf(get: Node, header: BBlock): Unit = {
    def preHeader() = ScalaCollections.singleton(header.predBlocks filter (isPreHeaderOf(_, header)))
    assert(preHeader().isEmpty, "pre-header already exists, use checkMovementTo()")
    checkMovementTo(get, preHeader() match { case Some(b) => b; case None => fail("still no pre-header?") })
  }

  def checkNoOptimization(get: Node): Unit = {
    rememberOp(get) { (checkSameOp, checkSameMem) =>
      check(get, changedExpected = false) { r =>
        checkSameOp(r)
        checkSameMem(r)
        r.id shouldBe get.id
      }
    }
  }

  object Adjusted {
    def unapply(n: Node): Option[Node] = Some(n match {
      case BitFieldExtract.JavaShortIntegralExtend(AsmType.I8, Adjusted(v)) => v
      case EOPConvert(Adjusted(v)) => v
      case _ => n
    })
  }

  object GS {
    def unapply(n: GetStatic) = Some(n.inMemory)
  }

  object GF {
    def unapply(n: GetField) = Some(n.inMemory)
  }

  testOpt("trivial getstatic") {
    makeCFG(0@@("ps1(x)", "g=gs1()"))
    n("g") should matchPattern { case Adjusted(N("x")) => }
  }

  testOpt("trivial getfield") {
    makeCFG(0@@("o=obj()", "pf1(o,x)", "g=gf1(o)"))
    n("g") should matchPattern { case Adjusted(N("x")) => }
  }

  testOpt("trivial arrayget") {
    makeCFG(0@@("a=obj()", "i", "pa(a,i,x)", "g=ga(a,i)"))
    n("g") should matchPattern { case Adjusted(N("x")) => }
  }

  testOpt("only getstatic") {
    makeCFG(0@@"g=gs1()")
    checkNoOptimization(n("g"))
  }

  testOpt("bad getstatic") {
    makeCFG(0@@("ps2(x)", "g=gs1()"))
    checkNoOptimization(n("g"))
  }

  testOpt("simple getstatic") {
    makeCFG(0@@("ps1(x)", "mem()", "g=gs1()"))
    checkEliminationBy(n("g")) { case Adjusted(N("x")) => }
  }

  testOpt("transitive getstatic") {
    makeCFG(0@@("ps1(x)", "mem()",
                "ps2(gs1())", "mem()",
                "g=gs2()"))
    checkEliminationBy(n("g")) { case Adjusted(N("x")) => }
  }

  testOpt("transitive getstatic to getstatic") {
    makeCFG(0@@("ps1(x)", "mem()",
                "g1=gs1()", "mem()",
                "g2=gs1()"))
    checkEliminationBy(n("g2")) { case Adjusted(N("x")) => }
  }

  testOpt("transitive getstatic to getstatic with vars") {
    makeCFG(
      0 ->
      (1@@"ps1(ic(37))" || 2@@"ps1(ic(42))") ->
      3 ->
      (4@@("mem()", "g1=gs1()") || 5@@("mem()", "g2=gs1()")) ->
      6@@("g3=gs1()"))
    checkEliminationBy(n("g3")) { case Phi(B(3), IConst(37), IConst(42)) => }
  }

  testOpt("transitive getstatic (yet another regression test)") {
    makeCFG(
      0@@("ps1(x)",
          "ps2(y)", "mem()",
          "g2=gs2()") ->
      (1 || 2@@"ps1(g2)") ->
      3@@"g1=gs1()")
    checkEliminationBy(n("g1")) { case Phi(B(3), Adjusted(N("x")), Adjusted(N("y"))) => }
  }

  testOpt("getstatic with 2 identical puts") {
    makeCFG(
      0 ->
      (1@@"ps1(x)" || 2@@"ps1(x)") ->
      3@@"g=gs1()")
    checkEliminationBy(n("g")) { case Adjusted(N("x")) => }
  }

  testOpt("getstatic with 2 equivalent puts") {
    makeCFG(
      0 ->
      (1@@"ps1(ic(0x0AAAAA37))" || 2@@"ps1(ic(0x0BBBBB37))") ->
      3@@"g=gs1()")
    checkEliminationBy(n("g")) { case IConst(0x37) => }
  }

  testOpt("getstatic with 2 different puts") {
    makeCFG(
      0 ->
      (1@@"ps1(x)" || 2@@"ps1(y)") ->
      3@@"g=gs1()")
    checkEliminationBy(n("g")) { case Phi(B(3), Adjusted(N("x")), Adjusted(N("y"))) => }
  }

  testOpt("getfield from new") {
    makeCFG(0@@("n=new(host)", "mem()", "g=gf1(n)"))
    checkEliminationBy(n("g")) { case IConst(0) => }
  }

  testOpt("getfield from new hard") {
    makeCFG(
      0@@("n=new(host)", "mem()") ->
      (1 || 2@@"pf1(n,ic(0))") ->
      3@@"g=gf1(n)")
    checkEliminationBy(n("g")) { case IConst(0) => }
  }

  testOpt("getfield from new failed") {
    makeCFG(
      0@@("n=new(host)", "mem()") ->
      (1 || 2@@"pf1(n,ic(1))") ->
      3@@"g=gf1(n)")
    checkEliminationBy(n("g")) { case Phi(B(3), IConst(0), IConst(1)) => }
  }

  testOpt("arrayget from newarray") {
    makeCFG(0@@("a=newarr(arr,ic(10))",
                "g=ga(a,ic(5))"))
    checkEliminationBy(n("g")) { case IConst(0) => }
  }

  testOpt("arrayget from arrayfill") {
    makeCFG(0@@("a=newarr(arr,ic(3))",
                "fill(a,0x01111137,0x02222242,0x03333356)",
                "g=ga(a,ic(1))"))
    checkEliminationBy(n("g")) { case IConst(0x42) => }
  }

  testOpt("arrayget with any index") {
    makeCFG(0@@"a=newarr(arr,ic(3))" -> 1@@"i=spinal()" -> 2@@"g=ga(a,i)")
    checkEliminationBy(n("g")) { case IConst(0) => }
  }

  testOpt("no RLE in unreachable code") {
    makeCFG(0@@"o=obj()"
      |>| UB -> 1 -> (2@@"g2=gf1(o)" || 3@@"g3=gf1(o)"))
    checkNoOptimization(n("g2"))
    checkNoOptimization(n("g3"))
  }

  testOpt("no RLE in merged unreachable code") {
    makeCFG(0@@"o=obj()" -> 2@@"g=gf1(o)"
      |>| UB -> 1@@"gf1(o)" -> 2)
    checkNoOptimization(n("g"))
  }

  testOpt("no getField from putField in merged unreachable code") {
    makeCFG(0@@"o=obj()" -> 2@@"g=gf1(o)"
      |>| UB -> 1@@"pf1(o,x)" -> 2)
    checkNoOptimization(n("g"))
  }

  testOpt("getField from putField after UCE") {
    makeCFG(0@@("o=obj()", "pf1(o,x)") -> 2@@"g=gf1(o)"
      |>| UB -> 1 -> 2)

    checkNoOptimization(n("g"))

    eliminateUnreachableCode()

    checkEliminationBy(n("g")) { case Adjusted(N("x")) => }
  }

  testOpt("simple getstatic duplication") {
    makeCFG(0@@("g1=gs1()", "mem()", "g2=gs1()"))
    checkEliminationBy(n("g2")) { case N("g1") => }
  }

  testOpt("hard getstatic duplication in loop") {
    makeCFG(
      0@@"g1=gs1()" ->
      wd(1@@("mem()", "g2=gs1()")) ->
      2)
    checkEliminationBy(n("g2")) { case N("g1") => }
  }

  testOpt("elimination with two values") {
    makeCFG(
      0 ->
      (1@@"ps1(ic(0x1AAAAA37))" || 2@@"ps1(ic(0x2BBBBB42))") ->
      3@@"g=gs1()"
    )
    checkEliminationBy(n("g")) { case Phi(B(3), IConst(0x37), IConst(0x42)) => }
  }

  testOpt("elimination with spoiling in cold code") {
    makeCFG(
      0@@"ps1(x)" ->
      (1 || 2@@("coldcode()", "b1=barrier()") || 3@@("coldcode()", "b2=barrier()")) ->
      4@@"g=gs1()"
    )
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(B(4), Adjusted(N("x")), GS(N("b1")), GS(N("b2"))) =>
    }
  }

  testOpt("elimination with many spoilers in single cold region") {
    makeCFG(
      0@@"ps1(x)" ->
      (1 || (2@@"b1=barrier()" || 3@@"b2=barrier()") -> 4@@"coldcode()") ->
      5@@"g=gs1()"
    )
    // however it would be great to insert single reload into block 4
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(B(5), Adjusted(N("x")),
                     Phi(B(4), GS(N("b1")), GS(N("b2")))) =>
    }
  }

  testOpt("elimination with trivial spoiling in cold code") {
    makeCFG(
      0@@"ps1(x)" ->
      (1@@("coldcode()", "ps1(ic(37))") || 2 || 3@@("coldcode()", "b=barrier()")) ->
      4@@"g=gs1()"
    )
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(B(4), IConst(37), Adjusted(N("x")), GS(N("b"))) =>
    }
  }

  testOpt("spoiler is located above get's upper point") {
    makeCFG(
      0 ->
      1 @@("coldcode()", "barrier()") ->
      2 @@ "o=obj()" ->
      (3 @@ "pf1(o,x)" || 4) ->
      5 @@ "g=gf1(o)"
    )
    // This could happen if argument has good type without any checks.
    gmo(n("g")).inCtrl = entryBlock
    // Note that we could put reload to block 4, but this is not done yet.
    checkNoOptimization(n("g"))
  }

  testOpt("arrayget's constant arg should not crash get upper point calculation") {
    makeCFG(
      0 @@ "a=obj()" ->
      (1 @@ "pa(a,ic(37),x)" || 2@@("coldcode()", "b=barrier()")) ->
      3 @@ "g=ga(a,ic(37))"
    )
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(B(3), Adjusted(N("x")), ArrayGet(_, N("b"), N("a"), IConst(37))) =>
    }
  }

  testOpt("no move because spoiling below") {
    makeCFG(
      0@@"g1=gs1()" ->
      wd(1@@("g2=gs1()", "ps1(x)")) ->
      2)
    checkEliminationBy(n("g2")) { case Phi(B(1), N("g1"), Adjusted(N("x"))) => }
  }

  testOpt("simple move out of loop") {
    makeCFG(
      0@@"p=mem()" ->
      wd(1@@("mem()", "g=gs1()")) ->
      2)
    checkMovementTo(n("g"), n("p"))
  }

  testOpt("disabled move out of loop") {
    makeCFG(
      0@@"p=mem()" ->
      wd(1@@("mem()", "g=gs1()")) ->
      2)
    env.disable(MoveLoadsOutOfLoops)
    try {
      checkNoOptimization(n("g"))
    } finally {
      env.enable(MoveLoadsOutOfLoops)
    }
  }

  testOpt("no move out of loop because control") {
    makeCFG(
      0@@"o=obj()" ->
      wd(1@@("nc(o)", "g=gf1(o)")) ->
      2)
    checkNoOptimization(n("g"))
  }

  testOpt("move out of loop hard: controlled by RTF in loop header") {
    makeCFG(
      0@@"o=obj()" ->
      (1@@"nc(o)" || 2@@("nc(o)", "mem()")) ->
      wd(3 -> 4@@"g=gf1(o)") ->
      5)
    checkMovementToPreHeaderOf(n("g"), b(3))
  }

  testOpt("move out of loop to pre-header") {
    makeCFG(
      0@@("o=obj()", "nc(o)") ->
      (1 || 2) ->
      wd(3@@("ps2(x)", "g=gf3(o)")) ->
      4)
    checkMovementToPreHeaderOf(n("g"), b(3))
  }

  testOpt("move out of many loops") {
    makeCFG(
      0@@"p=mem()" ->
      wd(1@@"mem()" ->
        wd(2@@"mem()" ->
          wd(3@@("mem()", "g=gs1()")))) ->
      4)
    checkMovementTo(n("g"), n("p"))
  }

  testOpt("move out of loop: control is in another loop") {
    makeCFG(
      0@@"o=obj()" ->
      dw(1@@"nc(o)") ->
      dw(2 ->
        dw(3@@("g=gf3(o)", "mem()")) ->
        4))
    checkMovementToPreHeaderOf(n("g"), b(2))
  }

  testOpt("move out of loop: with dependent put") {
    makeCFG(
      0@@"p=mem()" ->
      wd(1@@("g=gs3()", "ps3(x)")) ->
      2)
    checkEliminationBy(n("g")) { case Phi(B(1), GS(N("p")), Adjusted(N("x"))) => }
  }

  testOpt("move out of many loops (loop dominates dirty operation)") {
    makeCFG(
      0 ->
      dw(
        1@@"p=mem()" ->
        dw(
          2 ->
          dw(
            3@@"g=gs3()" ->
            4) ->
          5@@"mem()") ->
        6@@"barrier()") ->
      7)
    checkMovementTo(n("g"), n("p"))
  }

  testOpt("move out of many loops (dirty operation is post-dominated by inner loop)") {
    makeCFG(
      0 ->
      dw(
        1@@"b=barrier()" ->
        dw(
          2 ->
          dw(
            3@@"g=gs3()" ->
            4) ->
          5@@"mem()") ->
        6) ->
      7)
    checkMovementTo(n("g"), n("b"))
  }

  testOpt("move out of loop with reloads") {
    makeCFG(
      0@@"m=mem()" ->
      wd(1@@"g=gs3()" ->
         (2 || 3@@("coldcode()", "b1=barrier()") || 4@@("coldcode()", "b2=barrier()"))) ->
      4)
    checkEliminationWithReloadsBy(n("g")) {
      case p @ Phi(B(1), GS(N("m")),
                         Phi(_, itself, GS(N("b1")), GS(N("b2")))) if p == itself =>
    }
  }

  testOpt("move out of loop is not afraid of trivial spoilers") {
    makeCFG(
      0@@"m=mem()" ->
      wd(1@@"g=gs3()" ->
         (2 || 3@@("coldcode()", "b=barrier()") || 4@@("coldcode()", "ps3(ic(37))"))) ->
      4)
    checkEliminationWithReloadsBy(n("g")) {
      case p @ Phi(B(1), GS(N("m")),
                         Phi(_, itself, GS(N("b")), IConst(37))) if p == itself =>
    }
  }

  testOpt("move out of loop is afraid of unreachable code") {
    makeCFG(
      0@@"m=mem()" ->
      wd(1 ->
         2@@"g=gs1()" -> 3@@"mem()") ->
      4 |>| UB -> 5 -> 3)

    // g should have memory inside of the loop
    n("g").asInstanceOf[HasInMemory].inMemory shouldBe b(1)

    // make b(4) unreachable with memory outside of the loop
    5.blockEnd.inMemory = n("m").asInstanceOf[MemoryNode]

    checkNoOptimization(n("g")) // analysis should be afraid of unreachable code
  }

  testOpt("bubble sort pattern unrolled") {
    makeCFG(
      0@@"a=obj()" ->
      wd(
        1@@("i0", "i1=add(i0,ic(1))", "v0=ga(a,i0)", "v1=ga(a,i1)") ->
        (2 || 3@@("pa(a,i0,v1)", "pa(a,i1,v0)")) ->
        4@@("i2=add(i0,ic(2))", "w1=ga(a,i1)", "w2=ga(a,i2)") ->
        (5 || 6@@("pa(a,i1,w2)", "pa(a,i2,w1)"))) ->
      7
    )
    checkEliminationBy(n("w1")) {
      case Phi(B(4), N("v1"), N("v0")) =>
    }
  }

  testOpt("bubble sort pattern unrolled with swapped puts") {
    makeCFG(
      0@@"a=obj()" ->
      wd(
        1@@("i0=pinned()", "i1=add(i0,ic(1))", "v0=ga(a,i0)", "v1=ga(a,i1)") ->
        (2 || 3@@("pa(a,i1,v0)", "pa(a,i0,v1)")) ->
        4@@("i2=add(i0,ic(2))", "w1=ga(a,i1)", "w2=ga(a,i2)") ->
        (5 || 6@@("pa(a,i2,w1)", "pa(a,i1,w2)"))) ->
      7
    )
    checkNoOptimization(n("w1")) // put to i0 conflicts with get from i1 :(
  }

  testOpt("bubble sort pattern peeled", ValueArgsMutation) {
    makeCFG(
      0@@"a=obj()" ->
      1@@("v0=ga(a,ic(0))", "v1=ga(a,ic(1))") ->
      (2 || 3@@("pa(a,ic(0),v1)", "pa(a,ic(1),v0)")) ->
      4@@("iInit=ic(1)") ->
      wd(
        5@@("i0=phi(iInit,i1)", "i1=add(i0,ic(1))", "w0=ga(a,i0)", "w1=ga(a,i1)") ->
        (6 || 7@@("pa(a,i0,w1)", "pa(a,i1,w0)")) ->
        8) ->
      8
    )
    // More sophisticated analysis could understand that phi-args of get operation should be changed in above blocks.
    // E.g. w0 moved to 4 should be considered as ga(a,iInit), to 8 as ga(a,i1).
    // In such a case it could be optimized to
    //   p @ Phi(B(5), Phi(B(4), N("v0"), N("v1")),
    //                 Phi(B(8), N("w1"), `p`))
    checkNoOptimization(n("w0"))
  }

  testOpt("getfield from phi", ValueArgsMutation) {
    makeCFG(
      0 ->
      (1@@("o1=obj()", "pf1(o1,ic(37))") || 2@@("o2=obj()", "pf1(o2,ic(42))")) ->
      3@@("p=phi(o1,o2)", "g=gf1(p)")
    )
    // Could be Phi(B(3), IConst(37), IConst(42)) but not done yet.
    checkNoOptimization(n("g"))
  }

  testOpt("hard arrayget passing through back edge", ValueArgsMutation) {
    makeCFG(
      0@@("a=newarr(arr,ic(10))","i0=ic(0)") ->
      dw(
        1@@(
          "i=phi(i0,ipp)",
          "ipp=add(i,ic(1))"
        ) ->
        (2@@"pa(a,ipp,ic(37))" || 3@@"pa(a,ipp,ic(42))") ->
        4@@("mem()", "g=ga(a,i)")
      ) ->
      5
    )
    // It's not easy but could be optimized to the following pattern using two non-SSA variables:
    //   Phi(B(1), IConst(0),
    //             Phi(B(4), IConst(37), IConst(42)))
    checkNoOptimization(n("g"))
  }

  testOpt("trivial array get in loop but optimizable", ValueArgsMutation) {
    makeCFG(
      0@@("a=newarr(arr,ic(10))","i0=ic(0)") ->
        dw(1@@(
          "i=phi(i0,ipp)",
          "mem()",
          "g=ga(a,i)",
          "ipp=add(i,ic(1))"
        )) ->
        2
    )
    // It could be optimized to ic(0)
    checkNoOptimization(n("g"))
  }

  testOpt("array ops on different array iterations should not be merged", ValueArgsMutation) {
    makeCFG(
      0@@("a=newarr(arr,ic(10))","i0=ic(0)") ->
      dw(1@@(
        "i=phi(i0,ipp)",
        "g=ga(a,i)",
        "p=pa(a,i,x)",
        "ipp=add(i,ic(1))"
      )) ->
      2
    )
    // It should not be optimized to phi(ic(0), x)
    // because `i` in `p` at previous iteration is not equal to `i` in `g` at this iteration.
    checkNoOptimization(n("g"))
  }

  testOpt("array ops on different array iterations should not be merged 2", ValueArgsMutation) {
    makeCFG(
      0@@("a=newarr(arr,ic(10))","i0=ic(0)") ->
        dw(1@@(
          "i=phi(i0,ipp)",
          "g=ga(a,add(i,i))",
          "p=pa(a,add(i,i),x)",
          "ipp=add(i,ic(1))"
        )) ->
        2
    )
    // It should not be optimized to phi(ic(0), x)
    // because `i` in `p` at previous iteration is not equal to `i` in `g` at this iteration.
    checkNoOptimization(n("g"))
  }

  testOpt("inductive get field should not be moved out of loop") {
    makeCFG(
      0@@("o0=new(host)") ->
      dw(
        1@@(
          "o=phi(o0,opp)",
          "g=gf1(o)",
          "opp=new(host)"
        )) ->
      2
    )
    checkNoOptimization(n("g"))
  }

  testOpt("inductive array get should not be moved out of loop") {
    makeCFG(
      0@@("a=obj()","i0=ic(0)") ->
      dw(
        1@@(
          "i=phi(i0,ipp)",
          "g=ga(a,i)",
          "ipp=add(i,ic(1))"
        )) ->
      2
    )
    checkNoOptimization(n("g"))
  }

  testOpt("non-inductive array get eliminated in loop") {
    makeCFG(
      0@@("a=obj()", "g1=ga(a,ic(0))") ->
      dw(1@@("mem()", "g2=ga(a,ic(0))")) ->
      2)
    checkEliminationBy(n("g2")) { case N("g1") => }
  }

  testOpt("non-inductive array get moved out of loop") {
    makeCFG(
      0@@("a=obj()", "m=mem()") ->
      dw(1@@("mem()", "g=ga(a,ic(0))")) ->
      2)
    checkMovementTo(n("g"), n("m"))
  }

  testOpt("two equivalent getfields moved out of loop with cold reloads and should be fully optimized to single one") {
    makeCFG(
      0@@"m=mem()" ->
      dw(
        10 ->
        (21@@("mem()", "g1=gs1()", "u1=use(g1)") || 22@@("mem()", "g2=gs1()", "u2=use(g2)")) ->
        30 ->
        (41 || 42@@("coldcode()", "b=barrier()")) ->
        50)
    )

    // I don't want to introduce multi-get checker DSL, so check everything manually.
    checkEliminationWithReloadsBy(n("g1")) { case _ => }

    def g1 = n("u1").asInstanceOf[FakeSpinalUnary].inValue
    def g2 = n("u2").asInstanceOf[FakeSpinalUnary].inValue

    for (g <- Seq(g1, g2)) {
      inside(g) { // workaround for problems of matchPattern and Phi
        case p @ Phi(B(10), GS(N("m")),
                            Phi(B(50), itself, GS(N("b")))) if p == itself =>
      }
    }

    g1 should not be g2 withClue ", they should be optimized to different values"
    eliminateEquivalentPhies()
    g1 shouldBe g2 withClue ", after another optimization they should be identical"
  }

  testOpt("two equivalent getfields moved out of different loops with cold reloads and should be fully optimized to single one") {
    makeCFG(
      0@@"m=mem()" ->
      dw(
        10 ->
        (21@@("mem()", "g1=gs1()", "u1=use(g1)") || dw(22@@("mem()", "g2=gs1()", "u2=use(g2)"))) ->
        30 ->
        (41 || 42@@("coldcode()", "b=barrier()") || 43@@"ps1(ic(37))") ->
        50)
    )

    // I don't want to introduce multi-get checker DSL, so check everything manually.
    checkEliminationWithReloadsBy(n("g1"), shouldCheckNoOptimization = false) { case _ => }

    def g1 = n("u1").asInstanceOf[FakeSpinalUnary].inValue
    def g2 = n("u2").asInstanceOf[FakeSpinalUnary].inValue

    for (g <- Seq(g1, g2)) {
      inside(g) { // workaround for problems of matchPattern and Phi
        case p @ Phi(B(10), GS(N("m")),
                            Phi(B(50), itself, GS(N("b")), IConst(37))) if p == itself =>
      }
    }

    g1 should not be g2 withClue ", they should be optimized to different values"
    eliminateEquivalentPhies()
    g1 shouldBe g2 withClue ", after another optimization they should be identical"
  }

  {
    val ff = makeSymField(s"fakeF", host, host).setJavaModifiers(Modifiers(STATIC))

    testOpt("transitive movement around cold code") {
      makeCFG(0@@"p=mem()" -> dw(1 -> (2 || 3@@("coldcode()", "b=barrier()")) -> 4))
      makeNodes { at =>
        at(4)
        n("g") = GetStatic(ff)
        n("h") = GetField(f1)(n("g"))
        n("ug") = FakeSpinalUnary(n("g").tpe)(n("g"))
        n("uh") = FakeSpinalUnary(n("h").tpe)(n("h"))
      }
      def g = n("ug").asInstanceOf[FakeSpinalUnary].inValue
      def h = n("uh").asInstanceOf[FakeSpinalUnary].inValue

      // I don't want to introduce multi-get checker DSL, so check everything manually.
      checkEliminationWithReloadsBy(n("g"), shouldCheckNoOptimization = false) { case _ => }

      inside(g) { // workaround for problems of matchPattern and Phi
        case p @ Phi(B(4),
                     Phi(B(1), GS(N("p")), itself),
                     GS(N("b"))) if p == itself =>
      }
      inside(h) { // workaround for problems of matchPattern and Phi
        case GF(B(4)) =>
        // No transitive optimization yet, but could be:
        // case p @ Phi(B(4),
        //              Phi(B(1), GF(N("p")), itself),
        //              GF(N("b"))) if p == itself =>
      }
    }
  }

  testOpt("movement to pre-header with get above") {
    makeCFG(
      0@@"g0=gs1()" ->
      (1 || dw(2@@"mem()" -> 3@@"g3=gs1()")) ->
      4@@"g4=gs1()"
    )
    checkEliminationBy(n("g4")) {
      case N("g0") =>
    }
  }

  testOpt("movement to pre-header with put above") {
    makeCFG(
      0@@"ps1(x)" ->
      (1 || dw(2@@"mem()" -> 3@@"g3=gs1()")) ->
      4@@"g4=gs1()"
    )
    checkEliminationBy(n("g4")) {
      case Adjusted(N("x")) =>
    }
  }

  testOpt("unoptimizable gets with different ctrl") {
    makeCFG(
      0@@"o=obj()" ->
      (1@@("nc(o)", "g1=gf1(o)") || 2@@("nc(o)", "g2=gf1(o)")) ->
      3
    )
    checkNoOptimization(n("g1"))
  }

  testOpt("get below gets with different ctrl") {
    makeCFG(
      0@@"o=obj()" ->
      (1@@("nc(o)", "g1=gf1(o)") || 2@@("nc(o)", "g2=gf1(o)")) ->
      3@@"g3=gf1(o)"
    )

    // Could be Phi(B(3), N("g1"), N("g2")), but we are afraid of non-dominating gets.
    checkNoOptimization(n("g3"))
  }

  testOpt("put above gets with different ctrl") {
    makeCFG(
      0@@"o=obj()" ->
      (1@@("nc(o)", "pf1(o,ic(37))") || 2@@("nc(o)", "pf1(o,ic(42))")) ->
      3 ->
      (4@@("nc(o)", "g1=gf1(o)") || 5@@("nc(o)", "g2=gf1(o)")) ->
      6
    )
    checkEliminationBy(n("g1")) {
      case Phi(B(3), IConst(37), IConst(42)) =>
    }
  }

  testOpt("strange control dependencies with the same memory") {
    makeCFG(
      0@@(
        "o=obj()",
        "g1=gf1(o)", // hacky get without nullcheck
        "nc(o)",
        "g2=gf1(o)")
    )
    n("g2") shouldNot be (n("g1")) // no value numbering, please
    checkEliminationBy(n("g2")) { case N("g1") => }
  }

  testOpt("get below conditional get") {
    makeCFG(
      0@@"o=obj()" ->
      (1@@("nc(o)", "g1=gf1(o)") || 2) ->
      3@@("mem()", "g3=gf1(o)")
    )
    checkNoOptimization(n("g3"))
  }

  testOpt("explicit non-value-numbered gets") {
    withDeferredOnCommitOptimizations {
      makeCFG(
        0@@("g1=gs1()", "g2=gs1()")
      )
      n("g1") shouldNot be (n("g2"))
      checkNoOptimization(n("g1")) // Let value numbering do its job.
    }
  }

  {
    // Deferred host to prevent fields type analysis.
    val host = makeSymClass("hostDeferred", symObj).markTurboClinited().setDeferred()
    val fields =
      for (t <- Seq(symA, symB, symI, symJ, symX, symXX))
        yield makeSymField(s"fake${t.getName}", t, host)

    for (f <- fields; fAnother <- fields) {
      testOpt(s"eop adjustments for (${f.getType}, ${fAnother.getType})") {

        makeCFG(0 -> (1 || (2 || 3) -> 23) -> 4)
        makeNodes { at =>
          at(1)
          PutStatic(f)(Null())

          at(2)
          n("get") = GetStatic(f)

          at(3)
          n("putGet") = GetStatic(fAnother)
          Node.withImplicitArgConversion(enrichArg()) {
            PutStatic(f)(depriveIfNeeded(n("putGet")))
          }

          at(4)
          n("g") = GetStatic(f)
        }

        checkEliminationBy(n("g")) {
          case Phi(B(4), Null(),
                         Phi(B(23), N("get"),
                                    Adjusted(N("putGet")))) =>
        }
      }
    }
  }

  testOpt("clinit diamondization simple") {
    // Context types are usually afraid of clinit check, so help them a lot: instance field of new object.
    makeCFG(0@@("o=new(host)", "pf1(o,ic(37))", "c=clinit(C)", "g=gf1(o)"))
    checkNoOptimization(n("g"))
    n("c").block shouldBe b(0) withClue ", no clinit wrapping without need"
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(_, GF(N("c")), IConst(37)) =>
    }
  }

  testOpt("clinit diamondization double") {
    makeCFG(
      0@@("o=new(host)", "pf1(o,ic(37))", "c=clinit(C)") ->
      (1@@("mem()", "g1=gf1(o)") || 2@@("mem()", "g2=gf1(o)")))
    checkEliminationWithReloadsBy(n("g2")) {
      case Phi(_, GF(N("c")), IConst(37)) =>
      case _ => fail()
    }
    all[InitializedTest].toSeq should have length(1)
  }

  testOpt("clinit diamondization in a loop") {
    makeCFG(0@@"m=mem()" -> dw(1@@("g=gs1()", "c=clinit(C)")) -> 2)
    checkEliminationWithReloadsBy(n("g")) {
      case p @ Phi(_, GS(N("m")),
                      Phi(_, GS(N("c")),
                             itself)) if p == itself =>
    }
  }

  testOpt("clinit diamondization in case of conservative inCtrl equal to clinit") {
    makeCFG(0@@("ps1(ic(37))", "c=clinit(C)", "g=gs1()"))
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(_, GS(N("c")), IConst(37)) =>
    }
  }

  testOpt("clinit diamondization in case of conservative inCtrl equal to clinit (with extra mem in the middle)") {
    makeCFG(0@@("ps1(ic(37))", "c=clinit(C)", "mem()", "g=gs1()"))
    checkEliminationWithReloadsBy(n("g")) {
      case Phi(_, GS(N("c")), IConst(37)) =>
    }
  }

  testOpt("JET-15019. ContextTypes.topCtrl for GetMemoryOperation") {
    // No on-commit optimization, turning off get-identity
    withDeferredOnCommitOptimizations {
      makeCFG(dw(0 @@ ("obj=xn()", "if(cmp(obj,null()))") -> (1 || 2 @@ ("ps=psnn(obj)", "gs=gsnn()", "gf=gf1(gs)")) -> 3))
      // Right now `gf` uses `gs` as an argument and key for context types, also node `gs` inCtrl is `ps`
      n("gf").asInstanceOf[GetField].obj shouldBe n("gs")

      recalculateContextTypes() shouldBe true
      // Right now node `gs` inCtrl is `entryBlock`
      n("gf").asInstanceOf[GetField].obj shouldBe n("gs")
    }

    // On-commit optimizations happened.
    // `gs` node died thanks to identity optimizations, now `gf` uses `obj` as an argument and key for context types.
    n("gf").asInstanceOf[GetField].obj shouldBe n("obj")

    // Now memory of `gf` can be moved out of loop
    optimizeMemoryReads()
    // At this point if topCtrl of `gf` was calculated as `gf.inCtrl` and not as lowest(gf.inCtrl, gf.inMemory),
    // then `gf`'s new inCtrl will be entryBlock, which can't be, since `gf` requires `obj` to be not null, which was
    // guaranteed by `gs` previously, but not guaranteed anywhere in entryBlock.
    withIncrementalGCM {
      n("gf").block shouldBe b(2)
    }
  }

}

