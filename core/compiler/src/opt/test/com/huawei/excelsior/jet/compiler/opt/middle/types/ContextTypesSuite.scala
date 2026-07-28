/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{FINAL, STATIC}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.middle.ContextTypesRecalculation
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.types.References.{RefNull, ReferenceApprox}
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.options.BoolOption.WorkaroundForJET16467
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeField
import com.huawei.excelsior.jet.compiler.types.{ReferenceTypes, TypesToolbox}

import scala.util.chaining.scalaUtilChainingOps

class ContextTypesSuite extends CompilerSuite with TypesToolbox with GlobalNodesBuilder with ContextTypesRecalculation {

  import TypeApproximationBuildingHelperStrict._
  private def nul = RefNull

  override def parsableAttributes() = {
    def staticField(t: ReferenceTypes.ReferenceType) = {
      makeSymField("f", symObj, t).setJavaModifiers(Modifiers(STATIC, FINAL))
    }

    Seq(
      new StringAttribute("obj")({ case (name, Seq()) => addObjNode(c(t(name))) }),
      new SimpleAttribute("deprive")({ case Seq(obj) => RawDeprive(obj) }),
      new StringAttribute("enrich")({ case (name, Seq(obj, imt)) => Enrich(sym(name))(obj, imt) }),
      new StringAttribute("gs")({ case (name, Seq()) => GetStatic(staticField(t(name))) }),
      new StringAttribute("test")({ case (name, Seq()) => InitializedTest(sym(name))() tap setCondition }),
      new StringAttribute("instof")({ case (name, Seq(obj)) => InstanceOf(sig(name))(obj) }),
    ) ++ super.parsableAttributes()
  }

  override def afterEach(): Unit = {
    rootMethod.setReturnType(SignatureType.javaLangObject)
    super.afterEach()
  }

  startPhase(CompilerPhase.PostInline)

  def unreachable: ReferenceApprox = null

  private def checkFilterAt[A](graph: SubGraph, makeFilter: (Block => Unit) => A, checkConditions: A => Unit): Unit = {
    makeCFG(graph)
    val state = makeNodes(makeFilter)
    removeHandlerAnchors()
    eliminateUnreachableCode()
    recalculateContextTypes()
    checkConditions(state)
  }

  private def checkFilter[A](graph: SubGraph, makeFilter: => A, checkConditions: A => Unit): Unit =
    checkFilterAt(graph, at => { at(0); makeFilter }, checkConditions)

  private def checkFilterObj(graph: SubGraph, checkBlocks: Seq[Block],
                             makeFilterByObj: Node => Unit,
                             inType: ReferenceApprox, checkTypes: Seq[ReferenceApprox]): Unit = {

    checkFilter(graph, {
      val obj = addObjNode(inType)
      makeFilterByObj(obj)
      obj
    }, { obj =>
      nodeTypeAt(obj, entryBlock) should beTA (inType)
      for ((b, t) <- checkBlocks zip checkTypes) {
        if (t != unreachable) {
          nodeTypeAt(obj, b.blockEnd) should beTA (t)
        } else {
          b.unreachable should be(true)
        }
      }
    })
  }

  def checkSpinalFilter(makeNode: Node => Unit,
                        in: ReferenceApprox, success: ReferenceApprox, failure: ReferenceApprox): Unit = {

    checkFilterObj(0 -> (1 || (xb(2) -> 3)), Seq(1, 2),
      makeNode,
      in, Seq(success, failure))
  }

  def checkIfFilter(makeNode: Node => Node,
                    in: ReferenceApprox, positive: ReferenceApprox, negative: ReferenceApprox): Unit = {

    checkFilterObj(0 -> (1 || 2), Seq(1, 2),
      { obj => setCondition(makeNode(obj)) },
      in, Seq(positive, negative))
  }


  def checkNullCheckFilter(in: ReferenceApprox, success: ReferenceApprox, failure: ReferenceApprox): Unit =
    checkSpinalFilter(NullCheck(_), in, success, failure)

  test("nullcheck simple") {
    checkNullCheckFilter(cn(tA), success = c(tA), failure = nul)
  }

  test("nullcheck redundant failure") {
    checkNullCheckFilter(c(tA), success = c(tA), failure = unreachable)
  }

  test("nullcheck redundant success") {
    // Note that redundant filter was not removed in current implementation.
    checkNullCheckFilter(nul, success = e, failure = nul)
  }


  test("checkcast simple") {
    checkSpinalFilter(CheckCast(sig(tB.symType))(_),
      cn(tA), success = cn(tB), failure = c(tA))
  }

  test("checkcast with usage in weakcast") {
    checkFilter[(CheckCast, WeakCast)](0@@("x=obj(I)", "cc=cc(I,x)", "wc=wcc(I,x,cc)"), {
      val cc = n("cc").asInstanceOf[CheckCast]
      val wc = n("wc").asInstanceOf[WeakCast]
      wc.hasDominatingCheck should be (true)

      (cc, wc)

    }, { case (cc, wc) =>
      cc.isCommitted should be (false)
      wc.hasDominatingCheck should be (false)
    })
  }

  test("checkcast not >=") {
    checkSpinalFilter(CheckCast(sig(tB.symType))(_),
      cn(tI), success = cn(tB), failure = c(tI))
  }

  test("checkcast not >= (array edition)") {
    checkSpinalFilter(CheckCast(sig(tB1D.symType))(_),
      cn(tI1D), success = cn(tB1D), failure = c(tI1D))
  }

  test("checkcast (half array edition)") {
    checkSpinalFilter(CheckCast(sig(tB1D.symType))(_),
      cn(tAI), success = cn(tB1D), failure = c(tAI))
  }


  def checkIfNullFilter(in: ReferenceApprox, success: ReferenceApprox, failure: ReferenceApprox): Unit =
    checkIfFilter({ obj => Cmp(TRefType, Condition.NE)(obj, Null()) },
      in, positive = success, negative = failure)

  test("if null") {
    checkIfNullFilter(cn(tA), success = c(tA), failure = nul)
  }

  test("if null inversed") {
    checkIfFilter({ obj => Cmp(TRefType, Condition.EQ)(obj, Null()) },
      cn(tA), positive = nul, negative = c(tA))
  }

  test("if null redundant negative") {
    checkIfNullFilter(c(tA), success = c(tA), failure = unreachable)
  }

  test("if null redundant positive") {
    checkIfNullFilter(nul, success = unreachable, failure = nul)
  }


  test("if instanceof") {
    checkIfFilter({ obj =>
      Cmp(IntType, Condition.NE)(InstanceOf(sig(tB.symType))(obj), IConst(0)) },
      cn(tA), positive = c(tB), negative = cn(tA))
  }


  test("if tau test simple") {
    checkIfFilter(TypeTest(PointGuard(tB), TauInfo.Unknown)(_),
      c(tA), positive = p(tB), negative = c(tA))
  }

  test("if tau test redundant") {
    checkIfFilter(TypeTest(PointGuard(tB), TauInfo.Unknown)(_),
      p(tB), positive = p(tB), negative = unreachable)
  }

  test("if tau test nullable") {
    checkIfFilter(TypeTest(PointGuard(tB), TauInfo.Unknown)(_),
      cn(tA), positive = pn(tB), negative = cn(tA))
  }

  test("if tau test nullable redundant positive") {
    checkIfFilter(TypeTest(PointGuard(tB), TauInfo.Unknown)(_),
      pn(tB), positive = pn(tB), negative = unreachable)
  }

  test("if tau test nullable redundant negative") {
    checkIfFilter(TypeTest(PointGuard(tB), TauInfo.Unknown)(_),
      pn(tA), positive = unreachable, negative = pn(tA))
  }


  def checkEqualIDescFilter(in: ReferenceApprox, success: ReferenceApprox, failure: ReferenceApprox, inverted: Boolean = false): Unit =
    checkIfFilter({ obj =>
      val cond = if (inverted) Condition.NE else Condition.EQ
      Cmp(AddrType, cond)(InstanceDescriptorBy(obj), InstanceDescriptor(tA.symType)()) },
        in, positive = success, negative = failure)

  test("if equal idesc") {
    checkEqualIDescFilter(c(tObj), success = p(tA), failure = c(tObj))
  }

  test("if equal idesc inverted") {
    checkEqualIDescFilter(c(tObj), success = c(tObj), failure = p(tA), inverted = true)
  }

  test("if equal idesc nullable") {
    checkEqualIDescFilter(cn(tObj), success = p(tA), failure = cn(tObj)) // failure could be c(tObj)
  }

  test("if equal idesc redundant positive") {
    checkEqualIDescFilter(p(tA), success = p(tA), failure = unreachable)
  }

  test("if equal idesc redundant negative") {
    checkEqualIDescFilter(p(tB), success = unreachable, failure = p(tB))
  }

  test("if equal idesc point nullable") {
    checkEqualIDescFilter(pn(tA), success = p(tA), failure = nul) // failure could be unreachable
  }


  test("enrich node problem") {
    rootMethod.setReturnType(SignatureType.JBCReference(symI))
    // Current implementation can understand
    // that enriched and deprived version of the same object have the same type.
    checkFilter[(Node, Node)](0@@("x=obj(Obj)", "cc(IB,x)", "wc=wc(I,x)", "e=enrich(I,x,wc)", "ret(e)"), {
      (n("x"), n("e"))

    }, { case (deprived, enriched) =>
      nodeType(deprived) should beTA (c(tObj))
      nodeType(enriched) should beTA (c(tObj))
      nodeTypeAt(deprived, 0.blockEnd) should beTA (c(tIB))
      nodeTypeAt(enriched, 0.blockEnd) should beTA (c(tIB))
    })
  }

  test("phi problem (different objs)") {
    checkFilter(0 -> (1@@("x=obj(Obj)", "cc(B,x)") || 2@@("y=obj(Obj)", "cc(C,y)")) -> 3@@"p=phi(x,y)", {
      n("p")

    }, { phi =>
      nodeTypeAt(phi, 3.blockEnd) should beTA (c(tA))
    })
  }

  test("phi problem (same obj)") {
    checkFilter(0@@"x=obj(Obj)" -> (1@@"cc(B,x)" || 2@@"cc(C,x)") -> 3, {
      n("x")

    }, { obj =>
      nodeTypeAt(obj, 3.blockEnd) should beTA (c(tA))
    })
  }

  test("clinit filter (as check)") {
    checkFilter[(Clinit, GetStatic)](0@@"clinit(A)" -> 1@@("cb=clinit(B)", "cc=clinit(C)", "g=gs(B)"), {
      (n("cb").asInstanceOf[Clinit], n("g").asInstanceOf[GetStatic])

    }, { case (cB, gs) =>
      isClassClinitedAt(tB, 0.blockEnd) shouldBe false
      isClassClinitedAt(tB, 1.blockEnd) shouldBe true
      cB dominates gs.inCtrl shouldBe true
      gs.inCtrl shouldBe cB
    })
  }

  test("clinit filter (as test)") {
    checkFilter[GetStatic](0@@"clinit(A)" -> 1@@"test(B)" -> (2 || 3@@"clinit(B)") -> 4@@"g=gs(B)", {
      n("g").asInstanceOf[GetStatic]

    }, { gs =>
      isClassClinitedAt(tB, 0.blockEnd) shouldBe false
      isClassClinitedAt(tB, 4.blockEnd) shouldBe true
      b(4) dominates gs.inCtrl shouldBe true
    })
  }

  test("clinit filter (as test & irreducible, JET-12656)") {
    checkFilter[GetStatic](0@@"clinit(A)" -> dw(11 -> 12@@"test(B)" -> (2 || 3@@"clinit(B)") -> 4@@"g=gs(B)") |>| (0 -> 12), {
      n("g").asInstanceOf[GetStatic]

    }, { gs =>
      isClassClinitedAt(tB, 0.blockEnd) shouldBe false
      isClassClinitedAt(tB, 4.blockEnd) shouldBe false // imperfection, could be true
      b(4) dominates gs.inCtrl shouldBe true
    })
  }

  test("no coarsening phi 1") {
    makeCFG((0@@("x=obj(A)", "y=obj(A)") -> 1 -> (3 || 4@@"p=phi(x,y)") -> 5@@"q=phi(x,p)") |>| (0 -> 2 -> 4))

    nodeType(n("q")) should be (c(tA))
  }

  test("no coarsening phi 2") {
    makeCFG((0@@("x=obj(A)", "y=obj(A)") -> (1 || 2) -> 3@@"p=phi(x,y)" -> 5@@"q=phi(p,y)") |>| (2 -> 4 -> 5))

    nodeType(n("q")) should be (c(tA))
  }

  test("phi recursion") {
    makeCFG(0@@"x=obj(IB)" -> dw(1@@("p=phi(x,d)", "d=deprive(p)")) -> 2)

    nodeType(n("p")) should be (c(tIB))
  }

  test("mutual phi recursion") {
    makeCFG(0@@("x=obj(A)", "y=obj(A)") -> dw(1@@("p=phi(x,q)", "q=phi(y,p)")) -> 2)

    nodeType(n("p")) should be (c(tA))
    nodeType(n("q")) should be (c(tA))
  }

  test("dumbbell loops") {
    makeCFG(0@@"x=obj(IB)" -> dw(1@@("p=phi(x,dp)", "dp=deprive(p)")) -> dw(2@@("q=phi(p,dq)", "dq=deprive(q)")) -> 3)

    nodeType(n("q")) should be (c(tIB))
  }

  test("non-monotonic flow") {
    makeCFG(0 @@ ("x=obj(I)", "y=obj(K)") -> dw(
      1 @@ ("p=phi(x,s)", "dp=deprive(p)") -> (
        dw(2 @@ ("q=phi(p,y)", "dq=deprive(q)") -> 3 @@ ("if(cmp(instof(K,dq),ic(0)))")) ||
          dw(4 -> 5 @@ ("if(cmp(instof(K,dp),ic(0)))"))
        ) -> 6 @@ ("s=phi(q,p)")
    ) -> 7)

    removeHandlerAnchors()
    eliminateUnreachableCode()

    recalculateContextTypes()
    env.disable(WorkaroundForJET16467)
    an[AssertionError] should be thrownBy {
      recalculateContextTypes()
    }
  }
}
