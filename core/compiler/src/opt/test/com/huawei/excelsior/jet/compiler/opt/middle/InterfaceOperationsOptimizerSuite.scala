/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.types.References.Point
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeType

import scala.PartialFunction.{cond, condOpt}

class InterfaceOperationsOptimizerSuite extends CompilerSuite
                                           with GlobalNodesBuilder
                                           with InterfaceOperationsOptimizer
                                           with ContextTypesRecalculation
                                           with SimplifyComponent {

  override def parsableAttributes() = {
    Seq(
      new UnnamedAttribute(() => addObjNode()),

      new SimpleAttribute("enrich")({ case Seq(obj, wc) => Enrich(symI)(obj, wc) }),

    ) ++ super.parsableAttributes()
  }

  def optimize() = {
    recalculateContextTypes() // required for finding dominating cast/instanceof
    optimizeInterfaceOperations()
    simplifyIR() // required to perform some value numbering in case of cyclic phies
    optimizeInterfaceOperations() shouldBe false // should not loop forever
  }

  /** Extract CIAO from some anchor (heuristically). */
  def ciao(n: Node) = {
    n.asInstanceOf[FakeUse].inValue match {
      case Enrich(_, _, ciao) => ciao
      case ciao => ciao
    }
  }

  object Const {
    def unapply(ciao: Node) = cond(ciao) { case IntegralConst(_) => true }
  }
  object WCLinked {
    def unapply(wc: WeakCast) =
      condOpt(wc.obj, wc.dominatingCheck) {
        case (N(obj), N(check)) if wc.hasDominatingCheck => (obj, check)
      }
  }
  object WCFull {
    def unapply(wc: WeakCast) =
      condOpt(wc.obj) {
        case N(obj) if !wc.hasDominatingCheck => obj
      }
  }

  startPhase(CompilerPhase.PostInline)

  test("weakcast: simple linking") {
    makeCFG(0@@("x", "cc=cc(I,x)", "u=use(wc(I,x))"))
    optimize()
    ciao("u") should matchPattern { case WCLinked("x", "cc") => }
  }

  test("weakcast: rematerialize to multiple use points") {
    makeCFG(0@@("x", "w=wc(I,x)") -> (1@@("cc1=cc(I,x)", "u1=use(w)") ||
                                    2@@("cc2=cc(I,x)", "u2=use(w)") ||
                                    3@@(               "u3=use(w)")))

    optimize()
    ciao("u1") should matchPattern { case WCLinked(_, "cc1") => }
    ciao("u2") should matchPattern { case WCLinked(_, "cc2") => }
    ciao("u3") should matchPattern { case WCFull(_) => }
  }

  test("weakcast: phi linked to common cast") {
    makeCFG(0 -> (1@@"x" || 2@@"y") ->
      3@@("p=phi(x,y)", "cc=cc(I,p)", "u=use(wc(I,p))"))
    optimize()
    ciao("u") should matchPattern { case WCLinked(_, "cc") => }
  }

  test("weakcast: phi linked to common cast (delayed, JET-12031)") {
    makeCFG(0 -> (1@@"x" || 2@@"y") ->
      3@@("p=phi(x,y)", "w=wc(I,p)") -> 4@@("cc=cc(I,p)", "u=use(w)"))

    replaceAllValueUsesByVar("w")
    ciao("u") shouldBe a[ReadVar]
    optimize()
    completeSSA()
    optimize()
    ciao("u") should matchPattern { case WCLinked("p", "cc") => }
  }

  test("weakcast: no motivation for phi pull up") {
    makeCFG(0 -> (1@@"x" || 2@@"y") ->
      3@@("p=phi(x,y)", "u=use(wc(I,p))"))
    optimize()
    ciao("u") should matchPattern { case WCFull("p") => }
  }

  test("weakcast: phi pull up to single cast") {
    makeCFG(0 -> (1@@("x", "cx=cc(I,x)") || 2@@"y") ->
      3@@("p=phi(x,y)", "u=use(wc(I,p))"))
    optimize()
    ciao("u") match {
      case Phi(_, WCLinked("x", "cx"), WCFull("y")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up multi level (and single level pull up gives nothing)") {
    //     0_____________
    //     |\  \ \  \ \  \
    // c1->1 2  3 4  5 6  7
    //      \|   \|   \|  |
    //      12   34   56<-|--c56
    //        \   |     \ |
    //         \  |    567
    //          \ | ___/
    //           \|/
    //            99
    makeCFG(0 -> (((1@@("x1", "c1=cc(I,x1)") || 2@@"x2") -> 12@@"p12=phi(x1,x2)") ||
                  ((3@@"x3"                 || 4@@"x4") -> 34@@"p34=phi(x3,x4)") ||
                  ((((5@@"x5" || 6@@"x6") -> 56@@("p56=phi(x5,x6)", "c56=cc(I,p56)")) ||
                    7@@"x7") -> 567@@"p567=phi(p56,x7)")
                 ) -> 99@@("p=phi(p12,p34,p567)", "u=use(wc(I,p))"))
    optimize()
    ciao("u") match {
      case Phi(_, Phi(_, WCLinked("x1", "c1"), _/*2*/),
                  _/*34*/,
                  Phi(_, WCLinked("p56", "c56"), _/*7*/)) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up and replace by light cast (global node type)") {
    makeCFG(0 -> (1@@"xC" || 2@@"y") ->
      3@@("p=phi(xC,y)", "u=use(wc(I,p))"))
    setNodeType("xC", Point(ReferenceType(symIB), mayBeNull = true))

    optimize()
    ciao("u") match {
      case Phi(_, Const(), WCFull("y")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up and replace by light cast (context node type)") {
    makeCFG(0 -> (1@@("x", "cx=cc(IB,x)") || 2@@"y") ->
      3@@("p=phi(x,y)", "u=use(wc(I,p))"))
    optimize()
    ciao("u") match {
      case Phi(_, Const(), WCFull("y")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up above loop simple") {
    makeCFG(0@@("x", "cx=cc(I,x)") ->
      dw(1@@"p=phi(x,y)" -> 2@@("y", "cy=cc(I,y)")) -> 3@@("u=use(wc(I,p))"))
    optimize()
    ciao("u") match {
      case Phi(_, WCLinked("x", "cx"), WCLinked("y", "cy")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up above loop recursive") {
    makeCFG(0@@("x", "cx=cc(I,x)") ->
      dw(1@@("p=phi(x,y,p)", "u=use(wc(I,p))") -> (2@@"y" || 3)) -> 4)
    optimize()
    ciao("u") match {
      case offs @ Phi(_, WCLinked("x", "cx"), WCFull("y"), offsP) if offs == offsP => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up above loop recursive (backward edges processed separately)") {
    makeCFG(0@@("x", "cx=cc(I,x)") ->
      dw(1@@("p=phi(x,yp)", "u=use(wc(I,p))") -> (2@@"y" || 3) -> 23@@"yp=phi(y,p)") -> 4)
    optimize()
    ciao("u") match {
      case offs @ Phi(_, WCLinked("x", "cx"), Phi(_, WCFull("y"), offsP)) if offs == offsP => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up in loop (bad idea if loop iterates many times, good idea if loop iterates zero times)") {
    makeCFG(0@@("x", "cx=cc(I,x)") ->
      dw(1@@("p=phi(x,y)", "y")) -> 2@@("u=use(wc(I,p))"))
    optimize()
    ciao("u") match {
      case Phi(_, WCLinked("x", "cx"), WCFull("y")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up but different interfaces for single object") {
    makeCFG(
      0 ->
      (1@@("x", "cc1=cc(I,x)") || 2@@("y", "cc2=cc(K,y)")) ->
      3@@"p=phi(x,y)" ->
      (4@@("w1=wc(I,p)", "u1=use(w1)") || 5@@("w2=wc(K,p)", "u2=use(w2)")))

    optimize()
    ciao("u1") match {
      case Phi(_, WCLinked("x", "cc1"), WCFull("y")) => // passed
      case x => fail(x.toString)
    }
    ciao("u2") match {
      case Phi(_, WCFull("x"), WCLinked("y", "cc2")) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: phi pull up (regression test: `p` is unoptimizable but is used to optimize `q`)") {
    //     0___
    //     |\  \
    //     1 2  3
    //      \|  |
    //       4  |
    //       |\ /
    //       | 5
    //       |/
    //       6
    makeCFG(0 -> (1@@"x" || 2@@"y") -> 4@@"p=phi(x,y)" -> 6 |>|
      4 -> 5 |>|
      0 -> 3@@("z", "cz=cc(I,z)") -> 5@@"q=phi(p,z)" -> 6@@("r=phi(p,q)", "u=use(wc(I,r))"))
    optimize()
    ciao("u") match {
      case Phi(_, WCFull("p"), Phi(_, WCFull("p"), WCLinked("z", "cz"))) => // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: no phi pull up in loop") {
    makeCFG(0@@"x" ->
      dw(1@@("p=phi(x,y)", "y")) -> 2@@("u=use(wc(I,p))"))
    optimize()
    ciao("u") should matchPattern { case WCFull("p") => }
  }

  test("weakcast: no phi pull up in loop recursive") {
    makeCFG(0@@"x" ->
      dw(1@@("p=phi(x,y,p)") -> (2@@"y" || 3)) -> 4@@("u=use(wc(I,p))"))
    optimize()
    ciao("u") should matchPattern { case WCFull("p") => }
  }

  test("weakcast: pull up to multiple casts") {
    makeCFG(0@@"x" -> (1@@"c1=cc(I,x)" || 2@@"c2=cc(I,x)") ->
      3@@("u=use(wc(I,x))"))
    optimize()
    ciao("u") match {
      case WCFull("x") => // it's not what we want, but current implementation can't do better
      case Phi(_, WCLinked("x", "c1"), WCLinked("x", "c2")) => fail("it actually passed, remove the case above and this fail()") // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: pull up to single cast") {
    makeCFG(0@@"x" -> (1@@"c1=cc(I,x)" || 2) ->
      3@@("u=use(wc(I,x))"))
    optimize()
    ciao("u") match {
      case WCFull("x") => // it's not what we want, but current implementation can't do better
      case Phi(_, WCLinked("x", "c1"), WCFull("x")) => fail("it actually passed, remove the case above and this fail()") // passed
      case x => fail(x.toString)
    }
  }

  test("weakcast: pull up and replace by light cast") {
    makeCFG(0@@"x" -> (1@@"c1=cc(IB,x)" || 2) ->
      3@@("u=use(wc(I,x))"))

    optimize()
    ciao("u") match {
      case WCFull("x") => // it's not what we want, but current implementation can't do better
      case Phi(_, Const(), WCFull("x")) => fail("it actually passed, remove the case above and this fail()") // passed
      case x => fail(x.toString)
    }
  }

  test("enrich: simple linked") {
    makeCFG(0@@("x", "c=cc(I,x)", "e=enrich(x,wc(I,x))", "u=use(e)"))
    optimize()
    ciao("u") should matchPattern { case WCLinked("x", "c") => }
  }

  test("enrich: light cast after class cast") {
    makeCFG(0@@("x", "c=cc(IB,x)", "e=enrich(x,wc(I,x))", "u=use(e)"))
    optimize()
    ciao("u") should matchPattern { case Const() => }
  }

  test("enrich: rematerialize to multiple use points") {
    makeCFG(0@@("x", "e=enrich(x,wc(I,x))") -> (1@@("c=cc(I,x)", "u1=use(e)") || 2@@"u2=use(e)"))
    optimize()
    ciao("u1") should matchPattern { case WCLinked("x", "c") => }
    ciao("u2") should matchPattern { case WCFull("x") => }
  }

}
