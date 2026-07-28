/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.explosion

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeType}

class ExplosionSuite extends CompilerSuite with GlobalNodesBuilder with Explosion with Universe  {

  val explosive = makeSymClass("Explosive", symObj)

  val field = makeSymField("field", symInt, explosive)

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("new")({
        case Seq() => New(sig(explosive))()
      }),
      new SimpleAttribute("get")({
        case Seq(obj) => GetField(field)(obj)
      }),
      new SimpleAttribute("put")({
        case Seq(obj, value) => PutField(field)(obj, value)
      }),
      new SimpleAttribute("null")({
        case Seq() => Null()
      }),
      new SimpleAttribute("newarr")({
        case Seq(len) => NewArray(sigI1D)(len)
      }),
      new SimpleAttribute("aic")({
        case Seq(arr, idx, len) => ArrayIndexCheck(sigI1D, trusted = false)(arr, idx, len)
      }),
      new SimpleAttribute("aget")({
        case Seq(arr, idx) => ArrayGet(sigI1D)(arr, idx)
      }),
    ) ++ super.parsableAttributes()
  }

  def check(toDie: String*)(toSurvive: String*)(additionalCheck: => Unit): Unit = {
    val toSurviveNodes = toSurvive map (n(_))
    val toDieNodes = toDie map (n(_))

    removeHandlerAnchors()
    eliminateUnreachableCode()
    explodeAllObjects()
    completeSSA()

    toDieNodes foreach (_ should not be (Symbol("committed")))
    // Account for referent after replacement (e.g. NewArrayMimic instead of NewArray)
    toSurviveNodes foreach (_ should (be (Symbol("committed")) or be (Symbol("referentCommitted"))))

    eliminateUnreachableCode()
    additionalCheck
  }

  test("field get default") {
    makeCFG(0@@("n=new()", "write()", "g=get(n)", "u=use(g)"))

    check("n", "g")() {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (IConst(0))
    }
  }

  test("field get after put") {
    makeCFG(0@@("n=new()", "p=put(n,ic(42))", "write()", "g=get(n)", "u=use(g)"))

    check("n", "p", "g")() {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (IConst(42))
    }
  }

  test("no gets") {
    makeCFG(0@@("n=new()", "p=put(n,ic(42))"))

    check("n", "p")() {}
  }

  test("simple phi V") {
    makeCFG(0@@("v1=ic(23)", "v2=ic(42)") -> (1@@("n1=new()", "p1=put(n1,v1)") || 2@@("n2=new()", "p2=put(n2,v2)")) -> 3@@("p=phi(n1,n2)", "u=use(get(p))"))

    check("n1", "n2", "p1", "p2")() {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (Phi(IntType)(b(3), n("v1"), n("v2")))
    }
  }

  test("put in phi") {
    makeCFG(0@@("n1=new()", "n2=new()") -> (1@@("p1=put(n1,ic(23))") || 2@@("p2=put(n2,ic(42))")) -> 3@@("p=phi(n1,n2)", "put(p,ic(37))", "write()", "g=get(p)", "u=use(g)"))

    check("n1", "n2", "p1", "p2", "g")() {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (IConst(37))
    }
  }

  test("put in phi arg") {
    makeCFG(0@@("n1=new()", "n2=new()") -> (1@@("p1=put(n1,ic(23))") || 2@@("p2=put(n2,ic(42))")) -> 3@@("p=phi(n1,n2)", "put(n1,ic(37))", "write()", "g=get(p)", "u=use(g)"))

    check()("n1", "n2", "p1", "p2") {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (n("g"))
    }
  }

  test("2 phi 1 arg") {
    makeCFG(0@@("n1=new()", "n2=new()") -> (1@@("p1=put(n1,ic(23))") || 2@@("p2=put(n2,ic(42))")) -> 3@@("ph1=phi(n1,n2)", "ph2=phi(n2,n1)", "g=get(ph1)", "u=use(g)"))

    check()("n1", "n2", "p1", "p2", "g") {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (n("g"))
    }
  }

  test("phi V in x-block") {
    makeCFG(0@@("v1=ic(23)", "v2=ic(42)", "n=new()") -> (1@@("n1=new()", "p1=put(n1,v1)", "xspinal()") || 2@@("n2=new()", "p2=put(n2,v2)", "xspinal()")) -> xb(3)@@("p=phi(n,n,n,n1,n,n2)", "u=use(get(p))"))

    check("n1", "n2", "p1", "p2")() {
      n("u").asInstanceOf[FakeSpinalUnary].inValue should be (Phi(IntType)(xb(3), n("v1"), n("v2")))
    }
  }

  test("phi array with array AIC") {
    makeCFG(0 -> (1@@("a1=newarr(ic(5))", "aic1=aic(a1,ic(0),ic(5))") || 2@@"a2=newarr(ic(5))") -> 3@@"p=phi(a1,a2)")

    check("a2")("a1", "aic1") {
    }
  }

  test("phi array with phi AIC") {
    makeCFG(0 -> (1@@"a1=newarr(ic(5))" || 2@@"a2=newarr(ic(5))") -> 3@@("p=phi(a1,a2)", "aic=aic(p,ic(0),ic(5))"))

    check()("a1", "a2", "aic") {
    }
  }

  test("phi array with phi array get") {
    makeCFG(0 -> (1@@"a1=newarr(ic(5))" || 2@@"a2=newarr(ic(5))") -> 3@@("p=phi(a1,a2)", "g=aget(p,ic(0))"))

    check("a1", "a2", "g")() {
    }
  }
}
