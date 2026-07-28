/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.STATIC
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

class EscapeWriteBarriersOptimizationSuite extends CompilerSuite with GlobalNodesBuilder with EscapeWriteBarriersOptimization {

  private val klass = symA
  private val field = makeSymField("f", klass, klass)
  private val sfield = makeSymField("sf", klass, klass).setJavaModifiers(Modifiers(STATIC))

  override def parsableAttributes(): Seq[Attribute] = Seq(
    new SimpleAttribute("new"       )({ case Seq() => New(sig(klass))() }),
    new SimpleAttribute("wb"        )({ case Seq(r, v) => EscapeWriteBarrier.Instance(r, v) }),
    new SimpleAttribute("get"       )({ case Seq(obj) => GetField(field)(obj) }),
    new SimpleAttribute("sf"        )({ case Seq() => GetStatic(sfield) }),
    new SimpleAttribute("ev"        )({ case Seq(obj) => EscapeWriteBarrier.Static(obj) }),
  ) ++ super.parsableAttributes()

  private def check(expected: Boolean) = {
    optimizeWriteBarriers() shouldBe expected
  }

  test("don't lift into cyclic phies") {
    makeCFG(1 -> wd((2 -> 3 -> 4) || (2 -> 4)) -> 5 |>|
      1@@("x=new()","y=get(x)") |>|
      2@@("fa=phi(y,fb)") |>|
      4@@("fb=phi(fa,x)","wb(x,fb)"))

    check(false)
  }

  test("receiver dominates phi args - 1") {
    makeCFG(1@@("x=new()","y=new()") -> (2@@("z=get(x)") || 3) -> 4@@("wb(x,phi(z,y))"))
    check(true)
  }

  test("receiver dominates phi args - 2") {
    makeCFG(1@@("y=new()","x=sf()") -> (2@@("z=get(x)") || 3) -> 4@@("wb(x,phi(z,y))"))
    check(true)
  }

  test("receiver doesn't dominate phi args") {
    makeCFG(1@@("y=new()","x=new()") -> (2@@("z=get(x)") || 3) -> 4@@("wb(x,phi(z,y))"))
    check(false)
  }

  test("escape point between phi args and barrier") {
    makeCFG(1@@("x=new()","y=new()") -> (2@@("z=get(x)","ev(x)") || 3) -> 4@@("wb(x,phi(z,y))"))
    check(false)
  }

  test("don't lift without a good reason") {
    makeCFG(1@@("x=new()") -> (2@@("y=new()") || 3@@("z=new()") || 4@@("u=get(x)")) -> 5@@("wb(x,phi(y,z,u))"))
    check(false)
  }

  test("don't lift into loops without a good reason") {
    makeCFG(1@@("x=new()") -> (wd(2@@("y=new()") -> 3) || wd(4@@("z=get(x)") -> 5)) -> 6@@("wb(x,phi(y,z))"))
    check(false)
  }

  test("lift into good loop") {
    makeCFG(1@@("x=new()") -> (2@@("y=new()") || wd(3@@("z=get(x)") -> 4)) -> 5@@("wb(x,phi(y,z))"))
    check(true)
  }

  test("lift into block with xhandler") {
    makeCFG(1@@("x=new()") -> ((2@@("y=new()","new()") -> 3@@("z=get(x)")) || (2 -> xb(4)@@("p=phi(x,x,y)"))) -> 5@@("wb(x,phi(z,p))"))
    check(true)
    assert(all[EscapeWriteBarrier.Instance].forall(_.block.singleXHandlerOrNull == null))
  }

}
