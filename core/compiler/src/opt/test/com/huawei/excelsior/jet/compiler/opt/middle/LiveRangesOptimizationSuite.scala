/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind as TKind
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeType}
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}

class LiveRangesOptimizationSuite extends CompilerSuite with GlobalNodesBuilder with LiveRangesOptimization {

  private val foo = makeSymClass("foo", symObj)
  private val field = makeSymField("f", symInt, foo)
  private val sgFooArr = sig(makeSymArray("foo[]", foo, 1))

  private val extraAttrs = Seq(
    new SimpleAttribute("putField")({ case Seq(obj, value) => PutField(field)(obj, value) }),
    new SimpleAttribute("controlled")({ case Seq() => FakeControlled(TRefType)() }),
    new SimpleAttribute("floatingUse")({ case Seq(arg) => FakeUnary(TRefType)(arg) }),
    new SimpleAttribute("controlledUse")({ case Seq(arg) => FakeControlledUnary(TRefType)(arg) }),
    new SimpleAttribute("arrayPut")({ case Seq(arr, index, value) => ArrayPut(sgFooArr)(arr, index, value) }),
  )
  override def parsableAttributes() = extraAttrs ++ super.parsableAttributes()

  private def make(attrs: String*): Unit = makeCFG(0@@@(attrs))

  private def spineShouldBe(spine: String*): Unit = b(0).spineForward.toSeq shouldBe spine.map(n(_))

  test("motivation") {
    make(
      "l0=new(foo)", "pf0=putField(l0,ic(42))",
      "l1=new(foo)", "pf1=putField(l1,ic(42))",
      "l2=new(foo)", "pf2=putField(l2,ic(42))",
      "l3=new(foo)", "pf3=putField(l3,ic(42))",

      "arr=newarr(foo[],ic(4))",

      "ap0=arrayPut(arr,ic(0),l0)",
      "ap1=arrayPut(arr,ic(1),l1)",
      "ap2=arrayPut(arr,ic(2),l2)",
      "ap3=arrayPut(arr,ic(3),l3)",
    )

    spineShouldBe("l0",               "pf0", "l1",        "pf1", "l2",        "pf2", "l3",        "pf3", "arr", "ap0", "ap1", "ap2", "ap3")
    optimizeLiveRanges()
    spineShouldBe("l0", "arr", "ap0", "pf0", "l1", "ap1", "pf1", "l2", "ap2", "pf2", "l3", "ap3", "pf3")
  }

  test("memory anti-dependency") {
    make(
      "arr=newarr(foo[],ic(2))",
      "l0=new(foo)",
      "l1=new(foo)",
        "read()",
      "ap0=arrayPut(arr,ic(0),l0)",
      "ap1=arrayPut(arr,ic(1),l1)",
    )

    spineShouldBe("arr", "l0", "l1", "ap0", "ap1")
    optimizeLiveRanges()
    spineShouldBe("arr", "l0", "l1", "ap0", "ap1")
  }

  test("controlled") {
    make(
      "x=new(foo)",
      "arr=newarr(foo[],ic(1))",
      "y=controlled()",
      "ap0=arrayPut(arr,ic(0),y)"
    )

    spineShouldBe("x", "arr", "ap0")
    optimizeLiveRanges()
    spineShouldBe("x", "arr", "ap0")
  }

  test("floatingUse & controlledUse") {
    make(
      "x=new(foo)",
      "arr=newarr(foo[],ic(1))",
      "y=floatingUse(arr)",
      "z=controlledUse(y)",
      "ap0=arrayPut(arr,ic(0),z)"
    )

    spineShouldBe("x", "arr", "ap0")
    optimizeLiveRanges()
    spineShouldBe("x", "arr", "ap0")
  }

  test("control and memory update") {
    make(
      "arr=newarr(foo[],ic(2))",
      "l0=new(foo)",
      "l1=new(foo)",
      "ap0=arrayPut(arr,ic(0),l0)",
        "gf=read()"
    )

    spineShouldBe("arr", "l0",        "l1", "ap0")
    optimizeLiveRanges()
    spineShouldBe("arr", "l0", "ap0", "l1")

    val gf = n("gf").asInstanceOf[HasInControl with HasInMemory]
    gf.inCtrl shouldBe n("l1")
    gf.inMemory shouldBe n("l1")
  }
}
