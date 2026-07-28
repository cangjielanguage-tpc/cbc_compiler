/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.opt.ir.CheckLevels
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.IRTransformationsCollection
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.{CompilerSuite, symlevel}

/**
 * Tests for CountedLoopsRecognizer
 *
 * @author ikireev
 * @author dbg
 * @author conwor
 */
class CountedLoopsRecognizerSuite extends CompilerSuite
  with GlobalNodesBuilder
  with CountedLoopsRecognizer
  with IRTransformationsCollection {

  import Condition._

  override def parsableAttributes() = {
    Seq(
      new SimpleAttribute("cmpne")({
        case Seq(l, r) => Cmp(l.tpe, Condition.NE)(l, r)
      }),

    ) ++ super.parsableAttributes()
  }

  private def areLoopsCounted: Seq[Boolean] = {
    requireNoGlobalCodeMotion()
    checkIRConsistency(CheckLevels.Important)
    splitCriticalEdges(withXHandlers = true)

    val loops = cfg.loops
    val counted = detectCountedLoops(loops).toSet
    loops.seq.sortBy(_.header.id) map counted
  }

  private def isLoopCounted: Boolean = areLoopsCounted match {
    case Seq(r) => r
    case rs => shouldNotReachHere(s"expected one loop, got ${rs.size}")
  }

  /**
   * 1 --> true  -> 2
   *   \
   *    -> false -> 3
   */

  {
    for (((start, cond, limit, step, counted), pos) <- Seq(
       tp(inv(),  LT,   any(),               1,  true)
      ,tp(inv(),  ULT,  any(),               3,  true)
      ,tp(inv(),  GT,   any(),               7,  true)
      ,tp(inv(),  UGT,  any(),               9,  true)

      ,tp(ic(0),  LE,   ic(9),               1,  true)
      ,tp(lc(0),  LE,   lc(9),               1,  true)

      ,tp(inv(),  LE,   ic(9),               1,  true)
      ,tp(ic(0),  LE,   inv(),               1,  false) // limit may be Int.MaxValue

      ,tp(ic(0),  LE,   ic(Int.MaxValue-2),  2,  true)
      ,tp(ic(0),  LE,   ic(Int.MaxValue-2),  6,  true)
      ,tp(ic(0),  LE,   ic(Int.MaxValue-1),  2,  false) // index = ..., MAX-3, MAX-1, MIN, MIN+2, ...
      ,tp(ic(0),  ULE,  ic(Int.MaxValue-1),  2,  true)  // index = ..., MAX-3, MAX-1, MAX+1, MAX+3, ...
      ,tp(ic(0),  LE,   ic(Int.MaxValue),    1,  false) // any value <= MAX
      ,tp(lc(0),  LE,   lc(Int.MaxValue),    1,  true)

      ,tp(ic(0),  GE,   ic(Int.MinValue+1),  2,  false) // TODO: should be true, not implemented yet
      ,tp(ic(1),  GE,   ic(Int.MinValue+1),  2,  false)
      ,tp(ic(0),  GE,   ic(Int.MinValue),    2,  false) // index = ..., MIN+2, MIN, MAX-1, ...

      ,tp(ic(0),  ULE,  ic(-3),              2,  true)
      ,tp(ic(0),  ULE,  ic(-1),              2,  false) // index = MAX-1, 0, ...
      ,tp(ic(8),  UGE,  ic(2),               2,  true)
      ,tp(ic(8),  UGE,  ic(0),               2,  false) // index = 4, 2, 0, MAX-1, ...

      ,tp(ic(0),  LT,   ic(1),               0,  false)

      ,tp(ic(0),  LT,   javaLen(),           2,  false) // index = 2^31-1, 0, ...
      ,tp(lc(0),  LT,   ajLen(),             2,  false)
      ,tp(lc(0),  LT,   cjLen(),             2,  false)
      ,tp(ic(0),  ULT,  javaLen(),           2,  true)  // index = 2^31-1, 2^31+1, ...
      ,tp(lc(0),  ULT,  ajLen(),             2,  true)
      ,tp(lc(0),  ULT,  cjLen(),             2,  true)
      ,tp(ic(-1), UGE,  javaLen(),          -2,  false) // index = 3, 1, 2^32-1, ...
      ,tp(lc(-1), UGE,  ajLen(),            -2,  false)
      ,tp(lc(-1), UGE,  cjLen(),            -2,  false)

      ,tp(ic(0),  NE,   javaLen(),           1,  false) // length is loop variant
      ,tp(lc(0),  NE,   ajLen(),             1,  false) // length is loop variant
      ,tp(lc(0),  NE,   cjLen(),             1,  false) // length is loop variant
      ,tp(ic(0),  NE,   inv(),               1,  true)

      ,tp(ic(0),  NE,   ic(9),               1,  true)
      ,tp(inv(),  NE,   inv(),               1,  true)
      ,tp(ic(0),  NE,   ic(9),               2,  false) // index is always even
      ,tp(ic(0),  NE,   ic(2),               6,  true)
      ,tp(inv(),  NE,   ic(9),               2,  false) // impossible without start value

      ,tp(ic(10), NE,   ic(0),              -2,  true)
      ,tp(ic(10), NE,   ic(1),              -1,  true)
      ,tp(ic(10), NE,   ic(1),              -2,  false) // index is always even

      ,tp(inv(),  EQ,   ic(5),               1,  true)
      ,tp(inv(),  EQ,   inv(),               1,  true)
      ,tp(ic(0),  EQ,   any(),               1,  false) // any could be incremented too
    )) {

      test(s"(i = $start; i $cond $limit; i += $step)") {
        makeCFG(cfg)

        addInductiveVariable(1, start(), cond, limit(), IntegralConst(start().tpe)(step))

        isLoopCounted shouldBe counted
      }
    }

    // Helpers:

    def cfg = 0 -> wd(1 -> 2) -> 3

    def ic(c: Int) = lazyValue(c.toString, IConst(c))
    def lc(c: Long) = lazyValue(c.toString + "L", LConst(c))
    def inv() = lazyValue("INV", addNode()) // some loop invariant (but not integral constant)
    def any() = lazyValue("ANY", addPinnedNode(1: Block)) // some loop variant
    def javaLen() = lazyValue("JAVA_LEN", JavaArrayLength(1: Block, addPinnedObjNode(1: Block))) // some Java array length
    def ajLen() = lazyValue("AJ_LEN", AJArrayLength(1: Block, addPinnedObjNode(1: Block))) // some AJ array length
    def cjLen() = lazyValue("CANGJIE_LEN", CangjieArrayLength(1: Block, addPinnedObjNode(1: Block))) // some Cangjie array length
  }

  test("loop with post-increment") {
    // for (int i = 0; i++ < 100; ) ...
    makeCFG(0 -> wd(1@@s"iv_LT(ic(0),ic(100),ic(1))" -> 2) -> 3)

    isLoopCounted shouldBe true
  }

  test("loop with pre-increment") {
    // for (int i = 0; ++i < 100; ) ...
    makeCFG(0 -> wd(1@@s"iv_LT_inc(ic(0),ic(100),ic(1))" -> 2) -> 3)

    isLoopCounted shouldBe true
  }

  test("long loop") {
    // for (long i = 0L; i < 100L; i++) ...
    makeCFG(0 -> wd(1@@s"iv_LT(lc(0),lc(100),lc(1))" -> 2) -> 3)

    // Loops with long index are not recognized (yet?).
    isLoopCounted shouldBe true
  }

  test("floating point loop") {
    // for (float i = 0; i < 100; i += 1) ...
    makeCFG(0 -> wd(1@@s"iv_LT(fc(0),fc(100),fc(1))" -> 2) -> 3)

    // Loops with floating point index are not recognized.
    isLoopCounted shouldBe false
  }

  test("loop with swapped branch exits") {
    // for (int i = 0; i < MAX_VALUE; i++) ...
    makeCFG(0 -> wd(1@@s"iv_LT(ic(0),ic(${Int.MaxValue}),ic(1))" -> 2) -> 3)

    If.invert((1: Block).blockEnd.asInstanceOf[If])

    isLoopCounted shouldBe true
  }

  test("loop with cmp with reversed params") {
    // for (int i = 0; param > i; i++) ...
    makeCFG(0@@"x" -> wd(1@@"iv_GT(ic(0),x,ic(1))" -> 2) -> 3)

    isLoopCounted shouldBe true
  }

  test("loop with cmp with reversed params and with swapped branch exits") {
    // for (int i = 0; param > i; i++) ...
    makeCFG(0@@"x" -> wd(1@@"iv_GT(ic(0),x,ic(1))" -> 2) -> 3)

    If.invert((1: Block).blockEnd.asInstanceOf[If])

    isLoopCounted shouldBe true
  }

  test("inf loop with bad exit") {
    makeCFG((0 -> wd(1 -> (2 || 3) -> 4) -> 5) |>| (3 -> 5))

    val phi = Phi.cyclic(IntType)(1, phi => Seq(IConst(0), Add(phi, IConst(1))))
    addCondition(1, phi)
    addCondition(3, phi, IConst(10), LT)

    isLoopCounted shouldBe false
  }

  test("loop with two forward inputs with equal start values") {
    // for (i = (0 or 0); i < 100; i++) ...
    makeCFG(0 -> (1 || 2) -> wd(3 -> 4) -> 5)

    val phi = Phi.cyclic(IntType)(3, phi => Seq(IConst(0), IConst(0), Add(phi, IConst(1))))
    addCondition(3, phi, IConst(100), LT)

    // Loop is actually counted.
    // However current implementation cannot handle this.
    isLoopCounted shouldBe false
  }

  test("loop with two forward inputs with different start values") {
    // for (i = (11 or 22); i < 100; i++) ...
    makeCFG(0 -> (1 || 2) -> wd(3 -> 4) -> 5)

    val phi = Phi.cyclic(IntType)(3, phi => Seq(IConst(11), IConst(22), Add(phi, IConst(1))))
    addCondition(3, phi, IConst(100), LT)

    // Loop is actually counted.
    // However current implementation cannot handle this.
    isLoopCounted shouldBe false
  }

  private def setupLoopWithRemovableAIC(removeAIC: Boolean): Unit = {
    // int[] arr = new int[n+1];
    // for (int i = 0; i <= n; i++) {
    //     arr[i] = 37;
    // }
    if (removeAIC) {
      makeCFG(0 -> wd(1 -> 2 -> 3) -> 5)
    } else {
      makeCFG(0 -> wd(1 -> 2 -> (3 || !4)) -> 5)
    }

    makeNodes { at =>
      at(0)
      val n = addNode()
      val arr = NewArray(sigInt1D)(
        Add(n, IConst(1))
      )

      at(1)
      val i = addInductiveVariable(IConst(0), LE, n, IConst(1))

      if (!removeAIC) {
        at(2)
        val aicBranch = 2.blockEnd.asInstanceOf[If]
        // Lowered ArrayIndexCheck(ctrl, mem, arr, i, len)
        val len = JavaArrayLength(arr)
        aicBranch.selector = Cmp(IntType, ULT)(i, len)
      }

      at(3)
      ArrayPut(sigInt1D)(arr, i, IConst(37))
    }
  }

  test("loop with non-removed aic") {
    setupLoopWithRemovableAIC(removeAIC = false)

    isLoopCounted shouldBe true
  }

  test("loop with removed aic") {
    setupLoopWithRemovableAIC(removeAIC = true)

    // Actually, it's counted.
    // We might calculate that (n <= Int.MaxValue - 1) if we have array of length (n+1).
    isLoopCounted shouldBe false
  }

  test("nested less-than loops") {
    // for (int i = 0; i < 100; i++)
    //   for (int j = 0; j < i; j++)
    //     ...
    makeCFG(0 -> wd(1@@"i=iv_LT(ic(0),ic(100),ic(1))" -> wd(2@@"iv_LT(ic(0),i,ic(1))" -> 3) -> 4) -> 5)

    areLoopsCounted shouldBe Seq(true, true)
  }

  test("nested less-or-equal loops") {
    // for (int i = 0; i <= 100; i++)
    //   for (int j = 0; j <= i; j++)
    //     ...
    makeCFG(0 -> wd(1@@"i=iv_LE(ic(0),ic(100),ic(1))" -> wd(2@@"iv_LE(ic(0),i,ic(1))" -> 3) -> 4) -> 5)

    areLoopsCounted shouldBe Seq(true, true)
  }

  test("inlined range iterator pattern") {
    // var hasNext = true
    // var i = 0
    // while (hasNext) {
    //   hasNext = i != 100
    //   i++
    //   ...
    // }
    makeCFG(0@@("t=true()", "x=ic(0)") -> wd(1@@("p=phi(t,c)", "i=phi(x,a)", "a=add(i,ic(1))", "c=cmpne(i,ic(100))", "if(p)") -> 2) -> 3)

    isLoopCounted shouldBe true
  }
}
