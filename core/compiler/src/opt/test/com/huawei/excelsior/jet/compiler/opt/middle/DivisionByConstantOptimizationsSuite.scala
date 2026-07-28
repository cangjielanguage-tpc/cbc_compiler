/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.ir.ValueNumbering
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.ArithNodesDSL

class DivisionByConstantOptimizationsSuite extends CompilerSuite with DivisionByConstantOptimizations with ArithNodesDSL {

  def opt(x: Node): Node = {
    val d@IDivRemByConstOp(c) = x
    getOptimizedDivRemByConst(d, c)
  }

  test("idiv by power of two optimization") {
    def odiv(n: Node, p: Int, w: Int, positive: Boolean): Node = {
      val x = add(n, lsr(asr(n, ic(p-1)), ic(w-p)))
      val y = asr(x, ic(p))
      if (positive) y else neg(y)
    }

    for (w <- Seq(32, 64);
         p <- 1 until w;
         positive <- if (p < w - 1) Seq(true, false) else Seq(false)
    )
    {
      val n = if (w == 32) iarg else larg
      val t = if (w == 32) ic(1 << p) else lc(1L << p)
      val d = if (positive) t else neg(t)

      opt(div(n, d)) should be (odiv(n, p, w, positive))
    }
  }

  test("idiv by const optimization") {
    def negOpt(q: Node, w: Int): Node = add(q, lsr(q, ic(w - 1)))

    for ((v1, v2, res) <- Seq(
      (iarg, ic(3), add(mulh(ic(0x55555556), iarg), lsr(iarg, ic(31)))),
      (iarg, ic(7), add(asr(add(mulh(ic(0x92492493), iarg), iarg), ic(2)), lsr(iarg, ic(31)))),
      (larg, lc(3), add(mulh(lc(0x5555555555555556L), larg), lsr(larg, ic(63)))),
      (larg, lc(7), add(asr(mulh(lc(0x4924924924924925L), larg), ic(1)), lsr(larg, ic(63)))),
      (iarg, ic(-3), negOpt(asr(sub(mulh(ic(0x55555555), iarg), iarg), ic(1)), 32)),
      (iarg, ic(-7), negOpt(asr(sub(mulh(ic(0x6DB6DB6D), iarg), iarg), ic(2)), 32)),
      (larg, lc(-3), negOpt(asr(sub(mulh(lc(0x5555555555555555L), larg), larg), ic(1)), 64)),
      (larg, lc(-7), negOpt(asr(mulh(lc(0xB6DB6DB6DB6DB6DBL), larg), ic(1)), 64))
    ))
    {
      opt(div(v1, v2)) should be (res)
    }
  }

  test("irem by power of two optimization") {
    def orem(n: Node, p: Int, w: Int): Node = {
      val x = add(n, lsr(asr(n, ic(p-1)), ic(w-p)))
      val y = and(x, if (w == 32) ic(-1 << p) else lc(-1L << p))
      sub(n, y)
    }

    for (w <- Seq(32, 64);
         p <- 1 until w;
         positive <- if (p < w - 1) Seq(true, false) else Seq(false)
    )
    {
      val n = if (w == 32) iarg else larg
      val t = if (w == 32) ic(1 << p) else lc(1L << p)
      val d = if (positive) t else neg(t)
      opt(rem(n, d)) should be (orem(n, p, w))
    }
  }

  test("irem by const optimization") {
    for ((v1, v2, res) <- Seq(
      (iarg, ic(3), sub(iarg, mul(ic(3), opt(div(iarg, ic(3)))))),
      (iarg, ic(7), sub(iarg, mul(ic(7), opt(div(iarg, ic(7)))))),
      (larg, lc(3), sub(larg, mul(lc(3), opt(div(larg, lc(3)))))),
      (larg, lc(7), sub(larg, mul(lc(7), opt(div(larg, lc(7))))))
    ))
    {
      opt(rem(v1, v2)) should be (res)
    }
  }

  test("udiv by power of two optimization") {
    def odiv(n: Node, p: Int): Node = {
      lsr(n, ic(p))
    }

    for (w <- Seq(32, 64); p <- 1 until w) {
      val n = if (w == 32) iarg else larg
      val d = if (w == 32) ic(1 << p) else lc(1L << p)
      opt(udiv(n, d)) should be (odiv(n, p))
    }
  }

  test("udiv by const optimization") {
    for ((v1, v2, res) <- Seq(
      (iarg, ic(3), lsr(umulh(ic(0xAAAAAAAB), iarg), ic(1))),
      (iarg, ic(7), lsr(add(lsr(sub(iarg, umulh(ic(0x24924925), iarg)), ic(1)),
        umulh(ic(0x24924925), iarg)), ic(2))),
      (larg, lc(3), lsr(umulh(lc(0xAAAAAAAAAAAAAAABL), larg), ic(1))),
      (larg, lc(7), lsr(add(lsr(sub(larg, umulh(lc(0x2492492492492493L), larg)), ic(1)),
        umulh(lc(0x2492492492492493L), larg)), ic(2)))
    ))
    {
      opt(udiv(v1, v2)) should be (res)
    }
  }

  test("urem by power of two optimization") {
    def orem(n: Node, p: Int, w: Int): Node = {
      val x = if (w == 32) ic((1 << p) - 1) else lc((1L << p) - 1)
      and(n, x)
    }

    for (w <- Seq(32, 64); p <- 1 until w) {
      val n = if (w == 32) iarg else larg
      val d = if (w == 32) ic(1 << p) else lc(1L << p)
      opt(urem(n, d)) should be (orem(n, p, w))
    }
  }

  test("urem by const optimization") {
    for ((v1, v2, res) <- Seq(
      (iarg, ic(3), sub(iarg, mul(ic(3), opt(udiv(iarg, ic(3)))))),
      (iarg, ic(7), sub(iarg, mul(ic(7), opt(udiv(iarg, ic(7)))))),
      (larg, lc(3), sub(larg, mul(lc(3), opt(udiv(larg, lc(3)))))),
      (larg, lc(7), sub(larg, mul(lc(7), opt(udiv(larg, lc(7))))))
    ))
    {
      opt(urem(v1, v2)) should be (res)
    }
  }
}
