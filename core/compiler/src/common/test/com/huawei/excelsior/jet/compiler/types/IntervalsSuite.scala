/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.types.Approximation.CC
import com.huawei.excelsior.jet.compiler.types.Intervals.*
import com.huawei.excelsior.jet.compiler.types.Intervals.IntervalApprox.{from, to}

class IntervalsSuite extends CompilerSuite {
  def int(point: Int): IntervalApprox = IntervalApprox(point)
  def int(from: Int, to: Int): IntervalApprox = IntervalApprox(from, to)

  for (((x, y, unionRes, intersectRes), pos) <- Seq(
    tp(int(-1, 1), int(1, 5),  int(-1, 5),  int(1)),
    tp(int(1, 5),  int(-1, 1), int(-1, 5),  int(1, 1)),
    tp(int(-1, 1), int(2, 5),  int(-1, 5),  Empty),
    tp(int(2, 5),  int(-1, 1), int(-1, 5),  Empty),
    tp(int(-1, 1), int(3, 5),  int(-1, 5),  Empty),
    tp(int(3, 5),  int(-1, 1), int(-1, 5),  Empty),
    tp(int(-1, 1), Infinity,   Infinity,    int(-1, 1)),
    tp(Infinity,   int(-1, 1), Infinity,    int(-1, 1)),
    tp(int(-1, 1), Empty,      int(-1, 1),  Empty),
    tp(Empty,      int(-1, 1), int(-1, 1),  Empty),
    tp(Infinity,   Empty,      Infinity,    Empty),
    tp(Empty,      Infinity,   Infinity,    Empty),
    tp(Infinity,   Infinity,   Infinity,    Infinity),
    tp(Empty,      Empty,      Empty,       Empty),
    tp(int(0),     int(10),    int(0, 10),  Empty),
    tp(int(10),    int(0),     int(0, 10),  Empty),
    tp(from(0),    to(0),      Infinity,    int(0)),
    tp(to(0),      from(0),    Infinity,    int(0)),
    tp(int(0),     int(0),     int(0),      int(0)),
    tp(int(10),    int(10),    int(10),     int(10)),
    tp(int(1, 2),  int(1, 2),  int(1, 2),   int(1, 2)))
  ) {
    test(s"signed ranges union $x $y") {
      (x union y) should be (unionRes)
    }

    test(s"signed ranges intersect $x $y") {
      x intersect y should be (intersectRes)
    }
  }

  for (((x, y), pos) <- Seq(
    tp((MinusInf,              Finite(Long.MinValue))),
    tp((Finite(Long.MinValue), Finite(-2))),
    tp((Finite(-2),            Finite(-1))),
    tp((Finite(-1),            Finite(0))),
    tp((Finite(0),             Finite(1))),
    tp((Finite(1),             Finite(2))),
    tp((Finite(2),             Finite(Long.MaxValue))),
    tp((Finite(Long.MaxValue), PlusInf))
  )) {
    test(s"signed numbers order $x $y") {
      x should be <= y
    }
  }

  for (((x, y), pos) <- Seq(
    tp(Empty,           int(-100, -100)),
    tp(int(-100, -100), int(-150, -100)),
    tp(int(-150, -100), int(-200, -100)),
    tp(int(-200, -100), int(-200, -50)),
    tp(int(-200, -50),  from(-200)),
    tp(from(-200),      Infinity)
  )) {
    test(s"signed ranges lattice linear $x $y") {
      (x <= y) should be (true) 
    }
  }
  
  for (((x, y, expected), pos) <- Seq(
    tp(Empty,     int(0),    CC.Less),
    tp(Empty,     int(1),    CC.Less),
    tp(int(1),    int(0),    CC.Incomparable),
    tp(int(0),    int(1),    CC.Incomparable),
    tp(int(0, 1), int(0),    CC.Greater),
    tp(int(0),    int(0, 1), CC.Less),
    tp(int(1),    int(0, 1), CC.Less),
    tp(int(0, 1), int(1),    CC.Greater),
    tp(int(0, 1), Infinity,  CC.Less)
  )) { 
    test(s"signed ranges compare $x $y $expected") {
      x.compare(y) should be (expected)
    }
  }

  for ((x, pos) <- Seq(
    tp(int(0)),
    tp(int(1)),
    tp(int(10, 13)),
    tp(int(-10, 25)),
    tp(Infinity),
    tp(from(0)),
    tp(to(0)),
  )) { 
    test(s"signed ranges not empty $x") {
      x should not be empty
    }
  }
}
