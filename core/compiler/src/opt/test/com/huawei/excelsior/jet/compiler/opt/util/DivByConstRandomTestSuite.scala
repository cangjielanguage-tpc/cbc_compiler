/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.util

import com.huawei.excelsior.jet.compiler.CompilerSuite
import xscala.util.UInt

import scala.util.Random

class DivByConstRandomTestSuite extends CompilerSuite with DivByConstMagicNumberComputationToolbox {

  val LimitD = 1000
  val LimitN = 1000

  test("Int magic number test") {
    val random = new Random(System.currentTimeMillis())
    for (_ <- 1 to LimitD;
         d = random.nextInt() if d.abs != 1 && d != 0;
         (m, s) = computeSignedMagicNumber(32, d);
         _ <- 1 to LimitN;
         n = random.nextInt()) {
      withClue(s"(m, s) = ($m, $s); (n, d) = ($n, $d):") {
        val q = substDivInt(n, d, m.toInt, s)
        q should be (n / d)
        n - q * d should be (n % d)
      }
    }
  }

  test("Long magic number test") {
    val random = new Random(System.currentTimeMillis())
    for (_ <- 1 to LimitD;
         d = random.nextLong() if d.abs != 1 && d != 0;
         (m, s) = computeSignedMagicNumber(64, d);
         _ <- 1 to LimitN;
         n = random.nextLong()) {
      withClue(s"(m, s) = ($m, $s); (n, d) = ($n, $d):") {
        val q = substDivLong(n, d, m, s)
        q should be (n / d)
        n - q * d should be (n % d)
      }
    }
  }

  test("UInt magic number test") {
    val random = new Random(System.currentTimeMillis())
    for (_ <- 1 to LimitD;
         d = random.nextInt() if d != 1 && d != 0;
         (m, a, s) = computeUnsignedMagicNumber(32, UInt(d).toLong);
         _ <- 1 to LimitN;
         n = random.nextInt()) {
      withClue(s"(m, a, s) = ($m, $a, $s); (n, d) = ($n, $d):") {
        val q = substDivUInt(n, d, m.toInt, a, s)
        q should be (udiv(n, d))
        n - q * d should be (urem(n, d))
      }
    }
  }

  test("ULong magic number test") {
    val random = new Random(System.currentTimeMillis())
    for (_ <- 1 to LimitD;
         d = random.nextLong() if d != 1 && d != 0;
         (m, a, s) = computeUnsignedMagicNumber(64, d);
         _ <- 1 to LimitN;
         n = random.nextLong()) {
      withClue(s"(m, a, s) = ($m, $a, $s); (n, d) = ($n, $d):") {
        val q = substDivULong(n, d, m, a, s)
        q should be (udiv(n, d))
        n - q * d should be (urem(n, d))
      }
    }
  }

}
