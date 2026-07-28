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

class DivByConstSmallTestSuite extends CompilerSuite with DivByConstMagicNumberComputationToolbox {

  val Limit = 700

  test("Int magic number test") {
    for (d <- (-Limit to -2) ++ (2 to Limit);
         (m, s) = computeSignedMagicNumber(32, d);
         n <- -Limit to Limit) {
      withClue(s"(m, s) = ($m, $s); (n, d) = ($n, $d):") {
        val q = substDivInt(n, d, m.toInt, s)
        q should be (n / d)
        n - q * d should be (n % d)
      }
    }
  }

  test("Long magic number test") {
    for (d <- (-Limit to -2) ++ (2 to Limit);
         (m, s) = computeSignedMagicNumber(64, d);
         n <- -Limit to Limit) {
      withClue(s"(m, s) = ($m, $s); (n, d) = ($n, $d):") {
        val q = substDivLong(n, d, m, s)
        q should be (n / d)
        n - q * d should be (n % d)
      }
    }
  }

  test("UInt magic number test") {
    for (d <- 2 to Limit;
         (m, a, s) = computeUnsignedMagicNumber(32, UInt(d).toLong);
         n <- 0 to Limit) {
      withClue(s"(m, a, s) = ($m, $a, $s); (n, d) = ($n, $d):") {
        val q = substDivUInt(n, d, m.toInt, a, s)
        q should be (udiv(n, d))
        n - q * d should be (n % d)
      }
    }
  }

  test("ULong magic number test") {
    for (d <- 2 to Limit;
         (m, a, s) = computeUnsignedMagicNumber(64, d);
         n <- 0 to Limit) {
      withClue(s"(m, a, s) = ($m, $a, $s); (n, d) = ($n, $d):") {
        val q = substDivULong(n, d, m, a, s)
        q should be (udiv(n, d))
        n - q * d should be (n % d)
      }
    }
  }

}
