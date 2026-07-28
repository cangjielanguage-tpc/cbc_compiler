/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import xscala.util.StringOps.*

import scala.util.Try

/** Tests for [[xscala.util.StringOps]].
  *
  * TODO: extract to `xscala-vm-dependent-share/test`
  */
class StringOpsSuite extends AnyFunSuite with Matchers {

  test("ToHexOption (manual)") {
    val data = Seq(
      ("FFFF", Some(0xFFFF)),
      ("+FFFF", Some(0xFFFF)),
      ("-FFFF", Some(-0xFFFF)),
      ("G", None),
      ("-", None),
      ("+", None),
      ("F", Some(0xF)),
      ("80000000", None),
      ("80000001", None),
      ("80000002", None),
      ("F8000000", None),
      ("F8000001", None),
      ("F7FFFFFF", None),
      ("FFFFFFF", Some(0xFFFFFFF)),
      ("FFFFFFFF", None))

    for ((str, hexOpt) <- data) {
      str.toHexOption shouldEqual hexOpt
    }
  }

  test("ToHexOption (randomized)") {
    val N = 1_000_000

    val random = new java.util.Random(System.currentTimeMillis())
    for (str <- Iterator.fill(N)(random.nextInt().toHexString)) {
      str.toHexOption shouldEqual Try(Integer.parseInt(str, 16)).toOption
    }
  }

  test("ToUnsignedHexOption (randomized)") {
    val N = 1_000_000

    val random = new java.util.Random(System.currentTimeMillis())
    for (x <- Iterator.fill(N)(java.lang.Long.toHexString(random.nextLong()))) {
      x.toUnsignedHexOption shouldEqual Try(java.lang.Long.parseUnsignedLong(x, 16)).toOption
    }
  }

}
