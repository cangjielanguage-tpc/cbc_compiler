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

class JetVersionSuite extends AnyFunSuite with Matchers {

  test("good parsing") {
    val data = Seq(
      ("7.2", 720),
      ("8.88", 888),
      ("10.0", 1000),
      ("10.5", 1050),
      ("10.75", 1075),
      ("11.03", 1103))

    for ((printableVersion, versionCode) <- data) {
      val v1 = JetVersion.fromPrintableVersion(printableVersion)
      val v2 = JetVersion.fromVersionCode(versionCode)

      for (v <- Seq(v1, v2)) {
        v.printableVersion shouldEqual printableVersion
        v.versionCode shouldEqual versionCode
      }
    }
  }

  test("bad parsing of printable version") {
    val data = Seq(
      "10",
      "3.141",
      "10.",
      "3.14.1",
      "10.A",
      "-5.0")

    for (badVersion <- data) {
      try {
        JetVersion.fromPrintableVersion(badVersion)
        fail(s"successfully parsed bad printable version: <$badVersion>")
      } catch {
        case _: IllegalArgumentException =>
        // as expected
      }
    }
  }

  test("bad parsing of version code") {
    val data = Seq(50, -5, 3)

    for (badVersion <- data) {
      try {
        JetVersion.fromVersionCode(badVersion)
        fail(s"successfully parsed bad version code: <$badVersion>")
      } catch {
        case _: IllegalArgumentException =>
        // as expected
      }
    }
  }
}
