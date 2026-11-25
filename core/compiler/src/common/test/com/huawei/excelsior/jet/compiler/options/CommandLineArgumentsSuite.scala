/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeEnvironment

class CommandLineArgumentsSuite extends CompilerSuite {
  test("Bundled cjcbc options enable filename suppression and inlining control") {
    val args = CommandLineArguments.expand("+IgnoreSourceFileNameForCbc +NeverInline")
    args shouldBe Seq("+IgnoreSourceFileNameForCbc", "+NeverInline")

    val env = new FakeEnvironment
    for (arg <- args; (option, value) <- Configuration.parse(arg)) {
      env.define(option, value)
    }
    env.enabled(BoolOption.IgnoreSourceFileNameForCbc) shouldBe true
    env.enabled(BoolOption.NeverInline) shouldBe true
  }

  test("Bundled boolean options retain order and supported value syntax") {
    CommandLineArguments.expand("  +NeverInline\t-FastBackEnd:- -DebugIrLogs+  ") shouldBe
      Seq("+NeverInline", "-FastBackEnd:-", "-DebugIrLogs+")
  }

  test("Individual arguments, paths and equation values are not split") {
    val args = Seq(
      "+IgnoreSourceFileNameForCbc",
      "+IgnoreSourceFileNameForCbc ",
      "/some source directory/catch.chir",
      "-outputname=some output",
      "-Stats=some output +NeverInline",
      "-Stats=\"some output\"",
      "")
    for (arg <- args) {
      CommandLineArguments.expand(arg) shouldBe Seq(arg)
    }
  }
}
