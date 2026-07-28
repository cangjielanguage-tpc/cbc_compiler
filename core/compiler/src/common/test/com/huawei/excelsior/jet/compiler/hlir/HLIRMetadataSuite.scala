/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import org.scalatest.matchers.dsl.MatcherWords.be
import xscala.matching.Regex

import HLIRMetadata.{LambdaCommonRegex, LambdaRegex, AutoEnvRegex, AutoEnvRegexLinkageName}


class HLIRMetadataSuite extends CompilerSuite {
  def assertMatch(regex: Regex, nameStr: String, result: Boolean): Unit = {
    regex.pattern.matcher(nameStr).matches should be (result)
  }
  
  test("Name parsing for inner functions Auto_Env") {
    val nameStr = "_ZN0118$Auto_Env__ZN17std.unittest.diff13PrettyPrinter17coloredWithBufferIc15appendWithColorS11a0E0a0b0b_fEvstd.unittest.diffE"

    assertMatch(AutoEnvRegex, nameStr, true)
  }

  test("Name parsing for lambda's Auto_Env") {
    val nameStr = "_ZN065$Auto_Env_std.unittest.prop_test$lambda.595std.unittest.prop_testE"

    assertMatch(AutoEnvRegex, nameStr, true)
  }

  test("Name parsing for LambdaCommon") {
    val nameStr = "_ZN089$LambdaCommon_T2_C_ZN17std.unittest.mock9StubChainEC_ZN17std.unittest.mock11MatchStatusEEE"

    assertMatch(LambdaCommonRegex, nameStr, true)
  }

  test("Name parsing for lambda function") {
    val nameStr = "_ZN0158$Lambda__ZN12std.unittest11$entry_main21ExecutableTestProject7executeERrecord._ZN12std.unittest11ParallelCtxERrecord._ZN12std.unittest9ReportCtxElambda-118-21E"

    assertMatch(LambdaRegex, nameStr, true)
  }
}
