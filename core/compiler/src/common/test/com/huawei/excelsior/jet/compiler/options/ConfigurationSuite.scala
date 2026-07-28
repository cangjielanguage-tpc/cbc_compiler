/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.jet.common.XString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** Tests for [[Configuration]].
  *
  * Look at description of options syntax in [[Configuration]]:
  *
  * @author ikireev
  */
class ConfigurationSuite extends AnyFunSuite with Matchers {

  test("EmptyOptions1") {
    assert(Configuration.parse("").isEmpty)
  }

  test("EmptyOptions2") {
    assert(Configuration.parse("         ").isEmpty)
  }

  private def makeOptionsSet(optionValuePairs: Any*) = {
    val set = mutable.HashSet.empty[(Option[?], Any)]
    for (Seq(u, v) <- optionValuePairs.grouped(2)) {
      val option = u.asInstanceOf[Option[?]]
      val value = option.parse(v.toString)
      set.addOne((option, value))
    }
    set
  }

  private def assertOptionsEquals(config: collection.Map[Option[?], Any], optionValuePairs: Any*): Unit = {
    val optionsSet = makeOptionsSet(optionValuePairs *)
    optionsSet.size shouldEqual config.size
    assert(optionsSet subsetOf config.toSet)
  }

  test("SetBoolOption") {
    val config = Configuration.parse("-FastBackEnd +NeverInline -DebugIrLogs-")

    assertOptionsEquals(config,
      BoolOption.FastBackEnd, false,
      BoolOption.NeverInline, true,
      BoolOption.DebugIrLogs, false)
  }

  test("SetBoolOption2") {
    val config = Configuration.parse("-fAsTbAcKeNd")

    assertOptionsEquals(config, BoolOption.FastBackEnd, false)
  }

  test("DeclareBoolOption") {
    val config = Configuration.parse(" -FastBackEnd:- -NeverInline:+ ")

    assertOptionsEquals(config,
      BoolOption.FastBackEnd, false,
      BoolOption.NeverInline, true)
  }

  test("SetStrOption") {
    val config = Configuration.parse("-Stats=stats   -JCAdvise=jcadvise   ")

    assertOptionsEquals(config,
      StrOption.Stats, "stats",
      StrOption.JCAdvise, "jcadvise")
  }

  test("SetStrOptionQuoted") {
    val config = Configuration.parse("-Stats=\"stats\" -JCAdvise=\" jc  advise  \"  ")

    assertOptionsEquals(config,
      StrOption.Stats, "stats",
      StrOption.JCAdvise, " jc  advise  ")
  }

  test("SetStrOptionQuotedHard") {
    val config = Configuration.parse("-Stats=\"here is string: \'some string\'\" -JCAdvise=\'\" \'  -InlineStat=\"\'\"  -ClinitAnalysisStat=\"-options=\'someOptions\' +Option1 -Option2+\" ")

    assertOptionsEquals(config,
      StrOption.Stats, "here is string: \'some string\'",
      StrOption.JCAdvise, "\" ",
      StrOption.InlineStat, "\'",
      StrOption.ClinitAnalysisStat, "-options=\'someOptions\' +Option1 -Option2+")
  }

  test("SetStrOptionEmpty") {
    val config = Configuration.parse("-Stats=\"\"  -InlineStat=   -JCAdvise:=\'\'   -ClinitAnalysisStat:=  ")

    assertOptionsEquals(config,
      StrOption.Stats, "",
      StrOption.InlineStat, "",
      StrOption.JCAdvise, "",
      StrOption.ClinitAnalysisStat, "")
  }

  test("DeclareStrOption") {
    val config = Configuration.parse("-Stats:=stats   -JCAdvise:=jcadvise   ")

    assertOptionsEquals(config,
      StrOption.Stats, "stats",
      StrOption.JCAdvise, "jcadvise")
  }

  test("AnyOptions1") {
    val config = Configuration.parse("-FastBackEnd +NeverInline -DebugIrLogs:-  -Stats=\"stats\" -JCAdvise=\" jc  advise  \"  ")

    assertOptionsEquals(config,
      BoolOption.FastBackEnd, false,
      BoolOption.NeverInline, true,
      BoolOption.DebugIrLogs, false,
      StrOption.Stats, "stats",
      StrOption.JCAdvise, " jc  advise  ")
  }

  test("AnyOptions2") {
    val config = Configuration.parse("+FastBackEnd -DebugIRLogs:- -Stats=\"stats\" -InlineNewTinySize=42")

    assertOptionsEquals(config,
      BoolOption.FastBackEnd, true,
      BoolOption.DebugIrLogs, false,
      NumOption.InlineNewTinySize, 42,
      StrOption.Stats, "stats")
  }

  test("UnknownOptions") {
    val config = Configuration.parse("-FastBackEnd11 +__NeverInline -DebugIrLogssss:- -DebugIrLogs_WithCodeGen:+  -StatsStats=\"stats\" -JCAdviseeeee=\" detailed  stats  \"  ")
    assert(config.isEmpty)
  }

  private def testSyntaxError(s: String): Unit = {
    try {
      val config = Configuration.parse(s)
      assert(false)
    } catch {
      case e: InternalError =>
        assert(e.getMessage.startsWith("Compiler options parser error"))
    }
  }

  test("SyntaxErrors") {
    // "Compiler options parser error: unexpected symbol before directive \"" + (char) c + "\""
    testSyntaxError(" FastBackEnd")

    // "Compiler options parser error: illegal option name"
    testSyntaxError("-   ")
    testSyntaxError("  + ")
    testSyntaxError("-[[")
    testSyntaxError("+123")

    // "Compiler options parser error: unexpected symbol after short option name \"+" + name + "<?>\""
    testSyntaxError("+FastBackEnd+")
    testSyntaxError("+FastBackEnd- ")

    // "Compiler options parser error: unexpected symbol after option name \"-" + name + "<?>\""
    testSyntaxError("-FastBackEnd() ")
    testSyntaxError("-Stats| ")

    // "Compiler options parser error: unexpected end of line after declaration mark \"-" + name + ":\""
    testSyntaxError("-FastBackEnd:")

    // "Compiler options parser error: unexpected symbol after declaration mark \"-" + name + ":<?>\""
    testSyntaxError("-FastBackEnd: ")
    testSyntaxError("-FastBackEnd:? ")
    testSyntaxError("-FastBackEnd:azaza ")
  }
}
