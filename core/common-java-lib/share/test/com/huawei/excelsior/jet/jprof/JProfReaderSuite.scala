/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import com.huawei.excelsior.jet.jprof.JProfFormat.*
import org.scalatest.funsuite.AnyFunSuite
import xscala.io.TextInput

/** @author ijorch
  * @author xappymah
  */
class JProfReaderSuite extends AnyFunSuite {

  test("simple test") {
    val parser = setUp()
    val data = parser.parse()

    assertResult(JProfVersion.current)(parser.getJProfVersion)
    assertResult(0)(data.sections.size)
  }

  test("real life test") {
    val parser = setUp(
      s"$SECTION_START ${SectionType.BLAME_PROF.sectionType}",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
      "  Handwritten-like profile file ",
      "  with spaces,",
      "  empty lines,",
      "  and comments.",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%",
      "",
      s"$ENTRY_START${EntryType.BLAME_STATS.entryType}",
      s"$COMMENT_LINE${ObjType.BLAME_HITS.objType} some data",
      s"$OBJ_INDENT${ObjType.BLAME_SAMPLES.objType} test1",
      ENTRY_END,
      "",
      s"$ENTRY_START${EntryType.BLAME_METHOD_HITS.entryType}",
      s"$OBJ_INDENT${ObjType.BLAME_METHOD.objType} test2",
      s"$OBJ_INDENT$OBJ_INDENT${ObjType.BLAME_STATE.objType} test3",
      s"   ${ObjType.BLAME_STATE.objType} test4",
      ENTRY_END,
      s"$ENTRY_START${EntryType.BLAME_METHOD_HITS.entryType}",
      ENTRY_END,
      SECTION_END)

    val jprofData = parser.parse()

    val secs = jprofData.sections
    assertResult(1)(secs.size)
    assertResult(SectionType.BLAME_PROF)(secs(0).tpe)

    val entries = secs(0).entries
    assertResult(3)(entries.size)
    assertResult(EntryType.BLAME_STATS)(entries(0).tpe)
    assertResult(EntryType.BLAME_METHOD_HITS)(entries(1).tpe)
    assertResult(EntryType.BLAME_METHOD_HITS)(entries(2).tpe)

    var objs = entries(0).objs
    assertResult(1)(objs.size)
    checkObj(objs(0), ObjType.BLAME_SAMPLES, "test1")

    objs = entries(1).objs
    assertResult(3)(objs.size)
    checkObj(objs(0), ObjType.BLAME_METHOD, "test2")
    checkObj(objs(1), ObjType.BLAME_STATE, "test3")
    checkObj(objs(2), ObjType.BLAME_STATE, "test4")

    objs = entries(2).objs
    assertResult(0)(objs.size)
  }

  private def setUp(input: String*) = {
    val header = Seq(HEADER, s"$VERSION_PREFIX$VERSION_CURRENT")
    JProfReader.apply(TextInput.fromStrings((header ++ input).toArray), "unit-test")
  }

  private def checkObj(actual: JProfData.Obj, expectedType: JProfFormat.ObjType, expectedAttrs: String): Unit = {
    assertResult(expectedType)(actual.tpe)
    assertResult(expectedAttrs)(actual.attributes)
  }
}
