/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import com.huawei.excelsior.jet.jprof.JProfFormat.*
import xscala.io.{Files, Path, TextOutput}
import xscala.text.Utf8Encoding

import java.io.Closeable
import java.io.IOException

/** Utility class for writing JProf files.
  *
  * <strict>WARNING:</strict> This writer **MUST BE** always synchronized with its AJ copy in runtime:
  * see `com.huawei.excelsior.jet.runtime.features.profiler.jprof.JProfWriter`
  *
  * Main constructor creates new jprof-file by the given [[profilePath]].
  *
  * @author xappymah
  * @author ijorch
  */
final class JProfWriter(profilePath: Path) extends Closeable {

  private val out = TextOutput.from(profilePath, encoding = Utf8Encoding)

  private var curSection: SectionType = _
  private var curEntry: EntryType = _
  private var curObj: ObjType = _
  private var wasAnyAttr = false

  def printHeader(): Unit = {
    assert(curSection == null)
    assert(curEntry == null)
    assert(curObj == null)

    append(HEADER)
      .appendNewLine()

    append(VERSION_PREFIX)
      .append(VERSION_CURRENT)
      .appendNewLine()
  }

  def sectionStart(sectionType: SectionType): Unit = {
    assert(curSection == null)
    assert(curEntry == null)
    assert(curObj == null)
    assert(sectionType != null)

    append(SECTION_START)
      .append(' ')
      .append(sectionType.sectionType)
      .appendNewLine()

    curSection = sectionType
  }

  def sectionEnd(): Unit = {
    assert(curSection != null)
    assert(curEntry == null)
    assert(curObj == null)

    append(SECTION_END)
      .appendNewLine()

    curSection = null
  }

  def entryStart(entryType: EntryType): Unit = {
    assert(curSection != null)
    assert(curEntry == null)
    assert(curObj == null)
    assert(entryType != null)
    assert(entryType.rootSection == curSection)

    append(ENTRY_START)
      .append(entryType.entryType)
      .appendNewLine()

    curEntry = entryType
  }

  def entryEnd(): Unit = {
    assert(curSection != null)
    assert(curEntry != null)
    assert(curObj == null)

    append(ENTRY_END)
      .appendNewLine()

    curEntry = null
  }

  private def appendLineComment(comment: String): Unit = {
    assert(curObj == null) // comment requires line break, while obj description cannot be broken in several lines

    append(COMMENT_LINE)
      .append(' ')
      .append(comment)
      .appendNewLine()
  }

  def objStart(objType: ObjType): Unit = {
    assert(curSection != null)
    assert(curEntry != null)
    assert(curObj == null)
    assert(objType.isDeprecated || objType.allowedRootEntries.contains(curEntry))
    assert(!wasAnyAttr)

    append(OBJ_INDENT)
      .append(objType.objType)
      .append(OBJ_DEF_SEPARATOR)

    curObj = objType
  }

  def objEnd(): Unit = {
    assert(curSection != null)
    assert(curEntry != null)
    assert(curObj != null)

    appendNewLine()

    curObj = null
    wasAnyAttr = false
  }

  private def attrDefaultSep() = {
    assert(curSection != null)
    assert(curEntry != null)
    assert(curObj != null)
    assert(wasAnyAttr)

    append(OBJ_DEF_SEPARATOR)
    this
  }

  private def attrAppend(str: String) = {
    assert(curSection != null)
    assert(curEntry != null)
    assert(curObj != null)

    if (wasAnyAttr) {
      attrDefaultSep()
    } else {
      wasAnyAttr = true
    }
    append(str)
    this
  }

  def attrAppendKeyValue(name: KeyName, value: Any) = {
    assert(name.containingObjects.contains(curObj), s"curObj = $curObj, name = $name")
    attrAppend(name.serialize(value))
  }

  private def append(s: String) = {
    out.print(s)
    this
  }

  private def append(ch: Char) = {
    out.print(ch)
    this
  }

  private def appendNewLine() = append(LINE_END)

  override def close(): Unit = {
    try {
      out.close()
    } catch {
      case _: IOException =>
        // do we even need to handle it?
        throw new AssertionError("Exception during closing the .jprof file after writing profile")
    }
  }
}