/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.jprof.JProfData.Section
import com.huawei.excelsior.jet.jprof.JProfFormat.{EntryType, ObjType, SectionType}
import xscala.io.{Files, Path, TextInput}
import xscala.matching.Pattern
import xscala.text.Utf8Encoding

import java.io.Closeable
import java.io.IOException
import scala.collection.mutable

/** Parse lines from given TextInput or file to construct [[JProfData]].
  *
  * @author ijorch
  * @author xappymah
  */
object JProfReader {
  private val CUID_PAT = Pattern.compile("cuid=([^ ]+)")
  private val SECTION_START_PREFIX = JProfFormat.SECTION_START + " "

  def apply(in: TextInput, fileName: String): JProfReader =
    new JProfReader(in, new JProfData, fileName)

  def apply(file: Path): JProfReader =
    apply(TextInput.from(file, buffered = true, encoding = Utf8Encoding), file.toString)
}

class JProfReader private (in: TextInput, jprofData: JProfData, fileName: String) extends Closeable {
  private var cuid2def: mutable.HashMap[String, JProfData.Obj] = _

  def parse(): JProfData = parse(false)

  def parse(expandCuids: Boolean): JProfData = {
    cuid2def = if (expandCuids) mutable.HashMap.empty else null

    checkHeader()
    // TODO: use ScalaCollections.iterateUntilNull
    var sec: Section = parseSection()
    while (sec != null) {
      jprofData += sec
      sec = parseSection()
    }

    jprofData
  }

  def getJProfVersion = jprofData.version

  private def skipCommentBlock(): Unit = {
    // the opening line of the block comment was already parsed,
    // so we only need to search for the closing one
    while (true) {
      val l = in.getLine()
      if (l == null) return
      if (l.startsWith(JProfFormat.COMMENT_BLOCK)) return
    }
  }

  /** Line is good iff it is not a part of a comment and not empty.
    * @return good line or `null` if none found until the end of file.
    */
  private def nextGoodLine(): String = {
    // skip comments and empty lines
    while (true) {
      val l = in.getLine()
      if (l == null) return null

      if (l.startsWith(JProfFormat.COMMENT_BLOCK)) {
        // Comment block char-sequence has higher priority than comment line char sequence.
        // So we need to check for it first.
        skipCommentBlock()
      } else if (l.isEmpty || l.startsWith(JProfFormat.COMMENT_LINE)) {
        // skip
      } else {
        return l.trim
      }
    }
    shouldNotReachHere()
  }

  private def checkHeader(): Unit = {
    // we read lines directly from input because no empty lines or comments are allowed before the header
    val headerString = in.getLine()
    if (JProfFormat.HEADER != headerString) {
      throw new JProfParsingException("Not a .jprof file", fileName)
    }

    val versionString = in.getLine()
    if ((versionString == null) || !versionString.startsWith(JProfFormat.VERSION_PREFIX)) {
      throw new JProfParsingException(s"Unexpected .jprof version string: $versionString", fileName)
    }

    val version = versionString.substring(JProfFormat.VERSION_PREFIX.length)
    jprofData.version = JProfVersion.fromString(version)
    if (!jprofData.version.isSupported) {
      throw new JProfParsingException(s"Unsupported .jprof version: $version", fileName)
    }
  }

  private def parseSection(): Section = {
    val sectionDef = nextGoodLine()
    if (sectionDef == null) return null

    val currentSection = JProfData.Section(parseSectionType(sectionDef))

    var end = false
    while (!end) {
      val entryStartOrSectionEnd = nextGoodLine()
      if (entryStartOrSectionEnd == null) {
        throw new JProfParsingException("Unexpected end of the file", fileName)
      }

      if (entryStartOrSectionEnd != JProfFormat.SECTION_END) {
        val entry = parseEntry(currentSection.tpe, entryStartOrSectionEnd)
        assert(entry != null)
        currentSection += entry
      } else {
        end = true
      }
    }

    currentSection
  }

  private def parseSectionType(sectionDef: String) = {
    if (!sectionDef.startsWith(JProfReader.SECTION_START_PREFIX)) {
      throw new JProfParsingException(s"Expected section start but got: $sectionDef", fileName)
    }

    val typeStr = sectionDef.substring(JProfReader.SECTION_START_PREFIX.length)

    val sectionType = SectionType.findSectionType(typeStr)
    if (sectionType == null) {
      throw new JProfParsingException(s"Unknown section: $typeStr", fileName)
    }
    sectionType
  }

  private def parseEntry(sectionType: JProfFormat.SectionType, entryDef: String) = {
    val currentEntry = JProfData.Entry(parseEntryType(sectionType, entryDef))

    var end = false
    while (!end) {
      val objDefOrEntryEnd = nextGoodLine()
      if (objDefOrEntryEnd == null) {
        throw new JProfParsingException("Unexpected end of the file", fileName)
      }

      if (objDefOrEntryEnd != JProfFormat.ENTRY_END) {
        val obj = parseObj(currentEntry.tpe, objDefOrEntryEnd)
        assert(obj != null)
        currentEntry += obj
      } else {
        end = true
      }
    }

    currentEntry
  }

  private def parseEntryType(sectionType: JProfFormat.SectionType, entryDef: String) = {
    if (!entryDef.startsWith(JProfFormat.ENTRY_START)) {
      throw new JProfParsingException(s"Expected entry definition but got: $entryDef", fileName)
    }

    val typeStr = entryDef.substring(JProfFormat.ENTRY_START.length)

    val entryType = EntryType.findEntryType(sectionType, typeStr)
    if (entryType == null) {
      throw new JProfParsingException(s"Unknown entry: $entryDef", fileName)
    }
    entryType
  }

  private def parseObj(entryType: JProfFormat.EntryType, objDef: String) = {
    val sepIdx = objDef.indexOf(JProfFormat.OBJ_DEF_SEPARATOR)
    var (typeStr, attrs) = if (sepIdx != -1) {
      (objDef.substring(0, sepIdx), objDef.substring(sepIdx + 1))
    } else {
      (objDef, "")
    }

    val objType = ObjType.findObjType(entryType, typeStr)
    if (objType == null) {
      throw new JProfParsingException(s"Object type '$typeStr' is not expected in entry of type '$entryType' in '$objDef'", fileName)
    }

    if (cuid2def != null) {
      val cuidMatcher = JProfReader.CUID_PAT.matcher(attrs)
      val found = cuidMatcher.find
      if (objType == ObjType.BLAME_CODE_UNIT_DEF) {
        assert(found)
        val cuid = cuidMatcher.group(1)
        cuid2def(cuid) = JProfData.Obj(objType, attrs)
      } else if (found) {
        val cuid = cuidMatcher.group(1)
        val cuidObj = cuid2def.getOrElse(cuid, throw new JProfParsingException(s"Unknown cuid $cuid", fileName))
        val prefix = attrs.substring(0, cuidMatcher.start(0))
        val ins = cuidObj.attributes
        val suffix = attrs.substring(cuidMatcher.end(0))
        attrs = prefix + ins + suffix
      }
    }

    JProfData.Obj(objType, attrs)
  }

  override def close(): Unit = {
    try {
      in.close()
    } catch {
      case _: IOException =>
        // do we even need to handle this?
        throw new AssertionError("Exception during closing the .jprof file after reading profile")
    }
  }
}