/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.jprof

import com.huawei.excelsior.jet.compiler.jprof.JProfManager.USGEntry
import com.huawei.excelsior.jet.jprof.JProfFormat.{EntryType, ObjType, SectionType}
import com.huawei.excelsior.jet.jprof.{JProfData, JProfFormat, JProfParsingException, JProfReader}
import xscala.io.{Files, Path}

import java.io.{FileNotFoundException, IOError}
import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/** This class manages various JProf routines in compiler.
  *
  * @author xappymah
  */
object JProfManager {

  /** Manager corresponding to the jprof file set via `jprofile` equation
    * (or the one from `jprofileDir` with the same name as output file).
    */
  private var _main: JProfManager = _

  private val EMPTY = new JProfManager() {
    override def toString = {
      assert(file == null)
      "EMPTY"
    }
    override def getUSGEntries = Seq.empty
    override def getSectionsByType(sectionType: JProfFormat.SectionType) = Seq.empty
  }

  def main: JProfManager = {
    if (_main == null) {
      _main = EMPTY
    }
    _main
  }

  def initMain(manager: JProfManager): Unit = {
    assert(_main == null)
    _main = manager
  }

  case class USGEntry(name: String, mask: Int)

  case class ClassNameAndCLID(classLoaderSID: String, className: String)

  case class MethodInfo(clazz: ClassNameAndCLID, name: String, sig: String) {
    def isKnown =
      (clazz.className != JProfFormat.CLASS_UNKNOWN) &&
        (name != JProfFormat.METHOD_NAME_UNKNOWN) &&
        (sig != JProfFormat.METHOD_SIG_UNKNOWN)
  }
}

class JProfManager(val file: Path = null) {
  private var data: JProfData = _

  if (file != null) {
    try {
      if (!file.exists) throw new FileNotFoundException(s"\"$file\" doesn't exist")
      if (file.isDirectory) throw new IllegalArgumentException(s"\"$file\" is a directory")

      if (Files.size(file) == 0) {
        // empty file is fine
      } else {
        Using.resource(JProfReader(file)) { reader =>
          data = reader.parse()
        }
      }
    } catch {
      case e: Exception =>
        data = null
        throw new IOError(e)
    }
  }

  def this(jprofPath: String) = {
    this(Path(jprofPath))
  }

  override def toString = file.name

  @nowarn("msg=match may not be exhaustive")
  def getUSGEntries: Seq[USGEntry] = {
    if (data == null) {
      return Seq.empty
    }

    val entries = ArrayBuffer.empty[USGEntry]
    val usgSections = data.getSectionsByType(SectionType.USG_PROF)
    for {
      sec <- usgSections
      entry <- sec.entries
    } {
      assert(entry.tpe == EntryType.USG_ENTRY)
      try {
        var name: String = null
        var mask = 0
        for (obj <- entry.objs) {
          obj.tpe match {
            case ObjType.USG_NAME =>
              name = obj.attributes
            case ObjType.USG_MASK =>
              mask = obj.parseIntAttribute
          }
        }
        entries += USGEntry(name, mask)
      } catch {
        case _: JProfParsingException => // ignore unparsable entries
      }
    }
    entries.toSeq
  }

  def getSectionsByType(sectionType: JProfFormat.SectionType): Seq[JProfData.Section] = {
    if (data == null) {
      return Seq.empty
    }
    data.getSectionsByType(sectionType)
  }

  def version = if (data != null) data.version else null
}