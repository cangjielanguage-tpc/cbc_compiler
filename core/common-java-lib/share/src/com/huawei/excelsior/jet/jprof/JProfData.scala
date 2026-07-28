/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import com.huawei.excelsior.jet.jprof.JProfData.Section

import scala.collection.mutable.ArrayBuffer

/** Internal representation of parsed JProf data.
  *
  * @author ijorch
  * @author xappymah
  */
object JProfData {

  /** Represents a section of profiler data, that contains a number of [[Entry entries]]. */
  case class Section private[jprof](tpe: JProfFormat.SectionType) {
    assert(tpe != null)

    private val _entries = ArrayBuffer.empty[Entry]
    def entries = _entries.toSeq

    private[jprof] def += (e: Entry): Unit = _entries += e
  }

  /** A single entry in a section. In turn, it may contain multiple [[Obj objects]]. */
  case class Entry private[jprof](tpe: JProfFormat.EntryType) {
    assert(tpe != null)

    private val _objs = ArrayBuffer.empty[Obj]
    def objs = _objs.toSeq

    private[jprof] def += (o: Obj): Unit = _objs += o
  }

  /** A lowest-level data object, characterized by type and attributes line. */
  case class Obj private[jprof](tpe: JProfFormat.ObjType, attributes: String) {
    assert(tpe != null)
    assert(attributes != null)

    def parseIntAttribute = attributes.toIntOption getOrElse {
      throw new JProfParsingException(s"Object '${tpe.objType}' has not a numeric attribute: $attributes")
    }
  }
}

class JProfData {
  private val _sections = ArrayBuffer.empty[Section]
  private var _version: JProfVersion = _
  private[jprof] def +=(s: Section): Unit = _sections += s
  private[jprof] def sections = _sections.toSeq

  def getSectionsByType(sectionType: JProfFormat.SectionType) =
    _sections.iterator.filter(_.tpe == sectionType).toSeq

  private[jprof] def version_=(v: JProfVersion): Unit = {
    assert(_version == null)
    _version = v
  }
  def version = _version ensuring (_ != null)
}
