/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind.{ADDR64, DWARF_SECTION}
import com.huawei.excelsior.jet.assembler.{Segment, Symbol}
import com.huawei.excelsior.jet.common.XString
import DwarfLinker.HeaderInfo
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CompilationUnit
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langcangjie.CangjieCompilationUnit
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava.JavaCompilationUnit
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections._
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.PrologueEndLabel
import com.huawei.excelsior.jet.compiler.symlevel.{Method, Type}

import scala.collection.mutable

/** DWARF version 4 implementation.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
object Dwarf {
  val VERSION = 4

  val DWARF_PARTS_OBJ_UID = XString("DEBUG")
  val FIXUP_SEGMENT_SUFFIX = "_FX"

  /////////////////////////////////////////////////////////////////////////////
  // DWARF data structures description

  // TODO-DATA-EMITTER: consider using data emitter, not extends it
  abstract class DataStructure extends DwarfEmitter {
    def close(): Segment = tearDown()
  }

  class Section extends Dwarf.DataStructure with Symbol {
    setUp(this)
  }

  class Entry extends Dwarf.DataStructure {
    setUp()
    val start = newBoundLabel
    val end = newLabel

    override def close(): Segment = {
      bind(end)
      super.close()
    }

    def isEmpty: Boolean = segment.isEmpty
    def nonEmpty: Boolean = segment.nonEmpty
  }

  /////////////////////////////////////////////////////////////////////////////
  // DWARF data collection

  private lazy val units = new mutable.LinkedHashMap[Type, CompilationUnit]

  def append(method: Method, seg: Segment)(implicit tp: TypeProvider): Unit = {
    val record = new CodeRecord(method, seg)
    if (!record.codeOriginLabels.exists(_.isInstanceOf[PrologueEndLabel])) {
      // Ugly patch for CodeRecords produced from stdlib methods (for more details look at CodeGenerator.genDebug
      // variable). Actually, empty CodeRecords are OK, but records from "std.core" namespace, linked into DWARF
      // structures in the middle of "default" namespace, broke all precious system of namespaces.
      // TODO-DWARF: kill DwarfTypes.open/closeNamespace and remove this workaround
      return
    }

    val `class` = method.getDeclaringClass
    val unit = if (`class`.isCangjieType) {
      val `package` = `class`.getCangjiePackage
      units.getOrElseUpdate(`package`, { new CangjieCompilationUnit(`package`) })
    } else {
      units.getOrElseUpdate(`class`, { new JavaCompilationUnit(`class`) })
    }
    unit.append(record)
  }


  /////////////////////////////////////////////////////////////////////////////
  // DWARF data linker

  private lazy val sections = List(DebugAbbrev, DebugFrame, DebugInfo, DebugLine, DebugPubnames, DebugStr)

  private def segmentName(section: Section) = section match {
    case DebugAbbrev    =>  "DEBUG_ABBREV"
    case DebugFrame     =>  "DEBUG_FRAME"
    case DebugInfo      =>  "DEBUG_INFO"
    case DebugLine      =>  "DEBUG_LINE"
    case DebugPubnames  =>  "DEBUG_PUBNAMES"
    case DebugStr       =>  "DEBUG_STR"
    case _ => shouldNotReachHere(s"unexpected section: $section")
  }

  def fixupCode(kind: RelocationKind, target: Symbol) = (kind, target) match {
    case (DWARF_SECTION, DebugAbbrev) => 0 // DWARF_FIXUP_DEBUG_ABBREV_SECTION
    case (ADDR64, _)                  => 1 // DWARF_FIXUP_ADDRESS
    case (DWARF_SECTION, DebugInfo)   => 2 // DWARF_FIXUP_DEBUG_INFO_SECTION
    case (DWARF_SECTION, DebugLine)   => 3 // DWARF_FIXUP_DEBUG_LINE_SECTION
    case (DWARF_SECTION, DebugStr)    => 4 // DWARF_FIXUP_DEBUG_STR_SECTION
    case _ => shouldNotReachHere(s"unexpected dwarf relocation: $kind, $target")
  }

  def setEquationsForRSP(set: (String, String) => Unit): Unit = sections foreach { section =>
    val namePrefix = segmentName(section)
    set(namePrefix, namePrefix)
    set(s"$namePrefix$FIXUP_SEGMENT_SUFFIX", s"$namePrefix$FIXUP_SEGMENT_SUFFIX")
  }

  def link(): Unit = {
    if (dwarfLinker == null) {
      return
    }

    for ((_, unit) <- units) {
      DebugInfo.include(unit)
    }

    val bytes = new mutable.LinkedHashMap[Section, Segment]
    for (section <- sections) {
      val seg = section.close()
      seg.freeze()
      bytes(section) = seg
    }

    dwarfLinker.start(new HeaderInfo(null, DWARF_PARTS_OBJ_UID, DWARF_PARTS_OBJ_UID))

    var secIdx = 1
    for ((section, bytes) <- bytes) {
      val generatedSegs = dwarfLinker.finishSection(secIdx, XString.ascii(segmentName(section)), bytes)
      secIdx += generatedSegs
    }

    dwarfLinker.finish()
  }


  /////////////////////////////////////////////////////////////////////////////
  // Environment support

  var typeProvider: TypeProvider = _
  var dwarfLinker: DwarfLinker = _
  var linkageName: Symbol => XString = _
  var typeHandleToType: Object => Type = _
}
