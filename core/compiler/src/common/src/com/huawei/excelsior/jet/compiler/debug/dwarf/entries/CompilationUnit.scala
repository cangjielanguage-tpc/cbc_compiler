/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.addressSize
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfLanguageEncodings.encodeLang
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.{CompUnit, CompUnitWithMain, Namespace}
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.{DebugAbbrev, DebugFrame, DebugLine, DebugPubnames}
import com.huawei.excelsior.jet.compiler.debug.info.CompilationUnitInfo

/** Container of main debug information entries collected for one compilation unit.
  *
  * For each CU there are line number program, public names and types entries corresponding to this CU.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
abstract class CompilationUnit(info: CompilationUnitInfo) extends Dwarf.Entry {
  private val pubNames = new PubNames(this)
  final def pubName(name: XString, at: Label): Unit = pubNames.append(name, at)

  private val lineNumberProgram = new LineNumberProgram
  final def sources = lineNumberProgram.sources

  protected def types: Types

  type NsInsideOutsidePartsProvider = () => (Dwarf.Entry, Dwarf.Entry)
  protected def finishedBodies(): NsInsideOutsidePartsProvider
  private def includeParts(bodies: NsInsideOutsidePartsProvider, types: NsInsideOutsidePartsProvider): Unit = {
    val (bodyInsideNs, bodyOutsideNs) = bodies.apply()
    val (typeInsideNs, typeOutsideNs) = types.apply()
    include(bodyOutsideNs)
    include(typeOutsideNs)
    if (bodyInsideNs.nonEmpty || typeInsideNs.nonEmpty) {
      abbreviationScope(Namespace)(unitName()) {
        include(bodyInsideNs)
        include(typeInsideNs)
      }
    }
  }

  def unitName(): XString = info.name

  def append(record: CodeRecord): Unit = {
    lineNumberProgram.append(record)
    DebugFrame.makeFDE(record)
  }

  def isMain: Boolean = false

  override def close(): Segment = {
    DebugLine.include(lineNumberProgram)

    initialLength(end)
    uhalf(Dwarf.VERSION)
    sectionOffset(DebugAbbrev.commonAbbreviations)
    ubyte(addressSize)
    val abbr = if (isMain) CompUnitWithMain else CompUnit
    abbreviationScope(abbr)(unitName(), encodeLang(info.language), info.producer, info.directory, lineNumberProgram) {
      includeParts(finishedBodies(), () => types.finish())
    }
    types.toPubnames.foreach(dt => pubName(dt.name, types.label(dt)))

    DebugPubnames.include(pubNames)

    super.close()
  }
}