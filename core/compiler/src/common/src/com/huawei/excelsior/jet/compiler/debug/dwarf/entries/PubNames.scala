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
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf

/** Part of .debug_pubnames section described public names of one compilation unit.
  *
  * 7.19 Name Lookup Tables.
  *
  * @author gatimosh
  * @author conwor
  */
final class PubNames(unit: CompilationUnit) extends Dwarf.Entry {
  initialLength(end)    // unit_length
  uhalf(2)              // version
  sectionOffset(unit)   // debug_info_offset
  sectionLength(unit)   // debug_info_length

  def append(name: XString, at: Label): Unit = {
    entryOffset(unit, at)
    nullTerminatedString(name)
  }

  override def close(): Segment = {
    uword(0) // terminate by an offset field containing zero (and no following string)
    super.close()
  }
}