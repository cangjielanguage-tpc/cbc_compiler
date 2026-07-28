/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.sections

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf

import scala.collection.mutable

/** String table (content of .debug_str section). DwarfEmitter used offset in this table as pointer to string.
  *
  * 7.5.4 Attribute Encodings (string, DW_FORM_strp)
  *
  * @author gatimosh
  * @author conwor
  */
object DebugStr extends Dwarf.Section {
  private val cache = new mutable.HashMap[XString, Label]
  def label(str: XString): Label = cache.getOrElseUpdate(str, { newLabel })

  override def close(): Segment = {
    for ((str, label) <- cache) {
      bind(label)
      nullTerminatedString(str)
    }
    super.close()
  }
}