/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.debug.dwarf

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.common.XString

object DwarfLinker {
  // XOMF_HEADER attributes for obj with DWARF segments
  class HeaderInfo(val source: XString, val name: XString, val uid: XString) {
  }
}

trait DwarfLinker {
  def start(headerInfo: DwarfLinker.HeaderInfo): Unit

  def finishSection(idx: Int, section: XString, bytes: Segment): Int

  def finish(): Unit
}