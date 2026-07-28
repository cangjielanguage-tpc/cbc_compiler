/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.sections

import com.huawei.excelsior.common.Arch.{ARM64, AMD64}
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf

/** Line number information (content of .debug_line section).
  *
  * 6.2 Line Number information
  *
  * @author gatimosh
  * @author conwor
  */
object DebugLine extends Dwarf.Section {
  val minimumInstructionLength = targetArch match {
    case AMD64 => 1// TODO-DWARF: replace this hardcode with ABI/Frame/Arch API
    case ARM64 => 1// TODO-DWARF: replace this hardcode with ABI/Frame/Arch API
    case arch => notImplemented(s"LNP for $arch")
  }

  val maximumOperationsPerInstruction = 1 // TODO-DWARF: how to use it?

  val defaultIsStmt = true
}