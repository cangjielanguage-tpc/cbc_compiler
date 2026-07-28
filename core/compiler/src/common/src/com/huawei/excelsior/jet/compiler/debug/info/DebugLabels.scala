/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.info

import com.huawei.excelsior.jet.assembler.{Label, Location}
import com.huawei.excelsior.jet.compiler.ir.{InlineContext, LexicalBlock}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type}

/** All information calculated by compilers for debug info generation provided as labels in method segment.
 *
 * @author conwor
 */
object DebugLabels {

  class DebugLabel extends Label

  /** Label indicates origin of machine instructions before or after it. */
  class CodeOriginLabel extends DebugLabel

  /** Label indicated end of prologue machine instructions. */
  case class PrologueEndLabel() extends CodeOriginLabel

  /** Label indicated begin of epilogue machine instructions. */
  case class EpilogueBeginLabel() extends CodeOriginLabel

  /** Label indicated start of synthetic machine instructions (not corresponding to any source code). */
  case class SyntheticCodeLabel() extends CodeOriginLabel

  /** Label indicated source position of next machine instructions. */
  case class SourceCodeLabel(context: InlineContext, line: Int, column: Int, scope: LexicalBlock = null) extends CodeOriginLabel

  /** Label indicated information about caller SP and RA for next machine instructions. */
  case class CallerFrameInfoLabel(callerSP: Location, callerRA: Location) extends DebugLabel

  /** Label indicated information about local variable. */
  case class LocalVarLabel(info: DebugLocalVar, location: Any, allocType: SignatureType) extends DebugLabel

  object LocalVarLabel {

    /**
      * Comparator for local variables located on the same line (or for variables not having any debug info at all).
      *
      * Sort such variables using the following heuristic rules, which seems to be respected, at least in easy cases:
      *   - for DWARF, the memory offset increases in the order of variable definitions;
      *   - for CBC, the register index increases in the reverse order.
      */
    private def defaultLessThan(lv1: LocalVarLabel, lv2: LocalVarLabel) = {
      (lv1.location, lv2.location) match {
        // CBC
        case (l1: Int, l2: Int) => l1 > l2

        // DWARF
        case (l1: Location.MemBased, l2: Location.MemBased) => l1.disp < l2.disp

        // fallback for safety
        case _ => lv1.info.name.compareTo(lv2.info.name) < 0
      }
    }

    /** Defines the order of storing <i>DW_TAG_variable</i> (dwarf) and <i>LVTableEntry</i> (.cbc) debug entries in the
      * output file. First arguments, ordered by index. Then other local variables having debug info, ordered by file
      * line number. Then variables without debug info.
      */
    def lessThan(lv1: LocalVarLabel, lv2: LocalVarLabel): Boolean = {
      (lv1.info, lv2.info) match {
        case (i1, i2) if i1.isArgument && i2.isArgument => i1.argIndex < i2.argIndex
        case (i1, i2) if !i1.isArgument && !i2.isArgument => (i1.declaration, i2.declaration) match {
          // We have no info here about the column where the variable is defined, only line number.
          //
          // TODO: propagate column number from bit code (from the second argument of the `INST_CALL` LLVM instruction
          //   where the first argument is the `llvm.dbg.declare` intrinsic).
          //   Or remove sorting altogether and preserve original order of debug info about locals as written in
          //   bitcode.
          case (Some(d1), Some(d2)) =>
            if (d1.line == d2.line) {
              defaultLessThan(lv1, lv2)
            } else {
              d1.line < d2.line
            }
          case (Some(_), None) =>
            true
          case (None, Some(_)) =>
            false
          case _ => defaultLessThan(lv1, lv2)
        }
        case (i1, i2) => i1.isArgument
      }
    }
  }
}
