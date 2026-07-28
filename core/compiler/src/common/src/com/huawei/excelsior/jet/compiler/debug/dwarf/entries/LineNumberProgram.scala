/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.compiler.Env.addressSize
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugLine._
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels._
import com.huawei.excelsior.jet.compiler.symlevel.Method

/** Line number program used to help debugger to associate locations in the source file with the
  * corresponding machine instructions addresses.
  *
  * 6.2. Line Number Information
  *
  * @author gatimosh
  * @author conwor
  */
class LineNumberProgram extends Dwarf.Entry {

  val sources = new Sources

  private class Body extends Dwarf.Entry {
    // Line Number Standard Opcodes (6.2.5.2)
    object LNS {
      def copy()                  : Unit = { ubyte(0x01) }
      def advancePc(delta: Int)   : Unit = { ubyte(0x02); uleb128(delta ensuring { _ > 0 }) }
      def advanceLine(delta: Int) : Unit = { ubyte(0x03); sleb128(delta) }
      def setFile(file: Int)      : Unit = { ubyte(0x04); uleb128(file) }
      def setColumn(column: Int)  : Unit = { ubyte(0x05); uleb128(column) }
      def negateStmt()            : Unit = { ubyte(0x06) }
      def setPrologueEnd()        : Unit = { ubyte(0x0a) }
      def setEpilogueBegin()      : Unit = { ubyte(0x0b) }
    }

    // Line Number Extended Opcodes (6.2.5.3)
    object LNE {
      def setAddress(symbol: Method): Unit = {
        ubyte(0)                // extended opcode marker
        ubyte(addressSize + 1)  // the length of instruction in bytes
        ubyte(2)                // opcode
        address(symbol)         // argument
      }

      def endSequence(): Unit = {
        ubyte(0) // extended opcode marker
        ubyte(1) // the length of instruction in bytes
        ubyte(1) // opcode
      }
    }

    private class State {
      class WithActionOnUpdate[T](private var value: T)(action: (T, T) => Unit) {
        def set(newValue: T): Boolean = {
          val change = value != newValue
          if (change) action(value, newValue)
          value = newValue
          change
        }
      }

      val address = new WithActionOnUpdate(0)             ({ (old, `new` ) => LNS.advancePc(`new` - old) })     // offset of current address from record start
      val file    = new WithActionOnUpdate(1)             ({ (_,   `new` ) => LNS.setFile(`new`) })             // current source file
      val line    = new WithActionOnUpdate(1)             ({ (old, `new` ) => LNS.advanceLine(`new` - old) })   // current source line number
      val column  = new WithActionOnUpdate(0)             ({ (_,   `new` ) => LNS.setColumn(`new`) })           // current source line column
      val isStmt  = new WithActionOnUpdate(defaultIsStmt) ({ (_,   _     ) => LNS.negateStmt() })               // indication that current instruction is a recommended breakpoint location
    }

    private def fileOf(method: Method): Int = {
      if (method.hasSourceFile) sources.id(method.getSourceFile) else 0
    }

    def append(record: CodeRecord): Unit = {
      val labels = record.codeOriginLabels

      if (labels.nonEmpty) {
        (labels.head, labels.tail) match {
          case (prologueEnd: PrologueEndLabel, other) =>
            val codeLabels = other.filter(_.isInstanceOf[SourceCodeLabel])
            if (codeLabels.nonEmpty) {
              val state = new State
              val method = record.scope

              // method start address and source file/line
              LNE.setAddress(method)
              state.file.set(fileOf(method))
              state.line.set(method.getSourceLine)

              if (!codeLabels.exists(_.asInstanceOf[SourceCodeLabel].line > 0)) {
                // we do not need any other LNE instructions when there are no SourceCodeLabels with real lines
                // we should not emit "copy" for initial address or set the address to the end of the method
                // just close the sequence. See example in the issue: #325
                LNE.endSequence()

              } else {
                // emit method start address and source file/line
                LNS.copy()

                // emit instructions for SourceCodeLabels with real lines
                state.address.set(prologueEnd.position)
                LNS.setPrologueEnd()
                if (other.head.position != prologueEnd.position) {
                  LNS.copy()
                }

                for (label <- other) {
                  val changed = label match {
                  case SourceCodeLabel(context, newLine, newColumn, _) =>
                    state.address.set(label.position) |
                    state.file.set(fileOf(context.method)) |
                    state.line.set(newLine) |
                    state.column.set(newColumn) |
                    state.isStmt.set(true)

                  case _: SyntheticCodeLabel =>
                    state.address.set(label.position) |
                    state.line.set(0) |
                    state.column.set(0) |
                    state.isStmt.set(false)

                  case _: EpilogueBeginLabel =>
                    false // setAddress(label.position) | LNS.setEpilogueBegin() // epilogue labels are ignored now TODO-DWARF: deal with them

                  case _ =>
                    shouldNotReachHere()
                  }

                  if (changed) {
                    LNS.copy()
                  }
                }

                // set the address to the end of the method and close the sequence
                state.address.set(record.seg.length)
                LNE.endSequence()
              }
            }

          case _ => shouldNotReachHere("unexpected sequence of code origin labels")
        }
      }
    }
  }

  private val body = new Body
  def append(record: CodeRecord): Unit = body.append(record)

  override def close(): Segment = {
    initialLength(end)    // unit_length
    uhalf(Dwarf.VERSION)  // version

    // header
    val headerEnd = newLabel
    initialLength(headerEnd)                // header_length
    ubyte(minimumInstructionLength)         // minimum_instruction_length
    ubyte(maximumOperationsPerInstruction)  // maximum_operations_per_instruction
    ubyte(if (defaultIsStmt) 1 else 0)      // default_is_stmt
    ubyte(0)                                // line_base
    ubyte(1)                                // line_range
    ubyte(13)                               // opcode_base
    ubyte(0)                                // standard_opcode_lengths[ 1] (DW_LNS_copy)
    ubyte(1)                                // standard_opcode_lengths[ 2] (DW_LNS_advance_pc)
    ubyte(1)                                // standard_opcode_lengths[ 3] (DW_LNS_advance_line)
    ubyte(1)                                // standard_opcode_lengths[ 4] (DW_LNS_set_file)
    ubyte(1)                                // standard_opcode_lengths[ 5] (DW_LNS_set_column)
    ubyte(0)                                // standard_opcode_lengths[ 6] (DW_LNS_negate_stmt)
    ubyte(0)                                // standard_opcode_lengths[ 7] (DW_LNS_set_basic_block)
    ubyte(0)                                // standard_opcode_lengths[ 8] (DW_LNS_const_add_pc)
    ubyte(1)                                // standard_opcode_lengths[ 9] (DW_LNS_fixed_advance_pc)
    ubyte(0)                                // standard_opcode_lengths[10] (DW_LNS_set_prologue_end)
    ubyte(0)                                // standard_opcode_lengths[11] (DW_LNS_set_epilogue_begin)
    ubyte(1)                                // standard_opcode_lengths[12] (DW_LNS_set_isa)
    include(sources)                        // files and directories
    bind(headerEnd)

    include(body)

    super.close()
  }
}
