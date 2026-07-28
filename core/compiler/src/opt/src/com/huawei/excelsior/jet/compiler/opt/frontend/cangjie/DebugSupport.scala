/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, NoPosition}
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langcangjie.CangjieDwarfTypes
import com.huawei.excelsior.jet.compiler.debug.info.{DebugDeclaration, DebugLocalVar}
import com.huawei.excelsior.jet.compiler.ir.{BytecodeOffset, LineNumber}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.{DIFile, DILexicalBlock, DISubprogram}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.DIFlag
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

/** Support of debug information collection for Cangjie language front.
  *
  * @author conwor
  * @author cypok
  */
trait DebugSupport { self: Universe with CangjieLLVMIRParser =>

  private[cangjie] def updateDebugInfoAfterParsing(cb: CB): Unit = {

    // 1. Update DebugTextPosBreakpoints: clean out bytecode offsets and join sequential breakpoints.

    for (block <- all[Block]) {
      var lastBreakpoint: DebugTextPosBreakpoint = null
      for (node <- block.spineForward.toList) {
        node match {
          case breakpoint: DebugTextPosBreakpoint =>
            // Debug line breakpoints are about source code positions.
            val pos = breakpoint.pos.asInstanceOf[BytecodePosition]
            if (!LineNumber.isKnown(pos.lineNumber)) {
              // Unknown position => useless breakpoint.
              strikeOut(breakpoint)
            } else {
              // Clear useless bitcode offset (at least to enable sequential breakpoints joining).
              assert(!BytecodeOffset.isSynthetic(pos.offset))
              breakpoint.pos = pos.copy(offset = BytecodeOffset.SYNTHETIC)

              // Perform sequential breakpoints joining.
              if (lastBreakpoint != null && lastBreakpoint.pos == breakpoint.pos) {
                strikeOut(breakpoint)
              } else {
                lastBreakpoint = breakpoint
              }
            }
          case _ =>
        }
      }
    }

    dbgPrinter.debugNodes("All graph after debug pos breakpoints updated with positions", { "(" + _.pos.toString + ")" })


    // 2. Replace variables StackAlloc.Local to StackAlloc.DebugVar.

    for ((varNode, debugInfo) <- cb.debugInfoForVariables) {
      varNode match {
        case StackAlloc.Local(allocType) =>

          val name = XString(debugInfo.name)
          assert(debugInfo.tpe != null)
          val varType = CangjieDebugToolbox.Types.bitcodeTypeToDebugType(debugInfo.tpe)
          val argIndex = debugInfo.arg - 1
          val decl = (debugInfo.file, debugInfo.line) match {
            case (file: DIFile, line) if file.fullPath != null =>
              val (lbLine, lbCol) = debugInfo.scope.resolve() match {
                case lb: DILexicalBlock => (lb.line, lb.column)
                case _ => (0, 0)
              }
              Some(DebugDeclaration(XString(file.fullPath), line, lbLine, lbCol))
            case _ => None
          }

          val isPointer = debugInfo.flags.contains(DIFlag.FlagObjectPointer)
          val info = DebugLocalVar(name, varType, argIndex, isPointer, decl)
          varNode.replaceBy(StackAlloc.DebugVar(allocType, info))

        case _ => CodeHelpers.shouldNotReachHere(s"unexpected varNode: $varNode")  // FIXME-UG support OHM slots here
      }
    }

    dbgPrinter.debugNodes("All graph after variables StackAlloc.Local replaced by StackAlloc.DebugVar", { "(" + _.pos.toString + ")" })


    // 3. Form method natural prologue (param variables assignments).

    def isParamVar(node: Node): Boolean = node match {
      case StackAlloc.DebugVar(_, info) if info.isArgument => true
      case _ => false
    }

    val paramAssignments = allNodes filter {
      case sm: StoreMemory => isParamVar(sm.addr) &&
        // Do not copy assignment of fake receiver parameter inserted for $preInit in @java-annotated classes!
        // TODO: create assignments from scratch and do not abuse copying of already created assignments.
        !sm.inValue0.isInstanceOf[NoValue]
      case cs: CopyStructure => isParamVar(cs.dst)
      case _ => false
    }

    insertCodeAfter(entryBlock) {
      withPos(NoPosition) {
        for (assign <- paramAssignments.toList) {
          assign.replaceValueUsesBy(Node.clone(assign))
          assign match {
            case assign: SpinalNode => strikeOut(assign)
            case _ => decommit(assign)
          }
        }
      }
      DebugPrologueEndBreakpoint()
    }

    dbgPrinter.debugNodes("All graph after natural prologue formed", { "(" + _.pos.toString + ")" })
  }
}
