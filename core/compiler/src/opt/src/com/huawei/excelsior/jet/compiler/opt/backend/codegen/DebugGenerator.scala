/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.Location.{IReg, mem}
import com.huawei.excelsior.jet.compiler.Env.{linkRegister, stackPointer}
import com.huawei.excelsior.jet.compiler.coverage.JcnoFileGenerator
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.*
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections.{singleElement, singleton}
import com.huawei.excelsior.jet.util.graph.PostDominators

/** Part of [[CodeGenerator]], responsible for debug info generation.
  *
  * @author conwor
  * @author gatimosh
  */
trait DebugGenerator { self: Universe with BackEnd =>

  private lazy val genDebug = self.asInstanceOf[Universe].genDebug &&
    // Every method with debug information should contain DebugPrologueEndMarker, but there could be methods in
    // compilation set, which were not processed by front-end with debug information generation (versioned methods
    // from stdlib which were compiled without debug information).
    all[DebugPrologueEndBreakpoint].nonEmpty // TODO-DEBUG: remove this workaround


  trait DebugGeneratorImpl { self: CodeGeneratorImpl =>

    ///////////////////////////////////////////////////////////////////////////
    // Debug info inside block

    private var currPos: CodeOriginLabel = _

    private def findCurrCodeOriginLabel(block: Block): CodeOriginLabel = {
      // when a huge block is splitted into [1:2] the 2 block must inherit debug pos from the latest known for block 1
      // otherwise debugger may observe incorrect value for unsaved variable as in test cjdb_within_setVar018
      val iterToFindPos = singleton(block.predBlocks) match {
        case Some(pred) => pred.spineBackward
        case None => Iterator.empty
      }
      val pos = iterToFindPos.collectFirst {
        case DebugBreakpointWithKnownInfo(context, line, column, scope) => SourceCodeLabel(context, line, column, scope)
      }
      pos getOrElse SyntheticCodeLabel()
    }

    private[codegen] final def initDebugLabelsForBlock(block: Block): Unit = {
      if (genDebug) {
        currPos = findCurrCodeOriginLabel(block)
      }
    }

    private[codegen] final def bindDebugLabels(node: Node): Unit = {
      if (genDebug) {
        node match {
          case _: DebugPrologueEndBreakpoint =>
            asm.bind(PrologueEndLabel())
            asm.bind(currPos)

          case DebugBreakpointWithKnownInfo(context, line, column, scope) =>
            val label = SourceCodeLabel(context, line, column, scope)
            if (currPos != label) {
              asm.bind(label)
              currPos = label
            }

          case _ =>
        }
      }
    }


    ///////////////////////////////////////////////////////////////////////////
    // Debug info in the whole IR

    private[codegen] final def checkDebugInfoConsistency(): Unit = {
      if (genDebug) {
        val prologueEnd = singleElement(all[DebugPrologueEndBreakpoint])
        val pDoms = PostDominators.augmented(cfg)
        for (cn <- all[SpinalNode]) {
          if (!(prologueEnd dominates cn)) {
            assert(DebugBreakpointWithKnownInfo.unapply(cn).isEmpty)
            if (prologueEnd.block == cn.block) {
              assert(cn dominates prologueEnd)
            } else {
              assert(pDoms.postDominates(prologueEnd.block, cn.block))
            }
          }
        }
      }
    }

    protected def locationOfDebugSlot(slot: FrameSlot): Any =
      mem(AsmType.NONE, stackPointer, slot.offsetFromSP)

    private[codegen] final def collectLocalVarsDebugInfo(sp: IReg): Unit = {
      if (genDebug) {
        for (sa @ StackAlloc.DebugVar(tpe, info) <- all[StackAlloc]) {
          asm.bind(LocalVarLabel(info, locationOfDebugSlot(sa.slot), tpe))
        }
      }
    }

    private var prologueGenerated: Boolean = false

    private[codegen] final def appendBlockDebugInfo(block: Block): Unit = {
      if (genDebug) {
        if (prologueGenerated) {
          asm.bind(findCurrCodeOriginLabel(block))
        }
        prologueGenerated = prologueGenerated || block.nodes.exists((node: Node) => node.isInstanceOf[DebugPrologueEndBreakpoint])
      }
    }

    ///////////////////////////////////////////////////////////////////////////
    // coverage counter insert

    def genCoverageCounter(locs: Array[(String, Array[Int])]): Unit = ???

  }
}
