/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.Worklist

import scala.collection.immutable.List

/**
 * InlineContextRegion is a concept used to determine CFG region original context for context-dependent code generation.
 *
 * @author conwor
 */
trait InlineContextRegions { self: Universe =>

  /** Creates InlineContextRegion around given `scope`. */
  def makeICRegionAround(scope: Scope, ic: InlineContext): Unit = withinScope(scope) {
    // Make enter in region

    val entryBlock = scope.entryBlock
    assert(entryBlock.inEdges.isEmpty)

    val enter = insertCodeAfter(entryBlock) { ICRegionEnter(ic)() }

    // Make exits out of region at return and at rethrow

    def makeExit(exit: LowerPoint): Unit = {
      if (exit != null) {
        insertCodeBefore(exit) { ICRegionExit(ic)() }
      }
    }

    makeExit(scope.exitPoint)
    makeExit(constructSingleHandler())
  }

  /** Eliminates pairs of enter/exit nodes, if there is no control operations
    * between them. Returns true iff some case was optimized. */
  def eliminateEmptyICRegions(): Boolean = {
    var changed = false
    for (exit <- all[ICRegionExit].toList) {
      exit.inCtrl match {
        case enter: ICRegionEnter =>
          assert(exit.ic == enter.ic)
          assert(enter.block == exit.block)
          strikeOut(exit)
          strikeOut(enter)
          changed = true
        case _ =>
      }
    }
    changed
  }

  /** Calculates top method of inline context without line number. More accurate calculation may
    * lead to problems (look at SharedErrorRTSCallBlock class and JET-10486 for more details). */
  def calcICRegionsMap(): Block => Method = {
    assert(irHasDifferentICRegions())

    type State = List[Method]

    val inputStates = Maps[Block].newQMap[State]
    val outputContexts = Maps[Block].newQMap[Method]
    val worklist = Worklist.empty[Block]

    def mergeIn(block: Block, state: State): Unit = {
      inputStates.get(block) match {
        case None =>
          inputStates(block) = state
          worklist += block
        case Some(currState) =>
          assert(currState == state)
      }
    }

    def trans(block: Block): Unit = {
      var state = inputStates(block)

      block.spineForward foreach {
        case x if x.hasXHandler =>
          mergeIn(x.xHandler, state)

        case enter: ICRegionEnter =>
          state = enter.ic.method :: state

        case exit: ICRegionExit =>
          val head :: tail = state
          assert(head == exit.ic.method)
          state = tail

        case _ =>
      }

      outputContexts(block) = state.head
      block.succBlocks foreach { succ => mergeIn(succ, state) }
    }

    inputStates(entryBlock) = List(rootMethod)
    worklist += entryBlock
    worklist.drain foreach trans

    assert(all[Block] forall { b => outputContexts.contains(b) || isUnreachableBar(b) })

    dbgPrinter.debugNodes("IC regions map calculated", {
      case x if isUnreachableBar(x) => null
      case block: Block => outputContexts(block).toString
      case _ => null
    })

    outputContexts
  }

  /** Returns true iff there are different IC regions in IR */
  def irHasDifferentICRegions() = all[ICRegionEnter].nonEmpty

}
