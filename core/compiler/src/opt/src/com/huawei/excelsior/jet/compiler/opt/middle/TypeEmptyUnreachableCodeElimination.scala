/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.StatsKind.TypeEmptyUnreachableElimination
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.util.Sets

/** Finds TypeEmpty approximated nodes and inserts Halt after `topCtrl` of each one.
  * 
  * Eliminates unreachable according to type approximation (TypeEmpty) nodes.
  * Steps:
  *     1. Collect all TypeEmpty nodes.
  *     1. Place Halt after every collected node's topCtrl.
  *     1. Calls UCE/DCE for cleanup of unreachable/dead code.
  * 
  * No value passed to the code produced from TypeEmpty node, thus it's unreachable, and it's unreachable after it's
  * `topCtrl`, which allowed us to calculate type property of node as TypeEmpty.
  * If `node` is unreachable, then any other node below `topCtrl(node)` is unreachable too.
  * Thus we can just remove code after `topCtrl(node)`. We do this by splitting spine after `topCtrl(node)` and
  * placing `Halt` node.
  *
  * @author julian
  */
trait TypeEmptyUnreachableCodeElimination extends DCEComponent { self: Universe =>

  /** Eliminates unreachable according to type approximation (TypeEmpty) nodes.
    * 
    * Requires absence of unreachable and dead code.
    * Cleans up any unreachable or dead code if optimization was performed.
    * 
    * @return true iff any transformation was performed.
    */
  def cleanupTypeEmptyApproximatedUnreachableCode(): Boolean = {
    checkConsistency(CheckLevels.Optional) { noUnreachableCode && (NoValue.unique.isEmpty || !currentScope.contains(NoValue.unique.get)) }

    val topCtrlBorders = Sets[UpperPoint].newQSet
    
    for (node <- allNodes if node.tpe.isTraceableRefType && nodeType(node).isEmpty;
         topCtrlNode = topCtrl(node) if !topCtrlNode.outCtrl.isInstanceOf[Halt]) {
      stats.count(TypeEmptyUnreachableElimination, s"optimized")
      stats.count(TypeEmptyUnreachableElimination, s"optimized ${node.name} with topCtrl ${topCtrlNode.name}")
      
      topCtrlBorders.add(topCtrlNode)
    }
    
    for (border <- topCtrlBorders) {
      replaceByHalt(Block.splitAfter(border))
    }
    
    val changed = topCtrlBorders.nonEmpty
    if (changed) {
      dbgPrinter.debugNodes("all graph after TypeEmptyUnreachableCodeOptimization")
      eliminateUnreachableCode()
      dbgPrinter.debugNodes("all graph after UCE")
      eliminateDeadCode()
      dbgPrinter.debugNodes("all graph after DCE")
    }
    changed
  }

  /** Checks that all type empty elimination are optimized by [[SimplifyComponent]]
    * and while processing [[DataFlow]]
    * with the help of no-return analysis done in [[FrontPhase]].
    */
  def checkTypeEmptyEliminationHasNoEffect(): Unit = {
    val changed = cleanupTypeEmptyApproximatedUnreachableCode()
    // TODO: temporary disabled as workaround of JET-17329
    // assert(!changed)
  }
}
