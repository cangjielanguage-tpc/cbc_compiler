/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.fast

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.BulldozerGCM
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.GlobalCodeMotion
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Worklist

/** Code ordering framework build upon standard [[GlobalCodeMotion]] without any optimizations.
  *
  * Used as a first main phase in fast [[BackEnd]] pipeline.
  *
  * @author conwor
  */
trait FastCodeOrdering { self: Universe with BackEnd =>

  /** [[BulldozerGCM]] rematerialize [[FragilePointerType]]-d nodes during upward AI step, using liveness information.
    * We do not collect this information and can not do the same. So all fragile pointers are rematerialized in the
    * whole IR.
    */
  private def rematerializeFragilePointers(): Unit =
    all[FloatingNode] filter (_.isFragilePointer) foreach Node.rematerializeCompletely

  /** Iterates all points in all blocks and complete [[CodeOrder]] in each of them. */
  private def completeCodeOrderInPoints(): Unit = {
    for (block <- all[Block]) {
      for (point <- block.spineForwardWithBlock) {
        val pointGroup = Sets[Node].newQSet(point.pinnedNodes filter (_.isGroupRoot))
        val remainingInDeps = Maps[Node].newMMap[Int]

        // 3.1. Start order with point
        val order = Worklist[Node](point)
        if (point == block) {
          // Add method parameters and phies first since they're pre-allocated
          val (paramsAndPhies, otherParamNodes) = block.paramNodes.partition {
            case _: Param | _: Phi => true
            case _ => false
          }
          order ++= paramsAndPhies
          order ++= otherParamNodes
        }

        // 3.2. Form initial order with independent nodes and DAG with the rest nodes
        for (node <- pointGroup if !order.contains(node)) {
          node.groupedArgs count (arg => pointGroup contains arg.groupRoot) match {
            case 0 => order.append(node)
            case inDep => remainingInDeps(node) = inDep
          }
        }

        // 3.3. Drain order reducing dependencies, removing from DAG new independent nodes and appending them to order
        for (node <- order.drain) {
          for (arg <- node.attachedArgs) {
            arg.asInstanceOf[FloatingNode] atLowerPoint lowerPoint(node)
          }
          CodeOrder.append(node, block)
          for (res <- node.attachedResults) {
            res.asInstanceOf[FloatingNode] atUpperPoint upperPoint(node)
          }

          for (use <- node.groupedUses; useRoot = use.groupRoot if pointGroup contains useRoot) {
            if (useRoot.isInstanceOf[BlockParamNode]) {
              assert(useRoot.block == block)
            } else {
              remainingInDeps(useRoot) match {
                case 1 => order append useRoot; remainingInDeps remove useRoot
                case inDep => remainingInDeps(useRoot) = inDep - 1
              }
            }
          }
        }
        assert(remainingInDeps.isEmpty)
      }

      CodeOrder.append(block.blockEnd, block)
    }
  }

  /** Post-process: drag [[FlagProducer]] and [[FragilePointerType]]-d nodes down to single uses. */
  private def dragNodesDown(): Unit =
    dragNodesToSingleUse(fromFastCodeOrdering = true)

  final def fastCodeOrdering(): Unit = {
    step("fragile pointers rematerialized", rematerializeFragilePointers())
    step("standard GCM done",               doGlobalCodeMotion(new GCMEngine(forceSheduleLate = true))) // TODO-FAST-BE: consider optimize memory anti-dependencies
    step("code order in points completed",  completeCodeOrderInPoints())
    step("nodes dragged down",              dragNodesDown())
  }
}
