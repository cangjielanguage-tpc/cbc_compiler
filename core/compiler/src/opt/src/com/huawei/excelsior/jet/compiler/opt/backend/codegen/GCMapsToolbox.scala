/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.abi.ABI.AltLocation
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.Immediate

trait GCMapsToolbox { self: Universe with BackEnd with CodeGenerator =>

  /** Calculates map from every point in IR which [[needGCMap]] to set of nodes which [[willBeCollectedInGCMap]] and live at this point. */
  protected def calcGCMaps(): collection.Map[Node, Set[Node]] = {
    val engine = new NodeLivenessEngine {
      override protected def valuesFilter(n: Node): Boolean = willBeCollectedInGCMap(n)

      override protected def processBlock(block: Block, output: Set[Node], updateLive: (Node, Set[Node]) => Unit): Set[Node] = {
        var curr = output
        for (node <- CodeOrder reversedIn block if !node.isInstanceOf[Phi]) {
          assert(node.isGroupRoot)
          curr &~= node.groupedValueResults.toSet
          node match {
            case sn: SpinalNode if sn.hasXHandler => curr ++= getLive(sn.xHandler)
            case _ =>
          }

          node match {
            case c: Call => {
              curr |= c.invokeArgs.filter(arg => arg.mayHaveResource && (arg.resource match {
                case fs: FrameSlot =>
                  // parameters passed on frame slots stay alive during the call
                  willBeCollectedInGCMap(arg)
                case _: AltLocation =>
                  assert(!willBeCollectedInGCMap(arg)) // only primitive parameters are currently passed on alt-locations
                  false // even when references are supported on alt-locs, they will be scanned via callee gcmaps
                case Immediate =>
                  // immediates can be presented, but should not be tracked by anyone
                  false
                case _: AnyReg =>
                  // parameters passed on registers should be tracked by callee gcmaps
                  false
                // no other parameters locations are expected, MatchError is intended
              })).toSet
            }
            case _ =>
          }

          if (needGCMap(node)) {
            updateLive(node, curr)
          }

          curr |= (node.groupedValueArgs filter valuesFilter).toSet
        }
        curr
      }
    }

    engine.calcLiveness()
    engine.live filter (x => needGCMap(x._1))
  }
}
