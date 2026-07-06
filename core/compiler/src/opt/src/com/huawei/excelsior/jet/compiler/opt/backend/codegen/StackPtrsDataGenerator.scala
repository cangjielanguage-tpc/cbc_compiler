/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.codegen

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{FrameSlot, Immediate, Resource}
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot

trait StackPtrsDataGenerator { self: Universe with BackEnd with CodeGenerator =>

  /** Returns true iff `node` resource contains pointer to stack. */
  private def containsStackPtr(n: Node): Boolean = (valueOf(n).producer match {
    case p: Param => 
      (rootMethod.hasRetByValParameter && p.num == rootMethod.getRetByValArgIdx) 
      || p.formalType.isRecord
    case _: StackAlloc => !n.isInstanceOf[StackAlloc] // producer is StackAlloc but not the node itself
    case _ => false
  }) && !n.resource.isInstanceOf[TailSlot]

  def needStackPtrsInfo(n: Node): Boolean = needGCMap(n)

  /** Calculates map from every point in IR which [[needStackPtrsInfo]] to set of nodes which [[containsStackPtr]] and live at this point. */
  protected def stackPointers(): collection.Map[Node, Set[Node]] = {
    val engine = new NodeLivenessEngine {
      override protected def valuesFilter(n: Node): Boolean = containsStackPtr(n)

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
            case c: Call =>
              curr |= c.invokeArgs.filter(arg => arg.mayHaveResource && (arg.resource match {
                case fs: FrameSlot => containsStackPtr(arg)
                case Immediate => false
                case _: AnyReg => false
              })).toSet
            case _ =>
          }

          if (needStackPtrsInfo(node)) {
            updateLive(node, curr)
          }

          curr |= (node.groupedValueArgs filter valuesFilter).toSet
        }
        curr
      }
    }

    engine.calcLiveness()
    engine.live filter (x => needStackPtrsInfo(x._1))
  }

}
