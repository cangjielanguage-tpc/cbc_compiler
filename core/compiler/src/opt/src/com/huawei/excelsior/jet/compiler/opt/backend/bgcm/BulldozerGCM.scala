/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm

import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.UAI
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.preferred.Preferred
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.ResourceSet
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.LoopsNormalizer
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement
import com.huawei.excelsior.jet.compiler.util.Sets

import scala.collection.mutable.ArrayBuffer

/** `Bulldozer` - register pressure sensitive global code motion framework with rematerialization,
  * spill hints and preferred registers calculation.
  *
  * Used as a first main phase in optimizing [[BackEnd]] pipeline.
  *
  * @author conwor
  * @author paul
  */
trait BulldozerGCM extends UAI with Preferred with LoopsNormalizer { self: Universe with BackEnd =>

  class LoopEnterRequirements {
    val storedNodes: Sets[Node]#QSet = Sets[Node].newQSet
    val registerNodes: Sets[Node]#QSet = Sets[Node].newQSet

    def append(node: Node, toStorage: Boolean, toRegisters: Boolean): Unit = {
      assert(!contains(node))
      assert(toStorage || toRegisters)
      if (toStorage) storedNodes += node
      if (toRegisters) registerNodes += node
    }

    def contains(node: Node): Boolean =
      storedNodes(node) || registerNodes(node)

    def shouldBeInStorage(node: Node): Boolean = storedNodes(node)
    def shouldBeOnRegister(node: Node): Boolean = registerNodes(node)
  }

  case class BGCMHints(preferredLocs: collection.Map[Node, ResourceSet],
                       unPreferredLocs: collection.Map[Node, ResourceSet],
                       loopEnterRequirements: collection.Map[Block, LoopEnterRequirements],
                       spillHintsOnEdges: collection.Set[Edge],
                       maxSpillPressure: Int)

  /** Each loop exit have control successors and may have exceptional handler. Loop continuation may be any one
    * (or several) of them, but bulldozer is ready to handle only control successors. For exceptional handler
    * continuation there is workaround now. This workaround await that loop exit will have only one control
    * successor from outer loop, which has only one predecessor.
    *
    * This method provides such invariant.
    * TODO: rewrite workaround and remove this method.
    */
  private def normalizeLoopExits(): Boolean = {
    val loops = cfg.loops
    var changed = false

    def canonicalize(exit: Block): Unit = {
      assert(exit.isInstanceOf[BBlock])
      assert(exit.handledXPoints.size == 1)
      assert(loops.inSameLoop(exit.singleXHandlerOrNull, exit))

      Block.splitBefore(exit.blockEnd)
      changed = true
    }

    for (loop <- loops; exit <- loop.exits) {
      exit.blockEnd match {
        case Goto(_, target) if (target.predBlocks.size > 1) && !loop.body(target) => canonicalize(exit)
        case branch: Branch if branch.exits forall { e => !loop.body(e.singleUse.block) } => canonicalize(exit)
        case _ =>
      }
    }
    changed
  }

  protected def insertSpoiledArgsSavers(node: Node): ArrayBuffer[Node] = {
    val savers = new ArrayBuffer[Node]
    for (case edge @ Edge(arg0, _) <- node.groupedValueInEdges if argumentShouldBeSaved(edge)) {
      val point = node match {
        case node: FloatingNode => node.lowerPoint
        case node: PinnedNode => node.point.asInstanceOf[LowerPoint]
      }
      val arg1 = arg0 match {
        case arg: SpoiledArgSaver => arg.arg
        case _ => arg0
      }
      val arg2 = temporaryResourcesForIntermediateCopy(arg1) match {
        case Some(temporals) => Copy.withoutValue(arg1, temporals) atLowerPoint point
        case None => arg1
      }
      val saver = SpoiledArgSaver(arg2) atLowerPoint point
      edge.source = saver
      savers += saver
    }
    savers
  }

  private def insertSpoiledArgsSavers(): Unit = {
    allNodes filter { _.isGroupRoot } foreach insertSpoiledArgsSavers
  }

  def doBulldozerGCM(): BGCMHints = {
    step("loops normalized",                      normalizeAllLoops())
    step("loop exits normalized",                 normalizeLoopExits())

    val engine = new GCMEngine(onlyEarly = true)
    step("first pass of BGCM, nodes located as high as possible", engine.schedule())
    step("some nodes dragged to their single uses",               dragNodesToSingleUse(fromFastCodeOrdering = false)) // Fragile pointers will be lazy rematerialized
    step("nodes dragged into cold code",                          engine.dragNodesIntoColdCode())
    step("spoiled arguments savers inserted",                     insertSpoiledArgsSavers())

    val bulldozerInterpreter = new UpwardAI(engine)
    step("second pass of BGCM passed", bulldozerInterpreter.iterate())

    // Verify that node points are in consistency with code order
    for (block <- all[Block]) {
      var upperPoint: UpperPoint = block
      for (node <- CodeOrder in block) {
        node match {
          case node: UpperPoint => upperPoint = node
          case node: FloatingNode => assert(node.upperPoint == upperPoint)
          case _ =>
        }
      }
    }

    // Verify that SpoiledArgSaver nodes consistency where not damaged by rematerialization
    assert(all[SpoiledArgSaver] forall { sas => singleElement(sas.uses).block == sas.block })

    val preferredUAI = new PreferredUAI(engine)
    preferredUAI.iterate()
    dbgPrinter.debugNodes("Third pass of BGCM, preferred registers calculated", info = preferredUAI.debugInfo)

    BGCMHints(preferredUAI.preferred,
      preferredUAI.unPreferred,
      bulldozerInterpreter.loopEnterRequirements,
      bulldozerInterpreter.spillHintsOnEdges,
      bulldozerInterpreter.maxSpillPressure)
  }
}
