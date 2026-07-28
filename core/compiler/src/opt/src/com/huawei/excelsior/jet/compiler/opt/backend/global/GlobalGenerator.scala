/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.global

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env.tailRegister
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder.LoopOrientation
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.BulldozerGCM
import com.huawei.excelsior.jet.compiler.opt.backend.fast.FastCodeOrdering
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, MachineDescription}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{FrameSlot, Immediate, Resource}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Worklist
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder

import scala.collection.mutable.ArrayBuffer

/** Registers allocation, final spill decisions and final code ordering framework. It uses pre-order and spill hints
  * from [[BulldozerGCM]] or [[FastCodeOrdering]] as a base spill and code order decisions, but correct them to achieve
  * all [[MachineDescription]] requirements for target platform.
  *
  * Used as a second main phase in optimizing [[BackEnd]] pipeline.
  *
  * @author conwor
  */
trait GlobalGenerator extends ImplicitChecksOptimizer with GeneratorUtilities with GenState with Values { self: Universe with BackEnd =>

  class GlobalGeneratorImpl extends AbstractInterpreter {

    type State = GeneratorState

    override protected def makeProxy(tpe: Type, actual: Node, block: Block) = makeSynonymProxy(actual, block)

    override protected def resolve(src: Node, dst: Node): Unit = {
      assert(valueOf(src) == valueOf(dst)) // This is all about synonyms

      src match {
        case proxy: Proxy =>
          // `src` is a proxy node that was created when AI came to block B with proxy state.
          // `dst` is a phi node that was created when AI has processed all B's predecessors and merged their states.
          assert(!dst.generated)
          assert(dst.isInstanceOf[Phi])
          assert(dst.block == src.block)

          dst.resource = proxy.resource
          proxy.replaceInCodeGenOrderTo(dst)
          replaceTransitively(proxy, dst)

        case _: Phi if src == dst =>
        case _ => shouldNotReachHere()
      }
    }

    override protected def startInputState(b: Block): State = {
      val state = new GeneratorState()
      for (param <- all[Param]) {
        state.add(param, paramLocation(param))
      }
      for (tail <- TailPointer.unique) {
        state.add(tail, tailRegister)
      }
      state
    }

    override protected def interpret(block: Block, state: State): Block = {
      new LocalGeneratorImpl(block, state, this).generateBlock()
      fixConstraintsForOtherPredecessors(block, state)
      block
    }

    override lazy val blocksOrder = {
      val order = NaturalCFGOrder(cfg, LoopOrientation.HEADER_FIRST)

      // For better register allocation in hot code, we want for each hot phi-point to have it's any hot predecessor
      // first in blocks ordering. We achieve this by moving cold predecessors later in order. We can move each
      // predecessor of phi point in range [predecessor, phi-point) because critical edges are splitted.

      val cold = findColdBlocks()
      val ws = Worklist.from(order.reverseIterator)
      val result = new ArrayBuffer[Block]
      for (block <- ws.drain) {
        result += block
        if (block.predBlocks.size > 1 && !cold(block)) {
          for (pred <- block.predBlocks if cold(pred) && ws.contains(pred)) {
            ws.remove(pred)
            result += pred
          }
        }
      }

      result.reverse
    }

    private val alreadyFixedBlocks = Sets[Node].newMSet

    /**
     * When some block has more than one predecessors, and first of them has been generated, it should produce
     * some constraints for other predecessors. This functions calculates this constraints, based on generated
     * predecessor output state and changed constraints.
     */
    private def fixConstraintsForOtherPredecessors(block: Block, state: State): Unit =  {
      import com.huawei.excelsior.jet.util.ScalaCollections.{singleElement, singleton}
      for (succ <- singleton(block.succBlocks) if !alreadyFixedBlocks(succ)) {
        alreadyFixedBlocks += succ
        val be = singleElement(block.succBlockEdges)
        val blockConstraints = block.blockEnd.constraints

        val phiByArg = (succ.phies map { phi => (phi.phiArg(be), phi) }).toMap

        if (!isO1Compiled && bGCMHints.loopEnterRequirements.contains(block)) {
          // We will patch constraints and liveness to avoid hardcore in GenState#mergeFrom function.
          // This code should be carefully reviewed and some better solution may be found.

          // Requirements are created now only for reducible & normalized loops, thus single forward edge source
          // dominates loop header and we can use all resources footprint from it's state to satisfy requirements.
          assert(block dominates succ)
          assert(succ.inEdges.size == 2)
          val backwardBranch = (succ.inEdges find { _ != be }).get
          val requirements = bGCMHints.loopEnterRequirements(block)
          val backwardConstraints = backwardBranch.source.block.blockEnd.constraints
          val succLiveness = liveness.in(succ)

          for (node <- liveness.out(block)) {
            assert(blockConstraints.contains(node))
            val delegateResource = blockConstraints.delegate(node).resource

            if (phiByArg.contains(node)) {
              val phi = phiByArg(node)
              val peer = phi.phiArg(backwardBranch)
              assert(requirements.contains(phi)) // All header phi-functions should be enumerated in requirements.

              assert(requirements.shouldBeInStorage(phi) == !requirements.shouldBeOnRegister(phi)) // Check requirements consistency

              if (requirements.shouldBeInStorage(phi)) {
                assert(delegateResource.isInstanceOf[FrameSlot]) // It cannot be immediate or register
              } else {
                // assert(delegateResource.isReg)
                // otherwise requirements are not satisfied. This means that this phi-function argument was spilled
                // no matter what BGCM wanted to. Additional hints required.
                // TODO: debug and make this shouldNotReachHere
              }

              backwardConstraints.setResource(peer, delegateResource)

            } else {

              def appendSynonymToConstraints(resource: Option[Resource]): Unit = {
                resource match {
                  case Some(resource) =>
                    // Append another node to constraints and liveness. It works because constraints do not use backend
                    // Values to differentiate stored keys, and all clients of Constraints (local generator & global
                    // states merge) are ok with this hack. This is dirty hack and it should be replaced with something else.
                    val delegate = state(resource)
                    backwardConstraints.addWithResource(delegate, resource)
                    blockConstraints.addWithResource(delegate, resource)
                    succLiveness.add(delegate)
                  case None =>
                  // Requirements are not satisfied, even with BGCM hints.
                  // TODO: debug and make this shouldNotReachHere
                }
              }

              backwardConstraints.setResource(node, delegateResource)

              (requirements.shouldBeOnRegister(node), requirements.shouldBeInStorage(node)) match {
                case (true, false) =>
                  if (delegateResource.isInstanceOf[FrameSlot]) {
                    appendSynonymToConstraints(state.registers(valueOf(node)).headOption)
                  }

                case (false, true) =>
                  assert(!delegateResource.isReg) // Based on SpillAssert hint inserted in BGCM

                case (true, true) =>
                  val anotherResource = delegateResource match {
                    case _: FrameSlot =>
                      assert(!node.isInstanceOf[Constant])
                      state.registers(valueOf(node)).headOption
                    case Immediate =>
                      assert(node.isInstanceOf[Constant])
                      state.registers(valueOf(node)).headOption
                    case _ =>
                      assert(delegateResource.isReg)
                      assert(!node.isInstanceOf[Constant])
                      Some(state.frameSlots(valueOf(node)).head)
                  }

                  appendSynonymToConstraints(anotherResource)

                case (false, false) =>
                  // Simple case for loop outsiders we do not care which resources will be on
              }
            }
          }

        } else {
          for (pe <- succ.inEdges if pe != be) {
            val pc = pe.source.block.blockEnd.constraints
            blockConstraints foreach { n =>
              val peer = phiByArg.get(n) map (_.phiArg(pe)) getOrElse n
              pc.setResource(peer, blockConstraints.delegate(n).resource)
            }
          }
        }
      }
    }
  }

}
