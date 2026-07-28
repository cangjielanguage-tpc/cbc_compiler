/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.local.nodegenoptions

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections.OrderedEnum
import com.huawei.excelsior.jet.compiler.util.Sets

/**
 * NodeGenOptions is a variant of code generation step.
 *
 * @author conwor
 * @author ikireev
 */
trait NodeGenOptionsComponent extends SimpleNodeGenOptions with ComplexNodeGenOptions { self: Universe with BackEnd =>

  trait NodeGenOptionsImpl extends SimpleNodeGenOptionsImpl with ComplexNodeGenOptionsImpl { self: LocalGeneratorImpl =>

    /** Allocation choice. */
    sealed abstract class Allocation {
      def isBlocked: Boolean
      def resource: Resource
      def touchedResources: ResourceSet
    }

    /** Unblocked allocation with lazy computation of final resource from available set. */
    class FreeAllocation(available: ResourceSet, target: Node) extends Allocation {
      def isBlocked: Boolean = false
      lazy val resource: Resource = selectBest(available, target)
      def touchedResources: ResourceSet = setOf(resource)
    }

    /** Allocation with blocked resource. */
    case class BlockedAllocation(resource: Resource,
                                 blocker: Node,
                                 cost: ReleasingCost,
                                 commonUses: CommonUsesType,
                                 freeReg: Resource,
                                 spillReg: Resource,
                                 target: Node) extends Allocation {
      def isBlocked: Boolean = true
      def touchedResources: ResourceSet = if (freeReg == null) setOf(resource) else setOf(resource, freeReg)

      def compare(that: BlockedAllocation): Boolean = {
        if (this.cost == ZERO_RELEASING_COST) {
          true
        } else if (that.cost == ZERO_RELEASING_COST) {
          false
        } else if (this.commonUses != that.commonUses) { // otherwise choose within the criteria tree rules
          this.commonUses < that.commonUses
        } else {
          this.cost <= that.cost
        }
      }
    }

    case class EmptyAllocation() extends Allocation {
      def isBlocked: Boolean = true
      def resource: Resource = shouldNotCallThis()
      def touchedResources: ResourceSet = emptySet
    }

    /** Relation between blocker node and current generating node. */
    enum CommonUsesType extends OrderedEnum[CommonUsesType]:
      case NO_COMMON_USES, HAS_COMMON_USES, HAS_COMMON_USES_IN_THIS_POINT

    import CommonUsesType.{valueOf as _, *} // import all except `valueOf`

    object NodeGenOptions {
      def apply(node: Node, normalized: Boolean): NodeGenOptions = {
        assert(node.isGroupRoot)

        node match {
          case _: Call                                => new CallGenOptions(node, normalized)
          case _ if hasSpoiledRegisters(node)         => new ComplexNodeGenOptions(node, normalized)
          case _ if node.groupedValueResults.size > 1 => new ComplexNodeGenOptions(node, normalized)
          case _                                      => new SimpleNodeGenOptions(node, normalized)
        }
      }
    }


    abstract class NodeGenOptions(val node: Node, val normalized: Boolean) {

      /////////////////////////////////////////////////////////////////////////////////////////////
      //// Main public API: sequences of allocation choices for results and spoiled

      def results: Seq[Allocation]
      def spoiled: Seq[Allocation]


      /////////////////////////////////////////////////////////////////////////////////////////////
      //// Secondary options - RP effect, releasing characteristics, e.t.c.

      /** Set of registers, that will be free after this node generation. */
      private val freeAfterGen: MutableResourceSet = {
        val result = emptyMSet()
        if (!isSynonym(node)) {
          for (edge <- node.groupedValueInEdges; value = valueOf(edge.source)) {
            // Even if edge marked with spill hint by BGCM, its source value may be not stored yet,
            // because its storing provided by StoreHint, inserted by BGCM above `node`, BUT backend may
            // look at `node` (and calculate its options) before this hint. Thus we should check that
            // value if already stored.
            // TODO: consider to use more strict code ordering in backend after BGCM
            val spilledAnyway = !isO1Compiled && bGCMHints.spillHintsOnEdges(edge) && state.savedInStorage(value)

            val valueRegisters = state.registers(value)

            val willBeFree = if (state.remainOneUse(value) || spilledAnyway) valueRegisters else {
              val withoutUniqueUses = valueRegisters.filter { r => !state.hasRemainingUsesOnFixedResource(value, r, except = node) }
              if (withoutUniqueUses.nonEmpty && withoutUniqueUses.size == valueRegisters.size) {
                withoutUniqueUses -= withoutUniqueUses.head // TODO: implement more intelligent choice
              }
              withoutUniqueUses
            }

            result |= willBeFree
          }
        }
        result
      }

      /** Difference between negative and positive RP effects.
        * Negative RP effect is a count of registers, that should be busy after this node generation.
        * Positive RP effect is a count of registers, that would be free after this node generation.
        */
      val rpEffect: Int = {
        // TODO: implement more common heuristic
        val negativeRPEffect = node match {
          case st: Copy if st.isStore => 0
          case _: Constant => 0
          case _ if hasValue(node) && state.live(node) => 1
          case _ => 0
        }

        negativeRPEffect - freeAfterGen.size
      }

      lazy val (
        /** Node has allocation that cannot be placed on required resources as they are busy by living nodes. */
        isBlocked,

        /** Node unblocking isn't possible without moving blocker node that has common use with this node. */
        isReleasedWithCommonUse,

        /** Node unblocking isn't possible without moving blocker node that has common use with this node in the next DAG point. */
        isReleasedWithCommonUseInThisPoint,

        /** Node unblocking isn't possible without moving blocker node to frame slot. */
        isReleasedWithSpill) =

      {
        var isBlocked = false
        var isReleasedWithCommonUse = false
        var isReleasedWithCommonUseInThisPoint = false
        var isReleasedWithSpill = false

        for (allocation <- results ++ spoiled if allocation.isBlocked) {
          isBlocked = true
          allocation match {
            case BlockedAllocation(_, _, cost, commonUses, _, _, _) =>
              if (cost > ZERO_RELEASING_COST) {
                isReleasedWithCommonUse |= commonUses >= HAS_COMMON_USES
                isReleasedWithCommonUseInThisPoint |= commonUses >= HAS_COMMON_USES_IN_THIS_POINT
                isReleasedWithSpill |= cost >= LOAD_RELEASING_COST
              }

            case EmptyAllocation() => // simulate worst case
              isReleasedWithCommonUse = true
              isReleasedWithCommonUseInThisPoint = true
              isReleasedWithSpill = true

            case _ => shouldNotReachHere()
          }
        }

        (isBlocked, isReleasedWithCommonUse, isReleasedWithCommonUseInThisPoint, isReleasedWithSpill)
      }


      /////////////////////////////////////////////////////////////////////////////////////////////
      //// Selection procedures

      /** Returns releasing options (cost and details) for `resource`. `target` is a node result, which
        * will occupy `resource`. `untouchable` is a set of resources, which cannot be used for releasing.
        */
      protected def estimateReleasing(resource: Resource, target: Node, untouchable: ResourceSet): BlockedAllocation = {
        assert(state contains resource, "why trying to release free resource?")
        val blocker = state(resource)

        val freeRegs = (if (blocker.isFP) state.freeFRegs else state.freeIRegs) &~ untouchable
        val cost = releasingCost(valueOf(blocker), resource, untouchable, freeRegs.nonEmpty)
        val freeReg = if (cost == REG_RELEASING_COST) freeRegs.head else null

        val spillReg = if (cost != STORE_LOAD_LOAD_RELEASING_COST) {
          null
        } else {
          target match {
            case st: Copy if st.isStore => (resRegs(blocker) - st.arg.resource).head
            case _ => shouldNotReachHere()
          }
        }

        val commonUses = {
          val uses = if (hasValue(blocker) && (target != null) && hasValue(target)) {
            val nodeLiveUses = state.remainingUses(valueOf(target))
            val blockerLiveUses = state.remainingUses(valueOf(blocker))
            blockerLiveUses.filter(use => !use.isInstanceOf[Constraints] && nodeLiveUses(use))
          } else {
            Iterator.empty
          }
          if (uses.iterator.nonEmpty) {
            if (uses.iterator.exists(lowerPoint(_) == dag.nextPoint)) {
              HAS_COMMON_USES_IN_THIS_POINT
            } else {
              HAS_COMMON_USES
            }
          } else {
            NO_COMMON_USES
          }
        }

        BlockedAllocation(resource, blocker, cost, commonUses, freeReg, spillReg, target)
      }

      /** Returns best Allocation, selected from `candidates` with `untouchable` set of resources which cannot
        * be used for releasing blockers and optional heuristics handler `target` - node itself or one of it's
        * attached results. */
      protected def select(candidates: ResourceSet, target: Node, untouchable: ResourceSet): Allocation = {
        val available = candidates filter {
          case fs: FrameSlot if fs.kind == FrameSlot.CallParam => true // TODO: what is this???
          case r => !state.contains(r) || freeAfterGen(r)
        }

        if (available.nonEmpty) {
          new FreeAllocation(available, target)

        } else if (candidates.nonEmpty) {
          val choices = candidates.asSeq map { r => estimateReleasing(r, target, untouchable) }
          choices.reduce[BlockedAllocation]((b1, b2) => if (b1.compare(b2)) b1 else b2)

        } else {
          assert(!normalized)
          EmptyAllocation()
        }
      }
    }

    /** Returns best resource from `available` set to be occupied by `target`. */
    protected def selectBest(available: ResourceSet, target: Node): Resource = {
      if (available.isSingleton || target == null) {
        // no opportunity or information for heuristics
        return available.head
      }

      val value = valueOf(target)

      val bgcmSet = if (!isO1Compiled) {
        (bGCMHints.preferredLocs.get(value.producer), bGCMHints.unPreferredLocs.get(value.producer)) match {
          case (None, None) => available

          case (Some(preferred), None) =>
            val intersection = available & preferred
            if (intersection.nonEmpty) intersection else available

          case (None, Some(unPreferred)) =>
            val exclusion = available &~ unPreferred
            if (exclusion.nonEmpty) exclusion else available

          case (Some(preferred), Some(unPreferred)) =>
            val best = (available & preferred) &~ unPreferred
            if (best.nonEmpty) best else {
              val positive = available & preferred
              if (positive.nonEmpty) positive else {
                val negative = available &~ unPreferred
                if (negative.nonEmpty) negative else available
              }
            }
        }
        // We do not select .head immediately, because local backend heuristics may take into account uses,
        // created after BGCM. For example (and maybe the only one example) - fixed constraints.
        // TODO - fix this and remove the whole code below.
      } else {
        available
      }

      // Workaround for nodes, that has register (variable) result allocation case, but has not local value.
      // Example is Catch node, which required only for it's memory result.
      // TODO: there should not be such nodes in backend.
      if (!state.live(value)) return bgcmSet.head //TODO: kill this after ijorch's exceptions arrive to trunk

      val intersection = bgcmSet & state.remainingRequiredResources(value)
      if (intersection.nonEmpty) intersection.head else bgcmSet.head
    }
  }
}
