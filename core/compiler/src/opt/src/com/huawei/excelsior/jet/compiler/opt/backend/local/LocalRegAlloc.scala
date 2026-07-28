/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.local

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

/**
  * Local register allocation utilities.
  *
  * @author conwor
  */
trait LocalRegAlloc { self: Universe with BackEnd with LocalGenerator =>

  /**
    * Releasing cost of some resource is a type of transfer nodes, that should be generated,
    * if we want to take away this resource from it's value group.
    */
  type ReleasingCost = Int

  val ZERO_RELEASING_COST = 0             // We would not generate any transfers
  val MOV_RELEASING_COST = 1              // We would insert copy from another register or from immediate
  val REG_RELEASING_COST = 2              // We would give another register to this value
  val LOAD_RELEASING_COST = 3             // We would generate load
  val STORE_LOAD_RELEASING_COST = 4       // We would generate store and load
  val STORE_LOAD_LOAD_RELEASING_COST = 5  // We would store some other value (spill), load blocker and than load spilled value, if required

  trait LocalRegAllocImpl { self: LocalGeneratorImpl =>

    /**
      * Make transfer nodes, that moves value from given `from` node to one of given `to` resources.
      *
      * @param from node, that produces some value on itself resource.
      * @param to set of allowed results of transfers chain.
      * @return sequence of transfers.
      */
    private def makeTransfers(from: Node, to: ResourceSet): Seq[Node] = {
      val fromKind = resourceKind(from.resource)

      // Find subset of `to` whereto `from` may be transferred without temporals
      val efficientSubSet = to filter { to =>
        temporaryResourcesForTransfer(resourceKind(to), fromKind, from).isEmpty
      }

      if (efficientSubSet.nonEmpty) {
        // Fast path
        Seq(Copy.withoutValue(from, efficientSubSet))

      } else {
        // Unoptimized slow path. Between `to` there may be resources with different kinds. Feel free to factorize
        // `to` set by resourceKind and select most efficient way to transfer.
        val toHeadKind = resourceKind(to.head)
        val temporals = temporaryResourcesForTransfer(toHeadKind, fromKind, from).get
        val t = Copy.withoutValue(from, temporals)
        Seq(Copy.withoutValue(t, to.filter(r => resourceKind(r) == toHeadKind)))
      }
    }

    /**
      * Appends transfers, that move value from currently available resources to given `allowed` resources.
      *
      * @param allowed allowed results for transfers.
      * @return sequence of appended transfers.
      */
    def moveValue(value: Value, allowed: ResourceSet, generate: Boolean): Seq[Node] = {
      val valueRegisters = state.registers(value)
      val valueSlots = state.frameSlots(value)
      val valueAltLocations = state.altLocations(value)

      val valueProducer = value.producer

      val cheapestResource = {
        if (valueRegisters.nonEmpty) valueRegisters.head
        else if (valueProducer.isInstanceOf[Constant]) Immediate
        else if (valueSlots.nonEmpty) valueSlots.head
        else valueAltLocations.head
      }

      val transfers = makeTransfers(state.getNode(value, cheapestResource), allowed)
      if (generate) {
        assert(allowed.isSingleton && (transfers.size == 1))
        generateNode(transfers.head, Seq(allowed.single), Seq.empty)
      }
      transfers
    }

    /** Releases given `resource` with using of given `freeReg`.
      *
      * Returns true iff value from `resource` was moved exactly to `freeReg`.
      */
    def releaseResource(value: Value, resource: Resource, busyRegs: ResourceSet, freeReg: Resource, spillReg: Resource): Boolean = {
      val result = releasingCost(value, resource, busyRegs, haveFreeRegs = freeReg != null) match {
        case ZERO_RELEASING_COST | MOV_RELEASING_COST | LOAD_RELEASING_COST =>
          false

        case REG_RELEASING_COST =>
          moveValue(value, setOf(freeReg), generate = true)
          true

        case STORE_LOAD_RELEASING_COST =>
          moveValue(value, setOf(newSpillSlotUsedAsWorkaroundFor15742(value)), generate = true)
          false

        case STORE_LOAD_LOAD_RELEASING_COST =>
          assert(spillReg != null)
          assert(state.contains(spillReg))
          val spillRegValue = valueOf(state(spillReg))

          releasingCost(spillRegValue, spillReg, busyRegs, haveFreeRegs = false) match {
            case ZERO_RELEASING_COST | MOV_RELEASING_COST | LOAD_RELEASING_COST => // nothing to do, spillReg may be taken from spillRegValue without any harm
            case REG_RELEASING_COST => shouldNotReachHere() // Free regs in STORE_LOAD_LOAD_RELEASING_COST, really?
            case _ => moveValue(spillRegValue, setOf(newSpillSlotUsedAsWorkaroundFor15742(spillRegValue)), generate = true)
          }

          moveValue(value, setOf(spillReg), generate = true)
          false
      }

      removeSynonyms(state.getNode(value, resource), block)

      result
    }

    /**
      * Returns releasing cost of given `resource`.
      *
      * @param resource checked resource
      * @param busyRegs registers, that could not be used as storage for releasing resource
      * @param haveFreeRegs whether there are free registers the same type as `resource`
      * @return releasing cost of `resource`
      */
    def releasingCost(value: Value, resource: Resource, busyRegs: ResourceSet, haveFreeRegs: Boolean): ReleasingCost = {
      val exclude = busyRegs + resource

      // 1. Value occupy any other register
      if (!(state.registers(value) subsetOf exclude)) {
        if (state.hasRemainingUsesOnFixedResource(value, resource)) {
          return MOV_RELEASING_COST
        } else {
          // On CBC non-volatile registers are considered more expensive to release,
          // as they are used in most of instructions now. Otherwise the releasing loop can occur.
          //
          // Let's assume we have all non-volatile registers busy, and we have to release one of them for given value (V1) from volatile resource,
          // and there is some other value (V2) that have both volatile and non-volatile allocated resources, and it has common use in this point with given value.
          // If we do not check whether resource is volatile, the releasing costs for both of them are equal, so we take the preferred non-volatile resource from V2,
          // and on the next round of arguments normalization the same situation will be for V2.
          if (targetArch == CBC && resource.isReg && !rootABI.isVolatile(resource.asReg) &&
              (state.registers(value) &~ exclude).forall(r => rootABI.isVolatile(r.asReg))) {
            return MOV_RELEASING_COST
          }
          return ZERO_RELEASING_COST
        }
      }

      // 2. Value is immediate, it may be loaded to register any time
      if (value.producer.isInstanceOf[Constant]) {
        return MOV_RELEASING_COST
      }

      // 3. If there are free registers, we may move value to one of them
      if (haveFreeRegs) {
        return REG_RELEASING_COST
      }

      // 4. Value occupy any unique (not param) frame slot
      if (!(state.frameSlots(value) subsetOf exclude)) {
        if (state.frameSlots(value) exists (x => !exclude(x) && x.asInstanceOf[FrameSlot].kind != FrameSlot.CallParam)) {
          return LOAD_RELEASING_COST
        }

        // 5. Value will die at the nearest call point, so it may live on param frame slot
        val point = dag.nextPoint
        if (point.isInstanceOf[Call] && state.remainingUses(value).forall(_ == point)) {
          // If there is only one live node use (in some call) and it is the next DAG point,
          // node may live at param FrameSlot without any problems.
          return LOAD_RELEASING_COST
        }
      }

      // 6. Spill case
      if (state.resources(value) exists (!_.isInstanceOf[FrameSlot])) {
      // state.registers(value).nonEmpty ? What about Immediate in state.resources(value) ? ask @conwor
        STORE_LOAD_RELEASING_COST
      } else {
        STORE_LOAD_LOAD_RELEASING_COST
      }
    }
  }
}
