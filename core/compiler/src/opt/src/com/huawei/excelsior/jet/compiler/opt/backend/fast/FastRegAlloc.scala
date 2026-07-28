/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.fast

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.Env.tailRegister
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.BulldozerGCM
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, MachineDescription}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.{Immediate, InvalidResource, MutableResourceSet, Resource, ResourceSet, emptyMSet, setOf, unionOf, universalSet}
import com.huawei.excelsior.jet.compiler.opt.ir.{Resources, Universe}
import com.huawei.excelsior.jet.util.ScalaCollections.{singleElement, singleton, uniqueValue}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Registers allocation and spill decisions framework. It uses pre-order from [[BulldozerGCM]] or [[FastCodeOrdering]]
  * as a final code order, completing it with transfer nodes and spill decisions to achieve all [[MachineDescription]]
  * requirements for target platform.
  *
  * Used as a second main phase in fast [[BackEnd]] pipeline.
  *
  * @author conwor
  */
trait FastRegAlloc { self: Universe with BackEnd =>

  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////
  // Common methods

  /** Returns part of edges, which should be allocated and normalized for `node` allocation.
    * This part is collected from own arguments of `node`.
    */
  private def ownInEdges(node: Node): Seq[Edge] = node.groupedValueInEdges.toList

  /** Returns part of edges, which should be allocated and normalized for `node` allocation.
    * This part is collected from constraints of `node`.
    */
  private def constrainedInEdges(node: Node): Seq[Edge] = node match {
    case node: LowerPoint if node.hasConstraints => node.constraints.groupedValueInEdges.toList
    case _ => List.empty
  }

  /** Returns constraints by `edge`. */
  private def constraints(edge: Edge): Constraints = edge.source match {
    case xPoint: XPoint => xPoint.owner.constraints
    case exit: BlockExit => exit.block.blockEnd.constraints
  }


  /////////////////////////////////////////////////////////////////////////////
  // Machine state classes

  /** State of some [[Value]] in current moment:
    *   - `resources` - set of resources occupied by value producer and synonyms.
    *   - `remainingUses` - counter of not generated yet uses of value in current block.
    */
  private class ValueState(val resources: MutableResourceSet = emptyMSet(), var remainingUses: Int = 0)

  /** State of the whole machine in current moment:
    *   - `content` - map from resource to node which occupy it right now.
    *   - `liveValues` - map from value to its local state (set of its resources and counter of remaining uses).
    */
  private class MachineState(block: Block) {
    private val content = Maps[Resource].newQMap[Node]
    private val liveValues = Maps[Value].newQMap[ValueState]

    for (node <- CodeOrder.in(block) if !node.isInstanceOf[Phi]; edge <- ownInEdges(node) ++ constrainedInEdges(node)) {
      state(edge.source).remainingUses += 1
    }

    def apply(resource: Resource): Node = content(resource)
    def get(resource: Resource): Option[Node] = content.get(resource)
    def valueOn(resource: Resource): Option[Value] = get(resource) map valueOf
    def busy(resource: Resource): Boolean = content.contains(resource)

    def state(value: Value): ValueState = liveValues.getOrElseUpdate(value, {
      val state = new ValueState()
      if (value.producer.isInstanceOf[Constant]) state.resources += Immediate
      state
    })

    def state(node: Node): ValueState = state(valueOf(node))

    def freeIRegs = allIRegsSet -- content.keysIterator
    def freeFRegs = allFRegsSet -- content.keysIterator
    def freeRegFor(node: Node): ResourceSet = regFileOf(node) match {
      case IREG => freeIRegs // TODO: why not use all reg files right here and not create job for FP slots recolorer?
      case FREG => freeFRegs
      case null => shouldNotReachHere(s"$node")
    }
    def freeRegFor(value: Value): ResourceSet = freeRegFor(value.producer)

    def resources(value: Value): MutableResourceSet = liveValues(value).resources

    /** Returns node contained `value` and occupied `resource`. */
    def nodeOn(resource: Resource, value: Value): Node = resource match {
      case Immediate => value.producer ensuring (_.isInstanceOf[Constant])
      case _ => apply(resource) ensuring (n => valueOf(n) == value)
    }

    /** Spoils `resource`. If it is occupied right now, updates state of occupier value. */
    def spoil(resource: Resource): Unit = for (node <- content.get(resource)) {
      (state(node).resources -= resource) ensuring (_.nonEmpty)
      content -= resource
    }

    /** Allocates `node` to `resource` and saves it in state if resource is shareable. */
    def update(resource: Resource, node: Node): Unit = {
      spoil(resource)
      node.resource = resource
      if (node.producesValue && (resource != InvalidResource)) {
        state(node).resources += resource
        if (resource != Immediate) {
          content(resource) = node
        }
      }
    }

    /** Checks if `value` has remaining uses and removes it from `this` state if there are no one. Returns true iff `value` was removed. */
    def tryToRemove(value: Value): Boolean = liveValues.get(value) match {
      case Some(s) if s.remainingUses == 0 =>
        content --= s.resources.iterator
        liveValues -= value
        true

      case _ => false
    }

    /** Checks if `node` value has remaining uses and removes it from `this` state if there are no one. Returns true iff `node` value was removed. */
    def tryToRemove(node: Node): Boolean = tryToRemove(valueOf(node))

    /** Decrease remaining uses counters for `edges` sources and try to remove them from state if it was their last use. */
    def decreaseUses(edges: Seq[Edge]): Unit = {
      for (edge <- edges; argValue = valueOf(edge.source)) {
        state(argValue).remainingUses -= 1
        tryToRemove(argValue)
      }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Block interpreter (local resources allocator)

  private class BlockInterpreter(block: Block, machine: MachineState) {

    /////////////////////////////////////////////////////////////////////////////
    // Current step

    /** Current generated node. */
    private var node: Node = _

    /** Set of resources fixed for `node` arguments, results and spoiled. Used to exclude from new resources selection. */
    private var untouchable: MutableResourceSet = _


    /////////////////////////////////////////////////////////////////////////////
    // Select utilities

    /** Selects any resource from `allowed` set where `value` already presented and may be used (not `untouchable`). */
    private def selectExistedResourceFor(value: Value, allowed: ResourceSet = universalSet): Option[Resource] = {
      val existedResources = machine.resources(value) & allowed
      (value.producer match {
        case _: ExecEnv => existedResources.asImmutable.ensuring(_ == eeIRegSet)
        case _ => existedResources &~ untouchable
      }).headOption
    }

    /** Selects any free resource from `allowed` set where `value` may be moved to and be used (not `untouchable`). */
    private def selectNewResourceFor(value: Value, allowed: ResourceSet = universalSet): Resource = {
      if (allowed.isUniverse) {
        (machine.freeRegFor(value) &~ untouchable).headOption getOrElse newSpillSlotUsedAsWorkaroundFor15742(value)
      } else {
        (allowed &~ untouchable).head
      }
    }

    /** Selects best resource from `allowed` set to use it as temporal or result of some node. */
    private def selectBestFrom(allowed: ResourceSet): Resource =
      allowed find (!machine.busy(_)) getOrElse allowed.head

    /** Selects best resource from `allowed` set to use it as temporal for transfer. */
    private def selectBestTemporalForTransfer(allowed: ResourceSet): Resource = {
      val freeToUse = allowed &~ untouchable
      if (freeToUse.nonEmpty) {
        selectBestFrom(freeToUse)
      } else {
        selectBestFrom(allowed)
      }
    }

    /** Selects best resource where `value` already exists to be source of transfer to `dst`. */
    private def selectBestSourceForTransfer(value: Value, dst: Resource): Resource = {
      val effective = machine.resources(value) find { src =>
        temporaryResourcesForTransfer(dst, src, machine.nodeOn(src, value)).isEmpty
      }
      effective getOrElse machine.resources(value).head
    }


    /////////////////////////////////////////////////////////////////////////////
    // Transfers generation utilities

    /** Make sure that `value` will be moved to `dst` which is free to use (if `dst` was occupied by some other value
      * it has been already saved somewhere else). Either ensures that `value` is already on `dst` (two fast-paths) or
      * creates several copies, inserts them in code order and update machine state.
      */
    private def move(value: Value, dst: Resource): Node = {
      def copy(dst: Resource, src: Resource): Node = {
        val delegate = machine.nodeOn(src, value)
        val copy = Copy.withoutValue(delegate, setOf(dst)) atLowerPoint lowerPoint(node)
        machine(dst) = copy
        CodeOrder.insertBefore(node, copy)
        copy.generated = true
        beDebugPrint(3)(s"value ${value.producer.id} copied to $dst")
        copy
      }

      machine.get(dst) match {
        case None if dst == Immediate =>
          value.producer ensuring (_.isInstanceOf[Constant]) // Fast path - `value` should be constant

        case Some(occupier) if valueOf(occupier) == value =>
          occupier // Fast path - `dst` already contains `value`

        case _ =>
          val src = selectBestSourceForTransfer(value, dst)
          val delegate = machine.nodeOn(src, value)

          temporaryResourcesForTransfer(dst, src, delegate) match {
            case None =>
              copy(dst, src)

            case Some(allowed) =>
              val tmp = selectBestTemporalForTransfer(allowed)
              machine.valueOn(tmp) match {
                case None =>
                  // `src` => `tmp` => `dst`
                  copy(tmp, src)
                  copy(dst, tmp)

                case Some(occupier) =>
                  // We are going to use `tmp` which is occupied. We should ensure that `occupier` will survive
                  // anywhere else (and not touch `dst` and `src` during this!). Also we should restore occupier
                  // on `tmp` if it was untouchable.
                  val tmpWasUntouchable = untouchable(tmp)
                  val dstWasUntouchable = untouchable(dst)
                  val srcWasUntouchable = untouchable(src)

                  if (!tmpWasUntouchable) untouchable += tmp
                  if (!dstWasUntouchable) untouchable += dst
                  if (!srcWasUntouchable) untouchable += src

                  // 1. Release `tmp`
                  ensureAlive(occupier)

                  // 2. `src` => `tmp` => `dst`
                  copy(tmp, src)
                  val result = copy(dst, tmp)

                  if (tmpWasUntouchable) {
                    // 3. Restore `tmp` if required (is it was untouchable)
                    move(occupier, tmp)
                  } else {
                    untouchable -= tmp
                  }
                  if (!dstWasUntouchable) untouchable -= dst
                  if (!srcWasUntouchable) untouchable -= src

                  result
              }
          }
      }
    }

    /** Make sure that `value` will survive anywhere else except `untouchable` set of resources.
      * Either finds out that `value` occupies some resource besides `untouchable` or selects any free resource
      * and move `value` to it. */
    private def ensureAlive(value: Value): Unit = {
      if (selectExistedResourceFor(value).isEmpty) {
        move(value, selectNewResourceFor(value))
      }
    }


    /////////////////////////////////////////////////////////////////////////////
    // Main algorithms: normalization, resources selection, unblocking, code pre-order iteration

    /** Make sure that all `edges` sources values occupy resources allowed for `edges` targets. Returns map from
      * source value to number of it's appearance in edges. Updates current `untouchable` set.
      */
    private def normalize(edges: Seq[Edge]): Maps[Value]#QMap[Int] = {
      val argCounters = Maps[Value].newQMap[Int]

      // 1. Separate one-resource allowed arguments from many-resource allowed
      val (singles, others) = (edges map (e => (e, allowedLocations(e)))) partition (_._2.isSingleton)

      // 2. Select resources for arguments (trying to reuse currently occupied resources)
      val selection = Maps[Edge].newQMap[Resource]
      for ((edge, allowed) <- singles ++ others) {
        val value = valueOf(edge.source)
        argCounters(value) = argCounters.getOrElse(value, 0) + 1
        val resource = selectExistedResourceFor(value, allowed) getOrElse selectNewResourceFor(value, allowed)
        if ((resource != Immediate) && !(eeIRegSet contains resource)) {
          untouchable += resource
        }
        selection(edge) = resource
      }

      // 3. Move all arguments to selected resources
      for (edge <- edges) {
        val arg = valueOf(edge.source)
        val dst = selection(edge)
        machine.valueOn(dst) match {
          case Some(occupier) if occupier != arg =>
            // TODO: if occupier is argument of `node` it may alive on its selected resource (or may not alive at all if it is its last use)
            ensureAlive(occupier)
          case _ =>
        }
        edge.source = move(arg, dst)
      }

      argCounters
    }

    /** Selects resources for results and spoiled allocations and make them free to use. Updates current `untouchable` set. */
    private def select(argCounters: Maps[Value]#QMap[Int]): (Seq[Resource], Seq[Resource]) = {
      case class Allocation(target: Node, candidates: ResourceSet, var selection: Resource = null)

      // 1. Separate one-resource allowed allocations from many-resource allowed
      val results = (node.groupedValueResults map { r =>
        var candidates = resultCandidates(r)(machine.resources)
        if (isBoundNode(node)) {
          candidates &= setOf(boundEdges(node) map (_.source.resource))
        }
        Allocation(r, candidates)
      }).toSeq
      val spoiled = spoiledResourcesSets(node) map (Allocation(null, _))
      val (singles, others) = (results ++ spoiled).partition(w => w.candidates.isSingleton)

      // 2. Select resources for results and spoiled
      val allocated = emptyMSet()
      for (wrapper <- singles ++ others) {
        if (wrapper.candidates.isEmpty) {
          assert(!wrapper.target.hasValueUses) // E.g. [[CheckCast]] // TODO: may be useless with standard lowering
          wrapper.selection = InvalidResource
        } else {
          wrapper.selection = selectBestFrom(wrapper.candidates &~ allocated)
          allocated += wrapper.selection
          untouchable += wrapper.selection
        }
      }

      // 3. Free allocated resources
      for (r <- allocated) {
        for (occupier <- machine.valueOn(r)) {
          argCounters.get(occupier) match {
            case Some(counter) if machine.state(occupier).remainingUses == counter =>
              // Nothing to do: `occupier` will die after this node generation.
            case _ =>
              // TODO: if occupier is argument of `node` it may alive on its selected resource
              ensureAlive(occupier)
          }
        }
      }

      (results.map(_.selection), spoiled.map(_.selection))
    }

    /** Finish resources allocation: updates `machine` and `node`. */
    private def allocate(results: Seq[Resource], spoiled: Seq[Resource]): Unit = {
      // 1. Reduce remaining uses of own arguments and remove dead ones
      if (!node.isInstanceOf[Phi]) {
        machine.decreaseUses(ownInEdges(node))
      }

      // 2. Spoil spoiled resources
      node.spoiled = spoiled
      node.spoiled foreach machine.spoil

      // 3. Special case for GCSafe calls
      node match {
        case call: Call if call.gcActions.generateGCSafeRegion =>
          // For optimizing back-end these spills inserted in BGCM.
          // TODO-FAST-BE: try to emulate this in FastCodeOrdering, not in RegAlloc.
          val args = call.groupedValueArgs.map(valueOf).toSet
          for (r <- allIRegsSet.iterator if machine busy r;
               ref = machine(r) if mayBeTraceableReference(ref);
               value = valueOf(ref) if !args(value)) {

            if (!(machine.resources(value) exists isStorageResource)) {
              move(value, newSpillSlotUsedAsWorkaroundFor15742(value))
            }
            machine.spoil(r)
          }

        case _ =>
      }

      // 4. Reduce remaining uses of constrained arguments and remove dead ones
      machine.decreaseUses(constrainedInEdges(node))

      // 5. Append results
      for ((target, resource) <- node.groupedValueResults zip results) {
        machine(resource) = target
        machine.tryToRemove(target)
      }

      // 6. Register node in frame
      registerNodeInFrame(node)
    }

    /** Interpret the whole code order of block, normalizing nodes, selecting resources and updating machine state.
      * Returns final machine state.
      */
    def interpret(): MachineState = {
      beDebugPrint(1)(s"block ${block.id} prepared to interpret")

      for (n <- CodeOrder.in(block).toList) {
        node = n
        untouchable = emptyMSet()

        val (results, spoiled) = node match {
          case _: Param | _: Phi =>
            // Pre-allocated nodes
            (node.allResultResources, node.spoiled)

          case _ =>
            // 1. Normalize incoming edges of node (only own, not constrained, because constraints normalization
            // may greedy block all resources, needed for results and spoiled selection).
            val argCounters = normalize(ownInEdges(node))
            beDebugPrint(3)(s"${node.id} normalized")

            // 2. Select resources for results and spoiled.
            val results = select(argCounters)
            beDebugPrint(3)(s"${node.id} resources selected")

            // 3. Now normalize constrained edges.
            node match {
              case node: LowerPoint if node.hasConstraints =>
                normalize(constrainedInEdges(node))
                beDebugPrint(3)(s"${node.id} constraints normalized")
              case _ =>
            }

            results
        }

        // 4. Finish allocation, updating `machine` and `node`.
        allocate(results, spoiled)
        node.generated = true
        beDebugPrint(2)(s"${node.id} allocated")
      }

      machine
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // CFG interpreter (global resources allocator)

  protected class FastRegAllocImpl {
    private val blocksToResolve = ArrayBuffer.empty[Block]
    private val alreadyFixedBlocks = Sets[Block].newMSet

    /** Returns input state for `block` interpretation. */
    private def inputState(block: Block): MachineState = {
      val machine = new MachineState(block)

      if (block != entryBlock) {
        val inEdges = block.inEdges.toSeq
        val ready = inEdges.forall(_.source.block.generated)
        val refInEdge = inEdges.find(_.source.block.generated).get
        val refConstraints = constraints(refInEdge)
        if (!ready) {
          blocksToResolve += block
        }

        for (node <- liveness in block) {
          node match {
            case phi: Phi if phi.block == block =>
              phi.resource = if (ready) {
                phi.replaceArgsBySeq(inEdges map (e => constraints(e).delegate(phi.phiArg(e))))
                phi.args.next().resource
              } else {
                refConstraints.delegate(phi.phiArg(refInEdge)).resource
              }

            case _ =>
              val delegates = inEdges map (e => constraints(e).delegate(node))
              uniqueValue(delegates) match {
                case Some(unique) if ready =>
                  machine(unique.resource) = unique
                case _ =>
                  val phi = Phi(node.tpe)(block +: delegates *)
                  phi.resource = refConstraints.delegate(node).resource
                  CodeOrder.insertAfter(block, phi)
              }
          }
        }
      }

      machine
    }

    private def resolvePhies(): Unit = {
      for (block <- blocksToResolve) {
        val inEdges = block.inEdges.toSeq

        for (phi <- block.phies) {
          if (isSynonym(phi)) {
            val producer = valueOf(phi).producer
            phi.replaceArgsBySeq(inEdges map (e => constraints(e).delegate(producer)))
          } else {
            phi.replaceArgsBySeq(inEdges map (e => constraints(e).delegate(phi.phiArg(e))))
          }
        }
      }
    }

    /** Synchronize allowed sets of constraints of other predecessors of our single successor block. */
    private def fixConstraintsForOtherPredecessors(block: Block): Unit = {
      for (succ <- singleton(block.succBlocks) if !alreadyFixedBlocks(succ)) {
        alreadyFixedBlocks += succ
        val controlInput = singleElement(block.succBlockEdges)
        val blockConstraints = block.blockEnd.constraints
        val phiByArg = (succ.phies map { phi => (phi.phiArg(controlInput), phi) }).toMap
        for (pe <- succ.inEdges if pe != controlInput) {
          val pc = pe.source.block.blockEnd.constraints
          blockConstraints foreach { n =>
            val delegate = phiByArg.get(n) map (_.phiArg(pe)) getOrElse n
            pc.setResource(delegate, blockConstraints.delegate(n).resource)
          }
        }
      }
    }

    def interpret(): Unit = {
      // During interpretation we will create phi-functions for loop headers with fake arguments (sequence of one node
      // from reference forward state). Later we will patch these phi-functions, but during creation we should disable
      // phi identity, because otherwise they will be transformed to their single argument. Also there are no reasons
      // to enable them afterwards.
      disablePhiIdentity()

      for (param <- all[Param]) param.resource = paramLocation(param)
      for (tail <- TailPointer.unique) tail.resource = tailRegister

      for (block <- cfg.topSort.order) {
        new BlockInterpreter(block, inputState(block)).interpret()
        fixConstraintsForOtherPredecessors(block)
        beDebugPrint(1)(s"block ${block.id} interpreted")
      }

      beDebugPrint(0)("before phies resolved")
      resolvePhies()
      beDebugPrint(0)("after phies resolved")
    }
  }
}
