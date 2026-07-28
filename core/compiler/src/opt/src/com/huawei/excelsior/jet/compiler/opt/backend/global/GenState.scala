/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.global

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.compiler.Env.stackPointer
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

import scala.collection.{immutable, mutable}
import scala.util.chaining.scalaUtilChainingOps

/**
 * Generator state is a class, that reflects current state of generation - unique resources content.
 *
 * @author conwor
 */
trait GenState { self: Universe with BackEnd =>

  class LocalValueState(
                         // Set of all resources, contained this value.
                         val resources: MutableResourceSet,

                         // Set of all registers, contained this value (subset of `resources`).
                         val registers: MutableResourceSet,

                         // Set of all frame slots, contained this value (subset of `resources`).
                         val frameSlots: MutableResourceSet,

                         // Set of all alt. locations, contained this value (subset of `resources`).
                         val altLocations: MutableResourceSet,

                         // Set of not yet generated uses (nodes).
                         val remainingUses: Sets[Node]#QSet,

                         // Map from resource R to set of not yet generated uses (edges),
                         // which require this value exactly on R.
                         val oneResourceUses: Maps[Resource]#QMap[immutable.Set[Edge]]) {

    def this() = this(
      emptyMSet(),
      emptyMSet(),
      emptyMSet(),
      emptyMSet(),
      Sets[Node].newQSet,
      Maps[Resource].newQMap[immutable.Set[Edge]])

    /** Returns copy of this LocalValueState with clean uses info. */
    def copyWithCleanUses() = new LocalValueState(
      resources.clone(),
      registers.clone(),
      frameSlots.clone(),
      altLocations.clone(),
      Sets[Node].newQSet,
      Maps[Resource].newQMap[immutable.Set[Edge]])

    /** Updates status of given `resource`. */
    def update(resource: Resource, status: Boolean): Unit = {
      resources(resource) = status
      resourceKind(resource) match {
        case ImmResourceKind =>
        case TailSlotResourceKind =>
        case _: RegResourceKind => registers(resource) = status
        case FrameSlotResourceKind => frameSlots(resource) = status
        case AltLocationResourceKind => altLocations(resource) = status
      }
    }
  }

  final class GeneratorState( // Map from resource to node, occupied this resource.
                              private var content: Maps[Resource]#QMap[NodeRef],

                              // Map from live value to it's state.
                              private var valuesStates: mutable.Map[Value, LocalValueState])

      extends AbstractInterpreter.State
  {
    protected type This = GeneratorState

    /** Constructs empty GeneratorState. */
    def this() = this(Maps[Resource].newQMap[NodeRef], Maps[Value].newQMap[LocalValueState])


    //////// AbstractInterpreter.State interface //////////////

    private def valueState(value: Value) = valuesStates.getOrElseUpdate(value, {
      val state = new LocalValueState()
      if (value.producer.isInstanceOf[Constant]) state(Immediate) = true
      state
    })

    /** @return lazy copy of this GeneratorState. */
    override protected def forkImpl(): GeneratorState = new GeneratorState(content, valuesStates)

    override def makeUnreachableCopy(): GeneratorState = shouldNotCallThis("no unreachable blocks during code generation")

    /** Implements lazy copy on write. */
    override protected def copyOnWriteImpl(): Unit = {
      content = Maps[Resource].newQMap[NodeRef](content)
      valuesStates = valuesStates map { case (k, v) => (k, v.copyWithCleanUses()) }
    }

    override def foreachPair(that: GeneratorState)(action: (Node, Node) => Unit): Unit = {
      for (x <- content.values; y <- that.content.get(x.deref.resource)) {
        action(x.deref, y.deref)
      }
    }

    override def mergeFrom(block: Block, states: Seq[GeneratorState], identity: Boolean)(f: (Type, Seq[Node]) => Node): GeneratorState = {
      def delegate(node: Node, from: ControlNode): Node = {
        val goto = from.asInstanceOf[Goto]
        val constraints = goto.constraints
        constraints.delegate(node match {
          case phi: Phi if phi.block == block => phi.phiArg(goto.outEdge)
          case _ => node
        })
      }

      lazy val anyGeneratedInput = block.inputs.find(_.generated).getOrElse {
        shouldNotReachHere("block without any generated inputs")
      }
      def anyGeneratedDelegate(node: Node): Node = delegate(node, anyGeneratedInput)

      val result = new GeneratorState()

      if (!identity) {
        // Conservatively use only resources selected in forward-edge constraints and create proxies for them
        for (node <- liveness.in(block)) {
          val resource = anyGeneratedDelegate(node).resource
          val rdf = node match {
            case phi: Phi if phi.block == block => phi
            case _ => f(node.tpe, Seq(node))
          }
          result.add(rdf, resource)
        }

      } else if (block.generated) {
        // Simply create phi-functions and resolve proxies, associated with them
        // There is no reason to create phies for all synonyms, because we cannot regenerate loop
        for (node <- liveness.in(block)) {
          val resource = anyGeneratedDelegate(node).resource
          val values = block.inputs.map(input => delegate(node, input))
          val rdf = node match {
            case phi: Phi if phi.block == block => phi tap { _.replaceArgsBySeq(values) }
            case _ =>
              // TODO: also check in Phi case
              //       One known problem on LWRT XKRN:
              //       - node: Phi[ThinType]
              //       - values: [Load[LongType], Load[ThinType]]
              assert(values forall (_.tpe == node.tpe))
              f(node.tpe, values)
          }
          result.add(rdf, resource)
        }

      } else {
        // Create phi for resource, selected in constraints, and for all synonyms, iff they are alive on all incomming edges
        for (node <- liveness.in(block)) {
          node match {
            case phi: Phi if phi.block == block =>
              // Actually, we can merge synonyms of our phi arguments, iff they are on the same resources, but
              // it looks like rare case, and it will complicate this function dramatically.
              val resource = anyGeneratedDelegate(node).resource
              val values = block.inputs.map(input => delegate(phi, input))
              phi.replaceArgsBySeq(values)
              result.add(phi, resource)

            case _ =>
              val value = valueOf(node)
              val subSet = (states map { _.resources(value) }).reduce { (x, y) => x & y }
              assert(subSet.nonEmpty)
              for (r <- subSet) {
                val values = states map { _ (r) }
                assert(values forall (_.tpe == node.tpe))
                result.add(f(node.tpe, values), r)
              }
          }
        }
      }

      result
    }


    //////// State interface ////////

    def takeResourceFromValue(value: Value, resource: Resource): Unit = {
      valuesStates(value)(resource) = false
      content -= resource
    }

    /** Starts session on active usage of this state. Invokes state copy. */
    def startSession(): Unit = { copyOnWrite() }

    /** Spoils given `resource`. */
    def spoilResource(node: Node, resource: Resource, block: Block): Unit = {
      assert(resource != stackPointer)
      if (resource == frame.EER) {
        node match {
          case _: ExecEnv | _: Proxy | _: Phi =>
          case _ => shouldNotReachHere()
        }
      }
      if (contains(resource)) {
        val x = apply(resource)
        removeSynonyms(x, block)
        takeResourceFromValue(valueOf(x), resource)
      }
    }

    /**
     * Sets, that given `node` are generated now.
      *
      * @param node generated node from this Value.
     */
    def add(node: Node, resource: Resource): Unit = {
      node.resource = resource
      if ((resource != InvalidResource) && hasValue(node)) { // Node with value could be allocated on InvalidResource, if it is call with void return type
        val value = valueOf(node)
        val vstate = valueState(value)
        vstate(resource) = true
        if (resource != Immediate) {
          content(resource) = node
        }
      }
    }

    /** Initialize values - clean not used values and updates values states with local uses. */
    def initValues(block: Block): Unit = {
      valuesStates mapValuesInPlace { (_, v) => v.copyWithCleanUses() }

      for (node <- block.nodes if node.isGroupRoot && !node.isInstanceOf[Phi]) {
        if (isSynonym(node)) {
          assert(node.isInstanceOf[Proxy])
        } else {
          for (edge <- node.groupedValueInEdges; value = valueOf(edge.source)) {
            val vstate = valueState(value)
            vstate.remainingUses += node
            val allowed = allowedLocations(edge)
            if (allowed.isSingleton) {
              val r = allowed.single
              val set = vstate.oneResourceUses.getOrElse(r, Set.empty[Edge])
              vstate.oneResourceUses(r) = set + edge
            }
          }
        }
      }

      valuesStates filterInPlace { (_, vstate) =>
        vstate.remainingUses.nonEmpty || { content --= vstate.resources.iterator; false }
      }
    }

    def live(value: Value): Boolean = valuesStates contains value

    def live(node: Node): Boolean = live(valueOf(node))

    def remainOneUse(value: Value): Boolean = valuesStates.get(value) match {
      case Some(vstate) => vstate.remainingUses.size == 1
      case None => false
    }

    def remainingUses(value: Value): collection.Set[Node] = valuesStates.get(value) match {
      case Some(vstate) => vstate.remainingUses
      case None => Set.empty[Node]
    }

    def remainingRequiredResources(value: Value): ResourceSet = valuesStates.get(value) match {
      case Some(vstate) => setOf(vstate.oneResourceUses.keysIterator)
      case None => emptySet
    }

    def hasRemainingUsesOnFixedResource(value: Value, resource: Resource, except: Node = null): Boolean = valuesStates.get(value) match {
      case Some(vstate) => vstate.oneResourceUses.get(resource) match {
        case Some(uses) => except == null || (uses exists { _.target != except })
        case None => false
      }
      case None => false
    }

    /** Free all value arguments of given generated `node`. */
    def freeLocalUsages(node: Node): Unit = {
      assert(node.generated)

      if (node.isGroupRoot && !node.isInstanceOf[Phi] && !isSynonym(node)) {
        for (edge <- node.groupedValueInEdges; value = valueOf(edge.source)) {
          // value may be already dead, if it has one remaining use (current node), but several edges,
          // and we have already process one of them
          for (vstate <- valuesStates.get(value)) {
            vstate.remainingUses -= node
            if (vstate.remainingUses.isEmpty) {
              content --= vstate.resources.iterator
              valuesStates -= value
            } else {
              val allowed = allowedLocations(edge)
              if (allowed.isSingleton) {
                val r = allowed.single
                val usesMap = vstate.oneResourceUses
                val currSet = usesMap(r)
                assert(currSet.contains(edge))
                if (currSet.size == 1) {
                  usesMap -= r
                } else {
                  usesMap(r) = currSet - edge
                }
              }
            }
          }
        }
      }
    }

    /** @return whether this state contains some value on given `resource`. */
    def contains(resource: Resource): Boolean = content.contains(resource)

    /** @return node, that occupies given `resource`. */
    def apply(resource: Resource): Node = content(resource).deref

    def valuesInfo(): String = (valuesStates.keys map { v =>
      resources(v).iterator.map(r => s"$r -> ${getNode(v, r)}").mkString(s"  VALUE: ${v.producer}\n    ", "\n    ", "\n")
    }).mkString("values:\n", "\n", "\n")

    /**
     * Returns node that presents given `value` on given `resource`.
     * Fails with assert, if there is no such node.
     */
    def getNode(value: Value, resource: Resource): Node = if (resource == Immediate) value.producer else apply(resource)

    def resources(value: Value): MutableResourceSet = valuesStates(value).resources
    def registers(value: Value): MutableResourceSet = valuesStates(value).registers
    def frameSlots(value: Value): MutableResourceSet = valuesStates(value).frameSlots
    def altLocations(value: Value): MutableResourceSet = valuesStates(value).altLocations

    def savedInStorage(value: Value): Boolean = !(resources(value) subsetOf registers(value)) //frameSlots(value).nonEmpty?

    def freeIRegs = allIRegsSet -- content.keysIterator
    def freeFRegs = allFRegsSet -- content.keysIterator
  }
}
