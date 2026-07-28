/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder.LoopOrientation
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.cfg.{Knots, Loops}
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.state.BGCMState
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.heuristics.SpillHeuristics
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections.OrderedEnum
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.Loop
import com.huawei.excelsior.jet.util.graph.ordering.NaturalCFGOrder

/** Bulldozer interpreter of CFG in upward direction. */
trait UAI extends BDAG with BGCMState with Knots with Loops with BlockInterpreter with SpillHeuristics { self: Universe with BackEnd =>

  /** Upward going abstract interpreter. Iterates CFG blocks in reversed "natural" CFG order with
    * header-first loop orientation. Collects states for  each block and supports `processing` logic in them. */
  class UpwardAI(val gcm: GCMEngine) extends CFGKnots with LoopsInUAI with BlockInterpreterImpl with Spill.LocalImpl { self =>

    ///////////////////////////////////////////////////////////////////////////////////////////////
    //// Block status

    enum BlockStatus extends OrderedEnum[BlockStatus]:
      case NOT_INTERPRETED, PARTIALLY_INTERPRETED, FULLY_INTERPRETED

    private val blocksStatus = Maps[Block].newQMap[BlockStatus]

    /** Returns `block` status. */
    def blockStatus(block: Block): BlockStatus =
      blocksStatus.getOrElseUpdate(block, BlockStatus.NOT_INTERPRETED)

    /** Update `block` status with `newStatus` which should be "bigger" (more interpreted) than current status. */
    def setBlockStatus(block: Block, newStatus: BlockStatus): Unit = {
      assert(blockStatus(block) < newStatus)
      blocksStatus(block) = newStatus
    }


    ///////////////////////////////////////////////////////////////////////////////////////////////
    //// Events

    /** Each event defines set of blocks which should be processed before event - requirements.
      * When all requirements are satisfied, event occurs and creates state for some dependent blocks. */
    abstract class Event {
      def requirements(): IterableOnce[Block]
      def apply(): Unit

      val notReadyRequirements: Sets[Block]#QSet = Sets[Block].newQSet(requirements())

      def setReady(block: Block): Unit = notReadyRequirements.remove(block) ensuring { _ == true }
      def isReady: Boolean = notReadyRequirements.isEmpty

      /** Simply interpret all requirements till the end. In the future this interpretation may be
        * complicated, with taken all requirements into account. */
      def interpretRequirementsTillTheEnd(): Unit = {
        for (block <- requirements().iterator) {
          val state = states(block)
          blockStatus(block) match {
            case BlockStatus.NOT_INTERPRETED => shouldNotReachHere()
            case BlockStatus.PARTIALLY_INTERPRETED => withState(state) { interpretBlock(block, tillTheEnd = true) }
            case BlockStatus.FULLY_INTERPRETED =>
              // Block was already interpreted till the end by some other event, which has the same requirement
              // TODO: verify that this is not a problem
          }
        }
      }
    }

    implicit object EventSetsAndMaps extends Sets.Default[Event] with Maps.Default[Event]

    private val awaitingEvents: Maps[Block]#QMap[Sets[Event]#QSet] = Maps[Block].newQMap[Sets[Event]#QSet]

    private def addEvent(event: Event): Unit = {
      if (event.isReady) {
        // Immediately apply this event during it's creation. Required for endless loops handling.
        event()
      } else {
        for (requirement <- event.notReadyRequirements) {
          awaitingEvents.getOrElseUpdate(requirement, { Sets[Event].newQSet }) += event
        }
      }
    }

    /** Creates all events for all loops and CFG knots. */
    protected def createEvents(): Unit = {
      for (loop <- gcm.loops) {
        assert(loop.header.isInstanceOf[BBlock])
        addEvent(new LoopEvent(loop))
      }
      for (point <- all[Block]) {
        if (point.succBlocks.size > 1) addEvent(new BranchPointEvent(point))
        if (point.predBlocks.size > 1) addEvent(new PhiPointEvent(point))
      }
    }

    /** Returns true, iff given `block` blocks some event, means that we cannot interpret it till the end. */
    protected def blockSomeEvents(block: Block): Boolean =
      awaitingEvents.contains(block)

    /** Notify all events, awaiting given `block`. If some of them became ready, apply them. */
    protected def notifyEvents(block: Block): Unit = {
      for (events <- awaitingEvents.get(block); event <- events) {
        event.setReady(block)
        if (event.isReady) {
          event()
        }
      }
    }

    class LoopEvent(loop: Loop[Block]) extends Event {
      def requirements(): IterableOnce[Block] = loop.exits flatMap { _.xSuccBlocks collect { case b: Block if !(loop.body contains b) => b } }
      def apply(): Unit = { interpretRequirementsTillTheEnd(); createLoopLRSAndStatesForBackwardBranches(loop, requirements().iterator.toList) }
    }

    class BranchPointEvent(point: Block) extends Event {
      def requirements(): IterableOnce[Block] = point.succBlocks
      def apply(): Unit = { interpretRequirementsTillTheEnd(); createStateForBranchPoint(point) }
    }

    class PhiPointEvent(point: Block) extends Event {
      def requirements(): IterableOnce[Block] = Seq(point)
      def apply(): Unit = { interpretRequirementsTillTheEnd(); createStatesForPhiPointPredecessors(point) }
    }


    ///////////////////////////////////////////////////////////////////////////////////////////////
    //// Global data

    protected val dag = new BulldozerDAG
    protected val states: Maps[Block]#QMap[State] = Maps[Block].newQMap[State]
    protected val loopLRSs: Maps[Loop[Block]]#QMap[LoopLRS] = Maps[Loop[Block]].newQMap[LoopLRS]
    private val lowerPoints = Maps[Node].newQMap[ControlNode]

    val loopEnterRequirements: Maps[Block]#QMap[LoopEnterRequirements] = Maps[Block].newQMap[LoopEnterRequirements]
    val spillHintsOnEdges: Sets[Edge]#QSet = Sets[Edge].newQSet
    var maxSpillPressure: Int = 0

    val interpreterCompilationMode = rootMethod.isInterpretationLoop

    protected def latestPointFor(node: FloatingNode): ControlNode =
      lowerPoints getOrElseUpdate(node, {
        val earliest = node.lowerPoint

        // `latestPointByUses` returns None for dead or unreachable code,
        // but in BGCM there should not be such code
        val latestByUses = gcm.latestPointByUses(node).get

        val antiDep = gcm.memoryAntiDependency(node, latestByUses)
        gcm.cfgHeuristics(earliest, antiDep getOrElse latestByUses)
      })


    /** Current interpreted block state. */
    private var _currentState: State = _

    def currentState: State = _currentState

    private def withState(state: State)(action: => Unit): Unit = {
      _currentState = state
      action
      _currentState = null
    }


    def iterate(): Unit = {
      // 1. Init base loop LRS, events, blocks order
      loopLRSs(null) = LoopLRS.empty
      createEvents()
      Spill.init(gcm)
      val order = NaturalCFGOrder(cfg, LoopOrientation.HEADER_FIRST).reverse

      // 2. Iterate all blocks in specific order.
      for (block <- order) {
        val state = states.getOrElseUpdate(block, {
          block.succBlocks.size match {
            case 0 =>
              // Empty state for block without successors
              // Block without successors may be from some loop (by outgoing exception edge)
              new State(new LocalLRS(), loopLRSs(gcm.loops.loopOf(block)), cold = gcm.cold(block))

            case 1 =>
              val succ = ScalaCollections.singleElement(block.succBlocks)
              assert(succ.predBlocks.size == 1) // Otherwise it is phi-point and predecessor state should be created in corresponding event
              new State(states(succ).local.copy(), loopLRSs(gcm.loops.loopOf(block)), cold = gcm.cold(block))

            case _ =>
              shouldNotReachHere("state for block " + block.id + " should have been created by some event")
          }
        })

        withState(state) { interpretBlock(block, tillTheEnd = !blockSomeEvents(block)) }
        notifyEvents(block)
      }
    }
  }

}
