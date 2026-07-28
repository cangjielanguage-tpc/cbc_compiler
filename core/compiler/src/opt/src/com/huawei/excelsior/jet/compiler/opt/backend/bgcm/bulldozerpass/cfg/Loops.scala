/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.cfg

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass.UAI
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.{Loop, LoopKind}

/** Part of UAI, responsible for loops processing. For each loop we create LoopLRS object which represents
  * history of loop context nodes - outsiders and arguments.
  *
  * Outsiders are nodes, which will be linked above loop, have uses below loop and do not have uses inside loop.
  * Arguments are nodes, which will be linked above loop, have uses inside loop and may have uses below loop.
  *
  * Main goal for LoopLRS creation is to collect context nodes, spilled somewhere inside loop, and use this
  * collection for inserting Store operations outside of loop.
  *
  * @author conwor
  * */
trait Loops { self: Universe with UAI with BackEnd =>

  trait LoopsInUAI { self: UpwardAI =>

    type NodeSet = collection.Set[Node]

    /** Returns three sets:
      *   1) Loop body - nodes, which was not generated below loop and could not be generated above loop.
      *   2) Loop arguments
      *   3) Loop outsiders
      * */
    private def loopStructure(loop: Loop[Block], exitStates: Seq[State]): (NodeSet, NodeSet, NodeSet) = {
      // 1. Collect lower cut - nodes, which may be generated on exit.
      // TODO: in the future we should select part of it and move above the loop.

      // Set of nodes, which will be live on exits
      val lowerCutArgs = Sets[Node].newQSet
      for (state <- exitStates) {
        // TODO: maybe exclude all constants from this set?
        // This will be subtask for "rematerialization outside-down", but it looks like reasonable fast path.
        lowerCutArgs ++= state.allNodes
      }

      // 2. Collect loop body - nodes, which upper points are in loop and they are not in lower cut.
      val body = Sets[Node].newQSet
      for (b <- loop.body; n <- b.nodes if n.isGroupRoot) {
        body ++= n.allGroupNodes // TODO: try to use only group roots here and in `argsOfLoopBody`
        n match {
          case cn: ControlNode => body ++= cn.projections
          case _ =>
        }
      }

      // 3. Determine loop arguments - nodes from upper cut, which are used in loop body except forward arguments of header phi-functions.
      val args = (body flatMap {
        case phi: Phi => phi.inEdges collect { case e if loop.body(e.useBlock) => e.source }
        case n => n.valueArgs
      }) filter { n => !body(n) && !inSpecialFile(n) }

      // 4. Determine loop outsiders - nodes from upper cut, which are used on any of exits, but do not used in loop.
      val outsiders = lowerCutArgs &~ (body | args)

      // Verify upper cut
      assert((args & outsiders) forall { n => lowerPoint(n) dominates loop.header })

      (body, args, outsiders)
    }

    def rematerializeFragilePointersInLoop(loop: Loop[Block], exitStates: Seq[State]): Unit = {
      def collectFragilePointers(nodes: Iterable[Node]): Iterator[FloatingNode] = nodes.iterator collect {
        case node: FloatingNode if node.isFragilePointer => node
      }

      def replaceUse(use: Edge, replacement: Node): Unit = {
        dag.replaceLiveBelowDep(use.source, replacement)
        use.source = replacement
      }

      /** Place one copy into loop header for all uses in this loop. */
      def rematerializeInNormalLoop(node: FloatingNode, inLoopUses: Iterator[Edge], outLoopUses: Iterator[Edge]): Unit = {
        if (outLoopUses.isEmpty) {
          dag.moveNode(node, loop.header)
        } else {
          val clone = dag.insertNodeClone(node, loop.header)
          inLoopUses.toList foreach (replaceUse(_, clone))
        }
      }

      /** Place specific copy before each use in this loop. */
      def rematerializeInIrrLoop(node: FloatingNode, inLoopUses: Iterator[Edge], outLoopUses: Iterator[Edge]): Unit = {
        if (outLoopUses.isEmpty) {
          // drag original node to first inner use (there is at least one) if there are no outer uses
          val firstUse = inLoopUses.next()
          dag.moveNode(node, lowerPoint(firstUse.target.groupRoot).inCtrl)
        }
        inLoopUses.toList foreach { e => replaceUse(e, dag.insertNodeClone(node, lowerPoint(e.target.groupRoot).inCtrl)) }
      }

      val (body, arguments, outsiders) = loopStructure(loop, exitStates)
      assert(collectFragilePointers(outsiders).isEmpty)

      collectFragilePointers(arguments) foreach { node =>
        val (inLoopUses, outLoopUses) = node.valueOutEdges.partition(e => body.contains(e.target.groupRoot))
        assert(inLoopUses.nonEmpty)
        if (loop.kind == LoopKind.IRREDUCIBLE) {
          rematerializeInIrrLoop(node, inLoopUses, outLoopUses)
        } else {
          rematerializeInNormalLoop(node, inLoopUses, outLoopUses)
        }
      }
    }

    /** Creates LoopLRS for `loop` and states for sources of all backward branches. Assume that all
      * `exitTargets` are already interpreted till the end. */
    def createLoopLRSAndStatesForBackwardBranches(loop: Loop[Block], exitTargets: Seq[Block]): Unit = {
      val header = loop.header
      val exitStates = exitTargets map { block => states(block) }

      val gotos = header.predBlocks.map(_.blockEnd).toList
      assert(gotos forall { _.isInstanceOf[Goto] })
      val goto = gotos.head

      // rematerialize fragile pointers for uses in loop body because we cannot pass their state through loop borders
      // Note: it may change loop structure
      rematerializeFragilePointersInLoop(loop, exitStates)

      val (body, arguments, outsiders) = loopStructure(loop, exitStates)

      val outer = loop.outer
      val outerLRS = loopLRSs(outer)

      val loopLRS = outerLRS.makeForInner(loop, arguments, outsiders)
      loopLRSs(loop) = loopLRS

      val baseState = new State(new LocalLRS, loopLRS, gcm.cold(header))

      // Decrease file limits to avoid Goto spill decisions at Goto themselves and make them at baseState calculation.
      // For more details look at function makeSpillForBlockIncomingEdges in BlockInterpreter.
      baseState.withFileLimitDecrement(file => spoiledRegistersAmount(goto, file)) {

        val isInterpreterLoop = interpreterCompilationMode && body.exists(_.isInstanceOf[InterpreterCaseMarker])

        // TODO: extract 1-3 steps to SpillHeuristics trait

        // 1. Append header phies to created state, selecting register/storage for them. At this point
        // we do not take exit states into account, as we decide to consider inner loop more important than outer.
        // TODO: reconsider this in the future.
        header.phies foreach { phi => baseState.appendNode(phi, tryToRegisters = true) }

        // 2. Append loop arguments to created state, selecting register/storage for them (and registering
        // this selection in loopLRS at the same time). At this point we still do not take exit states into account
        // (except decision about constants). TODO: reconsider this in the future.
        for (arg <- arguments) {
          assert(!outerLRS.isOutsider(arg))
          assert(!(CodeOrder contains arg))

          lazy val isReallyNeededOnRegistersInLoop = arg.valueOutEdges exists { use =>
            body(use.target) && !gcm.cold(use.target.block) && State.argumentWillBeInRegisters(use, isAlive = true)
          }

          lazy val isNeededOnRegistersForOuterLoop = {
            val exitStatesWhereNodeLive = exitStates filter { state => !state.cold && state.live(arg) }
            exitStatesWhereNodeLive.nonEmpty && exitStatesWhereNodeLive.forall { _.inRegister(arg) }
          }

          // TODO: comment
          val tryToRegisters =
            !isInterpreterLoop && (generatedOnRegister(arg) || isReallyNeededOnRegistersInLoop || isNeededOnRegistersForOuterLoop)

          baseState.appendNode(arg, tryToRegisters)
        }

        // 3. Append loop outsiders to created state, selecting register/storage for them (and registering
        // this selection in loopLRS at the same time). At this point we take exit states into account, because
        // actually we implement branch point decision for them, and, for example, there is no reason to move up
        // our outsider to registers even if we have free, but this outsider is on storage for all exits.
        // TODO: review decision about spilling constant arguments for moving outsider to register
        for (outsider <- outsiders) {
          assert(!(CodeOrder contains outsider))

          if (isInterpreterLoop) {
            // Look at the comment in arguments processing
            baseState.appendNode(outsider, tryToRegisters = false)

          } else if (outerLRS.isOutsider(outsider)) {
            baseState.appendNode(outsider,
              tryToRegisters = !outerLRS.wasSpilledInThisLoopOrOuter(outsider))

          } else {
            // Node is either argument of outer loop or it' local node. We try to move it on register if and only
            // iff it is on registers on each exit, where it is alive.
            //
            // This decision may be wrong. E.g. if our outsider is on storage on one exit, but this exit is
            // from cold part of our loop and above this cold code there is no high RP and this outsider may be
            // on register in the whole loop except this cold code. Then there may be optimal to make spill in
            // our cold code than in outer loop.
            // TODO: try to construct real example or find one in applications/benches.
            baseState.appendNode(outsider,
              tryToRegisters = exitStates forall { s => s.inRegister(outsider) || !s.live(outsider) })
          }
        }

        baseState.checkLimit()
      }

      // 4. Create states for backward branches, based on collected (and checked) LRS.
      for (branch <- loopBackwardEdges(loop); dependent = branch.source.block) {
        assert(dependent.succBlocks.size == 1)
        assert(!states.contains(dependent))

        val state = new State(baseState.local.copy(), loopLRS, gcm.cold(dependent))
        replacePhiToArgs(state, branch)

        state.checkLimit()
        states(dependent) = state
      }
    }


    /** Creates requirements in `forward` block, which is an enter to loop. Requirements describe, on which
      * resources (register/storage) nodes should be passed into loop from `forward`.
      *
      * Actually this is part of phi-point event, but it look reasonable to put it in Loops part of UAI.
      * */
    def createRequirementsForDroppedLoop(forward: Block, headerState: State): Unit = {
      val loop = headerState.loop
      val hintsPoint = forward.blockEnd

      val requirements = new LoopEnterRequirements
      loopEnterRequirements(forward) = requirements

      // 1. Process loop outsiders.
      for (outsider <- loop.immediateOutsiders) {
        // There are problems with properties "headerState.inStorage" and "loop.wasSpilled". In general they should be
        // equal, because if we spill loop outsider somewhere in loop, it should not rise up on registers again, thus
        // it will be in storage in headerState. But. First of all, loop may be cold and for cold code loop storage
        // disabled, so "loop.wasSpilled" will be false and "headerState.inStorage" may be any value. Second, in some
        // loop exit, it's continuation may be by exceptional edge. We ignore exceptional spills, if normal continuation
        // has node on registers. Thus "headerState.inStorage" may be false and "loop.wasSpilled" may be true.
        // TODO: think about all this

        if (headerState.inStorage(outsider)) {
          dag.insertHintAbove(hintsPoint, outsider, BulldozerHint.spillAssert)
          requirements.append(outsider, toStorage = true, toRegisters = false)
        } else {
          // For outsiders that was not spilled in our loop we do not do anything to not interfere with outer loop,
          // because we don't care if it will be spilled through our loop.
        }
      }

      // 2. Process loop arguments.
      for (argument <- loop.arguments) {
        // Just "loop.wasSpilled" is not enough, look at comment in outsiders processing.
        val toStorage = loop.wasSpilled(argument) || headerState.inStorage(argument)

        // TODO: this decision should be made based on arguments uses and spill-points in loop body.
        val toRegisters = argument match {
          case _ if zeroCostRematerialization(argument) && toStorage => false
          case _ => headerState.inRegister(argument)
        }

        val hint = (toStorage, toRegisters) match {
          case (true, true)   => BulldozerHint.storeLoad
          case (true, false)  => BulldozerHint.spillAssert
          case (false, true)  => BulldozerHint.load
          case (false, false) => shouldNotReachHere()
        }

        dag.insertHintAbove(hintsPoint, argument, hint)
        requirements.append(argument, toStorage, toRegisters)
      }

      // 3. Process loop phi-functions
      for (phi <- loop.loop.header.phies) {
        val inStorage = headerState.inStorage(phi)
        requirements.append(phi, toStorage = inStorage, toRegisters = !inStorage)
      }
    }
  }
}
