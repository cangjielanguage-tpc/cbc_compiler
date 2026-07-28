/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Worklist

/** BGCM of one block in CFG. Applies local heuristics based on RP cost and selected target. Implements all code
  * ordering routines, rematerialization and hints inserting.
  *
  * @author conwor
  */
trait BlockInterpreter { self: Universe with BackEnd with UAI =>

  trait BlockInterpreterImpl { self: UpwardAI =>

    def interpretBlock(block: Block, tillTheEnd: Boolean): Unit = {

      /** Returns current point, which means last point, already linked in order. */
      def currentPoint: LowerPoint = (CodeOrder in block).next() match {
        case n: FloatingNode => n.lowerPoint
        case n: BlockParamNode => n.block.outCtrl
        case n: LowerPoint => n
      }

      /** Returns next point in not generated yet part of block. */
      def nextPoint: UpperPoint = currentPoint.inCtrl

      /** Insert hint between current point (last generated control node) and it's control argument.
        * If there is no current point (means that we just enter the block and generate it's blockEnd),
        * inserts hint to all block successors. */
      def insertHint(key: Node, hint: BulldozerHint.Proto): Unit = {
        if ((CodeOrder in block).nonEmpty) {
          makeGenStep(dag.insertHintAbove(currentPoint, key, hint))

        } else {
          if (block.blockEnd.isInstanceOf[Goto]) {
            shouldNotReachHere(s"unexpected hint: $hint, spill at Goto node should be covered by makeSpillForBlockIncomingEdges")

          } else {
            for (succ <- block.succBlocks) {
              assert(blockStatus(succ) == BlockStatus.FULLY_INTERPRETED)
              dag.insertHintAtAlreadyGeneratedBlockStart(succ, key, hint)
            }
          }

          // This code emulate State.process behaviour. TODO: try to remove this dangerous copy-paste.
          if (hint.spillAssert) {
            currentState.moveToStorage(key)
          } else {
            assert(hint.spill)
            currentState.moveToRegisters(key)
          }
        }
      }

      /** Inserts spill for nodes, live through `node` to avoid RP limit overrun.
        * Do not spill `node` arguments except those who can be passed on storage into `node`. */
      def spillLiveThroughNodes(node: Node): Unit = {
        for (diff <- currentState.calculateDiffs(node) if diff.spillAmount > 0) {
          // 1. Collect nodes, which may not be spilled (node arguments, passed on registers only)
          val notToSpill = Sets[Node].newQSet(node.groupedValueResults)
          for (inEdge @ Edge(arg, _) <- node.groupedValueInEdges) {
            val (_, mayPassedInStorage) = State.passedValueResourceType(inEdge)
            if (!mayPassedInStorage) notToSpill += arg
          }

          // 2. Spill required amount of nodes, inserting and making step with SpillAssert hints above current point
          for (x <- LocalSpill.selectFrom(diff.file, diff.spillAmount, notToSpill)) {
            insertHint(x, BulldozerHint.spillAssert)
          }
        }
      }

      /** Inserts spill for `node` results, iff they were in storage below. Do not spill nodes always generated
        * on storage to not pollute IR with extra hints. */
      def insertSpillNodesForResults(node: Node): Unit = {
        for (result <- node.groupedValueResults) {
          if (currentState.inStorage(result) && generatedOnRegister(result)) {
            insertHint(result, BulldozerHint.spill)
          }
        }
      }

      /** Collects set of edges incoming to `node`, which should be passed on registers in `node` but should be
        * spilled immediately below `node`, as they are in storage now. */
      def collectArgsSpilledBelowNode(node: Node): Sets[Edge]#QSet = {
        val spilled = Sets[Edge].newQSet
        for (inEdge @ Edge(arg, _) <- node.groupedValueInEdges) {
          if (currentState.inStorage(arg) && State.argumentWillBeInRegisters(inEdge, isAlive = true)) {
            spilled += inEdge
          }
        }
        spillHintsOnEdges ++= spilled
        spilled
      }

      def rematerializeAndGen(node: FloatingNode): Unit = {
        def linkedAndDomByNextPoint(node: Node) = (CodeOrder contains node.groupRoot) && nextPoint.dominates(upperPoint(node.groupRoot))

        val (usesToReplace, remainingUses) = node.valueOutEdges.partition(e => linkedAndDomByNextPoint(e.target))
        assert(usesToReplace.nonEmpty)
        val nodeToGen = if (remainingUses.nonEmpty) {
          val clone = dag.insertNodeClone(node, nextPoint)
          usesToReplace.toList foreach { _.source = clone }
          currentState.replace(node, clone)
          clone
        } else {
          node
        }
        makeGenStep(nodeToGen)
      }

      /** Rematerialize live fragile pointers iff `node` could invalidate them. */
      def rematerializeFragilePointersIfCouldBeInvalidatedAt(node: Node): Unit = {

        /** Returns true iff `node` is a point where fragile pointers could be invalidated. */
        def fragilePointersInvalidationPoint(node: Node) = node match {
          case b: Block if gcm.loops.seq.exists(blockIsLoopExitTarget(b, _)) =>
            // Actually exit edge could not invalidate fragile pointers but in most cases loop body will include
            // such point (Call or GCPoint) so we rematerialize all live fragile pointers right here to prevent
            // inserting copies inside loop.
            // TODO: improve analysis for counted loops without invalidation points.
            true

          case _ => couldInvalidateFragilePointers(node)
        }

        lazy val shouldRematerialize = fragilePointersInvalidationPoint(node) ||
          ((node == block) && (block.openedIdomHammock exists { b =>
            b.spineWithBlock exists
              // In this case we rematerialize fragile pointers right here, because otherwise we should insert
              // rematerialization copies on some paths in hammock and complete SSA-form which is not implemented
              // yet (TODO: feel free to implement it).
              fragilePointersInvalidationPoint
          }))

        val fragilePointers = Worklist.from(currentState.allNodes collect {
          case n: FloatingNode if n.isFragilePointer => n
        })

        if (fragilePointers.nonEmpty && shouldRematerialize) {
          currentState.onAppend.withCallback({
            case n: FloatingNode if n.isFragilePointer => fragilePointers += n
            case _ =>
          }) {
            fragilePointers.track foreach rematerializeAndGen
          }
        }
      }

      /** Consider phi-point with several incoming blockEnds (all of them are Goto). If Goto node spoils some resource
        * on target architecture (e.g. accumulator on CBC), it may provoke spill, and we should select for spill the
        * same one node for all incoming blockEnds. */
      def makeSpillForBlockIncomingEdges(): Unit = {
        val ends = block.predBlocks.map(_.blockEnd).toList
        if (ends.exists(_.isInstanceOf[Goto])) {
          assert(ends.forall(_.isInstanceOf[Goto]))
          // All goto are equal in terms of spoiled resources amount, we can use any of them to calculate diffs.
          assert(allRegFiles forall { file => (ends map { end => spoiledRegistersAmount(end, file)}).toSet.size == 1 })
          for (diff <- currentState.calculateDiffs(ends.head) if diff.spillAmount > 0) {
            for (x <- LocalSpill.selectFrom(diff.file, diff.spillAmount, exclude = Set.empty)) {
              insertHint(x, BulldozerHint.spillAssert)
            }
          }
        }
      }

      /** Appends `node` to generation order, inserts all required hints and updates DAG/State structures. */
      def makeGenStep(node: Node): Unit = {

        /** Links `node` in code order. Pins node to current point. */
        def link(node: Node): Unit = {
          def linkImpl(node: Node): Unit = {
            node match {
              case floating: FloatingNode => floating atLowerPoint currentPoint
              case _ =>
            }
            CodeOrder.prepend(node, block)
          }

          node.attachedResults foreach linkImpl
          node match {
            case node: ControlNode => node.projections foreach linkImpl
            case _ =>
          }
          linkImpl(node)
          node.attachedArgs foreach linkImpl
        }

        def withGCActions(node: Node): Unit = node match {
          case call: Call if call.gcActions.generateGCSafeRegion =>
            val liveSet = currentState.registerNodesSet(RegFile.IREG)
            val args = call.groupedValueArgs.toSet
            for (n <- liveSet if !args(n) && mayBeTraceableReference(n)) {
              insertHint(n, BulldozerHint.spillAssert)
            }

          case _ =>
        }

        // TODO: make something with this code

        def genNodeWithSpillsAround(node: Node): Unit = {
          spillLiveThroughNodes(node)
          insertSpillNodesForResults(node)
          val spilledArgs = collectArgsSpilledBelowNode(node)

          currentState.process(node, {
            for (Edge(arg, _) <- spilledArgs) {
              insertHint(arg, BulldozerHint.spill)
            }
          })

          withGCActions(node)

          node match {
            case sn: SpinalNode if sn.hasXHandler => mergeWithHandler(currentState, sn)
            case _ =>
          }

          node match {
            case _: BlockParamNode => // nothing to do
            case _ => dag.processNode(node); link(node)
          }

          for (Edge(arg, _) <- spilledArgs) {
            insertHint(arg, BulldozerHint.store)
          }

          for (Edge(saver: SpoiledArgSaver, _) <- node.groupedValueInEdges) {
            makeGenStep(saver)
          }
        }

        rematerializeFragilePointersIfCouldBeInvalidatedAt(node)

        // TODO: try to refactor this script
        node match {
          case `block` =>
            if (block.isInterpreterCaseStart) {
              for (nodeOnReg <- currentState.registerNodesSet() if currentState.loop.isArgument(nodeOnReg)) {
                insertHint(nodeOnReg, BulldozerHint.spillAssert)
              }
            }
            block.paramNodes foreach {
              case c: Catch => genNodeWithSpillsAround(c)
              case p: Phi => // nothing to do, spill requirement passed to predecessors
              case p => insertSpillNodesForResults(p); currentState.process(p, {})
            }
            makeSpillForBlockIncomingEdges()
            block.paramNodes foreach link
            link(node)

          case _: BulldozerHint | _: SpoiledArgSaver =>
            dag.processNode(node)
            currentState.process(node, {})
            link(node)

          case _ =>
            genNodeWithSpillsAround(node)
        }
      }


      /** Returns RP difference come out of given `node` generation. */
      def rpDiffCost(node: Node): Int = {
        val diffs = currentState.calculateDiffs(node)
        (diffs map { _.pressureDiff }).sum // TODO: what about spill amount? It's time to use it!
      }

      /** Selects new target for bulldozer, if previous was already generated. */
      def selectTarget(): Node = {
        nextPoint // TODO: implement more complicated selection
      }

      /** Creates crown filter by selected target. */
      def filterByTarget(target: Node): Node => Boolean = {
        def generateOnlyTargetDependencies(node: Node): Boolean = {
          upperPoint(node) == target
        }

        // Generate all nodes from crown, to avoid to push them to extra execution paths
        // TODO: sometimes it's better to push node to extra execution paths, than to increase RP.
        def generateEverything(node: Node): Boolean =
          true

        target match {
          case bb: BBlock if bb.predBlocks.size <= 1 =>
            // If block has 1 or 0 incomming edges, we generate everything from crown
            // to avoid pushing nodes to excessive execution paths.
            generateEverything

          case b: Block if gcm.loops.depth(b) < gcm.loops.depth(b.idomBlock) =>
            // If immediate dominator of current block has bigger loop nest, we generate
            // everything to avoid pushing nodes to it.
            generateEverything

          case _ =>
            // Otherwise, we generate only target dependencies and push all other nodes
            // to immediate dominator of current block.
            generateOnlyTargetDependencies
        }
      }


      //// Initialization

      if (blockStatus(block) == BlockStatus.NOT_INTERPRETED) {
        makeGenStep(block.blockEnd)
        var next = block.blockEnd.inCtrl
        while (next.isInstanceOf[BulldozerHint]) {
          if (!(CodeOrder contains next)) {
            assert(dag.crown contains next)
            makeGenStep(next)
          }
          next = next.asInstanceOf[BulldozerHint].inCtrl
        }
      }


      //// Main loop

      var target: Node = null
      var targetFilter: Node => Boolean = null

      while (true) {
        if (currentState.local.spillPressure > maxSpillPressure) {
          maxSpillPressure = currentState.local.spillPressure
        }

        val currentCrown = dag.crown filter {
          case n: ControlNode => n == nextPoint
          case n: FloatingNode => currentPoint dominates latestPointFor(n)
        }

        // 1. Apply positive heuristics to crown
        var crownChanged = false

        // crown should be rechecked on nodes containing because some nodes could be generated out of order (e.g. fragile pointers)
        // TODO: make crown auto updatable
        for (node <- currentCrown if dag.crown.contains(node) && rpDiffCost(node) <= 0) {
          makeGenStep(node)
          crownChanged = true
        }

        // TODO: positive subset heuristic, cluster heuristic

        // 2. Select target and make one step to it
        if (!crownChanged) {
          if (target == null || (CodeOrder contains target)) {
            target = selectTarget()

            if (target == block && !tillTheEnd) {
              // This block may not be generated to the end, until some event occur
              setBlockStatus(block, BlockStatus.PARTIALLY_INTERPRETED)
              return
            }

            targetFilter = filterByTarget(target)
          } // Confirm target selection, if required

          val subCrown = currentCrown.iterator withFilter targetFilter

          if (subCrown.nonEmpty) {
            makeGenStep(subCrown.next()) // TODO: this selection may be more complicated

          } else {
            assert(target == block)
            makeGenStep(block)
            setBlockStatus(block, BlockStatus.FULLY_INTERPRETED)
            return
          }
        }
      }

      shouldNotReachHere()
    }
  }
}
