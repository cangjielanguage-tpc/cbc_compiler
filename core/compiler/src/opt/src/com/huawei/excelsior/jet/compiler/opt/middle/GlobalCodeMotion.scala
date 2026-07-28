/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.{Stage, symlevel}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.graph.Loops
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections, Worklist}

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of Dr.Click's GCM (global code motion) algorithm that links all nodes to blocks.
 * Detailed description of original algorithm can be found in following paper:
 * ''"Combining Analyses, Combining Optimizations" by Clifford Noel Click, Jr. (1995);
 * Chapter 6 "Optimizing Without the Global Schedule"''.
 *
 *
 * __Definitions__:
 *
 * - __Pinned__ node - node which has already been linked to some block before GCM.
 * - __Incoming memory__ of block `B` - node which produces memory that live at start of block `B`.
 * - __Outgoing memory__ of block `B` - node which produces memory that live at end of block `B`.
 * - __Upper bound__ of node `N` (defined in suppose that all arguments of node `N` are already
 *   linked) - the first block dominated by all `N`'s arguments.
 * - __Lower bound__ of node `N` (defined in suppose that all uses of node `N` are already
 *   linked) - the last block which dominate all `N`'s uses.
 *
 * __Restrictions are__:
 *
 * - Pinned nodes could not be re-linked.
 * - Node should be linked to block that is dominated by all its arguments.
 * - Node should be linked to block that dominates all its uses.
 *
 * __Pinned nodes calculation__:
 *
 * Pinned nodes are: ''root, blocks, block ends, control nodes, phi-functions, phi-arguments (special nodes), catches''.
 * All their projections (if they exist) are pinned nodes too.
 *
 * For now we define ''cmp'' as pinned node too, but only because we could not properly process them later
 * (in block splitting and in code generation).
 *
 *
 * __The assumption of the reachability__:
 *
 * GCM works in assumption that IR contains no unreachable nor dead code.
 * This is acheived by running UCE (unreachable code elimination) and DCE (dead code elimination) before GCM starts.
 * Specifically, GCM needs that every node in the IR is reachable from pinned nodes by use-def and def-use chains, which
 * is guaranteed by UCE & DCE.
 * Exception from assumption of the reachability is leaf nodes ( see [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.LeafNode]]) -
 * nodes that have no node arguments. We process them specially.
 *
 *
 * __Algorithm taken into account only direct dependencies (data and control)__:
 *
 * - '''Schedule early'''.
 *   Recursive procedure, that links each node to its upper bound. For this purposes, it first schedules up
 *   node arguments, then calculates upper bound for node and link node to it. If node is pinned, it is not movable.
 *   In this restriction and in <i>the assumption of reachability</i> schedule up procedure is not infinitely
 *   recursive.
 *
 * - '''Find late'''.
 *   Recursive procedure, that finds lower bound for each node. For this purpose, it first finds late and
 *   schedules late (see below) all node uses, then calculates lower bound for node. This procedure is not
 *   infinitely recursive for the same reason as schedule early is not infinitely recursive.
 *
 * - '''Schedule late'''.
 *   After upper bound (`UB`) and lower bound (`LB`) for node `N` is defined, we could link it to any block
 *   on `[UB, LB]` dominators tree range. It is so, because every block `B` from `[UB, LB]`
 *   is dominated by all arguments of `N` and dominate all uses of `N`.
 *   We select "the best" block from the range by placing `N` as late as possible (thus avoiding
 *   unnecessary node execution on some control paths) in minimal loop nested block (thus moving the node
 *   from inside loops where possible).
 *
 *
 * __Memory anti-dependencies__:
 *
 * If node `G` use memory produced by node `P`, there should not be other memory producer nodes between `P` and `G`.
 * We could guarantee this by one of two possibilities: place `G` to `P`'s block or
 * place `G` to block `B` such as `incomingMemory(B) == G`.
 * In any case later stages (block splitter or code generator) always could find linear order of nodes within a block
 * that does not violate anti-dependence between memory writers and readers.
 *
 *
 * __Incoming memory calculation__:
 *
 * If block `B` contains memory phi-function `F`, then `incomingMemory(B) == F`.
 * Otherwise, `incomingMemory(B) == outgoingMemory(P)` for any `P in preds(B)` (since there could not be more than one
 * live memory node at each CFG point, all `B`'s incoming blocks must have the same outgoing memory).
 *
 *
 * __Algorithm taken into account anti-dependencies__:
 *
 * - '''Schedule early'''. As in previous algorithm.
 * - '''Find late'''. As in previous algorithm.
 * - '''Schedule late'''. If node `N` does not use memory, then it scheduled as in previous algorithm.
 *   Otherwise, found range `[UB, LB]` of block candinates is filtered with the additional criteria:
 *   `b => (b == UB) || incomingMemory(b) == memoryInput(N)`. Selection used as in previous algorithm.
 *
 *   It works because of two facts:
 *
 *   - If there is some block in range `[UB, LB]` that have incoming memory equals to `N`'s memory argument,
 *     then range is not empty, and linking node `N` to one of the filtered blocks is correct
 *     (`N` could use such memory, because it is incoming to its block)
 *   - Otherwise, block `UB` has incoming memory not equals to `N`'s memory argument.
 *     As `UB` is the upper bound of `N`, memory argument of `N` must be placed at `UB` before
 *     (otherwise, our IR is incorrect, and we should fail with assertion).
 *     So we could link `N` directly to `UB` block.
 *
 * @author conwor
 * @author paul
 */
trait GlobalCodeMotion extends MemoryOptimizations { self: Universe =>

  private def unpin(n: FloatingNode): Unit = {
    n atUpperPoint null
  }

  /** Returns upper point for given node's range.
    *
    * Note: for BlockEnd will return its inCtrl.
    */
  def upperPoint(n: Node): UpperPoint = n match {
    case n: FloatingNode => n.upperPoint
    case n: BlockParamNode => n.block
    case n: UpperPoint => n
    case n: BlockEnd => n.inCtrl
  }

  /** Calculates upper point for given node's range,
    * based on its arguments which satisfy predicate `p`.
    */
  def upperPointByArgs(n: FloatingNode, p: Edge => Boolean): UpperPoint = {
    val argUpperPoints = n.inEdges collect { case e if p(e) => upperPoint(e.source) }
    argUpperPoints.foldLeft(n.scope.entryBlock: UpperPoint)(ControlNode.lowest)
  }

  /** Returns lower point for given node's range.
    *
    * Note: for Block will return its outCtrl.
    */
  def lowerPoint(n: Node): LowerPoint = n match {
    case n: FloatingNode => n.lowerPoint
    case n: BlockParamNode => n.block.outCtrl
    case n: LowerPoint => n
    case n: Block => n.outCtrl
  }

  private var gcmDone = false
  private var incrementalGCM = false

  /** Require that GCM is not done. Throws exception otherwise. */
  def requireNoGlobalCodeMotion(): Unit = require(!gcmDone, "GCM results should be invalidated somewhere earlier")

  /** Require that GCM is done somewhere earlier. Throws exception otherwise. */
  def requireGlobalCodeMotion(): Unit = require(gcmDone, "GCM should be done somewhere earlier")

  /** Require that GCM is done somewhere earlier and all nodes are pinned. Throws exception otherwise. */
  def requireAllNodesPinned(): Unit = require(gcmDone && !incrementalGCM, "all nodes must be pinned by GCM somewhere earlier")

  /** Require that incremental GCM is in effect. Throws exception otherwise. */
  def requireIncrementalGCM(): Unit = require(incrementalGCM, "incremental GCM should be in effect")

  /** Invalidate GCM results. */
  private def undoGlobalCodeMotion(): Unit = {
    if (gcmDone) {
      for {
        n <- collect[FloatingNode](allScopes.flatMap(_.allNodes))
      } {
        unpin(n)
      }
      gcmDone = false
      incrementalGCM = false
    }
  }

  private def pinEarly(n: FloatingNode): Unit = {
    if (!n.pinned) {
      n atUpperPoint upperPointByArgs(n, _ => true)
    }
  }

  private def blocksRange(latest: ControlNode, earliest: ControlNode): Iterator[Block] =
    collect[Block](cfg.dominators.range(latest.block, earliest.block))

  private def memoryRedefinitionIn(block: Block, mem: MemoryNode): Option[SpinalMemoryNode] = {
    mem.memoryUses collectFirst { case x: SpinalMemoryNode if x.block == block => x }
  }

  private def movable(n: Node) = n.isInstanceOf[FloatingNode]


  class GCMEngine(onlyEarly: Boolean = false, allowRematerialization: Boolean = false, optimizeMemoryAntiDependency: Boolean = false, forceSheduleLate: Boolean = false) {
    // Lazy, because do not used in first pass
    lazy val loops = cfg.loops
    lazy val cold = findColdBlocks()

    private val liveNodes = Sets[Node].newMSet

    final def schedule(): Unit = {
      all[FloatingNode] foreach unpin
      liveNodes.clear()

      val movableUnusedNodes = if (genDebug) {
        all[StackAlloc] collect { case n @ StackAlloc.DebugVar(_, _) if n.uses.isEmpty => n }
      } else {
        Iterator.empty
      }

      Closure.withPostAction(liveNodes, (allNodes filterNot movable) ++ movableUnusedNodes)(_.args filter movable) {
        case n: FloatingNode => pinEarly(n)
        case _ =>
      }
      if (!onlyEarly && !isO1Compiled || forceSheduleLate) {
        scheduleLate(cfgHeuristics)
      }
    }

    /** Returns latest point for `n` based on its uses or None iff `n` is dead or unreachable node. */
    final def latestPointByUses(n: FloatingNode): Option[ControlNode] = {
      if (n.block.unreachable) {
        None
      } else {
        val usePoints = n.outEdges map (e => Projection.skip(e.usePoint)) filterNot {
          usePoint => usePoint == null || // Use is not bound to any point (it was not processed in scheduleEarly), so it is dead
            usePoint.block.unreachable
        }
        if (usePoints.isEmpty) {
          None
        } else {
          Some(usePoints reduce { (x, y) => x nearestDom y })
        }
      }
    }

    /** Returns corrected `latest` point for `n` based on memory anti-dependency or None iff `n` is not
      * dependent on memory or its memory anti-dependency does not dominate `latest`.
      */
    final def memoryAntiDependency(n: FloatingNode, latest: ControlNode): Option[ControlNode] = n match {
      case HasInMemory(inMem) =>
        // If node requires memory, select lowermost point in range [earliest, latest] with appropriate memory
        // TODO: make memory anti-dependency a regular IR edge
        val earliest = n.lowerPoint

        // `block` is the lowermost block in range [earliest, latest] which contains at least one point with
        // appropriate memory. All subrange [earliest, block) has appropriate memory. All subrange (block, latest]
        // has not appropriate memory.
        val block = blocksRange(latest, earliest) find (_.memoryAfter == inMem) getOrElse earliest.block

        // `memRedef` is the last point in `block` with appropriate memory.
        val memRedef = memoryRedefinitionIn(block, inMem) getOrElse block.blockEnd

        if (latest dominates memRedef) {
          None

        } else {
          val point = memRedef nearestDom latest

          // If `memRedef` dominates `latest`, `point` will be equal to `memRedef`. Also there could
          // be situation when none of `memRedef` and `latest` dominates each other. Consider `block` having
          // some exceptional edges, which is going to `latest`. In this case `point` is a point from `block`
          // which dominates both `latest` and `memRedef`.
          assert(point.block == block)

          Some(point)
        }

      case _ => None
    }


    private var allNodesAreDraggedInColdCode = false

    final def cfgHeuristics(earliest: ControlNode, latest: ControlNode): ControlNode = {
      // 0. Unreachable code handling
      if (latest.block.unreachable) {
        return earliest
      }

      // 1. Cold code
      var range = blocksRange(latest, earliest).toList
      if (cold(range.head)) {
        if (allNodesAreDraggedInColdCode) {
          assert(range forall cold)
        } else {
          range = range filter cold
        }
      } else {
        assert(!(range exists cold))
      }

      // 2. Min loops nest
      val b = range minBy loops.depth

      // 3. Select point
      b.blockEnd nearestDom latest
    }

    protected def needsRematerialization(n: FloatingNode): Boolean = false


    private var _irWasChanged: Boolean = false
    def irWasChanged = _irWasChanged

    protected final def scheduleLate(cfgHeuristics: (ControlNode, ControlNode) => ControlNode): Unit = {

      def pinLate(n: FloatingNode): Unit = {
        def pinLateImpl(latest: ControlNode): Unit = {
          val newPoint = cfgHeuristics(n.lowerPoint, latest)
          n atLowerPoint newPoint.asInstanceOf[LowerPoint]
        }

        def optimizedMemoryAntiDependency(n: GetMemoryOperation, mem: MemoryNode, latestByUses: ControlNode): ControlNode = {
          // TODO: feel free to analyze memory graph outside of `mem` block
          var curr = mem
          while (MemoryDependencies.readCouldBeMovedBelowWrite(n, curr)) {
            memoryRedefinitionIn(curr.block, curr) match {
              case Some(next) if next dominates latestByUses => curr = next
              case None if curr.block.blockEnd dominates latestByUses => return curr.block.blockEnd
              case _ => return curr
            }
          }
          curr
        }

        val latestByUses = latestPointByUses(n) match {
          case Some(point) => point
          case None => return // `n` is dead or unreachable, there is no need to schedule it
        }

        val antiDep = memoryAntiDependency(n, latestByUses)

        (n, antiDep) match {
          case (n: GetMemoryOperation, Some(baseMemory: MemoryNode)) if optimizeMemoryAntiDependency =>
            val optimizedMemory = optimizedMemoryAntiDependency(n, baseMemory, latestByUses)
            pinLateImpl(optimizedMemory)

            if (baseMemory strictDominates n.lowerPoint) {
              // We move `node` below it's memory anti-dependency, so we should update it's incoming memory,
              // or IR will be incorrect.
              n.memoryEdge.source = n.lowerPoint.asInstanceOf[HasInMemory].inMemory
              _irWasChanged = true
            }

          case _ =>
            pinLateImpl(antiDep getOrElse latestByUses)
        }
      }

      // We should collect order of nodes based on their uses before calling `pinLate` because it can modify uses lists.
      def closureMovableUses(nodes: IterableOnce[Node]): ArrayBuffer[FloatingNode] = {
        val order = new ArrayBuffer[FloatingNode]()
        Closure.withPostAction(Sets[Node].newMSet, nodes)(_.uses filter movable) {
          case n: FloatingNode if liveNodes(n) => order.append(n)
          case _ =>
        }
        order
      }

      // Temporarily disabled. TODO: enable
      // val rematerialized = Sets[Node].newQSet

      if (allowRematerialization) {
        for (node <- closureMovableUses(all[FloatingNode] filter needsRematerialization)) {
          def equiv(e1: Edge, e2: Edge): Boolean = (e1.usePoint, e2.usePoint) match {
            case (x, y) if x == y => true
            case (null, _) | (_, null) => false // skip dead nodes
            case (x, y) => cfgHeuristics(node.lowerPoint, x) domComparable cfgHeuristics(node.lowerPoint, y)
          }
          Node.rematerialize(node, equiv _) foreach { rem =>
            pinEarly(rem)
            pinLate(rem)
            // rematerialized += rem
          }
        }
      }

      val start = allNodes filter { n => !movable(n) || n.isInstanceOf[LeafNode[_]] }
      for (node <- closureMovableUses(start) /* if !rematerialized(node) */) {
        pinLate(node)
      }
    }

    /** Leaves all nodes at their earliest points, except the ones which may be moved to cold code. */
    final def dragNodesIntoColdCode(): Unit = {
      scheduleLate { (earliest, latest) =>
        assert(!latest.block.unreachable)

        if (!cold(latest.block) || cold(earliest.block)) {
          earliest

        } else {
          val earliestCold = ScalaCollections.lastElement(blocksRange(latest, earliest) filter cold).get
          earliestCold.blockEnd nearestDom latest
        }
      }

      // TODO: improve fragile pointers rematerialization in cold code (JET-12176)
      var rematerialized = true
      while (rematerialized) {
        // Do rematerialization in loop for proper moving chains of Lea
        rematerialized = false
        for (node <- all[FloatingNode] if node.isFragilePointer && !cold(node.block)) {
          Node.rematerializeConditionally(node, e => cold(e.target.block)).toSeq match {
            case Seq(`node`) => // no rematerialization happened
            case copies =>
              assert(copies.size > 1)
              val origPoint = node.upperPoint
              for (copy <- copies) {
                copy.valueUses.map(_.block).toSeq match {
                  case Seq(useBlock) if cold(useBlock) =>
                    // `useBlock` is approximation and actually may be above `origPoint` making loops in DAG.
                    // But we have checked that `node` block is not cold, so `origPoint` could not be in cold `useBlock`.
                    assert(origPoint dominates useBlock)
                    copy atUpperPoint useBlock
                  case uses =>
                    assert(uses forall (!cold(_)))
                    copy atUpperPoint origPoint
                }
              }
              rematerialized = true
          }
        }
      }

      allNodesAreDraggedInColdCode = true
    }

  }

  def withGCM[T](engine: GCMEngine = new GCMEngine())(action: => T): T = {
    doGlobalCodeMotion(engine)
    try {
      withChecks(action)
    } finally {
      undoGlobalCodeMotion()
    }
  }

  def doGlobalCodeMotion(engine: GCMEngine = new GCMEngine()): Unit = stage(Stage.GCM) {
    assert(!gcmDone)
    engine.schedule()
    gcmDone = true

    dbgPrinter.debugNodes("GCM done")
    checkConsistency(CheckLevels.Optional) { checkDefUseDominanceImpl() }
  }

  /** During incremental GCM nodes are pinned on demand using pinEarly strategy.
    *
    * The point of any floating node can be recalculated by accessing its upperPoint, lowerPoint or block.
    *
    * Note that the point does not persist during arbitrary IR transformations
    * and it will be automatically updated or invalidated to preserve the following invariants:
    *
    * 1. If a floating node is pinned, it is pinned to the earliest possible point.
    * 2. If a floating node is __not__ pinned, its floating uses are transitively __not__ pinned.
    *
    * Note: nested incremental GCM braces are allowed.
    */
  def withIncrementalGCM[T](action: => T): T = {
    if (gcmDone) {
      assert(incrementalGCM)
      assert(shouldRepinAfterStructuralChange)
      action

    } else {
      gcmDone = true
      incrementalGCM = true
      try {
        afterStructuralChange.withCallback(repinAfterStructuralChange) {
          onPointRecalculation.withCallback(pinEarly) {
            withChecks(action)
          }
        }
      } finally {
        undoGlobalCodeMotion()
      }
    }
  }

  /** Same as [[withIncrementalGCM]], but does nothing if full GCM is in action. */
  def withAnyGCM[T](action: => T): T = {
    if (gcmDone) {
      action
    } else {
      withIncrementalGCM(action)
    }
  }

  private def withChecks[T](action: => T): T = {
    val r = action
    // Don't move this check into finally because we don't want to override action's exception by later check's one.
    checkConsistency(CheckLevels.Optional) { checkDefUseDominanceImpl() }
    r
  }

  private def repinAfterStructuralChange(e: Edge): Unit = {
    def unpinWithUses(n: FloatingNode): Unit = {
      if (n.pinned) {
        unpin(n)
        collect[FloatingNode](n.uses) foreach unpinWithUses
      }
    }

    e.target match {
      case n: FloatingNode if n.pinned =>
        val arg = e.source
        if (!shouldRepinAfterStructuralChange || arg == null || !arg.isCommitted || cond(arg) { case arg: FloatingNode => !arg.pinned }) {
          unpinWithUses(n)

        } else { // arg is either PinnedNode or pinned FloatingNode

          // this assert as well as the fact that n.pinned is true
          // guarantees that onPointRecalculation callback will not be triggered below
          assert(collect[FloatingNode](n.args) forall (_.pinned))

          val oldPoint = n.upperPoint
          unpin(n)
          pinEarly(n)
          if (n.upperPoint != oldPoint) {
            // if point changed, then we must re-pin uses as well
            n.outEdges foreach repinAfterStructuralChange
          }
        }

      case _ => // target is either PinnedNode or unpinned FloatingNode
    }
  }

  private var shouldRepinAfterStructuralChange = true

  /** Runs given `action` during incremental GCM with point recalculation after structural change disabled.
    *
    * Note: floating node can still be pinned on-demand by accessing upperPoint, lowerPoint and block.
    */
  def withoutRepinAfterStructuralChange[T](action: => T): T = {
    assert(incrementalGCM)
    assert(shouldRepinAfterStructuralChange)
    shouldRepinAfterStructuralChange = false
    try {
      action
    } finally {
      shouldRepinAfterStructuralChange = true
    }
  }

  def checkDefUseDominance(): Unit = {
    // Don't mess with unholy BackEnd GCM.
    if (currentPhase < CompilerPhase.BackEnd) {
      if (gcmDone) {
        checkDefUseDominanceImpl()
      } else {
        withIncrementalGCM {
          checkDefUseDominanceImpl()
        }
      }
    }
  }

  private def checkDefUseDominanceImpl(): Unit = {
    for (defNode <- allNodes) {
      val defPoint = defNode match {
        case n: Projection => n
        case n: Block => n
        case n: SpinalNode => lowerPoint(n.outCtrl)
        case n => lowerPoint(n)
      }

      // Note that either of defPoint or usePoint may be null for dead nodes in full GCM.
      if (defPoint != null) {
        for (useEdge <- defNode.outEdges; useNode = useEdge.target) {
          val usePoint = useNode match {
            case _: Block | _: XPoint => null
            case _: Phi => useEdge.usePoint
            case n: Projection => n
            case n => lowerPoint(n)
          }
          if (usePoint != null) {
            if (defPoint.block.unreachable) {
              assert(usePoint.block.unreachable, s"unreachable $defNode at $defPoint should not be used by reachable $useNode at $usePoint")
            } else {
              assert(defPoint dominates usePoint, s"$defNode at $defPoint should dominate $useNode at $usePoint")
            }
          }
        }
      }
    }
  }
}
