/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.util.ScalaCollections.{mapWith, toMultiMap}
import com.huawei.excelsior.jet.compiler.util.{Log, Maps, Sets}
import com.huawei.excelsior.jet.util.graph.{BiGraph, Loop}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections, Worklist}

import scala.collection.mutable.ArrayBuffer

/** Framework for complex CFG transformations (Xi-transformations) backed by CFG copying:
  *   - Arbitrary block copying;
  *   - Loop peeling;
  *   - Loop unrolling;
  *   - Versioning.
  *
  * @author liontiger
  */
trait XiTransform { self: Universe =>

  /** See [[xiTransformAndPostProcess]]. */
  def xiTransform[A](schedule: XiScheduler => A): A =
    xiTransformAndPostProcess(schedule) { (_, res) => res }

  /** Runs `schedule` with newly created [[XiScheduler]],
    * which can be used to schedule IR graphs and blocks for copying (e.g. versioning, peeling etc.).
    *
    * After scheduling the blocks are copied by [[XiCloner.process]].
    *
    * Then `postProcess` action is performed given a map from original nodes to copied ones, wrapped in [[XiResult]].
    */
  def xiTransformAndPostProcess[A, B](schedule: XiScheduler => A)(postProcess: (XiResult, A) => B): B = {
    val cloner = new XiCloner
    val scheduler = new XiScheduler(cloner)
    val res = schedule(scheduler)
    val xi = cloner.process()
    postProcess(xi, res)
  }

  /** Result of xi-transformation, representing essentially a map from original control node to copied ones.
    *
    * Note: only control nodes (not floating nodes) can persist through copying process.
    */
  class XiResult private[XiTransform](_copies: IterableOnce[(ControlNode, ControlNode)]) {
    private val copies = Maps[ControlNode].newQMap(toMultiMap(_copies)).withDefaultValue(null)

    // collect projections, because they are created implicitly with corresponding owner
    for (n <- copies.keys.to(ArrayBuffer) if n.projections.nonEmpty) {
      val ncopies = copies(n)
      if (ncopies.size == 1) {
        copies ++= n.projections zip (ncopies.head.projections map (Seq(_)))
      } else {
        copies ++= toMultiMap(ncopies flatMap (n.projections zip _.projections))
      }
    }

    /** Returns the only copy of given node with explicit cast to corresponding type. */
    def copyOf[N <: ControlNode](n: N): N = ScalaCollections.singleElement(copiesOf(n))

    /** Returns all copies of given node with explicit cast to corresponding type. */
    def copiesOf[N <: ControlNode](n: N): Iterator[N] = copies(n).iterator map (_.asInstanceOf[N])
  }

  /** Minimal set of blocks which should be copied to version all anchors under the single versioning test.
    * This set contains all blocks on all paths from anchors' common dominator (test location) to every anchor.
    */
  def versioningSubGraph(anchors: LowerPoint*): collection.Set[Block] = {
    anchorSubGraphWithEntry(entryOfAnchorSubGraph(anchors), anchors)
  }

  private def anchorSubGraphWithEntry(entry: LowerPoint, anchors: Seq[LowerPoint]): collection.Set[Block] = {
    BiGraph.anchorSubGraph(cfg, entry.block, anchors map (_.block): _*)
  }

  private def entryOfAnchorSubGraph(anchors: Seq[LowerPoint]): LowerPoint = {
    assert(!anchors.exists(_.block.unreachable))
    lowerPoint(anchors.reduce[ControlNode](_ nearestDom _)) ensuring { entry => anchors forall entry.dominates }
  }

  /** Provides public high-level interface for xi-transformations scheduling.
    *
    * All transformations (except for [[unsafe]] ones) always preserve semantics of the program.
    */
  class XiScheduler private[XiTransform](cloner: XiCloner) {

    /** Returns true if node is scheduled for copying. */
    def shouldCopy(n: Node): Boolean = cloner.isMarked(n)

    /** Schedules given `anchors` for versioning under given `test`.
      *
      * The whole "anchor subgraph" is marked for copying (see [[markAnchorSubGraph]]).
      *
      * Note: `test` will pass for the copied version and fail for the original one.
      *
      * Example transformation with X & Y as anchors:
      * {{{
      *                            test
      *                          f/    \t
      *                          /      \
      *        A                A        A'
      *       / \              / \      / \
      *      B   Y   ----->   B   Y    B'  Y'
      *      |   |            |   |    |   |
      *      X   D            X   |    X'  |
      *      |                |   |____|_ _|
      *      C                |__ _____| |
      *                          |       D
      *                          C
      * }}}
      */
    def version(test: PredicateConstructor, anchors: LowerPoint*): (Seq[If], Block, Block) = {
      val (entry, res @ (_, trueBlock, _)) = markAnchorSubGraph(anchors) { entryPoint =>
        val res = insertEmptyDiamondBefore(entryPoint, test)
        (entryPoint.block, res)
      }

      val e = ScalaCollections.singleElement(entry.inEdges.filter(_.source.block == trueBlock))
      cloner.schedulePostActions(new ExtractEdges(entry, e))

      res
    }

    /** Schedules given `loop` for versioning under given `test`.
      *
      * Whole loop is scheduled for copying, with versioning point at pre-header.
      *
      * Note: `test` will pass for the copied version and fail for the original one.
      *
      * Example transformation:
      * {{{
      *                                     A
      *                                     |
      *                                ___ test ___
      *        A   ____               |f  ____    t|   ____
      *        |  |    |              |  |    |    |  |    |
      *       _V__V_   |             _V__V_   |   _V__V_   |
      *      |      |  |            |      |  |  |      |  |
      *      | loop |__|   ----->   | loop |__|  | copy |__|
      *      | body |               | body |     | body |
      *      |______|               |______|     |______|
      *         |                      |            |
      *         V                      V            V
      *         B                      B            B
      * }}}
      */
    def version(test: PredicateConstructor, loop: Loop[Block]): (Seq[If], Block, Block) = {
      val (preHeader, _) = getOrCreateLoopPreHeader(loop)
      version(test, Seq(preHeader.blockEnd, loop.header.outCtrl): _*)
    }

    /** Schedules given `loop` for peeling.
      *
      * Whole loop is scheduled for copying, with back-edges of original loop redirected to the copied one.
      *
      * Example transformation:
      * {{{
      *        A   ____               A        ____    ____
      *        |  |    |              |       |    |  |    |
      *       _V__V_   |             _V____   |   _V__V_   |
      *      |      |  |            |      |  |  |      |  |
      *      | loop |__|   ----->   | loop |__|  | copy |__|
      *      | body |               | body |     | body |
      *      |______|               |______|     |______|
      *         |                      |            |
      *         V                      V            V
      *         B                      B            B
      * }}}
      */
    def peel(loop: Loop[Block]): Unit = {
      require(loop.header.isInstanceOf[BBlock])
      cloner.mark(loop.body.toSeq: _*)
      cloner.schedulePostActions(new ExtractEdges(loop.header, loopBackwardEdges(loop).toSeq: _*))
    }

    /** Schedules given `loop` for unrolling 'n' times.
      *
      * Whole loop is scheduled for copying n times, with
      * back-edges of original loop redirected to the first copied loop,
      * back-edges of the first copied loop redirected to the second copied loop and so on, and finally
      * back-edges of the last copied loop redirected to the original one.
      *
      * Example transformation:
      * {{{
      *                                   __________________________________________________
      *        A   ____               A  |     ____         ____                ____        |
      *        |  |    |              |  |    |    |       |    |              |    |       |
      *       _V__V_   |             _V__V_   |   _V____   |   _V____          |   _V____   |
      *      |      |  |            |      |  |  |  1   |  |  |  2   |         |  |  n   |  |
      *      | loop |__|   ----->   | loop |__|  | copy |__|  | copy |__ ... __|  | copy |__|
      *      | body |               | body |     | body |     | body |            | body |
      *      |______|               |______|     |______|     |______|            |______|
      *         |                      |            |            |                   |
      *         V                      V            V            V                   V
      *         B                      B            B            B                   B
      * }}}
      */
    def unroll(loop: Loop[Block], n: Int): Unit = {
      require(loop.header.isInstanceOf[BBlock])
      require(n >= 1)
      cloner.mark(n, loop.body.toSeq: _*)

      val originalHeader = loop.header
      val originalBackwardEdges = loopBackwardEdges(loop).toSeq
      cloner.schedulePostActions(xi => {
        val copies = xi.copiesOf(originalHeader).toSeq
        for ((target, edges) <- (copies :+ originalHeader) zip (originalBackwardEdges +: copies.map(_.inEdges.toSeq))) {
          redirectEdges(target, edges: _*)
        }
      })
    }

    /** Schedules given `block` for copying, with `targetEdges` of this `block` extracted to the copied one.
      *
      * Example transformation:
      * {{{
      *   targetEdges: (B -> block), (C -> block)
      *
      *      A   B   C               A        B      C
      *       \  |  /                |         \    /
      *        block     ----->    block       copied
      *        /   \               /   \       /    \
      *       X     Y             X     Y     X      Y
      * }}}
      */
    def extract(block: Block, targetEdges: Edge*): Unit = {
      cloner.mark(block)
      if (targetEdges.nonEmpty) {
        cloner.schedulePostActions(new ExtractEdges(block, targetEdges: _*))
      }
    }

    /** Unsafe transformations that ''do not'' preserve semantics of the program. */
    object unsafe {

      /** Schedules given `anchors` for copying with given `targetEdge` target replaced by copied graph entry.
        *
        * The whole "anchor subgraph" is marked for copying (see [[markAnchorSubGraph]]).
        *
        * Example transformation with X as anchor and (X -> B) as targetEdge:
        * {{{
        *        A            A
        *        |            |
        *        X   ----->   X
        *        |            |
        *        B            X'
        *                     |
        *                     B
        * }}}
        *
        * Example transformation with X & Y as anchors and (Y -> D) as targetEdge:
        * {{{
        *        A                A
        *       / \              / \
        *      B   Y   ----->   B   Y
        *      |   |            |    \
        *      X   D            X     A'
        *      |                |    / \
        *      C                |   B'  Y'
        *                       |   |   |
        *                       |   X'  D
        *                       |_ _|
        *                         |
        *                         C
        * }}}
        */
      def copy(targetEdge: Edge, anchors: LowerPoint*): Unit = {
        val entry = markAnchorSubGraph(anchors) { entryPoint =>
          Block.splitBefore(entryPoint)
          entryPoint.block
        }
        redirect(targetEdge, _.copyOf(entry))
      }

      /** Schedules redirection of given `edges` to the block produced by `target` function.
        *
        * Example transformation with C already marked for copying and `_.copyOf(C)` target function and (C -> D) edge:
        * {{{
        *        A                A
        *       / \              / \
        *      B   C   ----->   B   C
        *       \ /             |   |
        *        D              |   C'
        *                       |_ _|
        *                         |
        *                         D
        * }}}
        *
        * Example transformation with `_ => C` target function and (B -> D) edge:
        * {{{
        *        A                A
        *       / \              /|
        *      B   C   ----->   B |
        *       \ /              \|
        *        D                C
        *                         |
        *                         D
        * }}}
        *
        * Example transformation with `_ => C` target function and (C -> D) edge:
        * {{{
        *                            __
        *        A                A |  |
        *       / \              / \V  |
        *      B   C   ----->   B   C  |
        *       \ /             |   |__|
        *        D              D
        * }}}
        */
      def redirect(edge: Edge, target: XiResult => Block): Unit = {
        cloner.schedulePostActions(xi => redirectEdges(target(xi), edge))
      }
    }

    /** Marks the subgraph formed by given `anchors` for copying (see [[BiGraph.anchorSubGraph]]).
      *
      * The entry point of this subgraph is pre-processed by given `processEntry` and
      * blocks are splitted after each anchor to minimize node copying.
      */
    private def markAnchorSubGraph[T](anchors: Seq[LowerPoint])(processEntry: LowerPoint => T): T = {
      val entryPoint = entryOfAnchorSubGraph(anchors)

      val res = processEntry(entryPoint)

      // Minimize amount of copied nodes.
      anchors foreach {
        case x: UpperPoint => Block.splitAfter(x)
        case _: BlockEnd => // no need to split
      }

      val blocks = anchorSubGraphWithEntry(entryPoint, anchors)
      blocks foreach (cloner.mark(_))

      res
    }
  }

  /** Stateful CFG cloner.
    *
    * Provides internal low-level interface for block cloning (or copying) along with all nodes pinned to such blocks.
    *
    * Copying is performed in several stages:
    *
    *   1. Blocks which are scheduled for copying are marked, possibly with appropriate post-action to go with it.
    *
    *   2. Cross-block data-flow and memory uses of nodes pinned to marked blocks are eliminated
    *      (using Vars and [[eliminateCrossBlockMemoryEdges]]).
    *
    *      Note: phi-functions in marked blocks are eliminated in this process.
    *
    *   3. Marked blocks are copied together with pinned nodes and CFG edges are copied according to the following rules:
    *     - Edges leading from marked blocks (to either marked or unmarked ones) are copied;
    *     - Edges leading from unmarked blocks (to either marked or unmarked ones) are not copied.
    *
    *      Note: copied blocks are created essentially as unreachable code.
    *
    *   4. All scheduled post-actions are performed, which should link original CFG to the copied one.
    *
    * Note that copied blocks remain unreachable from stage 3 until the end of stage 4,
    * so dominators must not be called during this period to preserve IR consistency.
    * To ensure this, incremental GCM is suspended and onCommit optimizations are deferred during stages 3 and 4.
    *
    * Note: post-actions are executed in a very fragile state,
    *       so they should avoid unnecessary IR modifications if possible,
    *       preferably only modifying CFG edges and nothing more.
    */
  private class XiCloner {

    private val marked = Maps[Block].newQMap[Int]
    private val postActions = Worklist.empty[PostAction]

    /** Marks given `blocks` for copying once. */
    def mark(blocks: Block*): Unit = {
      mark(1, blocks: _*)
    }

    /** Marks given `blocks` for copying `n` times. */
    def mark(n: Int, blocks: Block*): Unit = {
      require(blocks forall (!marked.contains(_)))
      marked ++= blocks map (x => x -> n)
    }

    /** Return true if node is marked for copying. */
    def isMarked(n: Node): Boolean = marked.contains(n.block)

    /** Schedules `_postActions` to be performed after CFG copying.
      *
      * Note: post-actions are executed in a very fragile state,
      *       so they should avoid unnecessary IR modifications if possible,
      *       preferably only modifying CFG edges and nothing more.
      */
    def schedulePostActions(_postActions: PostAction*): Unit = {
      postActions ++= _postActions
    }

    /** Performs actual copying of marked blocks and runs post-actions afterwards. */
    def process(): XiResult = {

      // Avoid unnecessary IR modification!
      if (marked.isEmpty) {
        assert(postActions.isEmpty)
        return new XiResult(Seq.empty)
      }

      // Critical edges must be eliminated before vars insertion!
      def markCriticalBlocks(n: Node) = n match {
        case b: Block =>
          // Newly inserted critical blocks should be marked iff they directly follow already marked block.
          // Note: if critical block connects unmarked block to marked one,
          //       then it should not be marked, because it may mess up post-action for block-successor.
          val pred = ScalaCollections.singleElement(b.predBlocks)
          if (marked.contains(pred)) {
            mark(marked(pred), b)
          }
        case _ =>
      }
      onCommit.withCallback(markCriticalBlocks) {
        splitCriticalEdges()
      }

      // Avoid copying return node, because it would break IR invariant (JET-13257)
      for (ret <- Return.unique if marked contains ret.block) {
        Block.splitBefore(ret)
      }

      withIncrementalGCM {

        eliminateCrossBlockMemoryEdges()
        eliminateCrossBlockControlEdges()
        eliminateCrossBlockValueEdges()

        // collect marked nodes as well as newly inserted Var nodes
        // Note: marked nodes must be collected before incremental GCM and dominators are suspended!
        val markedNodes = mapWith(collectMarkedNodes()) { n => marked(n.block) }

        // Since all copied nodes are created essentially as unreachable code,
        // dominators info remains inconsistent until all post-actions are finished.
        // So we must make sure that nobody triggers dominators recalculation
        // during copying and during post-actions (e.g. incremental GCM).
        // All onCommit optimizations must be deferred as well,
        // because they can mess up node copying process or trigger GCM / dominators.
        withDeferredOnCommitOptimizations {
          withoutRepinAfterStructuralChange {
            currentScope.withoutGraphTools {
              val xi = copyNodes(markedNodes)

              // Note: worklist allows recursive scheduling of post actions from other post actions.
              postActions.drain foreach (_ apply xi)

              marked.clear()
              postActions.clear()

              xi
            }
          }
        }
      }
    }

    private def eliminateCrossBlockControlEdges(): Unit = {
      // Repin nodes in unmarked blocks that have inCtrl in marked block,
      // because after inCtrl will be copied, IR will become inconsistent (no SSA completion for control edges).
      eliminateCrossBlockInCtrlUses(marked.keys)
    }

    private def eliminateCrossBlockValueEdges(): Unit = {

      // Clear cached dominating check information from WeakCasts,
      // because otherwise it may be cleared after marked nodes are collected,
      // then dependent WeakCast may be moved to marked block and its uses won't be eliminated (see JET-12110)
      all[CheckCast] foreach (_.unlinkDependentWeakCasts())

      // replace phies in marked blocks by vars in order to eliminate data-flow cycles
      def replacePhiesByVars(b: Block): Unit = b.phies.toList foreach replacePhiByVar
      for (b <- marked.keys) {
        replacePhiesByVars(b)
        // also replace phies in successors to avoid dealing with `addEdgeWithTemplate` during block copying
        for (s <- b.xSuccBlocks if !marked.contains(s)) {
          replacePhiesByVars(s)
        }
      }

      // collect marked nodes to eliminate their uses in non-marked blocks
      val markedNodes = collectMarkedNodes()

      // Note: inserted Var nodes are not re-collected here,
      //       because they are guaranteed to not have cross-block uses.

      // eliminate uses in non-marked blocks
      for (n <- markedNodes if n.producesValue) {
        // eliminate uses in phies
        for (phi <- collect[Phi](n.valueUses).toList if phi.isCommitted) {
          replacePhiByVar(phi)
        }

        // eliminate cross-block uses
        replaceValueUsesByVar(n)(_.useBlock != n.block)
      }
    }

    private def collectMarkedNodes(): collection.Set[Node] = {
      def shouldCopy(n: Node): Boolean = marked.contains(n.block) && !n.isInstanceOf[Projection]
      Closure[Node](Block.withParamNodes(marked.keys))(_.uses filter shouldCopy)
    }

    /** Note: marked nodes must be passed explicitly to avoid triggering incremental GCM and dominators. */
    private def copyNodes(markedNodes: collection.SeqMap[Node, Int]): XiResult = {
      val copies = Maps[Node].newQMap[Array[Node]]

      def needsCopying(n: Node) = markedNodes.contains(n) && !(copies contains n)

      def prepareCatches(entry: (Block, Int)): Unit = {
        val (b,copiesNum) = entry
        for (q <- collect[Catch](b.paramNodes)) {
          copies(q) = Array.fill(copiesNum)(null)
        }
      }

      def copyBlock(b: Block, idx: Int) = b.proto() match {
        case xb: XBlock => collect[Catch](b.paramNodes).foreach(q => copies(q)(idx) = Catch(xb)) ; xb
        case b:  BBlock => b
      }

      def copy[N <: Node](n: N, idx: Int): N = {
        if (!copies.contains(n)) {
          n
        } else {
          copies(n)(idx).asInstanceOf[N]
        }
      }

      def makeCopy(n: Node, idx: Int): Node = {
        n match {
          // Note: projections are implicitly copied with their owner
          case _: Projection | _: Phi => shouldNotReachHere("projections and phies must be filtered out before copying")
          // Note: we can't simply copy table jump without duplicating its symbol,
          //       otherwise we will have two different segments associated with a single symbol.
          case _: TableJump => shouldNotReachHere("xi-transformations are prohibited after lowering")
          // Note: avoid copying return node, because it would break IR invariant (JET-13257)
          case _: Return => shouldNotReachHere("return node must not be copied")
          // Note: most of blockParamNodes shouldn't be copied. Catch node should be copied with XBlock
          case _: BlockParamNode => shouldNotReachHere("block parameters shouldn't be copied explicitly")
          // Note: edges between blocks will be added during post-processing after copying
          case b: Block => withPos(b) {
            copyBlock(b, idx)
          }
          case _ => Node.clone(n, copy(_, idx))
        }
      }

      // Note: Catch nodes should be copied with their blocks,
      // so reserve place for them before copying
      marked.foreach(prepareCatches)
      // Note: circular dependencies should be eliminated by now
      val worklist = Worklist.from[Node](marked.keys)
      for (n <- worklist.drain) {
        val notProcessedArgs = n.args filter needsCopying
        if (n.isInstanceOf[Block] || notProcessedArgs.isEmpty) {
          val copiesNum = markedNodes(n)
          if (copiesNum == 1) {
            // frequent case
            copies(n) = Array(makeCopy(n, 0))
          } else {
            copies(n) = Array.tabulate(copiesNum)(makeCopy(n, _))
          }
          worklist ++= n.uses filter needsCopying
        } else {
          worklist ++= notProcessedArgs
          worklist += n
        }
      }

      // post-process copied nodes
      for (n <- copies.keys) {
        val num = markedNodes(n)
        n match {
          case blockEnd: BlockEnd =>
            for (idx <- 0 until num; (succ: Block, exit) <- blockEnd.exits map (_.singleUse) zip copy(blockEnd, idx).exits) {
              val target = copy(succ, idx)
              target.addArg(exit)
            }

          case sn: SpinalNode if sn.hasXHandler =>
            for (idx <- 0 until num) {
              copy(sn.xHandler, idx).addArg(copy(sn, idx).xpoint)
            }

          case _ =>
        }
      }

      new XiResult(for (b <- marked.keys.iterator; p <- b.points; c <- copies(p)) yield p -> c.asInstanceOf[ControlNode])
    }
  }

  private type PostAction = XiResult => Unit

  private class ExtractEdges(target: Block, edges: Edge*) extends PostAction {
    require(edges forall (_.target == target))
    override def apply(xi: XiResult): Unit = {
      redirectEdges(xi.copyOf(target), edges: _*)
    }
  }

  private def redirectEdges(target: Block, edges: Edge*): Unit = {
    assert(target.phies.isEmpty, "cannot redirect edges to a block with phies")
    assert(edges.nonEmpty)
    for (e <- edges) {
      target.addArg(e.source)
      makeUnreachable(e)
    }
  }

  object XiTransform {
    def log = Log(Kind.XiTransform)

    // Xi-transformations often:
    // - bloat code size, so they should not be used before serialization, or
    // - weigh nodes, so they should not be used after lowering (because lowered nodes often don't have scales).

    def enabled: Boolean = (CompilerPhase.Serialization < currentPhase) && (currentPhase < CompilerPhase.Lowering)
    def enabled(opt: BoolOption): Boolean = enabled && env.enabled(opt)
  }
}
