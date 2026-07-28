/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.RTSProc
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Nodes, Universe}
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.util.Callback
import com.huawei.excelsior.jet.util.ScalaCollections.mapWith
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections, Worklist}

import scala.PartialFunction.condOpt
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps

/**
 * Kernel nodes are basic nodes, that define the structure of the IR.
 *
 * 1) Block, BBlock, XBlock
 * 2) Phi
 * 3) BlockEnd and its subtypes (Return, Throw, Goto, Halt, If, Switch)
 *
 * @author paul
 * @author cypok
 * @author conwor
 */
trait KernelNodes { self: Universe with Nodes =>


  /////////////////////////////////////////
  // Block & Co.

  /**
   * Block is special node that forms the start of Basic Block. It is the only node that may have multiple
   * control arguments: the enters of the block.
   *
   * Any block may have any number of control arguments and produces one control result.
   * Nodes that have control input and output belonging to the block forms the skeleton of the Basic Block.
   *
   * Block also has reference to the [[com.huawei.excelsior.jet.compiler.opt.ir.nodes.KernelNodes.XBlock XBlock]]:
   * it is a block where control transferred when an exception occurs.
   */
  abstract class Block protected (proto: BlockProto[_ <: Block]) extends NodeWithVarArgs(proto) with UpperPoint with MemoryNode with NotProducesValue {
    selfBlock =>

    /** Returns iterator over nodes pinned to a point in this block (projections excluded). */
    final def nodes: Iterator[Node] = ScalaCollections.iterateUntilNull[Node](blockEnd.nextPinned) {
      case `selfBlock` => null
      case x: LowerPoint => x.inCtrl.nextPinned
      case x => x.nextPinned
    }

    /** Returns iterator over block's points (block, blockEnd and spinal nodes). */
    final def points: Iterator[ControlNode] = pointsBackward

    /** Returns iterator over block's points in top-down order (block, spinal nodes and blockEnd).
      * It WAS less effective than `pointsBackward`, now it is the same.
      * TODO: remove `pointsBackward`, `spineBackward`. */
    final def pointsForward: Iterator[ControlNode] = ScalaCollections.iterateUntilNull[ControlNode](selfBlock) {
      case _: BlockEnd => null
      case x: UpperPoint => x.outCtrlOrNull // Null may be if block has no blockEnd (e.g. during lowering)
    }

    /** Returns iterator over block's points in bottom-up order (blockEnd, spinal nodes and block). */
    final def pointsBackward: Iterator[ControlNode] = ScalaCollections.iterateUntilNull[ControlNode](blockEnd) {
      case `selfBlock` => null
      case x: LowerPoint => x.inCtrl // TODO: ensuring (_ != null)
    }

    /** Returns iterator over block's spinal nodes in some order. */
    final def spine: Iterator[SpinalNode] = spineBackward

    /** Returns iterator over block's spinal nodes in top-down order.
      * It WAS less effective than `spineBackward`, now it is the same.
      * TODO: remove `pointsBackward`, `spineBackward`. */
    final def spineForward: Iterator[SpinalNode] = collect[SpinalNode](pointsForward)

    /** Returns iterator over block's spinal nodes in bottom-up order. */
    final def spineBackward: Iterator[SpinalNode] = collect[SpinalNode](pointsBackward)

    /** Returns iterator over block's spinal nodes in bottom-up order and block itself. */
    final def spineWithBlock: Iterator[UpperPoint] = spineBackward ++ Iterator.single(this)

    /** Returns iterator over block itself and block's spinal nodes in top-down order. */
    final def spineForwardWithBlock: Iterator[UpperPoint] = Iterator.single(this) ++ spineForward

    /** Returns iterator over block's param nodes. */
    final def paramNodes: Iterator[BlockParamNode] = collect[BlockParamNode](pinnedNodes)

    /** Returns iterator over block's phies. */
    final def phies: Iterator[Phi] = collect[Phi](paramNodes)


    private[ir] def blockControlNum = ControlNode.BlockStartControlNum

    private var _hasValidControlNums = true

    final def hasValidControlNums = _hasValidControlNums

    final def invalidateControlNums(): Unit = {
      _hasValidControlNums = false
    }

    private var allowSpineChangedControlNumInvalidation = true

    private[ir] final def spineChanged(keepControlNums: Boolean): Unit = {
      if (allowSpineChangedControlNumInvalidation) {
        _hasValidControlNums &= keepControlNums
      }
      xpointsCache = null
      hasUniqueXHandler = false
    }

    private[ir] final def refreshControlNums(): Unit = {
      if (!_hasValidControlNums) {
        if (spineBackwardIsBroken || blockEnd == null) {
          // this may happen while deserialization of this block
          // if domination of two nodes from this block is asked
          // or during parsing
          Block.refreshBlockControlNums(ControlNode.BlockStartControlNum, ControlNode.StepControlNum, spineForward)
        } else {
          Block.refreshBlockControlNums(ControlNode.BlockEndControlNum, -ControlNode.StepControlNum, spineBackward)
        }
        _hasValidControlNums = true
      }
    }

    def inputs: Seq[ControlNode] = argsSeq.asInstanceOf[Seq[ControlNode]]

    final def updateInput(i: Int, n: Node): Unit = {
      updateArg(i, n)
    }

    final def updateInputs(ns: Seq[Node]): Unit = {
      for ((n, i) <- ns.zipWithIndex) {
        updateInput(i, n)
      }
    }

    protected[KernelNodes] def refreshDominators(): Unit = { if (isCommitted) scope.refreshDominators(this) }

    def reachable: Boolean = {
      if (scope.inDeserialization) {
        scope.cfg.dominators contains this
      } else {
        scope.cfg.topSort contains this
      }
    }

    def unreachable: Boolean = !reachable

    private[ir] override def commitImpl(): Unit = {
      super.commitImpl()
      refreshDominators()
    }

    private[ir] override def decommitImpl(): Unit = {
      for (p <- paramNodes) {
        decommit(p)
      }

      super.decommitImpl()
    }

    override def argChanged(idx: Int): Unit = {
      super.argChanged(idx)
      refreshDominators()
    }

    final override def block = this

    private var _blockRef: BlockRef = new BlockRef(this)

    override final def blockRef = _blockRef

    final def refreshBlockRef(): Unit = {
      _blockRef.invalidate()
      _blockRef = new BlockRef(this)
    }

    private var _blockEnd: BlockEnd = _

    final def blockEnd: BlockEnd = _blockEnd

    final def blockEnd_= (end: BlockEnd): Unit = {
      spineChanged(keepControlNums = true)

      if (_blockEnd != null) {
        assert(_blockEnd.block == this)
        _blockEnd.block = null
        _blockEnd.succBlocks foreach { _.refreshDominators() }
      }
      _blockEnd = end
      if (end != null) end.block = this

      xSuccBlocks foreach (_.refreshDominators())
    }

    // If you are looking for predBlockEdges, it is equal to inEdges

    final def succBlockEdges: Iterator[Edge] = if (blockEnd != null) blockEnd.succBlockEdges else Iterator.empty

    final def succBlocks: Iterator[BBlock] = if (blockEnd != null) blockEnd.succBlocks else Iterator.empty

    final def xSuccBlocks: Iterator[Block] = succBlocks ++ handledXPoints.map(_.handler)

    final def xSuccBlockEdges: Iterator[Edge] = succBlockEdges ++ handledXPoints.map(_.xEdge)

    /** Returns iterator over this block predecessor blocks (maybe with repetitions). */
    final def predBlocks: Iterator[Block] = args map (_.block)

    private var xpointsCache: List[XPoint] = _

    // In some places we rely on the fact that all throwing nodes have the same handler.
    // TODO: remove this constraint
    private var hasUniqueXHandler = false

    // FIXME: ugly hack for bytecode parsing stage (DataFlow)
    // TODO: kill me please with no mercy
    var spineBackwardIsBroken = false

    /** Exception exits from all throwing spinal nodes of this block in top-down order. */
    private def xpointsList: List[XPoint] = {
      if (xpointsCache ne null) {
        checkConsistency(CheckLevels.Optional) {
          val (xc, unique) = (xpointsCache, hasUniqueXHandler)
          xpointsCache = null
          hasUniqueXHandler = false
          val dummy = xpointsList
          if (xc != xpointsCache || unique != hasUniqueXHandler) {
            shouldNotReachHere(s"\nxpoints cache inconsistency: block $id\n  was:[$xc, $unique],\n  has: [$xpointsCache, $hasUniqueXHandler]\n")
          }
        }
      }

      if (xpointsCache eq null) {
        def hasXPoint(n: SpinalNode) = { /*TODO:enable assert(n.hasXPoint == n.canThrow);*/ n.hasXPoint }
        xpointsCache = {
          // (spineForward collect { case n if hasXPoint(n) => n.xpoint }).toList
          // below is workaround; TODO: always use spineForward
          val forward = spineBackwardIsBroken || blockEnd == null
          val sp = if (forward) spineForward else spineBackward
          val xpoints = sp collect { case n if hasXPoint(n) => n.xpoint }
          if (forward) xpoints.toList else {
            val buf = ListBuffer.empty[XPoint]
            xpoints foreach (buf prepend _)
            buf.toList
          }
        }
        hasUniqueXHandler = ScalaCollections.haveSame(xpointsCache)(_.handlerOption)
      }
      xpointsCache
    }

    /** Exception exits from all throwing spinal nodes of this block in top-down order. */
    def xpoints: Iterator[XPoint] = xpointsList.iterator

    /** All XPoints of this block which have XBlock handlers. */
    def handledXPoints: Iterator[XPoint] = {
      if (!scope.hasXEdges) {
        return Iterator.empty
      }
      val xs = xpointsList
      if (hasUniqueXHandler) { // fast path
        if (xs.isEmpty || !xs.head.hasHandler) Iterator.empty else xs.iterator
      } else {
        xs.iterator filter (_.hasHandler)
      }
    }

    /** Returns iterator over this block xHandlers with no duplication. */
    final def xHandlers: Iterator[XBlock] = {
      if (!scope.hasXEdges) {
        return Iterator.empty
      }
      val xs = xpointsList
      if (hasUniqueXHandler) { // fast path
        if (xs.isEmpty) Iterator.empty else xs.head.handlerOption.iterator
      } else {
        Sets[XBlock].newQSet(xs.iterator flatMap (_.handlerOption)).iterator
      }
    }

    final def hasXHandlers: Boolean = {
      if (!scope.hasXEdges) {
        return false
      }
      val xs = xpointsList
      if (hasUniqueXHandler) { // fast path
        xs.nonEmpty && xs.head.hasHandler
      } else {
        handledXPoints.nonEmpty
      }
    }

    /** Returns single xHandler of this block or null if this block does not have xHandler.
      * Fails with error if this block has several different xHandlers.
      */
    final def singleXHandlerOrNull: XBlock = {
      if (!scope.hasXEdges) {
        return null
      }
      val xs = xpointsList
      assert(hasUniqueXHandler)
      if (xs.isEmpty) null else xs.head.handlerOrNull
    }

    /** Returns single xHandler of this block or None if this block does not have xHandler.
      * Fails with error if this block has several different xHandlers.
      */
    final def singleXHandlerOption: Option[XBlock] = Option(singleXHandlerOrNull)

    final def redefinesMemory: Boolean = memoryUses.nonEmpty

    /** Returns iterator over memories coming into `this` block from reachable inputs. */
    def reachableMemoriesBefore: Iterator[MemoryNode]

    // FIXME: dominators.idom can't handle unreachable code (works only if `dominators contains this`)
    final def idomBlock: Block = scope.cfg.dominators.idom(this)

    /** Adds new incoming edge to this block and update phi-functions. */
    def addInEdge(from: Node, phiArgs: Phi => Node): Unit = {
      addArg(from)
      for (phi <- phies) phi.addArg(phiArgs(phi))
    }

    private[KernelNodes] lazy val codeOrder: Worklist[Node] = Worklist.empty

    // TODO: current implementation of isCold is very poor:
    // it only detects xblocks and bblocks with explicit ColdCodeMarker.
    // However we have more powerful ColdRegionDetector (e.g. it also detects
    // blocks with incoming edges only from another cold blocks).
    // So we should replace this method with general implementation
    // of ColdRegionDetector which should detect explicit markers, throws, catches, etc.
    def isCold: Boolean

    final def markAsCold(): Unit = {
      if (!isCold) {
        insertCodeAfter(this) { ColdCodeMarker() }
      }
    }

    final def markAsWarm(): Unit = {
      if (!isCold) {
        insertCodeAfter(this) { WarmCodeMarker() }
      }
    }

    def isInterpreterCaseStart: Boolean = spine exists { _.isInstanceOf[InterpreterCaseMarker] }

    /** Returns iterator by blocks from hammock between `this`.idom and `this`, including both of them. */
    def closedIdomHammock: Iterator[Block] = {
      val idom = idomBlock
      val processed = Sets[Block].newMSet
      val frontier = Sets[Block].newQSet
      frontier += this

      new Iterator[Block] {
        override def hasNext = frontier.nonEmpty

        override def next() = if (!hasNext) Iterator.empty.next() else {
          val next = frontier.head
          frontier -= next
          processed += next
          if (next != idom) {
            frontier ++= next.predBlocks filterNot processed
          }
          next
        }
      }
    }

    /** Returns iterator by blocks from hammock between `this`.idom and `this`, excluding both of them. */
    def openedIdomHammock: Iterator[Block] = {
      val idom = idomBlock
      closedIdomHammock filter { b => (b != this) && (b != idom) }
    }
  }

  object Block {
    /** Splits block after the given point.
      * Given node will be the last spinal node of the splitted block.
      *
      * Floating control uses of the node are also splitted and moved to the block below.
      * Unless `keepControlled` is set in which case such uses are left as is.
      *
      * {{{
      *    |
      *   node
      *    | \   <----- split here by default
      *    | controlled
      *    |     <----- split here if keepControlled
      *   next
      *    |
      * }}}
      */
    def splitAfter(point: UpperPoint, keepControlled: Boolean = false): Goto = withPos(point) {
      // TODO: this transformation is incorrect if GCM was done,
      // add smth like this:
      // requireNoGlobalCodeMotion()

      val block = point.block
      val end = block.blockEnd
      val newStart = point.outCtrl
      require(end != null)

      val goto = Goto(point, point.memoryAfter)
      val newBlock = BBlock(goto)

      if (!block.hasValidControlNums) {
        newBlock.invalidateControlNums()
      }

      block.refreshBlockRef()

      Block.withoutSpineChangedControlNumInvalidation(block) {
        Block.withoutSpineChangedControlNumInvalidation(newBlock) {
          newStart.inCtrl = newBlock
        }
      }
      newBlock.blockEnd = end

      if (!keepControlled) {
        point.replaceUses { case ControlEdge(_, _: FloatingNode) => newBlock }
      }

      goto
    }

    /** Splits block before the given point.
      * Given node will be the first spinal node of the new block.
      *
      * {{{
      *    |
      *   prev
      *    | \
      *    | controlled
      *    |     <----- split here
      *   node
      *    |
      * }}}
      */
    def splitBefore(point: LowerPoint): Goto =
      splitAfter(point.inCtrl, keepControlled = true)

    /** Adds new incoming edges from `args` to the block specified by template `edge`.
      * Phi-function arguments corresponding to the new edges are taken from `edge`.
      */
    def addEdgesWithTemplate(args: IterableOnce[Node], edge: Edge): Unit =
      args.iterator foreach (addEdgeWithTemplate(_, edge))

    /** Adds new incoming edge from `arg` to the block specified by template `edge`.
      * Phi-function arguments corresponding to the new edge are taken from `edge`.
      */
    def addEdgeWithTemplate(arg: Node, edge: Edge): Unit = {
      val target = edge.target.asInstanceOf[Block]
      target.addInEdge(arg, _.phiArg(edge))
    }

    /** Remove `edges` from corresponding target blocks. */
    def removeEdges(edges: Iterable[Edge]): Unit = {
      if (edges.nonEmpty) {
        val block = edges.head.target.asInstanceOf[Block]
        val (own, rest) = edges partition (_.target == block)
        removeOwnEdges(own)
        removeEdges(rest)
      }
    }

    /** Remove `edge` from corresponding target block. */
    def removeEdge(edge: Edge): Unit = removeOwnEdges(List(edge))

    private def removeOwnEdges(edges: Iterable[Edge]): Unit = {
      assert(edges.toSet.size == edges.size)
      val target = edges.head.target.asInstanceOf[Block]

      val indices = edges.map(_.targetArgIndex)
      target.removeInEdges(indices)
      if (target.phies.nonEmpty) {
        bulkReplace {
          val phiIndices = indices
          for (phi <- target.phies.toArray) {
            phi.removeInEdges(phiIndices)
            replaceTransitively(phi, commit(phi))
          }
        }
      }

      // `removeEdge` and `removeEdges` methods should be used carefully to avoid unexpected unreachable code not
      // reachable from unreachable bar (free unreachable blocks in terms of UCE). In most cases you could append
      // new edges to block and only after that remove old ones.
      ensureNoFreeUnreachableBlocks()
    }

    def collectNodes(block: Block): collection.Set[Node] = withIncrementalGCM {
      Closure[Node](block.points ++ block.paramNodes)(_.uses filter (_.block == block))
    }

    /** For each block in iterable adds it's parameter nodes.
      * Can be useful as base for closure from blocks by their uses.
      * @return iterator over all given blocks and their parameter nodes
      * @see [[BlockParamNode]]
     */
    def withParamNodes(blocks: IterableOnce[Block]): Iterator[Node] =
      blocks.iterator.flatMap(b => Iterator(b) ++ b.paramNodes)

    def withoutSpineChangedControlNumInvalidation[T](block: Block)(action: => T): T = {
      assert(block.allowSpineChangedControlNumInvalidation)
      block.allowSpineChangedControlNumInvalidation = false
      try {
        action
      } finally {
        checkConsistency(CheckLevels.Optional)(Block.verifyBlockControlNums(block))
        block.allowSpineChangedControlNumInvalidation = true
      }
    }

    def refreshBlockControlNums(start: Long, step: Long, nodes: IterableOnce[SpinalNode]): Long = {
      var num = start
      for (ctrl <- nodes.iterator) {
        num = math.addExact(num, step)
        assert(ControlNode.BlockStartControlNum < num && num < ControlNode.BlockEndControlNum)
        ctrl.blockControlNum = num
      }
      num
    }

    def tryRefreshBlockControlNums(ctrlBefore: UpperPoint, ctrlAfter: LowerPoint): Unit = {
      require(ctrlAfter.inCtrl != ctrlBefore)
      val block = ctrlBefore.block ensuring (_ == ctrlAfter.block)
      if (block.hasValidControlNums) {
        val startNum = ctrlBefore.blockControlNum
        val endNum = ctrlAfter.blockControlNum
        assert(startNum < endNum)

        @tailrec
        def refreshImpl(ctrl: ControlNode, lastNum: Long): Boolean = {
          val step = math.min(ControlNode.InsertionStepControlNum, (lastNum - startNum) / 2)
          ctrl match {
            case `ctrlBefore` => true

            case ctrl: SpinalNode if step > 0 =>
              val num = lastNum - step
              ctrl.blockControlNum = num
              refreshImpl(ctrl.inCtrl, num)

            case _ => false
          }
        }

        if (!refreshImpl(ctrlAfter.inCtrl, endNum)) {
          block.invalidateControlNums()
        }
      }
    }

    def verifyBlockControlNums(b: Block): Unit = {
      if (b.hasValidControlNums) {
        for (Seq(x, y) <- b.pointsForward.sliding(2)) {
          assert(x.blockControlNum < y.blockControlNum,
            s"blockControlNum of $x (${x.blockControlNum}) should be less than blockControlNum of $y (${y.blockControlNum})")
        }
      }
    }
  }

  /** Encapsulates reference from [[SpinalNode]] to its [[Block]].
    *
    * Provides additional indirection between spinal node and its block,
    * which allows this link to be safely invalidated and recalculated on-demand.
    *
    * {{{
    *      +--->  Block  <-->  BlockRef
    *      |        |             ^
    *      |        |             |
    *      |       SN1  ----------+
    *      |        |             |
    *      |        |             |
    *      |       SN2  ----------+
    *      |        |             |
    *      |        |             |
    *      |       SN3  ----------+
    *      |        |
    *      |        |
    *      +-->  BlockEnd
    * }}}
    */
  final class BlockRef private[ir] (private var _block: Block) {
    private[ir] def block: Block = _block
    private[ir] def invalidated = _block == null
    private[ir] def invalidate(): Unit = _block = null
  }

  object BlockRef {
    private[ir] val INVALID = new BlockRef(null)
  }


  object CodeOrder {
    def in(block: Block): Iterator[Node] = block.codeOrder.iterator
    def reversedIn(block: Block): Iterator[Node] = block.codeOrder.reverseIterator

    def contains(x: Node): Boolean = x.block != null && x.block.codeOrder.contains(x)

    def prev(x: Node): Node = x.block.codeOrder.pred(x.groupRoot).get
    def next(x: Node): Node = x.block.codeOrder.succ(x.groupRoot).get
    def from(x: Node): Iterator[Node] = x.block.codeOrder.trackFrom(x.groupRoot)
    def after(x: Node): Iterator[Node] = x.block.codeOrder.trackAfter(x.groupRoot)

    val onChange = new Callback[Worklist[Node]]
    private def change(order: Worklist[Node])(f: Worklist[Node] => Unit): Unit = {
      onChange.apply(order)
      f(order)
    }

    // TODO: BGCM prepends all group parts in code order, backend appends only group roots. Do something with it!

    def append(x: Node, block: Block): Unit       = change(block.codeOrder)         { _.append(x) }
    def prepend(x: Node, block: Block): Unit      = change(block.codeOrder)         { _.prepend(x) }
    def remove(x: Node): Unit                     = change(x.block.codeOrder)       { _ -= x }
    def insertBefore(before: Node, x: Node): Unit = change(before.block.codeOrder)  { _.insertBefore(before, x) }
    def insertAfter(after: Node, x: Node): Unit   = change(after.block.codeOrder)   { _.insertAfter(after, x) }
    def moveBefore(before: Node, x: Node): Unit   = change(before.block.codeOrder)  { _.moveBefore(before, x) }
    def replace(from: Node, to: Node): Unit       = change(from.block.codeOrder)    { _.replace(from, to) }
    def clearIn(block: Block): Unit               = change(block.codeOrder)         { _.clear() }

    def pullUp(x: Node): Unit                     = moveBefore(prev(x), x)
  }


  abstract class BlockProto[N <: Block](argTpe: Type) extends VarArgs[N]()(argTpe)(ControlType) with PrototypeStrictNodeClass[N, N] {
    // TODO: this should be done via prototypes, but ControlMemoryTagged requires node to be Spinal :(
    final override def tagsMask = Tags.controlMemoryMask
  }


  final class BBlock private extends Block(BBlock) {
    def reachableMemoriesBefore: Iterator[MemoryNode] = predBlocks collect {
      case in if in.reachable => in.blockEnd.inMemory
    }

    def isCold: Boolean = collect[ColdNode](points).nonEmpty
    override def inputs: Seq[BlockExit] = super.inputs.asInstanceOf[Seq[BlockExit]]
  }

  object BBlock extends BlockProto[BBlock](ControlType) {
    def newInstance() = new BBlock

    /** Creates new predecessor block for given block.
      * New block borrows given edges from the block to itself.
      *
      * @param block the block which will loose specified edges and receive new single edge
      * @param extractedEdges input edges of the block which will be extracted to new block
      * @return new block which is predecessor of the given block
      */
    def extractInputEdges(block: BBlock, extractedEdges: Seq[Edge]): BBlock = {
      require(block != entryBlock) // it's hard to create predecessor of entry block
      require(extractedEdges.nonEmpty) // pointless, it's unclear which values to use as phi arguments

      assert(extractedEdges forall (_.target == block))
      assert(extractedEdges.toSet.size == extractedEdges.size)

      val newBlock = BBlock(extractedEdges map (_.source): _*)
      val phiArgFromNewBlock = mapWith(block.phies) { phi =>
        Phi(phi.tpe)(newBlock +: (extractedEdges map phi.phiArg): _*)
      }

      // memory may be conservative if all memories for edges are equal
      val newBlockMem = if (block.redefinesMemory) newBlock else block.memoryAfter
      val gotoFromNewBlock = Goto(newBlock, newBlockMem)
      block.addInEdge(gotoFromNewBlock, phiArgFromNewBlock)
      Block.removeEdges(extractedEdges)

      newBlock
    }
  }


  /**
   * When implicit exception occurs the control is transferred to the special Block called XBlock that is linked
   * to the normal block.
   *
   * The body of XBlock begins with phies and [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Catch Catch]] operation
   * that retrieves a thrown exception from execution environment of current thread.
   * After that control is transferred to the series of blocks containing the following operation:
   *
   * <pre>
   * if (thrownException instanceof ExceptionTable[i].ExceptionType)
   *     goto ExceptionTable[i].Handler
   * else
   *     goto nextBlockInSeries
   *</pre>
   *
   *, where ExceptionTable[i].Handler is the control corresponding to the exception handler in an exception table row.
   * Series of blocks always ends with a block containing `Throw` operation: rethrowing an exception to the
   * caller method.
   *
   * Any block that has operations throwing implicit exceptions has XBlock reference (`Block.exHandler`),
   * even if there is no ExceptionTable rows that may potentially catch exception for the bytecode range of the block.
   * In this case, XBlock directly transfers control to a block containing just
   * [[com.huawei.excelsior.jet.compiler.opt.ir.Nodes.Throw Throw]] operation.
   * In other words, we enclose the whole method body into artificial try block
   * which catch clause catches all exceptions and rethrows (unwinds) them to the caller.
   *
   * See [[com.huawei.excelsior.jet.compiler.opt.frontend.bytecode.ExceptionHandlersBuilder ExceptionHandlersBuilder]] for more details.
   */
  final class XBlock private extends Block(XBlock) {

    /** Blocks that have spinal nodes with exception edges to this XBlock. */
    def tryBlocks: Iterator[Block] =
      args collect { case x if x != null => x.block } // null args filter is a hack for deserialization, TODO: remove it

    def reachableMemoriesBefore: Iterator[MemoryNode] = inputs.iterator collect {
      case in if in.owner.block.reachable => in.owner.memoryBefore
    }

    def catchNode: Catch = single[Catch](paramNodes)

    override def inputs: Seq[XPoint] = super.inputs.asInstanceOf[Seq[XPoint]]

    def isCold = true
  }

  object XBlock extends BlockProto[XBlock](ControlType) {
    def newInstance() = new XBlock

    /** Creates a copy of given XBlock, borrowing given edges from it.
      *
      * The original block is split to avoid any node copying (except for phies, catch and other [[BlockParamNode]]s).
      * The sources of borrowed edges are split to ensure the invariant of one xhandler per block.
      * TODO: rework when this invariant is gone
      *
      * @param block the block which will loose specified edges
      * @param extractedEdges input edges of the block which will be extracted to new block
      * @return new block which is a copy of the given block
      */
    def extractInputEdges(block: XBlock, extractedEdges: Seq[Edge]): XBlock = {
      require(extractedEdges.nonEmpty) // pointless, it's unclear which values to use as phi arguments

      assert(extractedEdges forall (_.target == block))
      assert(extractedEdges.toSet.size == extractedEdges.size)

      // split extracted throwing nodes into separate blocks
      for (Edge(xp: XPoint, _) <- extractedEdges) {
        Block.splitBefore(xp.owner)
        Block.splitAfter(xp.owner)
      }

      val gotoFromOldBlock = Block.splitAfter(block)
      val nextBlock = gotoFromOldBlock.target

      val inputs = extractedEdges map (_.source)
      extractedEdges foreach { _.source = null }
      val newBlock = XBlock(inputs: _*)

      val copies = mapWith(block.paramNodes) {
        case x: Phi   => Phi(x.tpe)(newBlock +: (extractedEdges map x.phiArg): _*)
        case x: Catch => Catch(newBlock)
      }

      // memory may be conservative if all memories for edges are equal
      val newBlockMem = if (block.redefinesMemory) newBlock else block.memoryAfter
      val gotoFromNewBlock = Goto(newBlock, newBlockMem)
      nextBlock.addArg(gotoFromNewBlock)

      for ((orig, copy) <- copies) {
        val phi = Phi(orig.tpe)(nextBlock, orig, copy)
        orig.replaceUses { case e if e.isValue && e.target != phi => phi }
      }

      Block.removeEdges(extractedEdges)

      newBlock
    }
  }


  /** Phi function.
    *
    *  It has one control argument that is always block to which the Phi function belongs.
    *  And it has several data arguments (corresponding in varargs of Phi node prototope),
    *  where each data argument is treated as coming from a corresponding predecessor block.
    *  Later means that we cannot put definition of the argument below of the corresponding predecessor block
    *  of phi function's block (cannot put it in the phi function's block).
    *
    *  Phi function can be imagined as join function of data coming from incoming blocks.
    *  The actual value of phi function is a value of one of its data arguments depending from which predecessor block
    *  the control comes to this block.
    *
    *  @define onlyBBlock
    * Note: must be called only for phi in [[com.huawei.excelsior.jet.compiler.opt.ir.nodes.KernelNodes.BBlock]].
    */
  final class Phi private (proto: Phi.Proto) extends NodeWithVarArgs(proto) with BlockParamNode with ArgDependentTypeNode {

    override def commitImpl(): Unit = {
      assert(this.arity == block.arity)
      super.commitImpl()
    }

    private var _block: Block = _

    override def block: Block = _block

    /** Gives corresponding data edge of phi by it's block control edge
      * {{{
      *          source1     source2
      *          c     d    c     d
      *          |     \   /      \
      *          |      . /        \
      *          |       /  \       \ <- data edges which phi merges
      *          |      /    \       \
      *         | Block | <-  |  Phi  |
      *            (Phi belongs to Block)
      * }}}
      *
      * @param controlInput the control edge of block which phi belongs to
      * @return input data edge of phi, this edge corresponds to control input source
      */
    def phiInput(controlInput: Edge): Edge = {
      assert(controlInput.target == this.block)
      this.inEdge(controlInput.targetArgIndex)
    }

    /** Returns node that transfers data to phi by its block control edge
      *
      * @param controlInput the control edge of block which phi belongs to
      * @return Node which transfers data token to phi function
      */
    def phiArg(controlInput: Edge): Node = phiInput(controlInput).source

    /** Returns control edge of block which phi belongs to, by phi input data edge
      *
      * @param phiInput data edge of phi function
      * @return control edge that corresponds to source of input data edge
      */
    def controlInput(phiInput: Edge): Edge = {
      assert(phiInput.target == this)
      block.inEdge(phiInput.targetArgIndex)
    }

    override def inEdgesByType(tpe: Type): Iterator[Edge] = if (tpe == this.tpe) inEdges else Iterator.empty
    override def isTypeDependency(edge: Edge): Boolean = true
  }

  object Phi {
    case class Proto (keyType: Type) extends VarArgs[Phi]()(keyType)(keyType) {
      assert(keyType.isValueType)
      def newInstance() = new Phi(this)
    }

    private def instance(tpe: Type)(args: Node*): Phi = args match {
      case Seq(block: Block, valueArgs*) =>
        Prototype.intern(Proto(tpe)).raw(valueArgs: _*) tap (_._block = block)
    }

    def apply(tpe: Type)(args: Node*): Node = {
      commit(instance(tpe)(args:_*))
    }

    def proto(tpe: Type) = {
      Prototype.intern(Proto(tpe))
    }

    def raw(tpe: Type)(args: Node*): Phi = {
      instance(tpe: Type)(args:_*)
    }

    def unapplySeq(phi: Phi) = Some(phi.block +: phi.argsSeq)

    /** Creates cyclic phi function that can reference itself in its arguments. */
    def cyclic(tpe: Type)(block: Block, makeArgs: Phi => Seq[Node]) = {
      val res = withDeferredOnCommitOptimizations {
        val phi = Phi.raw(tpe)(block +: Seq.fill(block.arity)(null): _*)
        phi.replaceArgsBySeq(makeArgs(phi))
        commit(phi)
      }
      // Note: deferred on-commit optimizations may decommit resulting phi function.
      res.deref
    }

    def transitivePhiArgs(p: Phi): collection.Set[Phi] = {
      def phiArgs(p: Phi) = collect[Phi](p.args)
      Closure(phiArgs(p))(phiArgs)
    }

    def transitivePhiUses(p: Phi): collection.Set[Phi] = {
      def phiUses(p: Phi) = collect[Phi](p.valueUses)
      Closure(phiUses(p))(phiUses)
    }

    def transitiveValueArgs(n: Node): collection.Set[Node] = {
      val args = Closure(n.valueArgs) {
        case p: Phi => p.args
        case _ => Iterator.empty
      }
      args.filterInPlace(!_.isInstanceOf[Phi])
      args
    }

    def transitiveValueUses(n: Node): collection.Set[Node] = {
      val uses = Closure(n.valueUses) {
        case p: Phi => p.valueUses
        case _ => Iterator.empty
      }
      uses.filterInPlace(!_.isInstanceOf[Phi])
      uses
    }
  }


  /////////////////////////////////////////
  // Block ending nodes

  /**
   * Node that ends a basic block: goto, branch, switch, throw, return.
   * Has one control input, one memory input and several (zero or more) control outputs.
   */
  abstract class BlockEnd protected (proto: BlockEndProto[_ <: Node])
    extends NodeWithFixedArgs(proto) with LowerPoint with HasInMemory with NotProducesValue {

    private var _block: Block = _
    override final def block = _block
    private[KernelNodes] def block_=(b: Block): Unit = _block = b

    private[ir] def blockControlNum = ControlNode.BlockEndControlNum

    private def exitsOutEdges: Iterator[Edge] = exits.iterator flatMap (_.outEdges)

    // Filter out all edges to non-blocks (e.g. Constraints)
    final def succBlockEdges: Iterator[Edge] = exitsOutEdges filter (_.target.isInstanceOf[BBlock])
    final def succBlocks: Iterator[BBlock] = exitsOutEdges collect { case Edge(_, b: BBlock) => b }

    def exits: Seq[BlockExit]

    override def projections: Seq[ControlNode] = exits filterNot ( _ == this )

    /** Move uses of this block end to unreachable code. */
    final def makeUsesUnreachable(): Unit = makeUnreachable(succBlockEdges)

    override def argChanged(idx: Int): Unit = {
      if (idx == inCtrlArg && isCommitted && block != null) {
        block.spineChanged(keepControlNums = true)
      }
      super.argChanged(idx)
    }

    private[ir] override def commitImpl(): Unit = {
      super.commitImpl()
      inCtrl.block.blockEnd = this
      scope.refreshDominators(this)
    }
  }

  abstract class BlockEndProto[N <: BlockEnd](argTypes: Type*)(resultType: Type)
          extends FixedArgs[N](argTypes: _*)(resultType)
          with PrototypeStrictNodeClass[N, N]


  /** Return from method operation.
   */
  class Return private (proto: Return.Proto) extends BlockEnd(proto) {
    def inValueArg = Return.InValueEdge.index
    def inValue = arg(inValueArg)
    def inValue_=(n: Node): Unit = { updateArg(inValueArg, n) }

    override val exits = Nil

    private def returnType = proto.retType match {
      case EopType.Plain => typeProvider.getAJObjectType // see workaround for JET-14374 in CangjieLLVMIRParser
      case _ => inlineContext.method.getReturnType.symType
    }
  }

  object Return {
    case class Proto private[Return] (retType: Type) extends BlockEndProto[Return](ControlType, MemoryType, retType)(UnreachableControlType) {
      require(retType.isValueType)

      def newInstance() = new Return(this)
    }

    def proto(retType: Type) = Prototype.intern(Proto(retType))
    def apply(inCtrl: Node, inMem: Node, inValue: Node) = proto(inValue.tpe).withExplicitArgs(inCtrl, inMem, inValue)
    def unapply(node: Return): Option[(Node, Node, Node)] =
      Some(node.inCtrl, node.inMemory, node.inValue)

    /** Returns the only Return node in the whole IR. May return `None` if there is no node at all.
      * See [[Universe.unifyReturns]] for details of the only Return construction.
      */
    def unique: Option[Return] = {
      val rets = all[Return]
      if (rets.isEmpty) None
      else Some(rets.next()) ensuring (!rets.hasNext, "there couldn't be more than one Return in IR")
    }

    object InValueEdge extends EdgeMatcher[Return](2)
  }


  /** Common trait for nodes that transfer control between two blocks: [[Goto]] and [[Branch.Exit]].
    *
    * Note: cannot be made inner of [[Block]] due to [[https://github.com/scala/bug/issues/4440]].
    */
  trait BlockExit extends ControlNode with NotProducesValue {
    def target: BBlock
    def outEdge: Edge

    def memoryBefore: MemoryNode
  }

  object BlockExit {
    def unapply(exit: BlockExit): Option[(Block, BBlock)] = Some(exit.block, exit.target)
  }


  /** Unconditional jump operation. */
  class Goto private extends BlockEnd(Goto) with BlockExit {
    def target: BBlock = ScalaCollections.singleElement(succBlocks)
    def targetEdge: Edge = ScalaCollections.singleElement(succBlockEdges)
    override def outEdge = targetEdge
    override val exits = Seq(this)
  }

  object Goto extends BlockEndProto[Goto](ControlType, MemoryType)(ControlType) {
    def newInstance() = new Goto

    /** Unnapplies pair (fromBlock, toBBlock). */
    def unapply(node: Goto): Option[(Block, BBlock)] = BlockExit.unapply(node)
  }


  /** Halt (fatal error) operation. It should not be executed.
    *
    * Does not return, so it hasn't control output
    *  (specified control output in prototype is not consumed by any other node).
    */
  class Halt private (proto: Halt.Proto) extends BlockEnd(proto) with ColdNode {
    override val exits = Nil
    def reason: Halt.Reason = proto.reason
  }

  object Halt {
    case class Proto private[Halt](reason: Reason)
      extends BlockEndProto[Halt](ControlType, MemoryType)(UnreachableControlType) {

      def newInstance() = new Halt(this)
    }

    def empty(): Proto = Halt.proto(Reason.Empty)
    def explained(explanation: String): Proto = Halt.proto(Reason.Explained(explanation))
    def afterThrow(throwMessage: String): Proto = Halt.proto(Reason.AfterThrow(throwMessage))
    def afterRTSCall(proc: RTSProc, msg: String): Proto = Halt.proto(Reason.AfterRTSCall(proc, msg))

    def proto(reason: Reason): Proto = Prototype.intern(Proto(reason))

    /** Halt generation reasons.
      *
      * Highly not recommended to use `Empty`.
      *
      * If you use many similar `Explained` reasons, then feel free to add your own reason.
      */
    enum Reason {
      case Empty
      case Explained(msg: String)
      case AfterRTSCall(proc: RTSProc, msg: String)
      case AfterThrow(msg: String)

      override def toString = this match {
        case Empty => "No reason provided"
        case Explained(msg) => s"Reason: $msg"
        case AfterRTSCall(proc, msg) => s"Inserted after ErrorRTSCall $proc. Reason: $msg"
        case AfterThrow(msg) => s"Inserted after throw. Reason: $msg"
      }
    }
  }

  /** Control projection of node with multiple exits */
  trait Projection extends ControlNode with NotProducesValue {
    type OwnerType <: LowerPoint
    def owner: OwnerType = arg(0).asInstanceOf[OwnerType]

    override def block = owner.block
  }

  object Projection {
    def skip[N >: ControlNode](n: N): N = n match {
      case prj: Projection => prj.owner
      case _ => n
    }
  }

  /** BlockEnd with more than one exit: If or Switch */
  trait Branch extends BlockEnd {

    override val exits: Seq[Branch.Exit]

    /** Returns option constant exit for given `selectorValue`. */
    def constExit(selectorValue: Node): Option[Branch.Exit]

    def constExit: Option[Branch.Exit] = constExit(selectorValue = selector)

    def selector = arg(Branch.SelectorEdge.index)
    def selector_=(c: Node): Unit = { updateArg(Branch.SelectorEdge.index, c) }

    private[ir] override def commitImpl(): Unit = {
      if (!isCommitted) {
        super.commitImpl()
        exits foreach { x => commit(x) ensuring (_ == x) }
        exits foreach { x => scope.refreshDominators(x) }
      }
    }

    private[ir] override def decommitImpl(): Unit = {
      if (isCommitted) {
        // before destroying Branch-Exit connection remove uses of results
        exits foreach { _.nullifyUses() }
        super.decommitImpl()
        exits foreach decommit
      }
    }
  }

  object Branch {
    trait Exit extends Projection with BlockExit {
      type OwnerType <: Branch
      def target = single[BBlock](uses)
      def outEdge = ScalaCollections.singleElement(outEdges)

      def memoryBefore = owner.memoryBefore

      private[ir] def blockControlNum = ControlNode.BranchExitControlNum

      private[ir] override def commitImpl(): Unit = {
        if (!isCommitted) {
          super.commitImpl()
          commit(owner) ensuring (_ == owner)
        }
      }

      private[ir] override def decommitImpl(): Unit = {
        if (isCommitted) {
          // cache source because it will be `null` after decommitImpl, note that it can already be null
          val o = owner
          // before destroying Branch-Exit connection remove args of branch
          if (o != null) o.nullifyArgs()

          super.decommitImpl()

          if (o != null) {
            assert(o.isCommitted)
            decommit(o)
          }
        }
      }
    }

    object Exit {
      abstract class Proto[N <: Exit] extends FixedArgs[N](BranchType)(ControlType)
    }

    object SelectorEdge extends EdgeMatcher[Branch](2)
  }

  /** Control-flow conditional branch instruction.
   *  Has two control outputs: `trueExit` & `falseExit`.
   */
  class If private extends BlockEnd(If) with Branch {
    override def constExit(selectorValue: Node): Option[Branch.Exit] = condOpt(selectorValue) {
      case const: ConstCondition => exit(const.value)
    }

    def exit(value: Boolean) =
      if (value) trueExit else falseExit

    val trueExit = If.Exit(true).raw(this)
    val falseExit = If.Exit(false).raw(this)

    override val exits: Seq[If.Exit] = Seq(trueExit, falseExit)

    def trueBlock  = trueExit.target
    def falseBlock = falseExit.target
  }

  object If extends BlockEndProto[If](ControlType, MemoryType, ConditionType)(BranchType) {
    def newInstance() = new If
    def unapply(x: If): Option[Node] = Some(x.selector)

    class Exit private (proto: Exit.Proto) extends NodeWithFixedArgs(proto) with Branch.Exit {
      type OwnerType = If
      override def name = owner.name + "/" + isTrue

      def isTrue = proto.isTrue

      /** The other exit. */
      def otherExit = owner.exit(!isTrue)
    }

    object Exit {
      case class Proto private[Exit] (isTrue: Boolean) extends Branch.Exit.Proto[Exit] {
        def newInstance() = new Exit(this)
      }

      def apply(isTrue: Boolean) = Prototype.intern(Proto(isTrue))
      def unapply(x: Exit) = Some(x.owner)
    }

    /** Safe transformation which doesn't change semantics. */
    def invert(branch: If): Unit = {
      branch.selector = Not(branch.selector)
      internal.swapExits(branch)
    }

    object internal {
      /** Plain transformation which changes semantics (inverts branch). */
      def swapExits(branch: If): Unit = {
        val te = branch.trueExit.outEdge
        val fe = branch.falseExit.outEdge
        te.source = branch.falseExit
        fe.source = branch.trueExit
      }
    }
  }

  object IfEq {
    /** Matches `if (x == y) t else f` and returns `(x, y, t, f)`. */
    object NonCommutative {
      def unapply(n: If) = PartialFunction.condOpt(n) {
        case i @ If(Cmp(Condition.EQ, x, y)) => (x, y, i.trueExit, i.falseExit)
        case i @ If(Cmp(Condition.NE, x, y)) => (x, y, i.falseExit, i.trueExit)
      }
    }

    /** Matches `if (x == y) t else f` with respect to commutativity of `==`. */
    object Commutative {
      def cond(n: Node)(matcher: PartialFunction[(Node, Node, If.Exit, If.Exit), Boolean]): Boolean =
        impl[Boolean, Boolean](n, false, x => x, matcher)

      def condOpt[A](n: Node)(matcher: PartialFunction[(Node, Node, If.Exit, If.Exit), A]): Option[A] =
        impl[A, Option[A]](n, None, Some.apply, matcher)

      private def impl[A, B](n: Node, default: B, trans: A => B, matcher: PartialFunction[(Node, Node, If.Exit, If.Exit), A]): B = {
        n match {
          case NonCommutative(x, y, t, f) =>
            val lifted = matcher.lift
            lifted((x, y, t, f)) orElse lifted((y, x, t, f)) map trans getOrElse default
          case _ =>
            default
        }
      }
    }
  }

  /** Matches `if (x == Null) t else f` and returns `(x, t, f)`. */
  object IfNull {
    def unapply(n: If) = condOpt(n) {
      case IfEq.NonCommutative(x, _: AnyNull, t, f) => (x, t, f)
    }
  }

  /** Matches `if (o instanceof tpe) t else f` and returns `(tpe, o, t, f)`. */
  object IfInstanceOf {
    def unapply(n: If) = condOpt(n) {
      case IfEq.NonCommutative(InstanceOf(tpe, o), IConst(0), f, t) => (tpe.symType, o, t, f)
    }
  }


  abstract class AnySwitch[C] protected(proto: AnySwitch.Proto[C, _ <: AnySwitch[C]]) extends BlockEnd(proto) with Branch with CompositeNode {

    type Exit <: AnySwitch.Exit[C]

    def cases = proto.cases

    def newExit(caseOpt: Option[C]): Exit

    val defaultExit: Exit = newExit(None)
    val caseExits: Seq[Exit] = cases map { l => newExit(Some(l)) }

    override val exits: Seq[Exit] = defaultExit +: caseExits

    def outCtrl(selectorValue: C): Exit = caseExits find (_.caseValue == selectorValue) getOrElse defaultExit
  }

  object AnySwitch {
    abstract class Proto[C, S <: AnySwitch[C]] private[KernelNodes](val selectorType: Type) extends BlockEndProto[S](ControlType, MemoryType, selectorType)(BranchType) {
      val cases: Seq[C]
    }

    trait Exit[C] extends Branch.Exit {
      type OwnerType <: AnySwitch[C]
      def caseOption: Option[C]
      def isDefault: Boolean = caseOption.isEmpty
      def caseValue = caseOption.get
      override def name = if (isDefault) s"${owner.name}/default" else s"${owner.name}[$caseValue]"

      def genCaseCheck(): Node
    }

    def dropExits[L](toDrop: AnySwitch.Exit[L]*): AnySwitch[L] = {
      val oldSwitch = ScalaCollections.uniqueValue(toDrop map (_.owner)).get
      assert(toDrop forall (!_.isDefault), "Not implemented yet")
      val toKeep = oldSwitch.caseExits filterNot toDrop.contains

      val newLabels = toKeep map (_.caseValue)
      val proto: AnySwitch.Proto[_, _] = oldSwitch match {
        case _: Switch => Switch(newLabels)
        case n: TauSwitch => TauSwitch(newLabels, n.info.filterByIndex(toKeep contains oldSwitch.caseExits(_)))
      }
      val newSwitch = proto(oldSwitch.inCtrl, oldSwitch.inMemory, oldSwitch.selector).asInstanceOf[AnySwitch[L]]
      oldSwitch.defaultExit replaceUsesBy newSwitch.defaultExit
      for ((oldExit, newExit) <- toKeep zip newSwitch.caseExits) {
        oldExit replaceUsesBy newExit
      }
      makeUnreachable(toDrop map (_.outEdge))
      decommit(oldSwitch)
      newSwitch
    }
  }

  /** Control-flow switch (computed goto) branch instruction.
    *  Has one default and several labeled control outputs.
    */
  class Switch private (proto: Switch.Proto) extends AnySwitch[Int](proto) {
    type Exit = Switch.Exit

    override def newExit(caseOpt: Option[Int]) = Switch.Exit(caseOpt).raw(this)

    override def constExit(selectorValue: Node): Option[Exit] = condOpt(selectorValue) {
      case IConst(value) => outCtrl(value)
    }
  }

  object Switch {
    case class Proto private[Switch] (cases: Seq[Int]) extends AnySwitch.Proto[Int, Switch](IntType) {

      def newInstance() = new Switch(this)
    }

    def apply(cases: Seq[Int]) = Prototype.intern(Proto(cases))
    def unapply(switch: Switch): Option[Node] = Some(switch.selector)

    class Exit private (proto: Exit.Proto) extends NodeWithFixedArgs(proto) with AnySwitch.Exit[Int] {
      type OwnerType = Switch
      def caseOption: Option[Int] = proto.caseOpt

      override def genCaseCheck() = {
        require(!isDefault)
        Cmp(IntType, Condition.EQ)(owner.selector, IConst(caseValue))
      }
    }

    object Exit {
      case class Proto private[Exit] (caseOpt: Option[Int]) extends Branch.Exit.Proto[Exit] {
        def newInstance() = new Exit(this)
      }

      def apply(caseOpt: Option[Int]) = Prototype.intern(Proto(caseOpt))
      def unapply(x: Exit) = Some(x.caseOption)
    }
  }

  class TauSwitch private(proto: TauSwitch.Proto) extends AnySwitch[Guard](proto) {
    assert(guards forall (g1 => guards forall (g2 => g1 == g2 || g1.disjointWith(g2))))
    assert(currentPhase > CompilerPhase.Serialization, "this node must not be serialized")

    type Exit = TauSwitch.Exit
    def guards = cases

    def info = proto.info

    override def newExit(caseOpt: Option[Guard]) = TauSwitch.Exit(caseOpt).raw(this)

    override def constExit(selectorValue: Node) = None // we could check selector type here but it seems unnecessarily hard
  }

  object TauSwitch {
    case class Proto private[TauSwitch](cases: Seq[Guard], info: TauInfo.PGO) extends AnySwitch.Proto[Guard, TauSwitch](TRefType) {
      def newInstance() = new TauSwitch(this)
    }

    def apply(guards: Seq[Guard], info: TauInfo.PGO) = Prototype.intern(Proto(guards, info))
    def unapply(x: TauSwitch) = Some(x.info, x.selector, x.cases)

    class Exit private (proto: Exit.Proto) extends NodeWithFixedArgs(proto) with AnySwitch.Exit[Guard] {
      type OwnerType = TauSwitch
      def caseOption = proto.guard

      override def genCaseCheck() = {
        require(!isDefault)
        require(owner.caseExits == Seq(this), "only if-like tau-switches are supported") // TODO: filter info if needed
        TauTest(caseValue, owner.info, owner.inCtrl, owner.selector)
      }
    }

    object Exit {
      case class Proto private[Exit] (guard: Option[Guard]) extends Branch.Exit.Proto[Exit] {
        def newInstance() = new Exit(this)
      }

      def apply(guard: Option[Guard]) = Prototype.intern(Proto(guard))
      def unapply(x: TauSwitch.Exit) = Some(x.caseOption)
    }
  }


  /** Node holding [[TauInfo]]. Either [[TauSwitch]] or [[If]]([[TauTest]]). */
  object TauBranch {
    def unapply(b: Branch): Option[(TauInfo, Node, Seq[Branch.Exit], Branch.Exit)] = condOpt(b) {
      case x @ If(TauTest(_, info, obj)) => (info, obj, Seq(x.trueExit), x.falseExit)
      case x @ TauSwitch(info, obj, _) => (info, obj, x.caseExits, x.defaultExit)
    }
  }


  /** Low-level table jump operation.
    * It has table of control outputs, and int argument in range 0..tableSize-1 which is used as a selector.
    */
  class TableJump private (proto: TableJump.Proto) extends BlockEnd(proto) with Branch {

    import TableJump.Exit

    def tableSize = proto.tableSize
    def tableSym = proto.tableSym

    def table = arg(3)

    override val exits: Seq[Exit] = IndexedSeq.tabulate(tableSize)(Exit(_).raw(this))

    override def constExit(selectorValue: Node): Option[Branch.Exit] = condOpt(selectorValue) {
      case IConst(value) => exits(value)
    }
  }

  object TableJump {
    case class Proto private[TableJump] (tableSize: Int, tableSym: Symbol)
      extends BlockEndProto[TableJump](ControlType, MemoryType, IntType, AddrType)(BranchType) {

      def newInstance() = new TableJump(this)
    }

    def apply(tableSize: Int, tableSym: Symbol) = Prototype.intern(Proto(tableSize, tableSym))

    class Exit private (proto: Exit.Proto) extends NodeWithFixedArgs(proto) with Branch.Exit {
      type OwnerType = TableJump
      def index = proto.index
      override def name = s"${owner.name}[$index]"
    }

    object Exit {
      case class Proto private[Exit] (index: Int) extends Branch.Exit.Proto[Exit] {
        def newInstance() = new Exit(this)
      }

      def apply(index: Int) = Prototype.intern(Proto(index))
    }
  }


  /** Node that connects unreachable [[XBlock]]s to [[unreachableBar]]. */
  class UnreachableThrowing extends NodeWithFixedArgs(UnreachableThrowing) with SpinalNode with CanThrow with NotProducesValue
  object UnreachableThrowing extends FixedArgs[UnreachableThrowing](ControlType, MemoryType)(ControlType)
    with SpinalNodePrototype[UnreachableThrowing] with ControlTagged[UnreachableThrowing] {
    protected def newInstance() = new UnreachableThrowing
  }


  /** Node that connects unreachable [[BBlock]]s to [[unreachableBar]]. */
  class UnreachableBlockEnd private extends BlockEnd(UnreachableBlockEnd) {
    implicit object UBEExitsSetsAndMaps extends Sets.Default[UnreachableBlockEnd.Exit] with Maps.Default[UnreachableBlockEnd.Exit]
    private val _exits = Sets[UnreachableBlockEnd.Exit].newQSet
    def exits = _exits.toSeq

    def newExit(): UnreachableBlockEnd.Exit = UnreachableBlockEnd.Exit(this) tap _exits.add

    private[ir] override def decommitImpl(): Unit = {
      if (isCommitted) {
        exits foreach decommit
        super.decommitImpl()
      }
    }
  }

  object UnreachableBlockEnd extends BlockEndProto[UnreachableBlockEnd](ControlType, MemoryType)(UnreachableControlType) {
    protected def newInstance() = new UnreachableBlockEnd

    class Exit private extends NodeWithFixedArgs(Exit) with Projection with BlockExit {
      type OwnerType = UnreachableBlockEnd

      def target = single[BBlock](uses)
      def outEdge = ScalaCollections.singleElement(outEdges)
      def memoryBefore = block
      private[ir] def blockControlNum = ControlNode.BlockEndControlNum

      override def decommitImpl(): Unit = {
        if (isCommitted) {
          owner._exits.remove(this)
          super.decommitImpl()
        }
      }
    }

    object Exit extends FixedArgs[Exit](UnreachableControlType)(ControlType) with PrototypeStrictNodeClass[Exit, Exit] {
      protected def newInstance() = new Exit
    }
  }


  /** Throw operation: throws exception object.
    *
    * If operation's block has xHandler then throw operation transfers control to it.
    * Otherwise throw operation rethrows exception to the caller method.
    */
  class Throw private extends NodeWithFixedArgs(Throw) with SpinalMemoryNode with CanThrow with NotProducesValue {
    def inValueArg = 2
    def inValue = arg(inValueArg)
    def inValue_=(n: Node): Unit = updateArg(inValueArg, n)

    override def hasXSite = false // it can throw but does not need XSite

    var shouldPreventBareSOEInstantiation = false
  }

  object Throw extends FixedArgs[Throw](ControlType, MemoryType, TRefType)(ControlType) with ControlMemoryTagged[Throw] {
    def newInstance() = new Throw

    def unapply(node: Throw): Option[(Node, Node, Node)] =
      Some(node.inCtrl, node.inMemory, node.inValue)
  }


  class XPoint extends NodeWithFixedArgs(XPoint) with Projection {
    type OwnerType = SpinalNode

    def hasHandler = uses.nonEmpty
    def handler = singleUse.asInstanceOf[XBlock]
    def handlerOrNull = if (hasHandler) handler else null
    def handlerOption = Option(handlerOrNull)

    def xEdge = ScalaCollections.singleElement(outEdges)

    override private[ir] def blockControlNum: Long = shouldNotCallThis()

    protected override def useAdded(use: Edge): Unit = {
      assert(hasHandler && handler == use.target)
      if (owner != null) owner.spineChanged(keepControlNums = true)
      scope.xedgesCount += 1
      super.useAdded(use)
    }

    protected override def useRemoved(use: Edge): Unit = {
      assert(!hasHandler)
      if (owner != null) owner.spineChanged(keepControlNums = true)
      scope.xedgesCount -= 1
      super.useRemoved(use)
    }
  }

  object XPoint extends FixedArgs[XPoint](XControlType)(ControlType) with PrototypeStrictNodeClass[XPoint, XPoint] {
    override def newInstance() = new XPoint()
    def unapply(x: XPoint) = Some(x.owner)

    object WithoutHandler {
      def unapply(n: XPoint) = !n.hasHandler
    }
  }

}
