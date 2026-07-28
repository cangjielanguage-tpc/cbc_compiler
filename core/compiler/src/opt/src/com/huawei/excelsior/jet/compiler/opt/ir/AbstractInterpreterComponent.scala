/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.Worklist

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * AbstractInterpreter interprets CFG by block interpretation function and merge/resolve/proxy technique.
 *
 * The class declares abstract method 'interpret' that provides abstract interpretation of a block in CFG.
 * This method takes an input ''state'' and produces a new output state.
 * While interpretation of a block, nodes are generated. Some of generated nodes form an output state that
 * can be consumed by successor blocks.
 *
 * As for any block there can be multiple input blocks in the graph, the task of merging input states arises.
 * This is done by `merge` method of State which is abstract here.
 *
 * As graph can contain cycles, we cannot compute all input states in the time we interpret given block.
 * In this case, we construct a ''proxy'' out states for unprocessed predecessors (usually backward branches) --
 * this is a state that is not computed yet, but will be computed when we reach blocks that have backward outgoing branches.
 * For the block `X` that was used before processing (for such blocks proxy state was generated) another task arises:
 * resolve proxy states for its uses that we constructed in a time we interpret the blocks which have incoming edges from `X`
 * (usually backward branches). This is done by `resolveWith` method
 * of State which is also abstract here. Resolution of proxy state is a process of replacing proxy data
 * with already computed one, merging computed data with another incoming computed data.
 * Resolution of proxy states can touch a sub-graph of the given graph where uses of proxies are scattered.
 * See [[com.huawei.excelsior.jet.compiler.opt.ir.NodeReplaceOptimizer]] for details on later.
 *
 * @author paul
 * @author conwor
 * @author cypok
 */
trait AbstractInterpreterComponent { self: Universe =>

  /** Invalid node. Marks invalid state: uninitialized data or merge of incompatible values. */
  protected lazy val Invalid = VerificationNode.raw()

  abstract class AbstractInterpreter {

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Abstract part

    type State <: AbstractInterpreter.State { type This = State }

    /** Creates an input state for a block without predecessors. */
    protected def startInputState(b: Block): State

    /** Interprets given block `b` by transforming given input state `s`.
      * @return possibly transformed out block.
      */
    protected def interpret(b: Block, s: State): Block


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Often overriding part

    /** Interpret given CFG edge by transforming given input state `s`.
      * @return possibly transformed blockExit.
      */
    protected def interpretEdge(blockExit: BlockExit, s: State): ControlNode = blockExit

    protected def debug(msg: String): Unit = {}

    protected def blocksOrder = cfg.topSort.order

    /** Resolve proxy state's element `src` with given value `dst`. */
    protected def resolve(src: Node, dst: Node): Unit = src match {
      case src: Proxy => dst match {
        case _: VerificationNode if src.uses.nonEmpty =>
        // When proxy node is resolved with verification node (invalid value), it is guaranteed by bytecode verifier
        // that it has no uses. However, such proxy node may be used by a bunch of dead phies.
        // Replacing such proxies provoke type check failures for phi arguments so we do not touch them at all
        // and let DCE remove them as dead.
        // TODO: move this logic into BytecodeParser?

        case _ => replaceTransitively(src, dst)
      }
      case _: VerificationNode =>
      case StackAlloc.DebugVar(_, _) => assert(src == dst)
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Implementation

    private val unprocessed = Worklist.empty[Block]
    private val readyStates = Maps[ControlNode].newQMap[State]
    private val proxyStates = Maps[Block].newQMap[ProxyState]
    private var fixedPointReached = false

    /** We iterate CFG in top sorted blocks order.
      *
      * For any step of iteration we do the following:
      *
      *  1. Compute an input state of the block. There can be two alternatives:
      *     - At least one of predecessors is processed.
      *       In such case all output states of processed predecessors are merged
      *       together with proxy states of all unprocessed predecessors.
      *
      *     - All of predecessors are unprocessed and we cannot compute input state.
      *       In such case this block is put back into the end of the `unprocessed` worklist
      *       and we continue iteration to the next block.
      *
      *  2. If input state is computed we perform abstract interpretation of the block
      *     that yields an output state of the block.
      *
      *  3. Resolve proxy states of this block if needed.
      *
      * At the end of the process no proxies must remain.
      *
      * All unreachable blocks are not interpreted.
      *
      * @return count of unreachable blocks (they were not interpreted).
      */
    def iterate(): Int = {
      readyStates.clear()
      proxyStates.clear()

      unprocessed.clear()
      unprocessed ++= blocksOrder
      fixedPointReached = false

      debug("after init")

      /** Number of unprocessed blocks occurred since the last processed block.
        *
        * If this number is equals to number of blocks in the `unprocessed` worklist
        * it means that we are caught in an endless loop and all of these blocks are unreachable.
        */
      var sinceLastProcessed = 0

      while (unprocessed.nonEmpty) {
        val bIn = unprocessed.head
        val state = inputState(bIn, allowUnprocessed = true, fixedPointReached)
        val unreachable = state == null
        unprocessed -= bIn

        if (unreachable && !fixedPointReached) {
          sinceLastProcessed += 1
          unprocessed += bIn // add this block info to the end of the unprocessed worklist

          // In most cases it is easy to check that `bIn` is truly unreachable, remove it and all outgoing edges.
          // You can find more details in JET-13749.

        } else {
          val proxiesToResolve = (dependentProxies(bIn) collect proxyStates filter (_.markReady())).toList

          if (!unreachable) {
            sinceLastProcessed = 0
            val bOut = interpret(bIn, state)
            debug("after " + bIn + " interpretation")

            // interpret block out state through outcoming CFG edges
            val exits = bOut.blockEnd.exits
            var useCnt = exits.size
            for (xIn <- exits if xIn.isCommitted) {
              useCnt -= 1
              val xst = if (useCnt > 0) state.fork() else state
              val xOut = interpretEdge(xIn, xst)
              readyStates.put(xOut, xst) ensuring (_.isEmpty)
            }
            // if some exits were optimized out, remove their states from map
            for (x <- exits if !x.isCommitted) {
              readyStates -= x
            }
          }

          // perform resolution of proxy states for bIn's backward branches
          for (ps <- proxiesToResolve) {
            val st = inputState(ps.key, allowUnprocessed = false, fixedPointReached)
            (ps.st foreachPair st)(resolve)
          }
        }

        fixedPointReached = fixedPointReached || (sinceLastProcessed == unprocessed.size)
      }

      sinceLastProcessed //TODO: return/remove seq of unreachable blocks and don't call UCE after interpretation?
    }

    /** Set input `st` for given `xpoint.handler` from exception edge of given `xpoint`. */
    def addXCtrl(xpoint: XPoint, st: State): Unit = {
      readyStates.put(xpoint, st) ensuring (_.isEmpty)
    }

    /**
      * Returns State at the beginning of corresponding block.
      * Creates unresolved proxy states for blocks with unprocessed predecessors.
      * Merges States that are at ends of all predecessors.
      * Returns `null` if we defer processing of this block.
      */
    private def inputState(block: Block, allowUnprocessed: Boolean, fixedPointIsReached: Boolean): State = {
      if (block == entryBlock) {
        assert(block.inputs.isEmpty)
        return startInputState(block)
      }

      // Input could be:
      // * ready (fully processed reachable)
      // * unreachable (treated as not ready and processed iff fixed point is reached)
      // * unprocessed (completely not ready)

      val (unprocessedInputs, processedInputs) = block.inputs partition (unprocessed contains _.block)
      val readyInputs = processedInputs filter readyStates.contains
      if (readyInputs.isEmpty) {
        // none of preds' states are ready yet => defer processing of the `block`
        return null.asInstanceOf[State]
      }

      val someReadyState = readyStates(readyInputs.head)

      if (!fixedPointIsReached) {
        // This assertion is correct only iff there were no unreachable code before AI starts to work.
        // TODO: maybe we should remove unreachable code before AI to simplify processing
        // assert(readyInputs.size == processedInputs.size)
      }

      if (unprocessedInputs.isEmpty) {
        // all predecessors are ready or unreachable => create merged state
        val st = if (readyInputs.size == 1) {
          someReadyState
        } else {
          lazy val unreachableState = someReadyState.makeUnreachableCopy()
          val processedStates = processedInputs map (readyStates.getOrElse(_, unreachableState))
          someReadyState.mergeFrom(block, processedStates, identity = true) { (tpe, values) => withPos(block)(Phi(tpe)(block +: values: _*)) }
        }
        readyStates --= readyInputs
        st

      } else {
        // some predecessor in unprocessed => create proxy state and check processed states
        assert(allowUnprocessed)
        val st0 = someReadyState.fork()
        val readyStatesForChecks = readyInputs map readyStates
        val ups = st0.mergeFrom(block, readyStatesForChecks, identity = false) { (tpe, values) => makeProxy(tpe, values.head, block) }
        proxyStates(block) = new ProxyState(block, ups, unprocessedInputs.size)
        ups.fork()
      }
    }


    protected def makeProxy(tpe: Type, actual: Node, block: Block): Node = withPos(block)(Proxy(tpe)(block))

    private def dependentProxies(b: Block): Iterator[Block] = b.xSuccBlocks

    private class ProxyState(val key: Block, val st: State, private var notReadyCount: Int) {
      def markReady(): Boolean = {
        notReadyCount -= 1
        if (notReadyCount > 0) false else { proxyStates -= key; true }
      }
    }
  }

  object AbstractInterpreter {

    /**Reference to value of type T */
    case class Ref[@specialized(Int) T <: AnyVal](private var value: T) {
      def apply() = value
      def update(x: T): Unit = { value = x }
    }

    /**
     * AbstractInterpreter.State represents state of interpreter as some IR objects (elements).
     *
     * For the different interpreters their states may accumulate elements in different ways. For example,
     * bytecode parser works with locals and stack slots, when SSA builder works with array of nodes.
     *
     * @author paul
     * @author conwor
     */
    abstract class State {
      
      protected type This <: State

      /** Returns lazy copy of this state */
      protected def forkImpl(): This

      /** Returns unreachable state compatible with this state. */
      def makeUnreachableCopy(): This

      /** Copy on write implementation */
      protected def copyOnWriteImpl(): Unit

      /** Merges `states` sequence into one state. `this` state does not participate as merge argument.
        * If all states in the sequence are identical, one of them may be returned without
        * any call to `mergeFunc` if `identity` flag is turned on.
        * `this` state may became destroyed and unusable after this operation.
        * 
        * @param mergeFunc merges given state elements corresponding to single value with given type
        */
      def mergeFrom(block: Block, states: Seq[This], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node): This

      /** Applies user-defined action for each pair of corresponding elements of `this` and `that` states. */
      def foreachPair(that: This)(action: (Node, Node) => Unit): Unit

      protected var sharedCount: Ref[Int] = Ref(1)

      protected def copyOnWrite(): Unit = {
        if (sharedCount() > 1) {
          copyOnWriteImpl()
          sharedCount() -= 1
          sharedCount = Ref(1)
        }
      }

      /** Returns a copy of this interpreter state */
      def fork(): This = {
        sharedCount() += 1
        val copy = forkImpl()
        copy.sharedCount = this.sharedCount
        copy
      }
    }
  }
}
