/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing

import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowAnalyzer.{BlockProcessResult, State, quote}
import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerificationUnit}
import com.huawei.excelsior.jet.util.Worklist

import scala.reflect.ClassTag

/** Data flow analysis abstraction. Iterates over CFG using worklist and computing states for every reachable basic block
  * starting from some initial state until reaching MFP.
  * A new state is computed while processing a block [[DataFlowAnalyzer.processBlock]].
  * It is assumed that before the block is processed the block's input state is forked and mutates while block processing
  * then it is merged to input states of all successor blocks in the end of block processing and after all is released.
  *
  * @author kit
  * @author cypok
  */
object DataFlowAnalyzer {
  trait State[S <: State[_]] {
    /** Merges that state to this one and returns true if this was changed.
      * This state is mutated, that state is immutable.
      */
    def mergeFrom(that: S): DataFlowMergeResult

    /** Release internal data structures after this state is merged into all successors. */
    def release(): Unit = {}
  }

  /** In the end of block processing we have a state that we should pass by normal control flow,
    * a state for traversing by exceptional control flow (may be `null` if there are no handlers).
    */
  // TODO: if you see this in allocation profile make me stack allocatable and mutable
  protected class BlockProcessResult[S](val normalState: S, val exceptionState: S)

  private def releaseStates(results: BlockProcessResult[_ <: State[_]]): Unit = {
    results.normalState.release()
    if (results.exceptionState != null) {
      results.exceptionState.release()
    }
  }

  abstract class WorkListVersion[B >: Null, S <: State[S]](verify: Boolean, verificationContext: VerifiableMethod)
    extends DataFlowAnalyzer[B, S, WorkListBlockProcessResult[B, S]](verify, verificationContext) {

    private val worklist = Worklist.empty[B]

    protected def analyze(): Unit = {
      worklist.clear()
      worklist += entryBlock

      for (block <- worklist.drain) {
        val result = processBlock(block, inputState(block))

        processSuccs(succBlocks(block), result.normalState)
        processSuccs(handlerBlocks(block), result.exceptionState)
        processSuccs(result.implicitSuccs, result.normalState)

        if (result.worklistAddition != null) {
          worklist += result.worklistAddition
        }

        releaseStates(result)
      }

      worklist.clear()
    }

    private def processSuccs(succs: Iterator[B], outState: S): Unit =
      worklist ++= (succs filter (inputState(_).mergeFrom(outState) != DataFlowMergeResult.UNCHANGED))
  }

  /** Also we have some successors (e.g. ret blocks successors) may be computed during the data flow we return this
    * information when it becomes available, and some additional block (e.g. ret block after jsr block processing)
    * that should be processed one more time.
    */
  final class WorkListBlockProcessResult[B >: Null, S](
    normalState: S,
    exceptionState: S,
    val implicitSuccs: Iterator[B], // may be empty
    val worklistAddition: B // may be null
  ) extends BlockProcessResult[S](normalState, exceptionState)

  abstract class RoundRobinVersion[B : ClassTag, S <: State[S]](verify: Boolean, verificationContext: VerifiableMethod)
    extends DataFlowAnalyzer[B, S, BlockProcessResult[S]](verify, verificationContext) {

    protected def analyze(topSortedBlocks: collection.Seq[B]): Unit = {
      // May be optimized to keep changed for every block and traverse not all blocks on steps after first.
      whileChanged { changed =>
        for (block <- topSortedBlocks) {
          val result = processBlock(block, inputState(block))

          if (processSuccs(succBlocks(block), result.normalState)) {
            changed()
          }
          if (processSuccs(handlerBlocks(block), result.exceptionState)) {
            changed()
          }

          releaseStates(result)
        }
      }
    }

    private def processSuccs(succs: Iterator[B], outState: S): Boolean =
      // Call .toArray() to evaluate the filter lambda for all elements.
      succs.filter(inputState(_).mergeFrom(outState) == DataFlowMergeResult.CHANGED).toArray.nonEmpty
  }

  private def quote(s: Any) = s"\"$s\""
}

/** Data flow analysis abstraction. Iterates over CFG using worklist and computing states for every reachable basic block
  * starting from some initial state until reaching MFP.
  *
  * @tparam B CFG's basic block
  * @tparam S state that is computed for every block
  */
abstract class DataFlowAnalyzer[B, S <: State[S], R <: BlockProcessResult[S]](verify: Boolean, verificationContext: VerifiableMethod)
  extends VerificationUnit(verify, verificationContext) {

  /** Input state of blocks. These states are mutated during analysis. */
  protected def inputState(block: B): S

  /** Computes new output states by block input state.
    * Input state should be forked in the beginning and should not be mutated during block processing.
    */
  protected def processBlock(block: B, inputState: S): R

  protected def entryBlock: B

  protected def succBlocks(block: B): Iterator[B]

  protected def handlerBlocks(block: B): Iterator[B]

  final protected def hasHandlers(block: B) = handlerBlocks(block).hasNext

  protected def debugEnabled = false

  protected def allInputStatesForDebug: collection.Map[B, S] = throw new UnsupportedOperationException

  final protected def log(s: String): Unit = if (debugEnabled) println(s)

  final protected def debugGraph(): Unit = {
    if (!debugEnabled) {
      return
    }

    log("")
    log("digraph cfg {")
    val allInputStates = allInputStatesForDebug
    for ((block, state) <- allInputStates) {
      val info = blockDebugInfo(block, state)
      val blockLabel = s"$block in = $state" + (if (info != null) ", " + info else "")
      log(s"${quote(block)}[label=${quote(blockLabel)}]")

      for (s <- succBlocks(block) if allInputStates.contains(s)) {
        log(s"${quote(block)} -> ${quote(s)}")
      }

      for (h <- handlerBlocks(block) if allInputStates.contains(h)) {
        log(s"${quote(block)} -> ${quote(h)}[style = dashed]")
      }
    }
    log("}")
  }

  protected def blockDebugInfo(block: B, state: S): String = null
}
