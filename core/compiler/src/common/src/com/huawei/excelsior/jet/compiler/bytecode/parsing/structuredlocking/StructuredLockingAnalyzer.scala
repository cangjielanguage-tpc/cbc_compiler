/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking

import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowAnalyzer.WorkListBlockProcessResult
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalysisResult._
import com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalyzer.{AnalysisFailedException, LockingInformation, State, analysisFailed}
import com.huawei.excelsior.jet.compiler.bytecode.parsing.{DataFlowAnalyzer, DataFlowMergeResult}
import com.huawei.excelsior.jet.compiler.options.BoolOption

import scala.collection.mutable

/** See [[com.huawei.excelsior.jet.runtime.thread.sync.StructuredLockingChecks]] for structured locking support overview.
  *
  * This class encapsulates data flow analysis of ''Structured Locking'' (JVMS 2.11.10 "Synchronization")
  * requirements for particular method.
  *
  * Algorithm is incomplete and heuristic-based - the main goal is to cover all commonly used bytecode producers
  * (javac/scalac/kotlinc/...) with the simplest analysis possible.
  *
  * Algorithm overview:
  *
  *  - There are two kinds of states: `Unknown` (aka `Top`) and `List[MonitorEnter]`. Merging rules:
  *     - `Top ^ x -> x`
  *     - `list1 ^ list2 -> if (list1 == list2) list1 else analysisFailed`
  *
  *  - Some exceptional edges are considered as "invisible" (not really executed). We consider paired monitor operations
  *    as non-throwing.
  *
  *  - Transformation rules:
  *     - MonitorEnter adds a monitor into current list.
  *     - MonitorExit checks that current monitor list is not empty and verifies that monitor acquire/release operations
  *       are in LIFO order w.r.t. to the monitor object.
  *     - Normal exit from method (via "return") checks that all monitors are exited.
  *     - Exceptional exit from method checks that all monitors are exited.
  */
object StructuredLockingAnalyzer {
  object LockingInformation {
    private val EMPTY = new LockingInformation[Nothing, Nothing](Map.empty, Map.empty, STRUCTURED)

    def empty[EN, EX] = EMPTY.asInstanceOf[LockingInformation[EN, EX]]

    private val _potentiallyUnstructured = new LockingInformation[Nothing, Nothing](null, null, POTENTIALLY_UNSTRUCTURED)
    private val _notPairedDueToJSR = new LockingInformation[Nothing, Nothing](null, null, NOT_PAIRED_DUE_TO_JSRS)

    def potentiallyUnstructured[EN, EX] = _potentiallyUnstructured.asInstanceOf[LockingInformation[EN, EX]]

    def potentiallyUnstructuredDueToJSRs[EN, EX] = _notPairedDueToJSR.asInstanceOf[LockingInformation[EN, EX]]
  }

  final class LockingInformation[EN, EX] private[StructuredLockingAnalyzer](
    _monitorPairs: collection.Map[EX, EN],
    _outerMonitors: collection.Map[EN, EN],
    val state: StructuredLockingAnalysisResult
  ) {
    assert(state != STRUCTURED || (_monitorPairs != null && _outerMonitors != null))

    /** Binds each MonitorExit to corresponding MonitorEnter. */
    def monitorPairs = {
      assert(state == STRUCTURED)
      _monitorPairs
    }

    /** Binds each MonitorEnter to embracing MonitorEnter (if one exists). */
    def outerMonitors = {
      assert(state == STRUCTURED)
      _outerMonitors
    }
  }

  object State {
    def top[EN] = new StructuredLockingAnalyzer.State[EN](null)
  }

  final class State[EN](private var _enters: List[EN] = List.empty[EN]) extends DataFlowAnalyzer.State[State[EN]] {
    def activeEnters = {
      assert(!isTop)
      _enters
    }

    def enters = _enters

    def hasEnters = _enters.nonEmpty

    def isTop = _enters == null

    override def mergeFrom(that: State[EN]): DataFlowMergeResult = {
      assert(!that.isTop)

      if (this.isTop) {
        this._enters = that._enters
        return DataFlowMergeResult.INITIALIZED
      }

      if (this._enters == that._enters) {
        return DataFlowMergeResult.UNCHANGED
      }

      analysisFailed(s"$this is not equal to $that")
    }

    override def toString: String = if (isTop) "[ ? ]" else _enters.mkString("[", ", ", "]")
  }

  private class AnalysisFailedException(reason: String) extends RuntimeException(reason)

  def analysisFailed(reason: String) = throw new AnalysisFailedException(reason)
}

abstract class StructuredLockingAnalyzer[B >: Null, EN, EX]
  extends DataFlowAnalyzer.WorkListVersion[B, State[EN]](false, null) {

  /** Returns whether `block` may normally exit from method (i.e. ends with `return` instruction). */
  protected def blockHasNormalExit(block: B): Boolean

  /** Returns whether `block` may abruptly exit from method (i.e. has throwing instructions without "catch all" handlers).
    * It should assume that monitor operations do not throw.
    */
  protected def blockHasExceptionalExit(block: B): Boolean

  protected def areMatching(exit: EX, enter: EN): Boolean

  protected def analyzeOneBlock(block: B, inputState: State[EN]): BlockStructure[EN, EX]

  protected val monitorPairs = mutable.HashMap.empty[EX, EN]
  protected val outerMonitors = mutable.HashMap.empty[EN, EN]
  protected val inputStates = mutable.HashMap.empty[B, State[EN]]

  override protected def inputState(block: B) =
    inputStates.getOrElseUpdate(block, if (block == entryBlock) new State else State.top)

  override protected def processBlock(block: B, inputState: State[EN]): WorkListBlockProcessResult[B, State[EN]] = {
    val currentBS = analyzeOneBlock(block, inputState)
    if (debugEnabled) {
      log(s"Block $block is $currentBS")
    }

    if (currentBS == BlockStructure.Error) {
      return analysisFailed("Erroneous block")
    }

    assert(!inputState.isTop)

    val normalState = transformState(inputState, currentBS, block)
    if (blockHasNormalExit(block) && normalState.hasEnters) {
      analysisFailed("Failed on normal exit")
    }

    // monitorenter throws before real acquiring, monitorexit never throws
    val exceptionalState = inputState
    if (blockHasExceptionalExit(block) && exceptionalState.hasEnters) {
      analysisFailed(s"$block has enters on exceptional exit")
    }

    new WorkListBlockProcessResult(normalState, exceptionalState, Iterator.empty, null)
  }

  private def transformState(state: State[EN], currentBS: BlockStructure[EN, EX], block: B): State[EN] = {
    currentBS match {
      case BlockStructure.Enter(enter) =>
        if (state.hasEnters) {
          val outer = state.enters.head
          outerMonitors.put(enter, outer) ensuring (_.isEmpty)
        } else {
          assert(!outerMonitors.contains(enter))
        }

        new State(enter +: state.enters)

      case BlockStructure.Exit(exit) =>
        if (state.enters.isEmpty) {
          return analysisFailed(s"$block in state $currentBS without enter")
        }

        val pairedEnter = state.enters.head
        if (!areMatching(exit, pairedEnter)) {
          return analysisFailed("Exit without pairing enter")
        }
        monitorPairs.put(exit, pairedEnter) ensuring (_.isEmpty)

        new State(state.enters.tail)

      case _ => state
    }
  }

  /** Checks if current method is structurally locked.
    *
    * Refer to [[com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking.StructuredLockingAnalyzer.LockingInformation]] for detailed description of analysis results.
    */
  final def analyzeLocking(): LockingInformation[EN, EX] = {
    try {
      analyze()
      if (debugEnabled) {
        log("accepted method")
        log(s"Exit -> Enter map: $monitorPairs")
        log(s"Enter -> OuterEnter map: $outerMonitors")
      }
      new LockingInformation(monitorPairs, outerMonitors, STRUCTURED)

    } catch { case fail: AnalysisFailedException =>
      if (debugEnabled) {
        log(s"analysis failed due to: ${fail.getMessage}")
      }
      LockingInformation.potentiallyUnstructured

    } finally {
      if (debugEnabled) {
        debugGraph()
      }
    }
  }
}
