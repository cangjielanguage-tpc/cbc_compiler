/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.ObjectBiGraph
import com.huawei.excelsior.jet.util.graph.analysis.DataFlowAnalysis

/** Analysis of local unmovable nodes (modifies IR!).
  *
  * Calculates for each control node `P` a set of nodes `S` such that each of them is an argument
  * of group of `BeginLocalUnmovable` cumulatively dominating `P` and an argument of group of
  * `EndLocalUnmovable` cumulatively post-dominating `P`.
  *
  * Note that extra phi-functions may be inserted during analysis and all `BeginLocalUnmovable` are eliminated afterwards.
  *
  * @author wellox
  * @author conwor
  * @author liontiger
  */
trait UnmovableAnalysis { self: Universe =>

  final class LocalUnmovableAnalysis private [UnmovableAnalysis] extends DataFlowAnalysis[ControlNode](new SpinalCFG(currentScope)) {

    /** Mapping:
      *   - from [[BeginLocalUnmovable]] to its [[BeginLocalUnmovable#obj obj]], or
      *   - from phi-function of those begins to phi-function of corresponding unmovable values.
      */
    type State = Map[Node, Node]

    override protected def init = Map.empty

    override protected def join(outputStates: IterableOnce[State]): State = outputStates.iterator reduce (_ ++ _)

    override protected def trans(node: ControlNode, inputState: State): State = node match {
      case b: Block =>
        var state = inputState
        for (key <- b.phies if key.valueArgs forall inputState.contains) {
          // Replace merged values with new phi.
          val mergedArgs = (key.valueArgs map inputState).toSeq
          state --= key.valueArgs
          state = state updated (key, Phi(TRefType)(b +: mergedArgs: _*))
        }
        state

      case node @ BeginLocalUnmovable(obj) =>
        inputState updated (node, obj)

      case EndLocalUnmovable(obj) =>
        // Remove all transitive args.
        // This is needed in case of backward loop branch with `phi(initBegin,loopBegin)`
        // in which case the state will contain `initBegin` instead of phi function,
        // because backward branch (and the loop body) was not yet processed.
        // See "tricky loop" in unit tests.
        val keys = Closure(obj) {
          case p: Phi => p.args
          case _: BeginLocalUnmovable => Iterator.empty
          case x => shouldNotReachHere(x)
        }
        inputState -- keys

      case _: Return =>
        assert(inputState.isEmpty, s"unpaired beginLocalUnmovable at return: $inputState")
        inputState

      case _ => inputState
    }
  }

  def checkLocalUnmovableConsistency(): Unit = {
    // Before post-inline following invariants may not hold for @Inline-annotated wrappers
    // of actual begin/endLocalUnmovable calls (e.g wrappers in JavaGCBarriers).
    if (CompilerPhase.PostInline < currentPhase && currentPhase < CompilerPhase.Preparation && !areVarsPresent && noUnreachableCode) {
      checkLocalUnmovableConsistencyImpl()
    }
  }

  private def checkLocalUnmovableConsistencyImpl(): Unit = {
    require(!areVarsPresent && noUnreachableCode)
    for (b <- all[BeginLocalUnmovable]) {
      val uses = Phi.transitiveValueUses(b)
      assert(uses forall (_.isInstanceOf[EndLocalUnmovable]), s"inconsistent uses of $b: $uses")
    }
    for (e <- all[EndLocalUnmovable]) {
      val args = Phi.transitiveValueArgs(e)
      assert(args forall (_.isInstanceOf[BeginLocalUnmovable]), s"inconsistent args of $e: $args")
    }
  }

  def analyzeLocalUnmovable(): LocalUnmovableAnalysis = {
    checkLocalUnmovableConsistencyImpl()
    val analysis = new LocalUnmovableAnalysis()
    eliminateBeginLocalUnmovable()
    analysis
  }

  private def eliminateBeginLocalUnmovable(): Unit = {
    for (n <- all[BeginLocalUnmovable]) {
      n.replaceValueUsesBy(n.obj)
      strikeOut(n)
    }
  }

}
