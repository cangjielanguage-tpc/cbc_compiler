/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph.analysis

import com.huawei.excelsior.jet.util.WhileChanged.whileChanged
import com.huawei.excelsior.jet.util.graph.BiGraph
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.mutable

/** Data-flow analysis abstraction.
  *
  * Iterates over given `graph` in [[TopSort]] order using round-robin algorithm.
  *
  * @author conwor
  */
abstract class DataFlowAnalysis[N](protected val graph: BiGraph[N]) {

  type State

  protected val topSort = graph.topSort

  private val inputStates:  mutable.Map[N, State] = mutable.HashMap.empty[N, State]
  private val outputStates: mutable.Map[N, State] = mutable.HashMap.empty[N, State]

  def in(n: N): State = inputStates.getOrElseUpdate(n, init)
  def out(n: N): State = outputStates.getOrElseUpdate(n, init)

  /** Initial state. */
  protected def init: State

  /** Join operation for given `outputStates`.
    *
    * Note: size of `outputStates` may be less than the number of corresponding predecessors
    *       if some of them are unreachable.
    */
  protected def join(outputStates: IterableOnce[State]): State

  /** Transfer function for given `node`. */
  protected def trans(node: N, inputState: State): State

  {
    whileChanged { changed =>
      for (n <- topSort.order) {
        val (oldIn, oldOut) = (in(n), out(n))
        val newIn = if (n == graph.start) oldIn else join(graph.preds(n) filter topSort.contains map out)
        val newOut = trans(n, newIn)
        if ((oldIn != newIn) || (oldOut != newOut)) {
          inputStates(n) = newIn
          outputStates(n) = newOut
          changed()
        }
      }
    }
  }

}
