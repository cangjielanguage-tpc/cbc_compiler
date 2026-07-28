/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.util

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Worklist

/** Backend-specific graphs.
  *
  * @author conwor
  * @author paul
  */
trait BackendGraphs { self: Universe with BackEnd =>

  /** GenerationDAG is a graph of block nodes while code linearization process with explicit dependencies between nodes.
    * Dependencies are of three types:
    *  1) def-use dependency from def to use;
    *  2) anti-dependency from node that uses some resource version to node that produces new version of this resource (e.g. memory);
    *  3) dependency from last linearized node to all non-linearized nodes and no other dependencies between linearized
    *     and non-linearized nodes (linearized chain has only internal edges).
    *
    * This graph have to be acyclic, because it represents normal order of nodes used in code generation.
    *
    * Note that Branch.Exit nodes are excluded from this graph.
    */
  final class GenerationDAG(block: Block) extends DAG(block) {

    private val preProcessedCrown = Worklist.empty[Node]
    private val processedCrown = Worklist.empty[Node]
    private var _nextPoint: ControlNode = block

    { addToCrown(block) }

    private def addToCrown(node: Node): Unit = {
      preProcessedCrown += node
    }

    private def removeFromCrown(node: Node): Unit = {
      preProcessedCrown -= node
      processedCrown -= node
    }

    /** Process crown of DAG with given `action`. Nodes, added to crown during process,
      * would appear in non-processed part of crown and would be processed in future.
      */
    def processCrown(action: Node => Unit): Unit = {
      for (node <- preProcessedCrown.drain) {
        processedCrown += node
        action(node)
      }
    }

    /** Moves all processed nodes of crown to not-processed part. Used during crown processing
      * to re-process already processed nodes.
      */
    def dropProcessed(): Unit = {
      processedCrown.drainTo(preProcessedCrown)
    }

    /** Tie given `node` to generation list. Rebuild current crown with `node` successors, that have all
      * predecessors already generated.
      *
      * @param node node from current crown
      */
    def tie(node: Node): Unit = {
      removeFromCrown(node)
      CodeOrder.append(node, block)
      node.generated = true
      for (succ <- _succs(node)) {
        if (_preds(succ) forall { _.generated } ) addToCrown(succ)
      }
      if (node == _nextPoint) {
        _nextPoint = _nextPoint match {
          case _: BlockEnd => null
          case p: UpperPoint => p.outCtrl
        }
      }
    }

    def nextPoint = _nextPoint

    /** @return snapshot of current DAG crown. */
    def crown: collection.Seq[Node] = preProcessedCrown.snapshot ++ processedCrown.snapshot

    private def transferArg(node: Node): Node = node.arg.groupRoot

    /** Appends given `node` to DAG. Fails with assert, if `node` is block, or block end, or branch result, or cmp,
      * or it is already exists in DAG, or it is not linked to DAG `block`.
      *
      * Also rebuilds DAG crown and fails with assert, if given node already generated, or it have already generated
      * successor.
      *
      * Fails with assert, if appended node has value uses.
      *
      * We do not transform edges between appended node and it's uses,
      * because they are created only when uses are generated (see replaceArgumentsToApplicable in LocalGenerator.scala).
      */
    private def append(node: Node): Unit = {
      // TODO: implement described assertions after JET-8204 fixed
      node.asInstanceOf[FloatingNode] atLowerPoint _nextPoint.asInstanceOf[LowerPoint]
      nodes += node

      // Add edges from new node to it's successors (blockEnd and point)
      val dependent = Sets[Node].newQSet(dependentNodes(node))
      _succs(node) = dependent
      for (x <- dependent) {
        _preds(x) += node
        removeFromCrown(x)
      }

      // Add edge from new node arg to new node
      val arg = transferArg(node)
      _preds(node) = Sets[Node].newQSet(Seq(block))
      _succs(block) += node
      if (arg.block == block) {
        _succs(arg) += node
        _preds(node) += arg
      }

      // Add new node to crown, if required
      if (_preds(node) forall { _.generated }) addToCrown(node)
    }

    def remove(node: Node): Unit = {
      val arg = transferArg(node)
      if (arg.block == block) {
        _succs(arg).remove(node)
      }
      removeFromCrown(node)
      for (succ <- dependentNodes(node).toList) {
        _preds(succ).remove(node)
        if (_preds(succ) forall (_.generated)) addToCrown(succ)
      }
    }

    def withCallbacks[A](action: => A) = onCommit.withCallback(append) {
      onDecommit.withCallback(remove) {
        action
      }
    }

    override protected def dependentNodes(node: Node): Iterator[Node] = node match {
      case hint: BulldozerHint => Iterator(hint.outCtrl) // Special case required to exclude point-dependent nodes
      case _ => super.dependentNodes(node)
    }
  }
}
