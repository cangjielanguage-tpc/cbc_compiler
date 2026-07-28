/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.ObjectBiGraph

import scala.collection.mutable.ArrayBuffer

/**
 * Various graphs formed by IR nodes.
 *
 * @author paul
 * @author conwor
 * @author cypok
 */
trait NodeGraphs { self: Universe =>

  /** DAG is a graph of block nodes with explicit dependencies between them.
    * Dependencies are of two types:
    *  1) def-use dependency from def to use;
    *  2) anti-dependency from node that uses some resource version to node that produces new version of this resource (e.g. memory).
    *
    * This graph have to be acyclic, because it represents normal order of nodes used in code generation.
    *
    * Note that Branch.Exit nodes are excluded from this graph.
    */
  class DAG(block: Block, strictlyByPoints: Boolean = false) extends ObjectBiGraph[Node] {

    private def groot(n: Node) = n match {
      case _: Constraints => false
      case x => x.isGroupRoot
    }

    val nodes = (block.nodes filter groot).to(ArrayBuffer)

    protected val _succs = Maps[Node].newQMap[Sets[Node]#QSet]
    protected val _preds = Maps[Node].newQMap[Sets[Node]#QSet]

    private def localUsesFilter(use: Node) = use match {
      case _: Phi | _: Constraints | _: XPoint => false
      case x if x.block == block => true
      case _ => false
    }

    private def localUsesMap(use: Node): Iterator[Node] = use match {
      case x if !x.isGroupRoot => localUses(x)
      case _ => Iterator(use)
    }

    protected def localUses(node: Node): Iterator[Node] = node.uses filter localUsesFilter flatMap localUsesMap

    protected def dependenciesByPoint(node: Node): Iterator[Node] = node match {
      case point: UpperPoint if strictlyByPoints =>
        Iterator.single(point.outCtrl) ++ point.pinnedNodes filter (n => n != point && groot(n))

      case _: ControlNode => Iterator.empty
      case n: BlockParamNode => Iterator.single(n.block.outCtrl)
      case n: FloatingNode => Option(n.lowerPoint).iterator
    }

    protected def dependentNodes(node: Node): Iterator[Node] = node match {
      case `block` =>
        nodes.iterator filter { _ != node }

      case x if x == block.blockEnd =>
        Iterator.empty

      case _ =>
        Iterator.single(block.blockEnd) ++ localUses(node) ++ dependenciesByPoint(node)
    }

    {
      for (node <- nodes) _succs(node) = Sets[Node].newQSet(dependentNodes(node))
      for (node <- nodes) _preds(node) = Sets[Node].newQSet
      for ((node, nodeSuccs) <- _succs; succ <- nodeSuccs) _preds(succ) += node
    }

    override def start = block
    override def succs(node: Node): Iterator[Node] = _succs(node).iterator
    override def preds(node: Node): Iterator[Node] = _preds(node).iterator

    def size: Int = nodes.size
  }

  object DAG {
    def apply(block: Block, strictlyByPoints: Boolean = false) = new DAG(block, strictlyByPoints)
  }
}
