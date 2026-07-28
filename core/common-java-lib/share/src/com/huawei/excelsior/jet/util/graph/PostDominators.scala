/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import scala.reflect.ClassTag

abstract class PostDominators[N] {
  def iPostDom(node: N): N
  def postDoms(node: N): Iterator[N]
  def postDominates(x: N, y: N): Boolean
}

object PostDominators {
  /** Post-dominance relation calculation.
    *
    * Post-dominance defined for pair (G, S), where G is a graph and S is a node
    * in this graph. `A` post-dominates `B`, if every path from `B` to S contains `A`.
    *
    * @author conwor
    */
  def apply[N](graph: BiGraph[N], sink: N): PostDominators[N] = new PostDominators[N] {
    private val reverseDominators = graph.reverse(sink).dominators

    def iPostDom(node: N) = reverseDominators.idom(node)
    def postDoms(node: N) = reverseDominators.doms(node)
    def postDominates(x: N, y: N) = reverseDominators.dominates(x, y)
  }


  /** Post-dominance relation calculation for augmented graph (apdom), which can have several
    * sinks and endless loops. Rough definition of the relation: `A` apdom `B` only if every
    * path from `B` reaches `A` at some point.
    *
    * NOTE!
    * The opposite is not true. For example in endless loop of two nodes `A` and `B` every
    * path from `A` reaches `B` and every path from `B` reaches `A`, but only one pair of
    * nodes will be in apdom relation. Thus apdom relation is a false-negative approximation
    * of post-dominance (pdom) relation.
    *
    *   A apdom B => A pdom B
    *
    * For graph with one sink and no endless loops apdom relation is equal to pdom relation.
    *
    * To calculate this relation, we create special `stopNode`, with predecessors comprising of
    * all graph nodes without successors (several sinks) and all endless loop headers. apdom
    * calculated as pdom for pair (graph, stopNode).
    *
    * @author conwor
    */
  def augmented[N : ClassTag](graph: BiGraph[N]): PostDominators[N] = {
    // Reversed graph node type - union of N and { invalidNode }
    // TODO: use union types after transition to Scala 3
    type RN = Any

    // We use invalid node of original graph as stop node to make post-dominance look like dominance.
    // iDom(graph.entry) == graph.invalidNode, iPostDom(graph.exit) == graph.invalidNode
    val stopNode = graph.invalidNode

    val pd = {
      val graphWithStopNode: BiGraph[RN] = new BiGraph[RN] {
        private val stopNodePreds: collection.Set[N] = {
          val leaves = graph.collectReachableFrom(graph.start) filter { n => graph.succs(n).isEmpty }
          val endlessLoopsHeaders = graph.loops.iterator collect { case l if l.exits.isEmpty => l.header }
          leaves ++ endlessLoopsHeaders
        }

        override def start = graph.start

        override def succs(n: RN) = n match {
          case `stopNode` => Iterator.empty
          case n: N if stopNodePreds(n) => graph.succs(n) ++ Iterator.single(stopNode)
          case n: N => graph.succs(n)
        }

        override def preds(n: RN) = n match {
          case `stopNode` => stopNodePreds.iterator
          case n: N => graph.preds(n)
        }

        override val invalidNode = new Object
      }

      PostDominators(graphWithStopNode, stopNode)
    }

    new PostDominators[N] {
      def iPostDom(node: N) = pd.iPostDom(node).asInstanceOf[N]
      def postDoms(node: N) = pd.postDoms(node) collect { case x: N if x != `stopNode` => x }
      def postDominates(x: N, y: N) = pd.postDominates(x, y)
    }
  }
}
