/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.mutable

/** Directed graph with dedicated start node.
  *
  * Can be traversed from to node to its successors.
  *
  * @author cypok
  * @author paul
  * @author conwor
  */
abstract class Graph[N] {

  /** Dedicated start node. */
  def start: N

  /** Successors of node `n`. */
  def succs(n: N): Iterator[N]

  /** Returns invalid node for this graph. */
  def invalidNode: N

  /**
   * Returns true, if this and `that` graph are topologically equal. It means, that
   * they have corresponding nodes, connected with corresponding edges.
   *
   * @param that another graph
   * @tparam M type of `that` graph nodes
   * @return true, if this and `that` graphs are topologically equal.
   */
  def topologicallyEquals[M](that: Graph[M]): Boolean = {
    val thisToThat = mutable.HashMap.empty[N, M]
    val thatToThis = mutable.HashMap.empty[M, N]

    def processOne(x: N, y: M): Boolean = {
      if (x == this.invalidNode || y == that.invalidNode)
        return false

      for (b <- thisToThat.get(x))
        return b == y
      for (a <- thatToThis.get(y))
        return a == x

      thisToThat(x) = y
      thatToThis(y) = x

      val pairs = succs(x) zipAll (that.succs(y), this.invalidNode, that.invalidNode)
      pairs forall { case (a, b) => processOne(a, b) }
    }

    processOne(start, that.start)
  }

  /** Returns stable-ordered set of graph nodes reachable by succs from `x`. */
  def collectReachableFrom(x: N): collection.Set[N] = Closure(x)(succs)

  def topSort: TopSort[N] = topSort(Iterator.single(start))

  def topSort(startNodes: Iterator[N]): TopSort[N] = new TopSort[N](this, startNodes)
}

/** Bidirected graph with dedicated start node.
  *
  * Can be traversed from to node to its successors
  * or from node to its predecessors.
  *
  * @author cypok
  */
trait BiGraph[N] extends Graph[N] { self =>
  /** Predecessors of node `n`. */
  def preds(n: N): Iterator[N]

  /** Returns graph with reversed edges. Sink will be start node of reversed graph. As original graph may have
    * several sinks and endless loops, nodes of reversed graph can be subset of nodes of original graph.
    */
  def reverse(sink: N): BiGraph[N] = {
    new BiGraph[N] {
      override def start = sink
      override def succs(n: N) = self.preds(n)
      override def preds(n: N) = self.succs(n)
      override def invalidNode = self.invalidNode
    }
  }

  /** Returns graph with filtered nodes from `this` graph. */
  def filter(f: N => Boolean): BiGraph[N] = {
    new BiGraph[N] {
      override def start = self.start ensuring f
      override def succs(n: N) = self.succs(n) filter f
      override def preds(n: N) = self.preds(n) filter f
      override def invalidNode = self.invalidNode
    }
  }

  def filterNot(f: N => Boolean) = filter(!f(_))

  /** Returns subgraph of `this` graph with `x` start node */
  def withStart(x: N): BiGraph[N] = {
    new BiGraph[N] {
      override def start = x
      override def succs(n: N) = self.succs(n)
      override def preds(n: N) = self.preds(n)
      override def invalidNode = self.invalidNode
    }
  }

  private def hasBackwardEdgesByTopSort(ts: TopSort[N]) = {
    ts.order exists (p => succs(p) exists (s => ts.lteq(s, p)))
  }

  def hasBackwardEdges: Boolean = hasBackwardEdgesByTopSort(topSort)
  def hasBackwardEdges(startNodes: Iterator[N]): Boolean = hasBackwardEdgesByTopSort(topSort(startNodes))

  def loops: Loops[N] = Loops(this)
  def loops(startNodes: Iterator[N]): Loops[N] = Loops(this, startNodes)

  /** Returns dominators on this graph. */
  def dominators: Dominators[N] = new Dominators(this)
}

object BiGraph {

  /** Returns all nodes on every path from entry to each anchor.
    * Entry must dominate every anchor.
    */
  def anchorSubGraph[N](graph: BiGraph[N], entry: N, anchors: N*): collection.Set[N] = {
    val body = mutable.LinkedHashSet.empty[N]
    body += entry
    Closure.collect(body, anchors)(graph.preds)
    assert(entry == graph.start || !body.contains(graph.start)) // naive dominance check, better than nothing
    body
  }
}

trait ObjectGraph[N >: Null] extends Graph[N] {
  override def invalidNode: N = null
}

trait ObjectBiGraph[N >: Null] extends BiGraph[N] with ObjectGraph[N]
