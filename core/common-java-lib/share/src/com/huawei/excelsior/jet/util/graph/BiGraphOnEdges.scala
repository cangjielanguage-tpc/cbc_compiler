/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.graph.BiGraphOnEdges.Edge

import scala.collection.mutable

/**
  * Immutable bidirectional multi-graph built by edges of type `E` between nodes of type `N`.
  * Edges might be ordered with optional `edgeOrd`.
  *
  * @author ijorch
  */
abstract class BiGraphOnEdges[N, E <: Edge[N]] protected (es: IterableOnce[E], edgeOrd: Option[Ordering[E]]) extends BiGraph[N] {

  private def newEdgeSet = mutable.LinkedHashSet.empty[E]

  private def ordered(set: mutable.LinkedHashSet[E]): mutable.LinkedHashSet[E] =
    newEdgeSet ++ (set.toSeq sorted edgeOrd.get)

  private var _edgeSet = newEdgeSet
  private val _nodeSet = mutable.LinkedHashSet.empty[N]
  private val _rootSet = mutable.LinkedHashSet.empty[N]
  private val nodeIns  = mutable.LinkedHashMap.empty[N, mutable.LinkedHashSet[E]]
  private val nodeOuts = mutable.LinkedHashMap.empty[N, mutable.LinkedHashSet[E]]

  protected def addEdge(edge: E): Unit = {
    def registerNode(n: N, isDst: Boolean) = if (_nodeSet(n)) {
      if (isDst) _rootSet -= n
      n
    } else {
      _nodeSet += n
      if (!isDst) _rootSet += n
      nodeIns(n) = newEdgeSet
      nodeOuts(n) = newEdgeSet
      n
    }

    _edgeSet += edge
    nodeIns(registerNode(edge.dst, true)) += edge
    nodeOuts(registerNode(edge.src, false)) += edge
  }

  es.iterator foreach addEdge
  if (edgeOrd.isDefined) {
    _edgeSet = ordered(_edgeSet)
    nodeIns.mapValuesInPlace((_, s) => ordered(s))
    nodeOuts.mapValuesInPlace((_, s) => ordered(s))
  }

  def isEmpty = _edgeSet.isEmpty

  def edgeSet: collection.Set[E] = _edgeSet
  def nodeSet: collection.Set[N] = _nodeSet
  def rootSet: collection.Set[N] = _rootSet

  def edges = _edgeSet.iterator
  def nodes = _nodeSet.iterator
  def roots = _rootSet.iterator
  def leaves = nodes filter (nodeOuts(_).isEmpty)

  def inEdges(n: N) = nodeIns.get(n) map (_.iterator) getOrElse Iterator.empty
  def outEdges(n: N) = nodeOuts.get(n) map (_.iterator) getOrElse Iterator.empty

  def contains(e: E) = _edgeSet(e)
  def contains(n: N) = _nodeSet(n)
  def isRoot(n: N) = _rootSet(n)
  
  def unreachable: collection.Set[N] = _nodeSet diff Closure(roots)(succs) diff Closure(leaves)(preds)

  override def succs(n: N) = outEdges(n) map (_.dst)
  override def preds(n: N) = inEdges(n) map (_.src)

  override def start = shouldNotCallThis("no single start")

  def edgesCount = _edgeSet.size
}

object BiGraphOnEdges {

  /** Edge goes from `src` node to `dst` node. */
  trait Edge[N] {
    def src: N
    def dst: N
  }
}
