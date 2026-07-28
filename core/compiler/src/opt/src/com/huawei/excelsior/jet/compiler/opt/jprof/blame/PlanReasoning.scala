/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.blame

import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.CallGraph.Edge
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.{CallGraph, Method}

import scala.collection.mutable

/** Provides facilities to record decisions made during inline planning.
  *
  * Public interface includes convenience methods
  * for obtaining and modifying individual edge/method reasons: [[apply]]s;
  * for bulk appending single reason for a number of edges/methods: [[forEdges]] and [[forRoots]];
  * for selecting and recording the first applicable reason out of a chain of reasons for given edge/method: [[forEdge]] and [[forRoot]].
  *
  * @author ijorch
  */
private[blame] class PlanReasoning private (private[blame] val enabled: Boolean) {
  import PlanReasoning._

  private type Reasons = mutable.Set[Reason]
  private val _edgeReasons = mutable.Map.empty[Edge, Reasons]
  private val _rootReasons = mutable.Map.empty[Method, Reasons]

  def clear(): Unit = {
    _edgeReasons.clear()
    _rootReasons.clear()
  }

  def isEmpty = _edgeReasons.isEmpty && _rootReasons.isEmpty

  def limitTo(graph: CallGraph, roots: collection.Set[Method]): PlanReasoning = if (!enabled) this else {
    val res = new PlanReasoning(enabled)
    val graphEdges = graph.edgeSet
    res._edgeReasons.addAll(this._edgeReasons.view.filterKeys(graphEdges))
    val graphNodes = graph.nodeSet
    res._rootReasons.addAll(this._rootReasons.view.filterKeys(graphNodes ++ roots))
    res
  }

  def ++(that: PlanReasoning): PlanReasoning = if (!enabled) this ensuring !that.enabled else {
    val res = new PlanReasoning(enabled) ensuring that.enabled

    def update[T](container: PlanReasoning => mutable.Map[T, Reasons]): Unit = {
      for ((e, newReasons) <- container(this).iterator ++ container(that).iterator) {
        container(res).updateWith(e) {
          case Some(reasons) => Some(reasons ++ newReasons)
          case None => Some(newReasons)
        }
      }
    }

    update(_._rootReasons)
    update(_._edgeReasons)
    res
  }

  private def reasons[T](container: mutable.Map[T, Reasons], e: T) = {
    if (enabled) {
      container.getOrElseUpdate(e, mutable.LinkedHashSet.empty)
    } else {
      mutable.Set.empty[Reason] // don't actually preserve any written objects
    }
  }

  def apply(e: Edge): Reasons = reasons(_edgeReasons, e)
  def apply(m: Method): Reasons = reasons(_rootReasons, m)

  def forEdges(edges: => Iterable[Edge])(reason: Reason): Unit = if (enabled) {
    for (edge <- edges) { apply(edge) += reason }
  }
  def forRoots(roots: => Iterable[Method])(reason: Reason): Unit = if (enabled) {
    for (root <- roots) { apply(root) += reason }
  }

  def forEdge(edge: Edge)(reasonChain: ReasonChain): Boolean = {
    findApplicable(reasonChain) { reason =>
      apply(edge) += reason
    }
  }
  def forRoot(root: Method)(reasonChain: ReasonChain): Boolean = {
    findApplicable(reasonChain) { reason =>
      apply(root) += reason
    }
  }
  private def findApplicable(reasonChain: ReasonChain)(record: Reason => Unit): Boolean = {
    val matching = reasonChain.chain.iterator.find(_.applies)
    if (enabled && matching.nonEmpty) {
      record(matching.get)
    }
    matching.nonEmpty
  }
}
private[blame] object PlanReasoning {
  def empty = new PlanReasoning(false)
  def apply(verbose: Boolean) = new PlanReasoning(verbose)

  final class ReasonChain private[PlanReasoning] {
    private[PlanReasoning] val chain = mutable.Buffer.empty[Reason]
    def || (next: Reason): ReasonChain = {
      chain += next
      this
    }
  }
  sealed abstract class Reason { self =>
    def name: String // used in `equals` and `hashCode` to treat applicable & explained reasons as equal to their builders
    def applies = true

    final def || (that: Reason) = new ReasonChain || this || that

    final override def hashCode = name.hashCode
    final override def equals(obj: Any) = obj match {
      case that: Reason => this.name == that.name
      case _ => false
    }
    override def toString = name
  }

  sealed class AutonamedReason extends Reason { self: Product =>
    override def name = self.productPrefix // see https://stackoverflow.com/a/13001046
  }
  case object ContractedSCCEdge         extends AutonamedReason
  case object ContractedSCCRoot         extends AutonamedReason
  case object IntegrallyHotInlinedEdge  extends AutonamedReason
  case object IntegrallyHotRoot         extends AutonamedReason
  case object OptimizedHotMethod        extends AutonamedReason
  case object OnlyImaginaryInEdges      extends AutonamedReason
  case object InlineRoot                extends AutonamedReason
  case object ConflictingHotMethod      extends AutonamedReason
  case object ClosestToInaccessibleRoot extends AutonamedReason
  case object ForcedEdge                extends AutonamedReason
  case object ForcedRoot                extends AutonamedReason
  case object SubgraphLocalHotEdge      extends AutonamedReason
  case object SubgraphLocalHotRoot      extends AutonamedReason
  case object SubgraphLocalHotCS        extends AutonamedReason
  case object StaticallyInlinedEdge     extends AutonamedReason

  sealed trait ApplicableReasonBuilder { self: AutonamedReason with Product =>
    def apply(_applies: => Boolean) = new Reason {
      override def name = self.name
      override def applies = _applies
    }
  }
  case object HotCallSite  extends AutonamedReason with ApplicableReasonBuilder
  case object LongTimeRoot extends AutonamedReason with ApplicableReasonBuilder

  object Explanation { def apply(_info: => Product)(_applies: => Boolean) = new Explanation(_info)(_applies) }
  final class Explanation private (_info: => Product)(_applies: => Boolean) {
    def applies = _applies
    def info = _info
  }
  sealed trait ExplainedReasonBuilder { self: AutonamedReason with Product =>
    def apply(e: Explanation) = new Reason {
      override def name = self.name
      override def applies = e.applies
      override def toString = name + e.info.productIterator.mkString("(", " ", ")")
    }
  }
  case object HotEdge          extends AutonamedReason with ExplainedReasonBuilder
  case object TinyTarget       extends AutonamedReason with ExplainedReasonBuilder
  case object FastEdge         extends AutonamedReason with ExplainedReasonBuilder
  case object ReachableFromHot extends AutonamedReason with ExplainedReasonBuilder
  case object HeavyRoot        extends AutonamedReason with ExplainedReasonBuilder
}
