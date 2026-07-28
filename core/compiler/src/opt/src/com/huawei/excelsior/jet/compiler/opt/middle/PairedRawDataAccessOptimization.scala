/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.util.ScalaCollections.zipMap
import com.huawei.excelsior.jet.util.{Numbering, ScalaCollections}
import com.huawei.excelsior.jet.util.graph.ObjectBiGraph
import com.huawei.excelsior.jet.util.graph.analysis.DataFlowAnalysis

import scala.PartialFunction.fromFunction

/** Basic optimization of paired acquire/release nodes.
  *
  * Their parity is checked using data-flow analysis.
  *
  * Optimization replaces paired acquire/release with begin/end local unmovable, thus avoiding expensive call.
  *
  * Note: In general case we can't reliably prove parity of acquire/release,
  *       because acquired address may escape into any call or put into any field, and later released somewhere else.
  *       So we try to somewhat guarantee correctness by analyzing all acquires and releases in the method at once,
  *       or not optimizing at all.
  *       However, this analysis covers only local parity cases, and may incorrectly optimize inter-procedural cases.
  */
trait PairedRawDataAccessOptimization { this: Universe =>

  def optimizePairedRawDataAccesses(): Boolean = {
    /** Preliminary fast-path check for acquire nodes which are directly used by release nodes. */
    object Paired {
      def unapply(a: AcquireRawData): Option[Seq[ReleaseRawData]] = {
        val releases = a.outEdges.collect{ case ReleaseRawData.Pointer(r) => r }.toSeq
        if (releases.nonEmpty && releases.forall(_.array == a.array)) {
          Some(releases)
        } else {
          None
        }
      }
    }

    // Workaround for JET-17168
    // TODO: fix properly and enable this optimisation for CBC
    if (targetArch == CBC) {
      return false
    }

    val acquires = all[AcquireRawData].toSeq

    if (acquires.isEmpty) {
      return false
    }

    val allReleasesHaveAcquire = all[ReleaseRawData] forall (_.pointer.isInstanceOf[AcquireRawData])
    if (!allReleasesHaveAcquire) {
      return false
    }

    val allAcquiresHaveReleases = acquires forall {
      case Paired(_) => true
      case _ => false
    }
    if (!allAcquiresHaveReleases) {
      return false
    }

    val analysis = new RawDataAnalysis(Numbering(acquires))

    if (all[ControlNode] map analysis.out exists (_.erroneous)) {
      return false
    }

    for (acquire @ Paired(releases) <- acquires) {
      val (ptr, begin) = insertCodeBefore(acquire) {
        (
          Add(ConcealRef(acquire.array), IntegralConst(AddrType)(RTConst.CangjieArray.BODY_OFFS.intValue)),
          BeginLocalUnmovable(acquire.array)
        )
      }
      for (release <- releases) {
        assert(release.pointer == acquire)
        assert(release.array == acquire.array)
        insertCodeAfter(release) { EndLocalUnmovable(begin) }
        strikeOut(release)
      }
      strikeOutWithValueUses(acquire, ptr)
    }

    true
  }

  private final class RawDataAnalysis(keys: Numbering[Node]) extends DataFlowAnalysis[ControlNode](new Graph) {
    import SingleState.*

    /** Meet-semilattice for a particular raw data.
      *
      * Top = Uninitialized
      *           /     \
      *    Acquired     Released
      *           \     /
      * Bottom = Erroneous
      */
    sealed abstract class SingleState
    object SingleState {
      case object Uninitialized extends SingleState
      case object Acquired extends SingleState
      case object Released extends SingleState
      case object Erroneous extends SingleState

      def meet(x: SingleState, y: SingleState) = (x, y) match {
        // Idempotency:
        case _ if x == y => x

        // Top element:
        case (Uninitialized, y) => y
        case (x, Uninitialized) => x

        // Bottom element:
        case _ => Erroneous
      }
    }

    /** Extrapolation of [[SingleState]] onto multiple raw data elements. */
    class State(private val rawStates: Array[SingleState]) {

      def erroneous = rawStates.contains(SingleState.Erroneous)

      def meet(that: State): State =
        updateImpl(zipMap(this.rawStates, that.rawStates)(SingleState.meet).toArray)

      def update(n: Node)(f: PartialFunction[SingleState, SingleState]): State = {
        if (keys contains n) {
          val idx = keys.number(n)
          val prev = rawStates(idx)
          f.lift(prev) match {
            case Some(next) => new State(rawStates.updated(idx, next))
            case None => this
          }
        } else {
          this
        }
      }

      def updateAll(f: PartialFunction[SingleState, SingleState]): State =
        updateImpl(rawStates.map(f orElse fromFunction(Predef.identity)))

      private def updateImpl(updated: Array[SingleState]): State =
        if (updated sameElements rawStates) this else new State(updated)

      override def equals(that: Any) = that match {
        case that: State => this.rawStates sameElements that.rawStates
        case _ => false
      }

      override def hashCode() = xscala.util.hashCode(rawStates)
    }

    object State {
      val uninitialized = new State(Array.fill(keys.order.size)(Uninitialized))
    }

    protected def init = State.uninitialized

    protected def join(outputStates: IterableOnce[State]): State =
      outputStates.iterator reduce (_ meet _)

    protected def trans(n: ControlNode, inputState: State) = {
      n match {
        case n: AcquireRawData => inputState.update(n) {
          case Uninitialized | Released => Acquired
          case Acquired => Erroneous
        }

        case n: ReleaseRawData => inputState.update(n.pointer) {
          case Acquired => Released
          case Uninitialized | Released => Erroneous
        }

        case XPoint.WithoutHandler() | _: Return => inputState updateAll {
          case Acquired => Erroneous
        }

        case _ => inputState
      }
    }

  }

  /** Graph of all control nodes. */
  private final class Graph extends ObjectBiGraph[ControlNode] {
    val start = entryBlock

    def succs(n: ControlNode): Iterator[ControlNode] = n.outEdges collect {
      case TaggedEdge(Tag.CONTROL | Tag.XCONTROL, _, use: ControlNode) => use
    }

    def preds(n: ControlNode): Iterator[ControlNode] = n match {
      case `start` => Iterator.empty
      case _ => n.inEdges collect {
        case TaggedEdge(Tag.CONTROL | Tag.XCONTROL, arg: ControlNode, _) => arg
      }
    }
  }
}
