/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.sync

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Tag, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.util.graph.ObjectBiGraph
import com.huawei.excelsior.jet.util.graph.analysis.DataFlowAnalysis
import com.huawei.excelsior.jet.util.{Closure, DisjointSet, ScalaCollections}

/** Various optimizations of synchronization regions.
  *
  * @author liontiger
  */
trait SynchronizationOptimization { self: Universe =>

  /** Slices synchronized regions into disjoint sub-regions, which do not intersect via control-flow.
    *
    * Such regions may be produced during optimizations, specifically [[XiTransform]] ones,
    * such as loop unrolling or simply copying part of the CFG with synchronized regions inside.
    *
    * For example the following region `(E1, X1, E2, X2, E3, X3)`,
    * where Ei and Xi mark monitor enters and exits respectively:
    * {{{
    *          |
    *          E1
    *          |
    *          X1
    *        /   \
    *       E2    E3
    *       |     |
    *       X2    X3
    *       |     |
    * }}}
    * will be sliced into 3 sub-regions: `(E1, X1)`, `(E2, X2)` and `(E3, X3)`.
    *
    * Note that nested regions must also be sliced if their outer region is being sliced.
    */
  def sliceSynchronizedRegions(): Boolean = {

    if (isUnstructuredLocking || areVarsPresent || currentPhase >= CompilerPhase.Lowering) {
      return false
    }

    checkConsistency(CheckLevels.Optional) {
      checkSynchronizationConsistency()
    }

    // fast path
    if (all[SynchronizedRegion] forall (_.enters.size <= 1)) {
      return false
    }

    val analysis = new MonitorAnalysis

    // Separate all monitors into disjoint sub-regions based on calculated equivalence relation.
    val subRegions = DisjointSet.from(all[MonitorOperation])(analysis.matching _)

    // Mapping from region to disjoint sub-regions.
    // Each sub-region is identified by monitor operation (enter or exit, does not matter).
    val groupedSubRegions = ScalaCollections.groupBy(subRegions.equivClasses)(_.syncRegion)

    // All regions to be sliced including nested ones.
    val slicedRegions = Closure(groupedSubRegions.keysIterator filter (groupedSubRegions(_).size > 1))(_.inners)

    for {
      r <- slicedRegions.toArray.sortBy(_.depth)
      m <- groupedSubRegions.getOrElse(r, Seq.empty)
    } {
      // Note that we can't use `r.outer` here, because the outer region is already sliced,
      // so we find the deepest entered region, which is the actual outer region.
      val outerRegion = analysis.enteredRegions(m)
        .maxByOption(_.depth)
        .getOrElse(SynchronizedRegion.noRegion())

      val subRegion = SynchronizedRegion(outerRegion)
      subRegions.equivElements(m) foreach (_.syncRegion = subRegion)
    }

    assert(slicedRegions forall { r => r.enters.isEmpty && r.exits.isEmpty })
    slicedRegions foreach decommit

    slicedRegions.nonEmpty
  }

  def checkSynchronizationConsistency(): Unit = {
    if (isUnstructuredLocking || areVarsPresent || currentPhase >= CompilerPhase.Lowering || all[MonitorOperation].isEmpty) {
      return
    }

    new MonitorAnalysis
  }

  /** Collects set of acquired monitor enters (not released by accompanying monitor exit) for each control node in IR.
    *
    * Performs data-flow analysis in which each monitor enter adds itself to the input state and each monitor exit
    * removes all enters in the same region from the state.
    * This way set of acquired monitor enters can always be viewed as a stack of outer region enters.
    *
    * Analysis assumes structured locking and fails if that assumption is wrong,
    * so can be used as a structured locking consistency check.
    */
  private class MonitorAnalysis extends DataFlowAnalysis[ControlNode](new MonitorAnalysis.Graph) {
    import MonitorAnalysis.State._

    type State = MonitorAnalysis.State

    protected def init = Uninitialized

    protected def join(outputStates: IterableOnce[State]) = {
      val states = outputStates.iterator.toSeq
      // All initialized states should have the same set of entered regions.
      assert(ScalaCollections.haveSame(states filter (_.isInstanceOf[Initialized]))(_.regions))
      Initialized(states.iterator map (_.monitors) reduce (_ | _))
    }

    protected def trans(n: ControlNode, inputState: State) = {
      val monitors = inputState.monitors
      Initialized(n match {
        case n: MonitorEnter =>
          assert(inputState.regions subsetOf n.syncRegion.allOuters.toSet)
          monitors + n

        case n: MonitorExit =>
          assert(inputState.regions contains n.syncRegion)
          assert((inputState.regions - n.syncRegion) subsetOf n.syncRegion.allOuters.toSet)
          monitors filterNot (_.syncRegion == n.syncRegion)

        case XPoint.WithoutHandler() | _: Return =>
          assert(monitors.isEmpty)
          monitors

        case _ => monitors
      })
    }

    def enteredRegions(x: MonitorOperation) = x match {
      case x: MonitorEnter => in(x).regions
      case x: MonitorExit => out(x).regions
    }

    def matching(x: MonitorOperation, y: MonitorOperation) = {
      def matchingImpl(enter: MonitorEnter, exit: MonitorExit) =
        enter.syncRegion == exit.syncRegion && (in(exit).monitors contains enter)

      (x, y) match {
        case (enter: MonitorEnter, exit: MonitorExit) => matchingImpl(enter, exit)
        case (exit: MonitorExit, enter: MonitorEnter) => matchingImpl(enter, exit)
        case _ => false
      }
    }
  }

  private object MonitorAnalysis {

    sealed abstract class State {
      def monitors: Set[MonitorEnter]
      def regions: Set[SynchronizedRegion] = monitors map (_.syncRegion)
    }
    object State {
      case object Uninitialized extends State {
        def monitors = Set.empty
      }
      case class Initialized(monitors: Set[MonitorEnter]) extends State
    }

    /** Skeleton graph (CFG with spinal nodes and projections) with exception edges of [[MonitorEnter]] nodes adjusted:
      *   - Each edge `(enter, enter.xpoint)` is replaced by edge `(enter.inCtrl, enter.xpoint)`.
      *
      * This adjustment simplifies [[MonitorAnalysis]] by explicitly showing that data flow state reaches `enter.xpoint`
      * before and not after the `enter` itself is processed.
      *
      * Example:
      * {{{
      *     Before adjustment:   |   After adjustment:
      *                          |
      *      prev                |    prev
      *       |__________        |     |___________
      *       |          |       |     |      |    |
      *      enter    xpoint1    |    enter   |  xpoint1
      *       |_______           |     |      |
      *       |       |          |     |      |
      *      next   xpoint2      |    next  xpoint2
      * }}}
      *
      * Note that this graph may contain unreachable code.
      */
    final class Graph extends ObjectBiGraph[ControlNode] {
      val start = entryBlock

      def succs(n: ControlNode): Iterator[ControlNode] = {
        val filteredSuccs = n match {
          case n: MonitorEnter => Iterator.single(n.outCtrl) // ignore XPoints of monitor enters
          case _ => n.outEdges collect {
            case TaggedEdge(Tag.CONTROL | Tag.XCONTROL, _, use: ControlNode) => use
          }
        }
        filteredSuccs flatMap {
          case x: MonitorEnter => Iterator(x, x.xpoint) // adjust XPoints of monitor enters
          case x => Iterator.single(x)
        }
      }

      def preds(n: ControlNode): Iterator[ControlNode] = n match {
        case `start` => Iterator.empty
        case XPoint(enter: MonitorEnter) => Iterator.single(enter.inCtrl) // adjust XPoints of monitor enters
        case _ => n.inEdges collect {
          case TaggedEdge(Tag.CONTROL | Tag.XCONTROL, arg: ControlNode, _) => arg
        }
      }
    }
  }

}
