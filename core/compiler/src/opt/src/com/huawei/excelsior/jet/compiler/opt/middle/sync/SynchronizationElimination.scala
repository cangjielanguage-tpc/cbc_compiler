/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.sync

import com.huawei.excelsior.jet.compiler.StatsKind.SyncElimination
import com.huawei.excelsior.jet.compiler.ir.{CallEscapeKind, EscapeKind, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.escape.EscapeAnalysis
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt
import scala.annotation.tailrec

trait SynchronizationElimination extends EscapeAnalysis { self: Universe =>

  private def eliminate(n: MonitorOperation, reason: String): Unit = {
    stats.count(SyncElimination, s"removed ${n.name} ($reason)", n)
    strikeOut(n)
  }

  private def eliminateSynchronization(reason: String)(isCandidate: SynchronizedRegion => Boolean): Boolean = {
    val syncRegions = (all[SynchronizedRegion] filter isCandidate).toList

    for (syncRegion <- syncRegions) {
      syncRegion.enters.toList foreach { eliminate(_, reason) }
      syncRegion.exits.toList foreach { eliminate(_, reason) }
      syncRegion.inners.toList foreach { inner => inner.outerRaw = syncRegion.outerRaw }
      decommit(syncRegion)
    }

    syncRegions.nonEmpty
  }

  def eliminateNestedSynchronization(): Boolean = {
    if (isUnstructuredLocking) {
      return false
    }

    eliminateSynchronization("nested") { inner =>
      inner.allOuters exists (outer => outer.singleMonitorObj.isDefined && outer.singleMonitorObj == inner.singleMonitorObj)
    }
  }

  def eliminateSynchronizationOnNew(): Boolean = {
    if ((currentPhase <= CompilerPhase.InterProceduralAnalysis) || isUnstructuredLocking) {
      // escape analysis is not ready
      return false
    }

    eliminateSynchronization("on new") { s =>
      s.singleMonitorObj match {
        case Some(anyNew: AnyNew) => mayRemoveSynchronizationOn(anyNew)
        case _ => false
      }
    }
  }

  def mayRemoveSynchronizationOn(newOp: AnyNew): Boolean = {
    if (isUnstructuredLocking) {
      return false
    }

    assert(currentPhase > CompilerPhase.InterProceduralAnalysis)

    if (implicitlyEscapedType(newOp.allocType.symType)) {
      // don't mess with these types
      false

    } else {
      !escapeKindOfNew(newOp).containsEscape
    }
  }


  def mergeSynchronizedBlocks(): Boolean = {
    // Merge pairs of MonitorExit and MonitorEnter such that
    // exit dominates enter, enter post-dominates enter
    // and there is no side effects between them, no exceptions and no memory nodes.
    // Controlled memory nodes (e.g. GetField) are OK.
    // (Heuristics are tuned for SPECjbb.)

    if (!env.enabled(BoolOption.SyncCoarsening) || isUnstructuredLocking || currentPhase >= CompilerPhase.Lowering) {
      return false
    }

    @tailrec
    def skipFree(n: ControlNode): ControlNode = n match {
      case n: BBlock => skipFree(ScalaCollections.singleton(n.inputs).orNull)
      case n: Goto => skipFree(n.inCtrl)
      case n: SpinalNode if SpinalNode.sideEffectFree(n) => skipFree(n.inCtrl)
      case _ => n
    }

    var changed = false
    for (secondRegion <- all[SynchronizedRegion]) {
      // Match the case when every enter of second region is preceded by one of exits of another region (first region).

      ScalaCollections.sequence((secondRegion.enters map { secondEnter =>
        condOpt(skipFree(secondEnter.inCtrl)) {
          case firstExit: MonitorExit if firstExit.obj == secondEnter.obj => firstExit
        }
      }).toSeq) match {
        case Some(firstExits) =>
          ScalaCollections.uniqueValue(firstExits map (_.syncRegion)) match {
            case Some(firstRegion) =>
              val reason = "coarsening"
              secondRegion.enters.toList foreach (eliminate(_, reason))
              firstExits foreach (eliminate(_, reason))

              secondRegion.replaceBy(firstRegion)

              changed = true

            case _ =>
          }
        case _ =>
      }
    }
    changed
  }
}
