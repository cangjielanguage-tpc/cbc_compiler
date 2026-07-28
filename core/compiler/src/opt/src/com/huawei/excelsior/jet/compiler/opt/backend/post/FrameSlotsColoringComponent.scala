/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.jet.assembler.Location.FReg
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.{FrameSlotsRecoloringMaxSPLimit, FrameSlotsRecoloringMaxSPLimitForPGOHosts}

import scala.collection.mutable.ArrayBuffer

/**
  * Recoloring of frame slots. Tries to assign frame slots to free FRegLoc
  * and merge not intersected frame slots.
  *
  * @author conwor
  */
trait FrameSlotsColoringComponent extends InterferenceGraphComponent with LiveRangesComponent { self: Universe with BackEnd =>

  /** Returns true iff given `slot` is regular spill slot means we can merge it with anyone else or sometimes move to FReg. */
  protected def isSpillSlot(slot: FrameSlot): Boolean =
    !slot.kind.isInstanceOf[FrameSlot.Param]

  private lazy val gcMapPoints = (allNodes filter {
    node => node.isGroupRoot && needGCMap(node)
  }).to(ArrayBuffer)

  /** Returns true iff given `vertex` may be merged with some FReg. */
  private def isMergeableWithFRegs(vertex: RangeVertex): Boolean = {
    // Actually it is recolorable, but float spill may occur only if there are no free FRegs in it's live range
    if (vertex.range.isFP) return false

    if (vertex.range.mayBeTraceableRef) {
      if (gcMapPoints exists { p => vertex.range contains p }) {
        return false
      }
    }

    true
  }

  def recolorFrameSlots(): Boolean = {
    if (env.enabled(DisableFrameSlotsRecoloring)) return false

    val limit = if (profile.isPGOHost) FrameSlotsRecoloringMaxSPLimitForPGOHosts else FrameSlotsRecoloringMaxSPLimit
    if (isO1Compiled) {
      stats.count(StatsKind.FrameSlotsColoring, "could not check max spill pressure limit because BGCM regalloc hints are disabled") // TODO-FAST-BE: do we need spill pressure check for FastBE?
      return false
    }
    if (bGCMHints.maxSpillPressure > env.valueOf(limit)) {
      stats.count(StatsKind.FrameSlotsColoring, "max spill pressure limit exceeded")
      return false
    }

    val spills = all[Transfer] collect {
      case t @ Transfer(_ ~~> (slot: FrameSlot)) if isSpillSlot(slot) => t
    }

    if (spills.isEmpty) {
      stats.count(StatsKind.FrameSlotsColoring, "no spill frame slots")
      return false
    }


    var changed = false

    def recolor(range: LiveRange, resource: Resource, msg: String): Unit = {
      range.values foreach {
        changed = true
        _.resource = resource
      }
      stats.count(StatsKind.FrameSlotsColoring, msg)
    }

    def merge(ranges: Iterable[LiveRange], msg: String): Unit = {
      if ((ranges.size < 2) || (ranges.map(_.resource).toSet.size < 2)) {
        // There is no reason to recolor (and mark changed) groups of ranges already located on the same slot
        return
      }

      // We cannot reuse any of slots from `ranges` because they may be used in other ranges of the same value (effect
      // appears only after we start use spill slots cache in [[FrameComponent]]). Consider the following example:
      //
      // values:                      V0    V1    V2
      // spill slots from cache       S0    S1    S2
      // ranges in CFG point P0       R00
      // ranges in CFG point P1       R01   R11
      // ranges in CFG point P2             R12   R22
      //
      // Before frame slots recoloring there are 3 spill slots used to store 3 different values. But recolorer may
      // reduce their number, using one slot for R00, R11, R12 and another for R01, R22. If recolorer will use slots
      // already allocated to nodes in ranges, it may use S0 for both of groups. To avoid this bug we just create new
      // spill slot for each recolored group.
      val slot = newFrameSlot(FrameSlot.Raw(stackSlotSize, stackSlotSize))

      for (r <- ranges) {
        recolor(r, slot, msg)
      }
    }

    // NOTE: if we will implement callback for node resource allocation and use it in LiveRanges.enableFor
    //  we should refactor this code (separate analysis and resources reallocation).
    LiveRanges.enableFor {

      stats.count(StatsKind.FrameSlotsColoring, "try to build IG")
      val ig = new InterferenceGraph(spills)

      val fRegs = if (env.enabled(DisableSlotsRecoloringToFRegs)) {
        Array.empty[FReg]
      } else if (env.enabled(UseAllFRegsForFrameSlotsRecoloringInPGOHosts) && profile.isPGOHost) {
        // We use all FRegs (volatile and non-volatile) for FS recoloring in PGO hosts.
        frame.abi.availableFRegs
      } else {
        frame.abi.volatileFRegs
      }

      if (ig.unmerged.size <= 2) {
        ig.unmerged.toSeq match {
          case Seq(range) if isMergeableWithFRegs(range) =>
            val co = FastPaths.oneRange(range, fRegs)
            for (c <- co) recolor(range.range, c, "fast path 1")

          case rs @ Seq(r1, r2) =>
            val noOneColored = rs filter isMergeableWithFRegs match {
              case Seq(r) =>
                val co = FastPaths.oneRange(r, fRegs)
                for (c <- co) recolor(r.range, c, "fast path 1")
                co.isEmpty

              case Seq(_, _) =>
                val (c1o, c2o) = FastPaths.twoRanges(ig, r1, r2, fRegs)
                for (c1 <- c1o) recolor(r1.range, c1, "fast path 2/1")
                for (c2 <- c2o) recolor(r2.range, c2, "fast path 2/2")
                c1o.isEmpty && c2o.isEmpty

              case _ => true // nothing can do
            }

            if (noOneColored && !ig.adjacent(r1, r2)) {
              merge(Seq(r1.range, r2.range), "fast path 2/change slot")
            }

          case _ => // nothing can do
        }

      } else {
        ig.appendResources(fRegs, isMergeableWithFRegs)

        if (!env.enabled(BuildIGOnly)) {
          stats.count(StatsKind.FrameSlotsColoring, "non-empty IG, range number = " + ig.unmerged.size)

          ig.simplify()

          val buf = ArrayBuffer.empty[LiveRange]

          for (x <- ig.completeSubGraph) ig.delegate(x) match {
            case ResourceVertex(fReg) =>
              for (RangeVertex(range) <- ig.group(x)) recolor(range, fReg, "slow path (move to fReg)")
            case RangeVertex(_) =>
              buf.clear()
              buf ++= ig.group(x) map { case RangeVertex(r) => r }
              merge(buf, "slow path (merge slot)")
          }
        }
      }
    }

    changed
  }
}
