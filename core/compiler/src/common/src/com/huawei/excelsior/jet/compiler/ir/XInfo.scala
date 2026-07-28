/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

import com.huawei.excelsior.jet.assembler.{Label, Segment}
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.ir.XInfo._
import com.huawei.excelsior.jet.compiler.ir.XSite.GCDeltaMap
import com.huawei.excelsior.jet.compiler.symlevel.MethodReference

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.scalaUtilChainingOps

/** Incrementally built collection of xsites & gcmaps.
  *
  * @author paul
  */
class XInfo {

  private var xsites: ArrayBuffer[XSite] = _
  private var siteLabels: ArrayBuffer[Label] = _
  private var handlerLabels: ArrayBuffer[Label] = _
  private var prepared = false
  private var _isDirtyForClassGC = false

  def markAsDirtyForClassGC(): Unit = {
    _isDirtyForClassGC = true
  }

  def isDirtyForClassGC = _isDirtyForClassGC

  def addXInfo(xinfo: XInfo): Unit = {
    assert(!prepared && !xinfo.prepared)
    if (xinfo.xsites != null) {
      rebaseGCMaps(xinfo)
      for (i <- xinfo.xsites.indices) {
        addXSite0(xinfo.xsites(i), xinfo.siteLabels(i), xinfo.handlerLabels(i))
      }
    }
  }

  private def addXSite0(xsite: XSite, siteLabel: Label, handlerLabel: Label): Unit = {
    assert(!prepared)
    if (xsites == null) {
      xsites = ArrayBuffer.empty[XSite]
      siteLabels = ArrayBuffer.empty[Label]
      handlerLabels = ArrayBuffer.empty[Label]
    }
    xsites += xsite
    siteLabels += siteLabel
    handlerLabels += handlerLabel
  }

  def addXSite(site: Label, handler: Label, kind: XSiteKind, accessOffset: Int,
               bytecodePos: Int, lineNumber: Int, inlineContext: InlineContext,
               calledMethodRef: MethodReference, softExceptionID: Int, domain: Domain): Unit = {
    val xsite = new XSite(kind, accessOffset, bytecodePos, lineNumber, inlineContext, calledMethodRef, getDeltaMap, softExceptionID, domain)
    addXSite0(xsite, site, handler)
  }

  def addXSite(siteOffset: Int, handlerOffset: Int, kind: XSiteKind, accessOffset: Int,
               bytecodePos: Int, lineNumber: Int, inlineContext: InlineContext,
               calledMethodRef: MethodReference, softExceptionID: Int, domain: Domain): Unit = {
    val xsite = new XSite(kind, accessOffset, bytecodePos, lineNumber, inlineContext, calledMethodRef, getDeltaMap, softExceptionID, domain)
    xsite.siteOffset = siteOffset
    xsite.handlerOffset = handlerOffset
    addXSite0(xsite, null, null)
  }

  private var allSlots: mutable.LinkedHashMap[Slot, LiveSlot] = _
  private var lastMapID = -1

  private var deltaList: ArrayBuffer[Slot] = _
  private var unmovableList: ArrayBuffer[Slot] = _
  private var deltaSlotsStart = NOT_STARTED

  private var liveRegisters = 0

  private var registersMask = NO_REG_MASK

  private var unmovableRegisters = NO_REG_MASK

  private def mapStarted = deltaSlotsStart != NOT_STARTED

  private def deltaListSize = if (deltaList == null) 0 else deltaList.size

  private def lazyCreateSlotsInfo(): Unit = {
    assert((allSlots == null) == (deltaList == null))
    if (allSlots == null) {
      allSlots = mutable.LinkedHashMap.empty[Slot, LiveSlot]
      deltaList = ArrayBuffer.empty[Slot]
    }
  }

  private def lazyCreateUnmovableSlotsInfo(): Unit = {
    assert(allSlots != null)
    if (unmovableList == null) {
      unmovableList = ArrayBuffer.empty[Slot]
    }
  }

  def startGCMap(): Unit = {
    assert(!mapStarted)
    deltaSlotsStart = deltaListSize
  }

  def addTracedSlot(_slot: Slot): Unit = {
    assert(mapStarted)
    lazyCreateSlotsInfo()

    var slot = _slot
    var ls = allSlots.get(slot).orNull
    if (ls != null) {
      slot = ls.slot // avoid storing duplicate equal slots in gcmaps
    } else {
      ls = new LiveSlot(slot, lastMapID + 1)
      allSlots.put(slot, ls)
    }

    if (ls.gcmapID != lastMapID) {
      deltaList += slot
    }
    ls.gcmapID = lastMapID + 1
  }

  def addUnmovableSlot(slot: Slot): Unit = {
    val ls = allSlots(slot)
    lazyCreateUnmovableSlotsInfo()
    unmovableList += ls.slot // avoid storing duplicate equal slots
  }

  def setRegistersMask(mask: Int): Unit = {
    assert(registersMask == NO_REG_MASK)
    registersMask = mask
  }

  def setUnmovableRegisters(mask: Int): Unit = {
    assert(unmovableRegisters == NO_REG_MASK)
    unmovableRegisters = mask
  }

  def getDeltaMap = {
    assert((registersMask != NO_REG_MASK) == mapStarted &&
           (registersMask != NO_REG_MASK) == (unmovableRegisters != NO_REG_MASK))

    val map: XSite.GCDeltaMap = if (!mapStarted) {
      null
    } else {
      // append just died slots to deltaList
      if (allSlots != null) {
        for (ls <- allSlots.values) {
          if (ls.gcmapID == lastMapID) {
            deltaList += ls.slot
          }
        }
      }
      val regsDelta = registersMask ^ liveRegisters
      liveRegisters = registersMask
      GCDeltaMap.create(regsDelta, unmovableRegisters, deltaList, deltaSlotsStart, deltaListSize, unmovableList) tap { _ =>
        lastMapID += 1
      }
    }
    deltaSlotsStart = NOT_STARTED
    unmovableList = null
    registersMask = NO_REG_MASK
    unmovableRegisters = NO_REG_MASK
    map
  }

  /** Rebase `from` info onto `this` info */
  private def rebaseGCMaps(from: XInfo): Unit = {
    if (from.isDirtyForClassGC) {
      this.markAsDirtyForClassGC()
    }

    var startIdxAddend = NOT_STARTED
    for (xsite <- from.xsites) {
      if (xsite.gcDeltaMap != null) {
        if (startIdxAddend == NOT_STARTED) { // first gcMap in `xinfo`
          val firstMap = xsite.gcDeltaMap
          val deltaLength = firstMap.deltaListEnd // start of first map is expected to be zero
          val deltaSlots = firstMap.deltaSlots
          assert(deltaSlots.length == deltaLength)

          startGCMap()
          deltaSlots foreach addTracedSlot
          setRegistersMask(firstMap.registersMask)
          setUnmovableRegisters(firstMap.unmovableRegistersMask)
          firstMap.unmovableSlots foreach addUnmovableSlot

          xsite.gcDeltaMap = getDeltaMap

          startIdxAddend = deltaListSize - deltaLength
          assert(startIdxAddend != NOT_STARTED)

          if (deltaLength < from.deltaListSize) {
            lazyCreateSlotsInfo()
            for (ls <- from.allSlots.values) {
              val old = this.allSlots.get(ls.slot).orNull
              ls.gcmapID += lastMapID
              if (ls.gcmapID == lastMapID) {
                // slots died just after `firstMap` already contained in `this.allSlots`
                assert(old != null && old.gcmapID == lastMapID)

              } else if (old != null) {
                old.gcmapID = ls.gcmapID

              } else {
                this.allSlots.put(ls.slot, ls)
              }
            }
            lastMapID += from.lastMapID

            for (s <- from.deltaList.iterator.drop(deltaLength)) {
              this.deltaList += allSlots(s).slot
            }
          }
          liveRegisters = from.liveRegisters

        } else {
          xsite.gcDeltaMap.rebase(this.deltaList, startIdxAddend)
          xsite.gcDeltaMap.unmovableSlots mapInPlace (allSlots(_).slot)
        }
      }
    }
  }

  def prepare(seg: Segment): Unit = {
    assert(!prepared)
    if (xsites != null) {
      for (i <- xsites.indices) {
        val x = xsites(i)
        val site = siteLabels(i)
        val handler = handlerLabels(i)
        if (site != null) {
          val siteOffset = seg.getLabelPosition(site)
          // Note: XSite.CALL must not clash neither with call-site nor with the start of the next instruction.
          //       As currently there are no supported platforms with single-byte call instructions
          //       and address of the next instruction after the call is easily accessible in run-time,
          //       we avoid said clash by binding XSite at call to the returnAddress-1.
          //       However, such arithmetic cannot be done on Labels, so we adjust `siteOffset` here.
          x.siteOffset = if (x.kind.isCall) siteOffset - 1 else siteOffset
          x.handlerOffset = if (handler != null) seg.getLabelPosition(handler) else XSite.NO_EXCEPTION_HANDLER
        }
      }

      // Replacing current unmovable slots/regs in every delta-map with actual deltas.
      // TODO: generate deltas for unmovable slots/regs online, just like all other information.
      var prevUnmovableSlots: ArrayBuffer[Slot] = null
      var prevUnmovableRegisters = 0
      for (x <- xsites if x.gcDeltaMap != null) {
        val currentUnmovableSlots = x.gcDeltaMap.unmovableSlots
        val currentUnmovableRegisters = x.gcDeltaMap.unmovableRegistersMask
        val changed = (x.gcDeltaMap != GCDeltaMap.emptyDelta) || {
          if (prevUnmovableRegisters != currentUnmovableRegisters || prevUnmovableSlots != currentUnmovableSlots) {
            // We are trying to modify global emptyDelta object. After that it will become non-empty.
            // So, we create new map to avoid this.
            x.gcDeltaMap = GCDeltaMap.createEmptyDeltaMap
            true
          } else {
            // Otherwise, there is nothing to change in this delta, so keep it emptyGlobal.
            // Also, no need to call prepareUnmovable in such case.
            false
          }
        }
        if (changed) {
          x.gcDeltaMap.prepareUnmovable(prevUnmovableSlots, prevUnmovableRegisters)
          prevUnmovableSlots = currentUnmovableSlots
          prevUnmovableRegisters = currentUnmovableRegisters
        }
      }
    }
    prepared = true
  }

  def getCollectedXSites = if (xsites != null) xsites else Seq.empty[XSite]
}

object XInfo {
  trait Slot {
    /** Integer value used for relative ordering of slots.
      * Note: some slots could be incomparable resulting in same integer value.
      */
    def order: Int
  }

  private class LiveSlot(val slot: Slot, var gcmapID: Int /* id of last gcmap where slot is alive */)

  private val NOT_STARTED = -1
  private val NO_REG_MASK = -1
}
