/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.ir.XSite.UNKNOWN_OFFSET
import com.huawei.excelsior.jet.compiler.symlevel.MethodReference

import scala.collection.mutable.ArrayBuffer

/** Exception site.
  *
  * @author alexm
  * @author paul
  */
final class XSite private[ir](

  /** Exception site kind. */
  val kind: XSiteKind,

  /** Accessed memory offset, for null check site.
    * It is used to validate if a hardware exception is a Java null check (in work mode).
    *
    * <p>
    * Implicit null check instructions access Java object at some offset X. If an object is `null`, then
    * the null check instruction accesses invalid address X, and hardware exception is converted
    * into Java NPE. In work mode, with additional null checks information (X), runtime can distinguish null checks
    * from accesses to invalid addresses. If implicit null check instruction accesses any other invalid address
    * besides X, runtime treats it as a fatal error.
    * </p>
    */
  val accessOffset: Int,

  /** Bytecode/bitcode position corresponding to this exception site. */
  val bytecodePos: Int,

  /** Source line number corresponding to this exception site. */
  val lineNumber: Int,

  val inlineContext: InlineContext,

  /** Reference to method called at this xSite. */
  val calledMethodRef: MethodReference,

  /** GCDeltaMap for this xsite. Could be null for some types of xsite. */
  var gcDeltaMap: XSite.GCDeltaMap,

  val softExceptionID: Int,

  val domain: Domain
) {
  if (kind.isCall) {
    // TODO: use more MethodReference and require non-null calledMethodRef
    //assert(calledMethodRef != null)
  } else {
    assert(calledMethodRef == null)
  }

  private var _siteOffset = UNKNOWN_OFFSET
  private var _handlerOffset = UNKNOWN_OFFSET

  private[ir] def siteOffset_=(newOffset: Int): Unit = {
    assert(_siteOffset == UNKNOWN_OFFSET && newOffset != UNKNOWN_OFFSET)
    _siteOffset = newOffset
  }

  private[ir] def handlerOffset_=(newOffset: Int): Unit = {
    assert(_handlerOffset == UNKNOWN_OFFSET && newOffset != UNKNOWN_OFFSET)
    _handlerOffset = newOffset
  }

  /** Exception site offset in code segment. */
  def siteOffset = _siteOffset ensuring (_ != UNKNOWN_OFFSET)

  /** Handler corresponding to the exception site. */
  def handlerOffset = _handlerOffset ensuring (_ != UNKNOWN_OFFSET)
}

object XSite {
  val NO_EXCEPTION_HANDLER = 0
  val UNKNOWN_OFFSET = -1

  /** Map with information about references to live objects at the corresponding xsite. */
  object GCDeltaMap {
    def createEmptyDeltaMap = new XSite.GCDeltaMap(0, 0, null, 0, 0, null)

    val emptyDelta = createEmptyDeltaMap

    private[ir] def create(registersMask: Int, unmovableRegistersMask: Int,
                           deltaList: ArrayBuffer[XInfo.Slot], start: Int, end: Int,
                           unmovableList: ArrayBuffer[XInfo.Slot]): GCDeltaMap = {
      if (registersMask == 0 && start == end && unmovableRegistersMask == 0 && unmovableList == null) {
        emptyDelta
      } else if (start == end) {
        new XSite.GCDeltaMap(registersMask, unmovableRegistersMask, null, 0, 0, unmovableList)
      } else {
        new XSite.GCDeltaMap(registersMask, unmovableRegistersMask, deltaList, start, end, unmovableList)
      }
    }
  }

  class GCDeltaMap private(val registersMask: Int, private var _unmovableRegistersMask: Int,
                           private var deltaList: ArrayBuffer[XInfo.Slot], private var start: Int, private var end: Int,
                           private var unmovableList: ArrayBuffer[XInfo.Slot]) {
    private var unmovablePrepared = false

    private[ir] def deltaListEnd = end

    private[ir] def rebase(deltaList: ArrayBuffer[XInfo.Slot], rebaseDelta: Int): Unit = {
      if (this.deltaList != null) {
        assert(this != GCDeltaMap.emptyDelta)
        this.deltaList = deltaList
        this.start += rebaseDelta
        this.end += rebaseDelta
      }
    }

    /** Before preparation `unmovableList` and `unmovableRegistersMask` store current live slots.
      * For further gc-map generation we have to store deltas in `unmovableList` and `unmovableRegistersMask`.
      */
    def prepareUnmovable(previousList: ArrayBuffer[XInfo.Slot], previousRegisters: Int): Unit = {
      assert(this != GCDeltaMap.emptyDelta)
      assert(!unmovablePrepared)
      unmovablePrepared = true

      _unmovableRegistersMask ^= previousRegisters

      if (previousList == null || previousList.isEmpty) {
        return
      }
      if (unmovableList == null || unmovableList.isEmpty) {
        unmovableList = previousList
        return
      }

      val deltaUnmovableList = ArrayBuffer.empty[XInfo.Slot]
      for (slot <- unmovableList) {
        if (!previousList.contains(slot)) {
          deltaUnmovableList += slot
        }
      }
      for (slot <- previousList) {
        if (!unmovableList.contains(slot)) {
          deltaUnmovableList += slot
        }
      }
      unmovableList = deltaUnmovableList
    }

    def isEmpty = registersMask == 0 && deltaList == null && unmovableRegistersMask == 0 && unmovableList == null

    def deltaSlots = if (deltaList != null) deltaList.slice(start, end) else ArrayBuffer.empty[XInfo.Slot]

    def unmovableSlots = if (unmovableList != null) unmovableList else ArrayBuffer.empty[XInfo.Slot]

    def unmovableRegistersMask = _unmovableRegistersMask

    override def toString = s"GCDeltaMap(" +
      s"regMask=0x${registersMask.toHexString}; " +
      s"slots={${deltaSlots.sortBy(_.order) mkString ", "}}; " +
      s"unmovableRegs=0x${unmovableRegistersMask.toHexString}; " +
      s"unmovableSlots={${unmovableSlots.sortBy(_.order) mkString ", "}}" +
      s")"
  }
}
