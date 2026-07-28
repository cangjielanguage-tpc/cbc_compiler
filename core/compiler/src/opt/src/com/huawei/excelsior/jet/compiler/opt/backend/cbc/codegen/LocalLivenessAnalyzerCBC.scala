/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.assembler.cbc.Local.LocX
import com.huawei.excelsior.jet.assembler.cbc.Register.IR
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.BackEndCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.{FrameSlotCBC, MAX_CBC_IREG_COUNT, MAX_CBC_REGS_COUNT, TypedFrameSlotCBC}
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen.LocalLivenessAnalyzerCBC.LocalType
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen.LocalLivenessAnalyzerCBC.LocalType.{CLEARED, REFERENCE, UNMOVABLE_REFERENCE}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.{LivenessHintsAtBlockStart, LivenessHintsGeneration}
import xscala.util.MathUtils
import xscala.util.MathUtils.*

import scala.collection.mutable

/** Tool used for liveness tracking of values on locals (registers and stack slots) during method code generation.
  * Used to approximate liveness of locals using single-pass sequential code processing.
  * Analyzer state should be reset ([[resetLiveness]]) with precise liveness information (from [[LivenessAnalysis]])
  * at points of synchronization with precise analysis.
  *
  * Points of synchronization with precise [[LivenessAnalysis]] are the places where liveness hints
  * (CBC: `alive.ref`, `unmovable.ref`, `alive.ref.diff`, `alive.unmovable.diff`) are generated.
  *
  * @author msharapov
  */
trait LocalLivenessAnalyzerCBC { self: Universe with BackEndCBC =>

  private val enabled = env.enabled(LivenessHintsGeneration)
  private val enabledBlockStartHints = enabled && env.enabled(LivenessHintsAtBlockStart)

  /** Analyzer state. */
  var aliveRefSlots: mutable.LinkedHashSet[XInfo.Slot] = null
  var aliveRegsMask: Int = -1
  var unmovableRegsMask: Int = -1

  /** Setup starting/updated state for local liveness analyzer. */
  def resetLiveness(mask: Int, unmovableMask: Int, slots: IterableOnce[XInfo.Slot]): Unit = {
    if (!enabled) return
    aliveRegsMask = mask
    unmovableRegsMask = unmovableMask
    aliveRefSlots = mutable.LinkedHashSet.from(slots)
  }

  /** Retrieve calculated liveness information. */
  def currentAliveRegsMask: Int = aliveRegsMask

  def currentUnmovableRegsMask: Int = unmovableRegsMask

  def currentAliveSlots: collection.Set[XInfo.Slot] = aliveRefSlots

  protected def localTypeOf(n: Node): LocalType = if n.tpe.isTraceableRefType then REFERENCE else CLEARED

  /** Mark locations of parameters of method corresponding to method signature.
    * Analyzer should be initialized ([[resetLiveness]]) with precise liveness before calling this method.
    * This method is used to adjust calculated state to correspond to JIT's local liveness analysis:
    * some of method's arguments may have no uses but JIT doesn't know that, so analyzer consider them alive.
    */
  def markParams(params: Seq[Param], paramLocs: Array[Location]): Unit = {
    if (!enabled) return
    assert(params.size == paramLocs.length)
    for ((p, l) <- params.iterator zip paramLocs.iterator) {
      p.resource match {
        case ir: IReg =>
          assert(l == ir)
          mark(ir, localTypeOf(p))
        case s: FrameSlotCBC => mark(s.local, localTypeOf(p))
        case _: FReg => // nothing to do
        case Location.INVALID =>
          l match {
            case ir: IReg => mark(ir, localTypeOf(p))
            case _: FReg => // nothing to do
            case ts: ABI.TailSlot => // nothing to do, tail slots are tracked by JIT only
            case _ => shouldNotReachHere(s"Unexpected param location: $l")
          }
        case ts: TailSlot => // nothing to do, tail slots are tracked by JIT only
        case r => shouldNotReachHere(s"Unexpected param resource: $r")
      }
    }
  }

  /** Node changes liveness in non-trivial way. For such nodes liveness tracked at time of CBC code generation. */
  protected def trackLivenessDuringCodegen(node: Node): Boolean = node match {
    case _: Call => true // hints should be inserted to the middle of CBC-instructions pattern
    case _: PreCall => true // no actions needed
    case _: (StackZeroing | EndLocalUnmovable) => true // complex semantics
    case _: (StoreMemory | CopyStructure | CopyStructureCBC | InitStringRecord) => true // could not process using general logic
    case _: Return if Isa12Mode => true // transfers incoming value to ABI-fixed register
    case _ => false
  }

  protected def updateLiveness(node: Node): Unit = {
    if (!enabled) return
    if (trackLivenessDuringCodegen(node)) {
      clearVolatiles(node)
      return
    }

    def checkArg(arg: Node): Unit = {
      val tpe: LocalType = localTypeOf(arg)
      arg match {
        case sa: StackAlloc =>
          sa.slot match {
            case _: TypedFrameSlotCBC => // untracked
            case sl: FrameSlotCBC => assert(check(sl.local, tpe))
          }
        case _ =>
          if (arg.mayHaveResource) {
            arg.resource match {
              case ireg: IReg => assert(check(ireg, tpe))
              case fs: TypedFrameSlotCBC => shouldNotReachHere(s"liveness tracking of typed slots is not supported $fs")
              case fs: FrameSlotCBC => assert(check(fs.local, tpe))
              case _ =>
            }
          }
      }
    }

    def markResource(x: Location, tpe: LocalType): Unit = x match {
      case ireg: IReg => mark(ireg, tpe)
      case fs: TypedFrameSlotCBC => shouldNotReachHere(s"liveness tracking of typed slots is not supported $fs")
      case fs: FrameSlotCBC => mark(fs.local, tpe)
      case _ =>
    }

    node match {
      case x if noCodeShouldBeGenerated(x) => // nothing to do
      case _ =>
        // check arguments are marked with correct type
        node.valueArgs.foreach(checkArg)

        node.allResultResources.foreach(resource => {
          val tpe: LocalType = localTypeOf(node)
          markResource(resource, tpe)
        })
    }
    clearVolatiles(node)
  }

  private def clearVolatiles(node: Node): Unit = {
    val volatile = node.spoiled.filter(_.isInstanceOf[IReg])
    volatile.foreach(res => mark(res.asIReg, LocalType.CLEARED))
  }

  def mark(r: IReg, tpe: LocalType): Unit = {
    if (!enabled) return
    tpe match {
      case CLEARED =>
        aliveRegsMask = clearBit(aliveRegsMask, r.asInstanceOf[IR].idx)
        unmovableRegsMask = clearBit(unmovableRegsMask, r.asInstanceOf[IR].idx)
      case REFERENCE =>
        aliveRegsMask = setBit(aliveRegsMask, r.asInstanceOf[IR].idx)
        unmovableRegsMask = clearBit(unmovableRegsMask, r.asInstanceOf[IR].idx)
      case UNMOVABLE_REFERENCE =>
        aliveRegsMask = setBit(aliveRegsMask, r.asInstanceOf[IR].idx)
        unmovableRegsMask = setBit(unmovableRegsMask, r.asInstanceOf[IR].idx)
    }
  }

  def mark(fs: XInfo.Slot, tpe: LocalType): Unit = {
    if (!enabled) return
    tpe match {
      case CLEARED => aliveRefSlots -= fs
      case REFERENCE => aliveRefSlots += fs
      case _ => shouldNotReachHere(s"unsupported type $tpe")
    }
  }

  def mark(loc: LocX, tpe: LocalType): Unit = {
    if (!enabled) return
    if (loc.encoding < MAX_CBC_IREG_COUNT) {
      mark(IR.fromOrdinal(loc.encoding), tpe)
    } else if (loc.encoding >= MAX_CBC_REGS_COUNT) {
      mark(FrameCBC.Slot(loc), tpe)
    } else {
      shouldNotReachHere(s"Unsupported LocX with idx: ${loc.encoding}")
    }
  }

  def check(r: IReg, tpe: LocalType): Boolean = {
    if (!enabledBlockStartHints) return true
    val expectedLiveness = tpe == REFERENCE || tpe == UNMOVABLE_REFERENCE
    val expectedUnmovable = tpe == UNMOVABLE_REFERENCE
    val actualLiveness = isBitSet(aliveRegsMask, r.asInstanceOf[IR].idx)
    val actualUnmovable = isBitSet(unmovableRegsMask, r.asInstanceOf[IR].idx)
    (actualLiveness == expectedLiveness) && (actualUnmovable == expectedUnmovable)
  }

  def check(slot: XInfo.Slot, tpe: LocalType): Boolean = {
    if (!enabledBlockStartHints) return true
    if (tpe == REFERENCE) {
      aliveRefSlots.contains(slot)
    } else {
      !aliveRefSlots.contains(slot)
    }
  }

  def check(loc: LocX, tpe: LocalType): Boolean = {
    if (!enabledBlockStartHints) return true
    if (loc.encoding < MAX_CBC_IREG_COUNT) {
      check(IR.fromOrdinal(loc.encoding), tpe)
    } else if (loc.encoding >= MAX_CBC_REGS_COUNT) {
      check(FrameCBC.Slot(loc), tpe)
    } else {
      shouldNotReachHere(s"Unsupported LocX with idx: ${loc.encoding}")
    }
  }

  def transferMark(from: IReg, to: IReg): Unit = {
    if (!enabled) return
    mark(to, getMark(from))
  }

  def transferMark(from: IReg, to: LocX): Unit = {
    if (!enabled) return
    mark(to, getMark(from))
  }

  def transferMark(from: LocX, to: IReg): Unit = {
    if (!enabled) return
    val tpe = if (from.encoding < MAX_CBC_IREG_COUNT) {
      getMark(IR.fromOrdinal(from.encoding))
    } else {
      getMark(FrameCBC.Slot(from))
    }
    mark(to, tpe)
  }

  def transferMark(from: XInfo.Slot, to: XInfo.Slot): Unit = {
    if (!enabled) return
    mark(to, getMark(from))
  }

  def transferMark(from: LocX, to: LocX): Unit = {
    if (!enabled) return
    val tpe = if (from.encoding < MAX_CBC_IREG_COUNT) {
      getMark(IR.fromOrdinal(from.encoding))
    } else if (from.encoding >= MAX_CBC_REGS_COUNT) {
      getMark(FrameCBC.Slot(from))
    } else {
      shouldNotReachHere(s"Unsupported LocX with idx: ${from.encoding}")
    }

    mark(to, tpe)
  }

  private def getMark(r: IReg): LocalType = {
    if (isBitSet(aliveRegsMask, r.asInstanceOf[IR].idx)) {
      if (isBitSet(unmovableRegsMask, r.asInstanceOf[IR].idx)) {
        UNMOVABLE_REFERENCE
      } else {
        REFERENCE
      }
    } else {
      CLEARED
    }
  }

  private def getMark(slot: XInfo.Slot): LocalType = {
    if (aliveRefSlots.contains(slot)) {
      REFERENCE
    } else {
      CLEARED
    }
  }
}

object LocalLivenessAnalyzerCBC {
  /** Type of value stored in local */
  protected[backend] enum LocalType {
    case CLEARED // uninitialized/spoiled/primitive
    case REFERENCE
    case UNMOVABLE_REFERENCE
  }
}
