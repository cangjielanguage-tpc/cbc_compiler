/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.cbc.Local.{Loc8, LocX}
import com.huawei.excelsior.jet.assembler.cbc.Register.{FR, IR}
import com.huawei.excelsior.jet.assembler.cbc.{Register, StackSlot}
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone, stackSlotSize}
import com.huawei.excelsior.jet.compiler.NotImplementedFeature.CBC
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.abi.cbc.FrameCBC
import com.huawei.excelsior.jet.compiler.opt.backend.FrameComponent
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.{FrameSlotCBC, MAX_CBC_REGS_COUNT, OHMSlotCBC, TypedFrameSlotCBC}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformDependentCBC
import com.huawei.excelsior.jet.compiler.options.BoolOption.PerformMassiveStackZeroingForCBC
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import xscala.util.MathUtils.alignUp

import scala.collection.mutable.ArrayBuffer

trait FrameComponentCBC extends FrameComponent with PlatformDependentCBC { self: Universe with BackEndCBC =>

  override protected def makeFrame(): FrameCBC = new FrameCBC(env, symbolLinker, codeUnit.getFrameProperties, MAX_CBC_REGS_COUNT)


  protected var tailParamCount: Int = _
  protected var untypedStackSlotsCount: Int = _
  protected var maxCalleeStackArgsCount: Int = _

  protected var usedNonVolIRegsMask: Int = _
  protected var usedNonVolFRegsMask: Int = _

  /** Slot for an argument of a method called from the currently generated one. */
  protected class ArgFrameSlotCBC(val argNum: Int) extends FrameSlotCBC(FrameSlot.CallParam) {
    override def offsetFromSP = shouldNotReachHere()
    override def toString = s"ArgFrameSlotCBC[$argNum]" + super.toString
  }

  override def makeFrameLayout(spoiledRegs: collection.Seq[Location.AnyReg], frameSlots: collection.Seq[FrameSlot]): Unit = {
    val nonVolIRegs = frame.savedRegsIterator collect { case r: IR if frame.abi.isNonVolatile(r) => r.nonVolIdx }
    val nonVolFRegs = frame.savedRegsIterator collect { case r: FR if frame.abi.isNonVolatile(r) => r.nonVolIdx }

    // TODO: consider using frame.getSaved{I,F}RegsBitMap instead
    // no need to shift, r.nonVolIdx did job for us
    usedNonVolIRegsMask = nonVolIRegs.map(idx => 1 << idx).fold(0)(_ | _)
    usedNonVolFRegsMask = nonVolFRegs.map(idx => 1 << idx).fold(0)(_ | _)

    val regsCount = MAX_CBC_REGS_COUNT // numerate stack-placed locals leaving the gap for any reg num to fit
    tailParamCount = frame.abi.paramLocations.count(_.isInstanceOf[TailSlot])

    val untypedSlotsStart = regsCount

    maxCalleeStackArgsCount = slotsForArguments.size
    
    assert(slotsForArguments.headOption.forall {
      case (_, h: ArgFrameSlotCBC) => h.argNum == 0 
    }, s"$slotsForArguments must start from 0")
    
    assert(slotsForArguments.size == 1 || slotsForArguments.valuesIterator.sliding(2).forall {
      case Seq(prev: ArgFrameSlotCBC, next: ArgFrameSlotCBC) => prev.argNum + 1 == next.argNum
    }, s"there are gaps in $slotsForArguments")

    val slotLocsStart = slotsForArguments.values.foldLeft(untypedSlotsStart) {
      case (currLocalNum, argSlot: ArgFrameSlotCBC) =>
        argSlot.local = LocX(currLocalNum)
        argSlot.untypedSlot = StackSlot.Untyped((currLocalNum - untypedSlotsStart) ensuring (_ == argSlot.argNum))
        currLocalNum + 1
      case _ => shouldNotReachHere()
    }

    val typedSlotsStart = frameSlots.filter(ts => !ts.isInstanceOf[TypedFrameSlotCBC] && !ts.isInstanceOf[OHMSlotCBC]).foldLeft(slotLocsStart) {
      case (currLocalNum, slot: FrameSlotCBC) =>
        slot.local = LocX(currLocalNum)
        slot.untypedSlot = StackSlot.Untyped(currLocalNum - untypedSlotsStart)
        currLocalNum + slotSizeToCount(slot)
      case _ => shouldNotReachHere()
    }

    untypedStackSlotsCount = typedSlotsStart - untypedSlotsStart

    // typed slots
    val ohmSlotsStart = frameSlots.collect {
      case x: TypedFrameSlotCBC => x
    }.foldLeft(typedSlotsStart) {
      case (currLocalNum, slot: TypedFrameSlotCBC) =>
        slot.local = LocX(currLocalNum)
        slot.typedSlot = StackSlot.Typed(currLocalNum - typedSlotsStart)
        currLocalNum + 1
    }

    // OHM slots
    frameSlots.collect {
        case x: OHMSlotCBC => x
      }
      .groupBy(_.kind.allocType).valuesIterator.flatten // we need to group OHM slots by its type to build correct OHM Multiset
      .foldLeft(ohmSlotsStart) {
        case (currLocalNum, slot) =>
          slot.local = LocX(currLocalNum)
          slot.ohmSlot = StackSlot.OffHeapMemory(currLocalNum - ohmSlotsStart)
          currLocalNum + 1
      }
  }

  private def slotSizeToCount(slot: FrameSlotCBC) = {
    val size = if (slot.size == 0) 1 else slot.size // TODO-CBC: workaround for zero size records (see JET-13286)
    alignUp(size, stackSlotSize) / stackSlotSize
  }

  /** Assign real address for given `slot`. */
  override protected def calculateAddressForSlot(slot: FrameSlot, index: Int): Unit = notImplemented(CBC, "FrameComponentCBC.calculateAddressForSlot")

  /** Created arch-specific frame slot with given parameters. */
  override protected def newFrameSlot(kind: FrameSlot.Kind): FrameSlot = new FrameSlotCBC(kind)

  protected def typedFrameSlot(kind: FrameSlot.Kind): Option[SignatureType] = kind match {
    case FrameSlot.Typed(allocType: SignatureType.TypeVariable) => Some(allocType)
    case FrameSlot.Typed(allocType) if allocType.isRecord => Some(allocType)
    case FrameSlot.NewOnStack(allocType) => Some(allocType)
    case _ => None
  }

  override protected def newFrameSlotForStackAlloc(kind: FrameSlot.Kind): FrameSlot = {
    (kind, typedFrameSlot(kind)) match {
      case (_, Some(allocType))                  => TypedFrameSlotCBC(kind, allocType)
      case (kind: FrameSlot.OffHeapMemory, None) => OHMSlotCBC(kind)
      case _                                     => newFrameSlot(kind)
    }
  }

  /** Creates new slot for argument with `offset` from TailPointer.
    * Note that unlike common code, in CBC we never address params from SP, 
    * see [[com.huawei.excelsior.jet.compiler.abi.cbc.ABICBC.stackParamsStartOffset]]. */
  override protected def newSlotForArg(trOffsetInBytes: Int): FrameSlot = {
    new ArgFrameSlotCBC(argNum = trOffsetInBytes / stackSlotSize)
  }

  /** Ensures that frame is full. */
  override def ensureFullFrame(): Unit = {}   //TODO-CBC

  override protected def configureMassiveStackZeroing(zeroed: ArrayBuffer[FrameSlot]): Unit = {
    if (!env.enabled(PerformMassiveStackZeroingForCBC)) return // for JET-17840

    val cbcZeroed = zeroed collect { case x: FrameSlotCBC if !x.isInstanceOf[TypedFrameSlotCBC] => x }
    if (cbcZeroed.isEmpty) {
      return
    }

    val zeroStart = cbcZeroed.head
    val zeroSize = (cbcZeroed map slotSizeToCount).sum

    // Zeroed frame slots are expected to be sorted in ascending order.
    assert(cbcZeroed forall (s => s.index >= zeroStart.index))
    // End of each zeroed frame slot should match the beginning of the next one (except for the last one).
    assert(cbcZeroed.length == 1 || (cbcZeroed sliding 2 forall (s => s(0).index + slotSizeToCount(s(0)) == s(1).index)))
    // Sum of individual frame slot sizes should be equal to the size of the whole slot block.
    assert(cbcZeroed.last.index + slotSizeToCount(cbcZeroed.last) - zeroStart.index == zeroSize)

    // TODO: It would be great to refactor frame slots so that CBC wouldn't require special handling.

    StackZeroing.Massive.setProperties(zeroStart, zeroSize)
  }
}

object FrameComponentCBC {
  val MAX_CBC_IREG_COUNT = IR.count
  val MAX_CBC_FREG_COUNT = FR.count
  val MAX_CBC_REGS_COUNT = MAX_CBC_IREG_COUNT + MAX_CBC_FREG_COUNT


  class FrameSlotCBC(kind: FrameSlot.Kind) extends FrameSlot(kind) {
    var local: LocX = _
    var untypedSlot: StackSlot.Untyped = _

    override def index = {
      assert(local != null)
      local.encoding
    }
  }

  class TypedFrameSlotCBC(kind: FrameSlot.Kind, val tpe: SignatureType) extends FrameSlotCBC(kind) {
    var typedSlot: StackSlot.Typed = _
  }

  class OHMSlotCBC(override val kind: FrameSlot.OffHeapMemory) extends FrameSlotCBC(kind) {
    assert(!isStandalone)
    var ohmSlot: StackSlot.OffHeapMemory = _
  }
}