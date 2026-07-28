/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.{addressSize, targetArch, targetPlatform}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.ir.XSite.GCDeltaMap
import com.huawei.excelsior.jet.compiler.options.BoolOption.GCMapsPreferMask
import xscala.io.{ByteBuffer, LEB128Encoder}
import xscala.util.MathUtils

import scala.annotation.{nowarn, tailrec}
import scala.collection.immutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * GC map encoder.
  *
  * @author ijorch
  * @author minium
  * @author wellox
  */
trait GCMapsGenerator { self: XTableGenerator =>

  def genGCMap(unfilteredXSitesInfo: ArrayBuffer[XSiteInfo]): ByteBuffer = {
    val xSitesInfo = unfilteredXSitesInfo filter (_.deltaMap != null)
    if (xSitesInfo.nonEmpty) {
      val gcmap = new ByteBuffer()
      var base = 0 // in run-time we drop base only once for each stack-frame,
                   // so at the start of gcmap decoding it is always zero and only gcmap commands modify it
      for (xsi <- xSitesInfo) {
        GCMapStatisticCollector.registerChanges(xsi.deltaMap.registersMask)
        base = genGCMap(xsi.deltaMap, gcmap, base)
        xsi.gcMapLength = gcmap.length
      }
      gcmap
    } else {
      null
    }
  }

  private val UNDEFINED = Int.MaxValue
  private val iRegNum = RTConst.GCMapDecoder.I_REGS_COUNT.intValue
  private val fRegNum = RTConst.GCMapDecoder.F_REGS_COUNT.intValue

  private def genGCMap(deltaMap: GCDeltaMap, buf: ByteBuffer, previousBase: Int): Int = {
    if (deltaMap.isEmpty) {
      return previousBase
    }
    // Encode given non-empty `deltaMap` as described in NewGCMaps.markdown#encoding.
    // Note that step 1 was done already during `deltaMap` construction.

    // Step 2: Initialization.
    var base = previousBase // updated by every command to keep the track of last encoded slot
    var lb = UNDEFINED // stores the previous value of base to properly form list command
    val maskWidth = RTConst.GCMapDecoder.MAX_MASK_WIDTH.intValue
    val list = ListBuffer.empty[Int]

    // Step 3: Split slots on stack alloc headers, unmovable slots and regular ones.
    var Seq(slots, stackAllocSlots, unmovableSlots) = numerate(deltaMap)

    if (stackAllocSlots.nonEmpty) {
      assert(base == 0)
      // MUST be written first: it relies on `base == 0` at start
      // as it has only the base-relative encoding, and no base-dropping one.
      base = writeSpecialList(ListStackAllocBased, stackAllocSlots, base, buf)
    }

    if (unmovableSlots.nonEmpty) {
      // unmovableSlots MUST be written with UNDEFINED base, because they may appear in the middle of gcmap
      // and don't have any base-relative encoding, only the base-dropping one.
      base = writeSpecialList(ListUnmovableSlots, unmovableSlots, UNDEFINED, buf)
    }

    // NOTE: for now we forcefully drop base to UNDEFINED, so next command will reset it.
    base = UNDEFINED // TODO: remove and ensure correctness

    def addToList(s: Int): Unit = {
      if (list.isEmpty) {
        lb = base
      }
      list += s
      base = s
    }

    def splitSlots(slots: List[Int], b: Int) = {
      val th = if (ignoreBase(b, slots.head)) 0 else b
      slots span (s => (th <= s && s < th + maskWidth))
    }

    // Step 4: select slot ranges and instructions to encode them
    while (slots.nonEmpty) {
      val (maskable, remainingSlots) = splitSlots(slots, base)
      (maskable, base) match {
        case (Nil, _) =>
          addToList(remainingSlots.head)
          slots = remainingSlots.tail

        case MaskSlots() => // mask can be used
          if (list.nonEmpty) {
            writeList(list.toList, lb, buf)
            list.clear()
          }
          MaskSlots.write(maskable, base, buf)
          base = if (ignoreBase(base, maskable.head)) {
            maskWidth - 1 // -1 because we must never point to not-yet-covered slot due to the format of single-offset command
          } else {
            base + maskWidth - 1
          }
          slots = remainingSlots

        case _ =>
          if (list.isEmpty) {
            lb = base
          }
          // TODO: probably less space may be occupied by gcmaps
          //       if here we wouldn't add all `maskable` slots into `list` at once
          //       but rather only add some initial part, so that next slots
          //       (together with those that currently are in `remainingSlots`)
          //       could still be encoded using mask.
          list ++= maskable
          base = list.max
          slots = remainingSlots
      }
    }

    // Step 5: write remaining slots
    writeList(list.toList, lb, buf)

    base
  }

  /** Fully write given `slots` using only the given `command`. */
  @tailrec
  private def writeSpecialList(command: ListCommand, slots: List[Int], base: Int, buf: ByteBuffer): Int = {
    if (slots.isEmpty) {
      return base
    }
    assert(slots.head >= 0)
    val (tail, b) = command.write(slots, base, buf)
    writeSpecialList(command, tail, b, buf)
  }

  /** Numerate slots in deltaMap, see NewGCMaps.markdown#slot-numbering. */
  private def numerate(deltaMap: GCDeltaMap) = {
    def onlyHeader(s: XInfo.Slot) = s match {
      case slot: Frame.Slot => slot.tracedByHeader
      case s => shouldNotReachHere(s"Unsupported $s")
    }

    def slotIdx(slot: XInfo.Slot): IndexedSeq[Int] = slot match {
      case slot: Frame.Slot =>
        val maxSlotIdx = RTConst.GCMapDecoder.MAX_STACK_SLOTS_NUMBER.intValue + iRegNum
        val minSlotIdx = -RTConst.GCMapDecoder.MAX_STACK_SLOTS_NUMBER.intValue - fRegNum
        val offset = slotOffset(slot)
        assert(offset >= 0, s"Slots with negative offset are not allowed: $slot")
        val idx = slot.base match {
          case SlotBase.TR => shouldNotReachHere()
          case _ => iRegNum + offset / addressSize
        }

        val subSlotsCount = if (onlyHeader(slot)) 1 else slot.size / addressSize ensuring (_ > 0)
        if (!(minSlotIdx <= idx && idx <= maxSlotIdx) || idx + subSlotsCount - 1 > maxSlotIdx) {
          throw new Exception(s"GCMap generation failed: $idx and $subSlotsCount are too big")
        }
        for (k <- 0 until subSlotsCount)
          yield idx + k
      case s => shouldNotReachHere(s"Unsupported $s")
    }
    def fRegMask(i: Int) = false
    def iRegMask(i: Int) = (deltaMap.registersMask & (1 << i)) != 0
    def unmovableRegMask(i: Int) = (deltaMap.unmovableRegistersMask & (1 << i)) != 0

    val deltaSlots = deltaMap.deltaSlots
    val unmovableSlots = deltaMap.unmovableSlots
    val (stackAllocHeaders, tracedSlots) = deltaSlots partition onlyHeader

    val numeratedSlots = ArrayBuffer.empty[Int]
    numeratedSlots ++= (0 until iRegNum) filter iRegMask
    numeratedSlots ++= (-fRegNum until 0) filter fRegMask
    numeratedSlots ++= (tracedSlots flatMap slotIdx)

    val numeratedHeaders = ArrayBuffer.empty[Int]
    numeratedHeaders ++= (stackAllocHeaders flatMap slotIdx)

    val numeratedUnmovableSlots = ArrayBuffer.empty[Int]
    numeratedUnmovableSlots ++= (0 until iRegNum) filter unmovableRegMask
    numeratedUnmovableSlots ++= (unmovableSlots flatMap slotIdx)

    Seq(numeratedSlots, numeratedHeaders, numeratedUnmovableSlots) map (_.sortInPlace().toList)
  }

  /** Write `list` in buffer with respect to `B`. */
  @tailrec
  @nowarn("msg=match may not be exhaustive")
  private def writeList(list: List[Int], B: Int, buf: ByteBuffer): Unit = (list, B) match {
    case (Nil, _) => // nothing to do
    case TwoSlots(s1, s2, tail) =>
      // can be encoded with two-slots-in-single-byte command
      // TODO: consider turning this command into two-offsets-in-single-byte to efficiently encode pairs of registers with numbers >= 16
      TwoSlots.write(s1, s2, buf)
      writeList(tail, s2, buf)

    case OneSlot(s, tail) =>
      OneSlot.write(s, buf)
      writeList(tail, s, buf)

    case OneSlotBased(s, tail) =>
      OneSlotBased.write(s, B, buf)
      writeList(tail, s, buf)

    case ListSlots() =>
      val (tail, b) = ListSlots.write(list, B, buf)
      writeList(tail, b, buf)

    case ListSlotsBased() =>
      val (tail, b) = ListSlotsBased.write(list, B, buf)
      writeList(tail, b, buf)

    case ListNegativeSlots() =>
      val (tail, b) = ListNegativeSlots.write(list, B, buf)
      writeList(tail, b, buf)
  }

  private def ignoreBase(base: Int, slot: Int) = (base > slot)

  private sealed trait Command {
    protected def encode(H: Int, L: Int) = {
      assert(MathUtils.isNBits(H, 4) && MathUtils.isNBits(L, 4))
      (H << 4) | L
    }
  }

  private object OneSlot extends Command {
    /** Encoded slot as this command and write to buffer. */
    def write(slot: Int, buf: ByteBuffer): Unit = {
      GCMapStatisticCollector.recordGCMap(s"OneSlot: $slot")
      buf.putByte(encode(H = 0, L = slot))
    }

    /** Checks if a slot can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = splitSlots lift arg

    def splitSlots: PartialFunction[(List[Int], Int), (Int, List[Int])] = {
      case (s :: tail, b) if (0 <= s) && (s < 16) && ignoreBase(b, s) => (s, tail)
    }
  }

  private object TwoSlots extends Command {
    /** Encoded slot as this command and write to buffer. */
    def write(slot1: Int, slot2: Int, buf: ByteBuffer): Unit = {
      assert(slot1 < slot2)
      GCMapStatisticCollector.recordGCMap(s"TwoSlots: $slot2|$slot1")
      buf.putByte(encode(H = slot2, L = slot1))
    }

    /** Checks if a slot can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = splitSlots lift arg

    def splitSlots: PartialFunction[(List[Int], Int), (Int, Int, List[Int])] = {
      case (s1 :: s2 :: tail, _) if (0 <= s1) && (s1 < 16) && (s2 < 16) => (s1, s2, tail)
    }
  }

  private object OneSlotBased extends Command {
    final val opCode = RTConst.GCMapDecoder.Code.ONE_SLOT_BASED_OPCODE.intValue

    /** Encoded slot as this command and write to buffer. */
    def write(slot: Int, base: Int, buf: ByteBuffer): Unit = {
      GCMapStatisticCollector.recordGCMap(s"OneSlotBased: $slot - $base|$opCode")
      buf.putByte(encode(H = slot - base, L = opCode))
    }

    /** Checks if a slot can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = splitSlots lift arg

    def splitSlots: PartialFunction[(List[Int], Int), (Int, List[Int])] = {
      case (s :: tail, b) if 0 < (s - b) && (s - b) < 16 && !ignoreBase(b, s) => (s, tail)
    }
  }

  private sealed trait ListCommand extends Command {
    def opCode: Int

    /** Writes all slots that fit into this command into given buffer.
      * @return list of remaining unencoded slots and new base.
      */
    def write(slots: List[Int], base: Int, buf: ByteBuffer) = {
      val (start, tail) = slots splitAt opCode
      GCMapStatisticCollector.recordGCMap(start.foldRight(s"List command with opcode=$opCode and slots")((x, acc) => acc + s" $x"))
      buf.putByte(encode(H = start.length, L = opCode))
      var b = if (ignoreBase(base, start.head)) 0 else base
      for (s <- start) {
        buf.putULEB((s - b).abs)
        b = s
      }
      (tail, b)
    }
  }

  private object ListSlotsBased extends ListCommand {
    final val opCode = RTConst.GCMapDecoder.Code.LIST_SLOTS_BASED_OPCODE.intValue

    /** Checks if given slots list can be encoded with this command. */
    def unapply(arg: (List[Int], Int)): Boolean = !ignoreBase(arg._2, arg._1.head)
  }

  private object ListSlots extends ListCommand {
    final val opCode = RTConst.GCMapDecoder.Code.LIST_SLOTS_OPCODE.intValue

    /** Checks if given slots list can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = ignoreBase(arg._2, arg._1.head) && arg._1.head >= 0
  }

  private object ListNegativeSlots extends ListCommand {
    final val opCode = RTConst.GCMapDecoder.Code.LIST_NEGATIVE_SLOTS_OPCODE.intValue

    /** Checks if given slots list can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = ignoreBase(arg._2, arg._1.head) && arg._1.head < 0
  }

  private object ListStackAllocBased extends ListCommand {
    final val opCode = RTConst.GCMapDecoder.Code.LIST_STACK_ALLOC_BASED_OPCODE.intValue
  }

  private object ListUnmovableSlots extends ListCommand {
    final val opCode = RTConst.GCMapDecoder.Code.LIST_UNMOVABLE_SLOTS_OPCODE.intValue

    override def write(slots: List[Int], base: Int, buf: ByteBuffer) = {
      // forcefully ignore `base` updates so that this command could be correctly used repeatedly in the middle of gcmap
      assert(base == UNDEFINED)
      val (list, _) = super.write(slots, base, buf)
      (list, UNDEFINED)
    }
  }

  private object MaskSlots extends Command {
    /** Encoded slots as this command and write to buffer. */
    def write(mask: List[Int], B: Int, buf: ByteBuffer): Unit = {
      val opcode = if (ignoreBase(B, mask.head)) {
        RTConst.GCMapDecoder.Code.MASK_SLOTS_OPCODE.intValue
      } else {
        RTConst.GCMapDecoder.Code.MASK_SLOTS_BASED_OPCODE.intValue
      }

      val len = maskLen(B, mask) ensuring (_ <= opcode)
      GCMapStatisticCollector.recordGCMap(mask.foldRight(s"As mask: len=$len")((x, acc) => acc + s" $x"))
      buf.putByte(encode(H = len, L = opcode))

      var m = encodeMask(B, mask)

      for (_ <- 0 until len) {
        assert(m != 0)
        buf.putByte(m.toInt & 0xFF)
        m >>>= 8
      }
    }

    /** Checks if a slots can be encoded with this command. */
    def unapply(arg: (List[Int], Int)) = {
      val (slots, base) = arg
      val preferMask = if (env.enabled(GCMapsPreferMask)) -1 else 0
      listLen(slots, base) > (maskLen(base, slots) + 1) + preferMask // maskLen + 1 is for opcode
    }

    /** The length of the shortest list command with which we can encode slots from the mask. */
    private def listLen(slots: List[Int], B: Int) = {
      def sizeULEB128(slots: List[Int]) =
        slots.fold(0)((acc, int) => acc + LEB128Encoder.calcSizeULEB128(int))

      @nowarn("msg=match may not be exhaustive")
      def listOpCode(slots: List[Int], b: Int) = (slots, b) match {
        case ListSlots() => ListSlots.opCode
        case ListSlotsBased() => ListSlotsBased.opCode
        case ListNegativeSlots() => ListNegativeSlots.opCode
      }

      @tailrec
      def listLenIter(slots: List[Int], B: Int, acc: Int): Int = (slots, B) match {
        case (Nil, _) => acc
        case TwoSlots(_, s2, tail) => listLenIter(tail, s2, acc + 1)
        case OneSlot(s, tail) => listLenIter(tail, s, acc + 1)
        case OneSlotBased(s, tail) => listLenIter(tail, s, acc + 1)
        case _ =>
          val L = listOpCode(slots, B)
          val (list, tail) = slots splitAt L
          listLenIter(tail, list.last, acc + 1 + sizeULEB128(list))
      }

      listLenIter(slots, B, 0)
    }

    /** Calculates the size of the mask in bytes. */
    private def maskLen(B: Int, mask: List[Int]) = (mask.last - (if (ignoreBase(B, mask.head)) 0 else B)) / 8 + 1

    /** Convert mask list to bit mask. */
    private def encodeMask(b: Int, mask: List[Int]) = {
      val B = if (ignoreBase(b, mask.head)) 0 else b
      @tailrec
      def maskToIntIter(mask: List[Int], x: Long): Long = {
        mask match {
          case Nil => x
          case s :: tail =>
            assert(s >= B)
            maskToIntIter(tail, x | (1L << (s - B)))
        }
      }

      maskToIntIter(mask, 0L)
    }
  }
}
