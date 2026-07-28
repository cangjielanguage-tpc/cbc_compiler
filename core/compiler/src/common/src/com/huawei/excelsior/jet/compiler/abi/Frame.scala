/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.{AsmType, Location}
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.abi.Frame.Mode.SPECIAL_FOR_THUNK
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot.UNKNOWN_OFFSET
import com.huawei.excelsior.jet.compiler.abi.frame.{FrameCodeGen, FrameDebug, FrameElements, FrameLayout}
import com.huawei.excelsior.jet.compiler.ir.XInfo
import xscala.util.hash

/** Frame for abstract platform.
  *
  * {{{
  * | ...                             |
  * |---------------------------------| <- SP after caller's frame build
  * | ...                             |                                          \                           \                       \
  * | ...                             | <- SP after caller's call instruction    | preHeaderSize             |                       |
  * | ...                             |                                          /                           |                       |
  * |---------------------------------|                                                                      | headerSize            |
  * | saved pushable registers        | <- FP (frame pointer register, optional) \                           |                       |
  * | ...                             |                                          | savedPushableRegsSize     |                       |
  * | ...                             |                                          /                           /                       |
  * |---------------------------------|                                                                                              | frameSize
  * | saved non-pushable registers    |                                          \                           \                       |
  * | ...                             |                                          | savedNonPushableRegsSize  |                       |
  * | ...                             |                                          /                           |                       |
  * |---------------------------------|                                                                      |                       |
  * | frame slots (spill) &           |                                          \                           |                       |
  * | stack alloc results             |                                          | stackAllocSize            |                       |
  * | ...                             |                                          /                           | bodySize              |
  * |---------------------------------| <- FMR (frame middle register, baseline only)                        |                       |
  * | alignment                       |                             \                                        |                       |
  * |---------------------------------|                             |                                        |                       |
  * | reserved area above SP          |    \                        | extraAllocSize                         |                       |
  * | ...                             |    | paramPassingAreaSize   |                                        |                       |
  * | ...                             |    |                        |                                        |                       |
  * | ...                             |    |                        |                                        |                       |
  * | ...                             |    /                        /                                        /                       /
  * |---------------------------------| <- SP after frame build (aligned to frameAlignment)
  * | ...                             |                                                                                              \
  * | ...                             |                                                                                              | additionalStackCheckSize
  * | ...                             | <- Last stack check                                                                          /
  * | ...                             |
  * }}}
  *
  * All sizes are taken in bytes.
  *
  * Difference between "header" and "body" groups made by type of stack allocation. Slots in "header"
  * allocated with assembler instructions "push", slots in "body" are allocated with assembler instruction
  * "sub SP", which means that we should make explicit stack checks to bodySize after header allocation.
  *
  * Float registers may be saved with push instructions (ARM64) or str (AMD64). In first case they are included
  * in header, in second - in body.
  *
  * Baseline-specific:
  *   1. FMR - Frame Middle Reg. Used to create address modes to spill slots during code generation before
  *   all frame sizes are known.
  *
  * Opt-specific:
  *   1. Lightweight frame (without fixed prologue & bodySize)
  *   1. No frame descriptor optimization
  *   1. No calls in method optimization
  *
  * FP (frame pointer register) is optional. If it is used, it points to the following structure:
  *
  * {{{
  *   | caller frame descriptor |
  *   |-------------------------|
  *   | return address          |
  *   | caller FP               |
  *   |-------------------------| <- FP
  * }}}
  *
  * On all architectures caller return address placed below caller frame descriptor (in JET ABI), so it can be
  * accessed through FP too.
  *
  * @author conwor
  * @author paul
  */
object Frame {
  object Slot {
    private[Frame] val UNKNOWN_OFFSET = Int.MinValue
  }

  /** Frame slot (abstraction of spill and AJ stack alloc) addressed by base and offset. */
  abstract class Slot private[abi](val size: Int, val alignment: Int, val tracedByHeader: Boolean) extends MemLocal.Slot, XInfo.Slot {
    private var _base: SlotBase = _
    private var _offset: Int = UNKNOWN_OFFSET

    def isBound: Boolean = _base != null && _offset != UNKNOWN_OFFSET

    def base   = { assert(isBound, s"base and offset were not yet defined for $this"); _base }
    def offset = { assert(isBound, s"base and offset were not yet defined for $this"); _offset }

    def bind(base: SlotBase, offset: Int): Unit = {
      assert(!isBound)
      this._base = base
      this._offset = offset
      assert(isBound)
    }

    override def toString = {
      val addr = if (isBound) base.toString + (if (offset < 0) "" else "+") + offset else "UNKNOWN_OFFSET"
      val align = if (size != alignment) s", align=$alignment" else ""
      s"Slot[$addr, $size bytes$align]"
    }

    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if this eq that => true
      case that: Slot => (this._base eq that._base) && (this._offset == that._offset) && (this.size == that.size) && (this.alignment == that.alignment) && (this.tracedByHeader == that.tracedByHeader)
      case _ => false
    }

    override def hashCode = hash(base, offset, size, alignment, tracedByHeader)

    def as(`type`: AsmType): MemLocal = field(`type`, 0)

    def field(`type`: AsmType, disp: Int): MemLocal = {
      assert(`type`.width.nbytes + disp <= size)
      mem(`type`, this, disp)
    }

    protected def baseRegister(): IReg

    override final def toMemBased(`type`: AsmType, disp: Int): MemBased = mem(`type`, baseRegister(), offset + disp)

    /** For GC maps base is always the same, so slots in XInfo can be distinguished by the offset only. */
    override def order = offset
  }

  enum Mode {
    /** Method's frame can reserve arbitrary amount of memory. */
    case FULL

    /** Method's frame can contain saved registers only. It is an optimization for leaf methods without throwable
      * operations, stack alloc or spill. */
    case LIGHTWEIGHT

    /** Frame must be empty. Used for tail-jump thunks. NOTE: thunks may use push and pop and even calls (e.g.
      * genInterfaceJump in ThunkGenerator).
      * TODO: rewrite thunks, using normal frame with tail jump like in [[HookInvokerGenerator]] and remove this mode. */
    case SPECIAL_FOR_THUNK
  }
}

abstract class Frame[IR >: Null <: IReg, FR <: FReg, XABI <: ABI[IR, FR]] protected(
  protected val env: Environment,
  protected val symbolLinker: SymbolLinker,
  protected val properties: FrameProperties,
  protected val useFramePointer: Boolean,
  protected val useFMRAddressing: Boolean)
    extends FrameElements[IR, FR, XABI] with FrameLayout[IR, FR, XABI] with FrameCodeGen[IR, FR, XABI] with FrameDebug {

  protected implicit val typeProvider: TypeProvider = env.getTypeProvider

  val abi: XABI = targetPlatform.abi(properties.getRealMethodType(varArgs = null)).asInstanceOf[XABI]

  protected def preHeaderSize: Int

  // TODO: do not make push on arm64 and remove this
  protected def fRegsArePushable: Boolean

  protected def framePointerSetupOffset: Int // TODO: refactor frame description and combine this with preHeaderSize
}
