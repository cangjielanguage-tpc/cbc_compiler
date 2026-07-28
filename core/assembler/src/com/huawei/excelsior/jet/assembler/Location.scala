/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.AsmType.NONE
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg, Mem}
import com.huawei.excelsior.jet.assembler.Width.BYTE

/** Abstract value location in machine.
  *
  * There are three basic types of locations: [[IReg]], [[FReg]] and [[Mem]].
  *
  * @author cypok
  * @author paul
  * @author conwor
  */
trait Location {
  def width: Width

  def isReg: Boolean
  def isIReg: Boolean
  def isFReg: Boolean
  def isMem: Boolean

  def asReg: AnyReg
  def asIReg: IReg
  def asFReg: FReg
  def asMem: Mem
}

object Location {
  trait AnyReg extends Location {
    // Deprecated is used to indicate that these methods should not be called non-virtually
    @deprecated override def isReg: Boolean = true
    @deprecated override def asReg: AnyReg = this
    @deprecated override def isMem: Boolean = false
    @deprecated override def asMem: Mem = shouldNotCallThis()
  }

  /** Integral location with fast access but their count may be limited. */
  trait IReg extends AnyReg {
    @deprecated override def isIReg: Boolean = true
    @deprecated override def asIReg: IReg = this
    @deprecated override def isFReg: Boolean = false
    @deprecated override def asFReg: FReg = shouldNotCallThis()
  }

  /** Floating-point location with fast access but their count may be limited. */
  trait FReg extends AnyReg {
    @deprecated override def isFReg: Boolean = true
    @deprecated override def asFReg: FReg = this
    @deprecated override def isIReg: Boolean = false
    @deprecated override def asIReg: IReg = shouldNotCallThis()
  }

  /** Location with slow access but their count is practically unlimited. */
  trait Mem extends Location {
    def `type`: AsmType
    override def width = `type`.width

    def field(`type`: AsmType, disp: Int): Mem // TODO: assert field lies within this location
    def as(`type`: AsmType) = field(`type`, 0)

    @deprecated override def isMem  : Boolean = true
    @deprecated override def asMem  : Mem = this
    @deprecated override def isReg  : Boolean = false
    @deprecated override def isFReg : Boolean = false
    @deprecated override def isIReg : Boolean = false
    @deprecated override def asReg  : AnyReg = shouldNotCallThis()
    @deprecated override def asFReg : FReg = shouldNotCallThis()
    @deprecated override def asIReg : IReg = shouldNotCallThis()
  }

  // TODO: use this class instead of intel AddrMode.Scaled
  case class Scaled(index: IReg, scale: Width)

  def scaled(index: IReg, scale: Width) = Scaled(index, scale)

  def mem(`type`: AsmType, symbol: Symbol, disp: Int) : MemStatic = MemStatic(`type`, symbol, disp)
  def mem(`type`: AsmType, symbol: Symbol)            : MemStatic = mem(`type`, symbol, 0)
  def mem(symbol: Symbol, disp: Int)                  : MemStatic = mem(NONE, symbol, disp)
  def mem(symbol: Symbol)                             : MemStatic = mem(symbol, 0)

  def mem(`type`: AsmType, base: IReg, disp: Int)   : MemBased = MemBased(`type`, base, disp)
  def mem(`type`: AsmType, base: IReg)              : MemBased = mem(`type`, base, 0)
  def mem(base: IReg, disp: Int)                    : MemBased = mem(NONE, base, disp)
  def mem(base: IReg)                               : MemBased = mem(base, 0)

  def mem(`type`: AsmType, base: IReg, scaled: Scaled, disp: Int) : MemBaseIndex = MemBaseIndex(`type`, base, scaled.index, scaled.scale, disp)
  def mem(`type`: AsmType, base: IReg, scaled: Scaled)            : MemBaseIndex = mem(`type`, base, scaled, 0)
  def mem(`type`: AsmType, base: IReg, index: IReg)               : MemBaseIndex = MemBaseIndex(`type`, base, index, BYTE, 0)

  def mem(`type`: AsmType, slot: MemLocal.Slot, disp: Int) : MemLocal = MemLocal(`type`, slot, disp)
  def mem(`type`: AsmType, slot: MemLocal.Slot)            : MemLocal = mem(`type`, slot, 0)

  /** Static memory location addressed by [symbol + disp]. */
  case class MemStatic(`type`: AsmType, symbol: Symbol, disp: Int) extends Mem {
    override def field(`type`: AsmType, disp: Int) = MemStatic(`type`, symbol, this.disp + disp)
  }

  /** Memory location addressed by [base + disp]. */
  case class MemBased(`type`: AsmType, base: IReg, disp: Int) extends Mem {
    override def field(`type`: AsmType, disp: Int) = MemBased(`type`, base, this.disp + disp)
    def disposed(disp: Int) = MemBased(`type`, base, this.disp + disp)
  }

  /** Memory location addressed by [slot + disp], where slot is from method frame. */
  object MemLocal {
    trait Slot {
      def toMemBased(`type`: AsmType, disp: Int): MemBased
    }
  }

  case class MemLocal(`type`: AsmType, slot: MemLocal.Slot, disp: Int) extends Mem {
    override def field(`type`: AsmType, disp: Int) = MemLocal(`type`, slot, this.disp + disp)
    def toMemBased = slot.toMemBased(`type`, disp)
  }

  /** Memory location addressed by [base + index*scale + disp]
    *
    * BaseIndex memory location indicates address in memory, calculated as:
    * `base` + `index` * scale.bytes() + (`disp` sign-extended to `base` type),
    * where `index` used as full-sized register.
    *
    * On 64-bit architectures `index` can hold 64-bit value or 32-bit value, but for amd64 and arm64
    * architectures 64-bit register contains 32-bit negative value with zero extension.
    *
    * So result of BaseIndex memory will be UNDEFINED if `index` is 32-bit negative value.
    */
  case class MemBaseIndex(`type`: AsmType, base: IReg, index: IReg, scale: Width, disp: Int) extends Mem {
    override def field(`type`: AsmType, disp: Int) = MemBaseIndex(`type`, base, index, scale, this.disp + disp)
  }

  /** Any Location, which is not Reg, IReg, FReg or Mem. */
  trait Other extends Location {
    @deprecated override def isReg  : Boolean = false
    @deprecated override def isIReg : Boolean = false
    @deprecated override def isFReg : Boolean = false
    @deprecated override def isMem  : Boolean = false
    @deprecated override def asReg  : AnyReg  = shouldNotCallThis()
    @deprecated override def asIReg : IReg    = shouldNotCallThis()
    @deprecated override def asFReg : FReg    = shouldNotCallThis()
    @deprecated override def asMem  : Mem     = shouldNotCallThis()
  }

  /** A part of some IReg or FReg. */
  trait SubReg[R <: AnyReg] extends Location.Other {
    /** Register which has this sub-register as a part. */
    def host: R
  }

  val INVALID = new Location.Other {
    override def toString = "Location{INVALID}"
    override def width = Width.WNONE
  }
}
