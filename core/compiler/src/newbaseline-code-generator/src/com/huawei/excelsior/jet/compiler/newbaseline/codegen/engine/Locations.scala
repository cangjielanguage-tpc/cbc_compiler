/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Location.{AnyReg, FReg, IReg, Mem}
import com.huawei.excelsior.jet.assembler.{AsmType, Location, Width}
import com.huawei.excelsior.jet.codeemitter.CodeEmitter
import com.huawei.excelsior.jet.compiler.Env.{addressSize, tailRegister, targetArch}
import com.huawei.excelsior.jet.compiler.newbaseline.codegen.engine.Locations.{LocationsClass, RegisterClass, getFloatRegSize, isInvalid}

import scala.collection.mutable

object Locations {
  abstract private class LocationsClass[L <: Location] {
    /** Smartly ordered set of locations which are busy right now. */
    val busy = mutable.LinkedHashSet.empty[L]

    def isBusy(loc: L): Boolean = busy.contains(loc)
    def isFree(loc: L): Boolean

    def isAllocatable(loc: L): Boolean

    def acquire(loc: L): Unit
    def release(loc: L): Unit
  }

  private class MemoryClass extends LocationsClass[Mem] {
    override def isFree(loc: Mem): Boolean = !isBusy(loc)

    override def isAllocatable(loc: Mem): Boolean = true // every memory location is allocatable

    override def acquire(loc: Mem): Unit = busy.add(loc) ensuring (_ == true)
    override def release(loc: Mem): Unit = busy.remove(loc) ensuring (_ == true)
  }

  private class RegisterClass[LR <: AnyReg] (
    /** Size of register. */
    val regSize: Int,
    availableRegs: IterableOnce[LR]
  ) extends LocationsClass[LR] {

    /** Smartly ordered set of registers which are free right now. */
    val free = mutable.LinkedHashSet.empty[LR] ++ availableRegs

    /** Delays spill of `reg`. */
    def delaySpill(reg: LR): Unit = {
      val wasUsed = busy.remove(reg)
      if (wasUsed) busy.add(reg) // move reg to the end of regs.busy
    }

    override def isFree(loc: LR): Boolean = free.contains(loc)

    override def isAllocatable(loc: LR): Boolean = free.contains(loc) || busy.contains(loc)

    override def acquire(loc: LR): Unit = {
      free.remove(loc) ensuring (_ == true)
      busy.add(loc) ensuring (_ == true)
    }

    override def release(loc: LR): Unit = {
      busy.remove(loc) ensuring (_ == true)
      free.add(loc) ensuring (_ == true)
    }
  }

  private def getFloatRegSize = {
    import com.huawei.excelsior.common.Arch._
    targetArch match {
      case ARM64 => 8
      case AMD64 => 16
      case CBC => shouldNotReachHere()
    }
  }

  private val INVALID_LOC = Location.INVALID

  /** Special location which cannot hold any value. */
  private[engine] def getInvalid: Location.Other = INVALID_LOC

  def isInvalid(loc: Location) = loc == INVALID_LOC
}

final class Locations(globalLocations: GlobalLocations, emitter: CodeEmitter) {
  private val frame = globalLocations.frame

  private val iRegs = {
    val availableIRegs = mutable.LinkedHashSet.empty[IReg] ++ frame.availableIRegs
    assert(emitter.scratches forall availableIRegs.contains)
    if (frame.abi.hasTail) {
      assert(availableIRegs.contains(tailRegister))
      assert(!emitter.scratches.contains(tailRegister))
      availableIRegs.remove(tailRegister)
    }
    new RegisterClass[IReg](addressSize, availableIRegs filterNot emitter.scratches.contains)
  }

  private val fRegs = new RegisterClass[FReg](getFloatRegSize, frame.availableFRegs)
  private val memory = new Locations.MemoryClass

  // FIXME: this is a cyclic dependency introduced after spill implementation, join them?
  var nodes: Nodes = _

  private def widthOf(`type`: NodeType): Width = {
    import NodeType._
    `type` match {
      case ADDR | THIN | TREF => Width.WPTR
      case INT | FLOAT => Width.W32
      case LONG | DOUBLE => Width.W64
      case LONG_DOUBLE_2 => shouldNotReachHere()
    }
  }

  def sizeOf(`type`: NodeType): Int = sizeOf(widthOf(`type`))

  def sizeOf(width: Width): Int = if (width == Width.WPTR) addressSize else width.nbytes

  private[engine] def fitsToLoc(`type`: NodeType, loc: Location): Boolean = {
    if (isInvalid(loc)) return true
    sizeOf(`type`) <= sizeOf(loc.width)
  }

  def maxNodeAsmType: AsmType = {
    assert(addressSize <= Width.W64.nbytes)
    AsmType.I64
  }

  private def getClassOf[L <: Location](loc: L): LocationsClass[L] = {
    if (loc.isIReg) {
      iRegs.asInstanceOf[LocationsClass[L]]
    } else if (loc.isFReg) {
      fRegs.asInstanceOf[LocationsClass[L]]
    } else if (loc.isMem) {
      memory.asInstanceOf[LocationsClass[L]]
    } else {
      shouldNotReachHere(loc)
    }
  }

  private def getRegistersClassOf[LR <: AnyReg](loc: LR): RegisterClass[LR] = {
    if (loc.isIReg) {
      iRegs.asInstanceOf[RegisterClass[LR]]
    } else if (loc.isFReg) {
      fRegs.asInstanceOf[RegisterClass[LR]]
    } else {
      shouldNotReachHere(loc)
    }
  }

  private def getRegistersClassFor(`type`: NodeType) = {
    assert(`type` != NodeType.LONG_DOUBLE_2)
    if (`type`.isFP) fRegs else iRegs
  }

  /** Returns true iff `loc` is managed by this allocator. */
  def isAllocatable(loc: Location) = getClassOf(loc).isAllocatable(loc)

  /** Returns true iff `loc` is busy. Fails with assert if `loc` is not allocatable. */
  def isBusy(loc: Location) = {
    assert(isAllocatable(loc))
    getClassOf(loc).isBusy(loc)
  }

  /** Returns true iff `loc` is not busy and can be used. Fails with assert if `loc` is not allocatable. */
  def isFree(loc: Location) = {
    assert(isAllocatable(loc))
    getClassOf(loc).isFree(loc)
  }

  /** Set of all locations busy right now. */
  def getAllBusy: collection.Set[Location] = {
    val all = mutable.HashSet.empty[Location]
    all ++= iRegs.busy
    all ++= fRegs.busy
    all ++= memory.busy
    all
  }

  /** Returns any free location without acquiring. Do not spill any nodes.
    * This method is unsafe (result should be bind or acquired after call) so all usages should be very grounded.
    */
  def getAnyFreeLocUnsafe(`type`: NodeType): Location = getAnyFreeLocUnsafe(`type`, _ => false) // TODO-DECAF (use default parameter)

  /** Returns any free location which does not satisfy `excludeRegs` predicate without acquiring. Do not spill any nodes.
    * This method is unsafe (result should be bind or acquired after call) so all usages should be very grounded.
    */
  private[engine] def getAnyFreeLocUnsafe(`type`: NodeType, exclude: AnyReg => Boolean): Location = {
    val regClass = getRegistersClassFor(`type`)
    if (sizeOf(`type`) <= regClass.regSize) {
      for (r <- regClass.free if !exclude(r)) {
        return r
      }
    }
    getAnyFreeMemUnsafe(`type`)
  }

  /** Returns memory location without acquiring. Do not spill any nodes.
    * This method is unsafe (result should be bind or acquired after call) so all usages should be very grounded.
    */
  def getAnyFreeMemUnsafe(`type`: NodeType): Mem = getAnyFreeMemUnsafe(`type`.toAsm)

  // TODO: reuse free frame slots (and review all manual allocations)
  private[engine] def getAnyFreeMemUnsafe(`type`: AsmType): Mem = globalLocations.allocateOnStackUntraced(`type`)

  /** Delays spill of `reg`. */
  private[engine] def delaySpill[LR <: AnyReg](reg: LR): Unit = getRegistersClassOf(reg).delaySpill(reg)

  /** Returns any free register without acquiring. May spill some other node.
    * This method is unsafe (result should be bind or acquired after call) so all usages should be very grounded.
    */
  private[engine] def getAnyFreeRegUnsafe[LR <: AnyReg](`type`: NodeType): LR = {
    val regClass = getRegistersClassFor(`type`).asInstanceOf[RegisterClass[LR]]
    assert(sizeOf(`type`) <= regClass.regSize)
    if (regClass.free.isEmpty) {
      val regToSpill = regClass.busy.head
      val nodeToSpill = nodes.getNodeAt(regToSpill)
      val spillLoc = globalLocations.allocateOnStackUntraced(nodeToSpill.asmType)
      nodes.transfer(nodeToSpill, spillLoc)
    }
    regClass.free.head
  }

  /** Returns any free register without acquiring. Tries to use one of the `preferred` registers. May spill some other node.
    * This method is unsafe (result should be bind or acquired after call) so all usages should be very grounded.
    */
  private[engine] def getAnyFreeIRegUnsafeWithPreferred(`type`: NodeType, preferred: IReg*): IReg = {
    assert(sizeOf(`type`) <= iRegs.regSize)
    preferred find isFree getOrElse getAnyFreeRegUnsafe[IReg](`type`)
  }

  def acquire(loc: Location): Any = if (!isInvalid(loc)) {
    assert(isAllocatable(loc))
    getClassOf(loc).acquire(loc)
    if (loc.isReg) {
      frame.registerUsedReg(loc.asReg)
    }
  }

  def release(loc: Location): Unit = if (!isInvalid(loc)) {
    getClassOf(loc).release(loc)
  }
}
