/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STRICT_MEM_MASK
import com.huawei.excelsior.jet.codeemitter.BranchOp.*
import com.huawei.excelsior.jet.codeemitter.CodeEmitter.{ShiftKind, adjustWidthToStandardReg, verifyImmForCompare, verifyStandardWidth}
import xscala.util.MathUtils.*

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.annotation.nowarn
import scala.math.toIntExact
import scala.util.chaining.scalaUtilChainingOps

/** Platform-independent assembler-like code generator.
  *
  * @author conwor
  * @author paul
  */
@nowarn("msg=match may not be exhaustive")
object CodeEmitter {
  /** Checks that `width` is standard for public API of CodeEmitter (W32/W64/WPTR). */
  private[codeemitter] def verifyStandardWidth(width: Width): Unit =
    assert((width == W32) || (width == W64) || (width == WPTR))

  private[codeemitter] def adjustWidthToStandardReg(width: Width): Width = width match {
    case W8 | W16 => W32
    case W32 | W64 | WPTR => width
  }

  private[codeemitter] def verifyImmForCompare(imm: Long, op: BranchOp, size: Int, signed: Boolean): Unit =
    assert(size > 4 || isNBits(signed, toIntExact(imm), size * 8))

  object SignedI32 {
    def unapply(x: Long): Option[Int] = if (x.toInt == x) Some(x.toInt) else None
  }

  private[codeemitter] enum ShiftKind {
    case LEFT
    case RIGHT
    case ARITH
  }
}

@nowarn("msg=match may not be exhaustive")
abstract class CodeEmitter protected(addressSizeInBits: Int, protected val scratchProvider: ScratchProvider, protected val symbolInfo: SymbolInfo,
                                     _impl: AsmEmitter) extends Emitter.Delegate(_impl) {

  assert(addressSizeInBits == 32 || addressSizeInBits == 64, addressSizeInBits)
  private val is32Bit = addressSizeInBits == 32

  private def asm = impl.asInstanceOf[AsmEmitter]

  final def acquireScratch() = scratchProvider.acquireScratch()
  final def releaseScratch(scratch: IReg): Unit = scratchProvider.releaseScratch(scratch)

  /** Returns true iff this emitter allowed to spoil `r` register. */
  final def canSpoil(r: IReg) = scratchProvider.contains(r)

  /** Returns array of all scratches used in this emitter. */
  final def scratches: Array[IReg] = scratchProvider.allScratches

  /** Takes one scratch from available, implements action with it and then returns it back. */
  final def borrowScratch(action: IReg => Unit): Unit = {
    val scratch = acquireScratch()
    try action(scratch) finally releaseScratch(scratch)
  }

  /** Appends `scratch` to provider, implements `action`, than take `scratch` back from provider. */
  final def withScratch(scratch: IReg)(action: => Unit): Unit = {
    scratchProvider.appendScratch(scratch)
    try action finally scratchProvider.removeScratch(scratch)
  }

  /** Removes `scratch` from provider if it exists, implements `action`,
    * than appends `scratch` back to provider if it was taken.
    */
  final def withoutScratch(scratch: IReg)(action: => Unit): Unit = {
    val spoiled = canSpoil(scratch)
    if (spoiled) scratchProvider.removeScratch(scratch)
    try action finally if (spoiled) scratchProvider.appendScratch(scratch)
  }

  protected final def borrowScratches(action: (IReg, IReg) => Unit): Unit =
    borrowScratch { s1 => borrowScratch { s2 => action(s1, s2) } }

  protected final def withMemOnScratch(mem: Mem)(action: IReg => Unit): Unit =
    borrowScratch { tmp => load(tmp, mem); action(tmp) }

  protected final def withImm32OnScratch(imm: Int)(action: IReg => Unit): Unit =
    borrowScratch { tmp => mov32(tmp, imm); action(tmp) }

  protected final def withImm64OnScratch(imm: Long)(action: IReg => Unit): Unit =
    borrowScratch { tmp => mov64(tmp, imm); action(tmp) }

  protected final def withSymbolOnScratch(sym: Symbol)(action: IReg => Unit): Unit =
    borrowScratch { tmp => lea(tmp, sym); action(tmp) }

  protected final def withImmOnScratch(imm: Long, width: Width)(action: IReg => Unit): Unit = size(width) match {
    case 8          => withImm64OnScratch(imm) { action }
    case 4 | 2 | 1  => withImm32OnScratch(imm.toInt) { action }
  }

  protected final def size(width: Width): Int = (if (width == WPTR) if (is32Bit) W32 else W64 else width).nbytes

  protected final def size(loc: Location): Int = size(loc.width)

  protected final def size(`type`: AsmType): Int = size(`type`.width)

  protected final def isFarAccess(target: Symbol) = !target.isInstanceOf[Label] && symbolInfo.isFarAccess(target)

  private def half32Type(`type`: AsmType): AsmType = `type` match {
    case I64 => I32
    case U64 => U32
    case F64 => F32
  }

  protected def low32Field(m: Mem) = m.field(half32Type(m.`type`), 0)
  protected def high32Field(m: Mem) = m.field(half32Type(m.`type`), 4)


  ///////////////////////////////////////////////////////////////////////////
  // AsmEmitter-like interface

  final def appendCode(code: Segment): Unit = asm.appendCode(code)
  final def alignCode(alignment: Int): Unit = asm.alignCode(alignment)


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers without link

  def jump(target: IReg): Unit
  def jump(target: Symbol): Unit
  def jumpIndirect(target: Mem): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Unconditional control transfers with link

  def call(target: IReg): Unit
  def call(target: Symbol): Unit
  def callIndirect(target: Mem): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Conditional (register with register) control transfers

  def branchIf(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit
  def branchIf(op: BranchOp, arg1: FReg, arg2: FReg, width: Width, target: Label): Unit

  // TODO: refactor this (more details in BranchOp TODO)
  def branchIfTest(op: BranchOp, arg1: IReg, arg2: IReg, width: Width, target: Label): Unit

  ///////////////////////////////////////////////////////////////////////////
  // Conditional (register with immediate) control transfers

  def branchIf(op: BranchOp, arg1: IReg, arg2: Long, width: Width, target: Label): Unit

  // TODO: refactor this (more details in BranchOp TODO)
  def branchIfTest(op: BranchOp, arg1: IReg, arg2: Long, width: Width, target: Label): Unit

  final def branchIfNull    (arg: IReg, target: Label): Unit = branchIf(EQ, arg, 0, WPTR, target)
  final def branchIfNotNull (arg: IReg, target: Label): Unit = branchIf(NE, arg, 0, WPTR, target)


  ///////////////////////////////////////////////////////////////////////////
  // Conditional (memory with immediate) control transfers

  def branchIf(arg1: Mem, op: BranchOp, arg2: Long, target: Label): Unit = {
    verifyImmForCompare(arg2, op, size(arg1.`type`), arg1.`type`.signed)
    // Filter out unsupported cases. Feel free to support them.
    if (size(arg1) < 4) op match {
      case  GT |  LT |  GE |  LE => assert(arg1.`type`.signed)
      case UGT | ULT | UGE | ULE => assert(!arg1.`type`.signed)
      case _ =>
    }
    val scratchWidth = adjustWidthToStandardReg(arg1.width)
    withMemOnScratch(arg1) { tmp => branchIf(op, tmp, arg2, scratchWidth, target) }
  }

  final def branchIfNull    (arg: Mem, target: Label): Unit = branchIf(arg ensuring (_.`type` == PTR), EQ, 0, target)
  final def branchIfNotNull (arg: Mem, target: Label): Unit = branchIf(arg ensuring (_.`type` == PTR), NE, 0, target)


  ///////////////////////////////////////////////////////////////////////////
  // Move from register to register

  protected def movImpl (dst: IReg, src: IReg, width: Width): Unit
  protected def fmovImpl(dst: FReg, src: FReg, width: Width): Unit

  final def mov (dst: IReg, src: IReg, width: Width): Unit = if (dst != src) movImpl (dst, src, width)
  final def fmov(dst: FReg, src: FReg, width: Width): Unit = if (dst != src) fmovImpl(dst, src, width)

  final def mov32 (dst: IReg, src: IReg): Unit = mov  (dst, src, W32)
  final def mov64 (dst: IReg, src: IReg): Unit = mov  (dst, src, W64)
  final def fmov32(dst: FReg, src: FReg): Unit = fmov (dst, src, W32)
  final def fmov64(dst: FReg, src: FReg): Unit = fmov (dst, src, W64)

  final def mov (dst: IReg, src: IReg): Unit = { assert(dst.width == src.width); mov (dst, src, dst.width) }
  final def fmov(dst: FReg, src: FReg): Unit = { assert(dst.width == src.width); fmov(dst, src, dst.width) }


  ///////////////////////////////////////////////////////////////////////////
  // Move immediate to register

  protected def movImpl(dst: AnyReg, imm: Long, width: Width): Unit

  final def mov(dst: AnyReg, imm: Long, width: Width): Unit = {
    assert(isNBitsSigned(imm, size(width tap verifyStandardWidth) * 8))
    movImpl(dst, imm, width)
  }

  final def mov32 (dst: IReg, imm: Int) : Unit = mov(dst, imm, W32)
  final def mov64 (dst: IReg, imm: Long): Unit = mov(dst, imm, W64)
  final def fmov32(dst: FReg, imm: Int) : Unit = mov(dst, imm, W32)
  final def fmov64(dst: FReg, imm: Long): Unit = mov(dst, imm, W64)

  final def fmov32(dst: FReg, imm: Float) : Unit = fmov32(dst, floatToRawIntBits(imm))
  final def fmov64(dst: FReg, imm: Double): Unit = fmov64(dst, doubleToRawLongBits(imm))

  final def movPtr(dst: IReg, imm: Long): Unit = mov(dst, imm, WPTR)
  final def movNull(dst: IReg): Unit = movPtr(dst, 0)


  ///////////////////////////////////////////////////////////////////////////
  // Move from register of one file to register of another file

  def mov(dst: IReg, src: FReg, width: Width): Unit
  def mov(dst: FReg, src: IReg, width: Width): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Swap contents of two registers

  /** Swap `r1` & `r2`. Always exchange full IRegs so both values retain all bits. */
  def swap(r1: IReg, r2: IReg): Unit

  /** Swap `r1` & `r2`. Width is used only as a hint, both values retain all bits after swap. */
  def fswap(r1: FReg, r2: FReg, width: Width): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Load effective address

  def lea(dst: IReg, src: Mem): Unit

  final def lea(dst: IReg, src: Symbol): Unit = lea(dst, mem(NONE, src))
  final def lea(dst: IReg, slot: MemLocal.Slot): Unit = lea(dst, mem(NONE, slot))


  ///////////////////////////////////////////////////////////////////////////
  // Load label absolute address or offset in method segment

  def loadLabelPosition(dst: IReg, src: Label): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Load/Store (memory <-> register)

  def load(dst: AnyReg, src: Mem): Unit
  def store(dst: Mem, src: AnyReg): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Store immediate to memory

  protected def storeImpl(dst: Mem, imm: Long): Unit

  final def store(dst: Mem, imm: Long): Unit = {
    assert(isNBits(dst.`type`.signed, imm, size(dst) * 8))
    storeImpl(dst, imm)
  }

  final def storeNull(dst: Mem): Unit = {
    assert(size(dst.`type`) == size(PTR))
    store(dst, 0)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Universal mov between two locations (registers or memory)

  protected def copyMem(dst: Mem, src: Mem): Unit = {
    assert(dst.`type` == src.`type`)
    borrowScratch { tmp => load(tmp, src); store(dst, tmp) }
  }

  final def copyAny(dst: Location, src: Location, `type`: AsmType): Unit =
    if (dst == src) { /* nop */ }

    else if (dst.isIReg && src.isIReg)  mov  (dst.asIReg, src.asIReg, `type`.width)
    else if (dst.isIReg && src.isFReg)  mov  (dst.asIReg, src.asFReg, `type`.width)
    else if (dst.isFReg && src.isIReg)  mov  (dst.asFReg, src.asIReg, `type`.width)
    else if (dst.isFReg && src.isFReg)  fmov (dst.asFReg, src.asFReg, `type`.width)

    else if (dst.isReg  && src.isMem)   load    (dst.asReg,            src.asMem.as(`type`))
    else if (dst.isMem  && src.isReg)   store   (dst.asMem.as(`type`), src.asReg)
    else if (dst.isMem  && src.isMem)   copyMem (dst.asMem.as(`type`), src.asMem.as(`type`))

    else shouldNotReachHere()


  ///////////////////////////////////////////////////////////////////////////
  // Untyped mov between two locations (registers or memory)
  // Type of copied value calculated from `dst` and `src` locations:
  //   - If anyone of them is memory, type taken from it.
  //   - If both of them are memory, their types should be the same.
  //   - If they are registers from the same file, natural type of this file is used.
  //   - If they are from the different register files, this method is forbidden.

  final def copyAny(dst: Location, src: Location): Unit =
    if (dst == src) { /* nop */ }
    else if (dst.isIReg && src.isIReg)  mov     (dst.asIReg,  src.asIReg)
    else if (dst.isFReg && src.isFReg)  fmov    (dst.asFReg,  src.asFReg)
    else if (dst.isReg  && src.isMem)   load    (dst.asReg,   src.asMem)
    else if (dst.isMem  && src.isReg)   store   (dst.asMem,   src.asReg)
    else if (dst.isMem  && src.isMem)   copyMem (dst.asMem,   src.asMem)
    else if (dst.isIReg && src.isFReg)  shouldNotReachHere("type must be specified, when FReg moved to IReg")
    else if (dst.isFReg && src.isIReg)  shouldNotReachHere("type must be specified, when IReg moved to FReg")
    else shouldNotReachHere()


  ///////////////////////////////////////////////////////////////////////////
  // Untyped mov between two memory locations of given size.

  // TODO: call memCopy for sizes greater than some limit
  def copyMem(dst: Mem, src: Mem, nbytes: Int): Unit = {

    def copyTail(asmType: AsmType, dst: Mem, src: Mem, tail: Int, disp: Int): Boolean = {
      if (tail >= size(asmType)) {
        copyAny(dst.field(asmType, disp), src.field(asmType, disp))
        return true
      }
      false
    }

    var copied = 0

    while (copied < alignDown(nbytes, size(PTR))) {
      copyAny(dst.field(PTR, copied), src.field(PTR, copied))
      copied += size(PTR)
    }

    var tail = nbytes - copied
    assert(tail == nbytes % size(PTR))
    assert(tail >= 0)

    if (copyTail(U32, dst, src, tail, copied)) {
      tail -= size(U32)
      copied += size(U32)
    }

    if (copyTail(U16, dst, src, tail, copied)) {
      tail -= size(U16)
      copied += size(U16)
    }

    if (copyTail(U8, dst, src, tail, copied)) {
      tail -= size(U8)
      copied += size(U8)
    }

    assert(tail == 0)
    assert(copied == nbytes)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic with registers

  def add (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def sub (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def mul (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def and (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def or  (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def xor (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit

  def div (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def udiv(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def rem (dst: IReg, src1: IReg, src2: IReg, width: Width): Unit
  def urem(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit

  final def add32(dst: IReg, src1: IReg, src2: IReg): Unit = add(dst, src1, src2, W32)
  final def add64(dst: IReg, src1: IReg, src2: IReg): Unit = add(dst, src1, src2, W64)
  // TODO consider to addPtr(dst: IReg, src1: IReg, src2: IReg) as the version with immediate exists

  def fadd(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit
  def fsub(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit
  def fmul(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit
  def fdiv(dst: FReg, src1: FReg, src2: FReg, width: Width): Unit


  ///////////////////////////////////////////////////////////////////////////
  // Arithmetic register with immediate

  protected def addImpl(dst: IReg, src: IReg, imm: Long, width: Width): Unit
  def mul (dst: IReg, src1: IReg, src2: Long, width: Width): Unit
  def and (dst: IReg, src1: IReg, src2: Long, width: Width): Unit
  def or  (dst: IReg, src1: IReg, src2: Long, width: Width): Unit
  def xor (dst: IReg, src1: IReg, src2: Long, width: Width): Unit

  final def add(dst: IReg, src: IReg, imm: Long, width: Width): Unit =
    if (imm == 0L) mov(dst, src, width) else addImpl(dst, src, imm, width)

  final def add32 (dst: IReg, src: IReg, imm: Int):  Unit = add(dst, src, imm, W32)
  final def add64 (dst: IReg, src: IReg, imm: Long): Unit = add(dst, src, imm, W64)
  final def addPtr(dst: IReg, src: IReg, imm: Long): Unit = add(dst, src, imm, WPTR)

  def sub(dst: IReg, src: IReg, imm: Long, width: Width): Unit = add(dst, src, if (width == W32) -imm.toInt.toLong else -imm, width)

  ///////////////////////////////////////////////////////////////////////////
  // Shifts

  protected def shift(dst: IReg, src1: IReg, src2: Int, width: Width, tp: ShiftKind): Unit
  protected def shift(dst: IReg, src1: IReg, src2: IReg, width: Width, tp: ShiftKind): Unit

  def lsl(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.LEFT)
  def lsr(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.RIGHT)
  def asr(dst: IReg, src1: IReg, src2: IReg, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.ARITH)

  def lsli(dst: IReg, src1: IReg, src2: Int, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.LEFT)
  def lsri(dst: IReg, src1: IReg, src2: Int, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.RIGHT)
  def asri(dst: IReg, src1: IReg, src2: Int, width: Width): Unit = shift(dst, src1, src2, width, ShiftKind.ARITH)


  ///////////////////////////////////////////////////////////////////////////
  // Memory barrier

  protected def memBarrierImpl(mask: Int): Unit

  final def memBarrier(kinds: BarrierKind*): Unit = {
    val mask = BarrierKind.toMask(kinds*) & ~STRICT_MEM_MASK
    if (mask != 0) memBarrierImpl(mask) // nothing to do for set of STRICT_MEM
  }
}
