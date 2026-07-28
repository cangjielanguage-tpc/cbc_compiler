/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode._

/** DSL for specifying shifted/extended argument of arithmetic and logical instructions.
  *
  * {{{
  * Mode of expression      ARM std asm syntax      Arg expression
  * --------------------    --------------------    -----------------
  * Register:               Rm                      Rm
  * Shifted register:       Rm, `shift` #amount     R(Rm, `shift`, amount)
  * Extended register:      Rm, `extend`            R(Rm, `extend`)
  * Extended & shifted:     Rm, `extend` #amount    R(Rm, `extend`, amount)
  *
  * --------------------------------------------------------------------
  * }}}
  * DSL for specifying memory argument of ARM64 load/store instructions.
  * Various addressing modes may be specified as given below.
  *
  * {{{
  * Name of AddrMode        ARM std asm syntax      Arg expression
  * --------------------    --------------------    -----------------
  * Base + imm offset:      [Xn, #imm]              M(Xn, imm)
  * Pre-indexed:            [Xn, #imm]!             M(PRE_IDX, Xn, imm)
  * Post-indexed:           [Xn], #imm              M(POST_IDX, Xn, imm)
  * Base + reg              [Xn, Xm]                M(Xn, Xm)
  * Base + scaled reg       [Xn, Xm, LSL #imm]      M(Xn, scaled(Xm))
  * Base + extended reg:    [Xn, Wm, UXTW]          M(Xn, uxtw(Wm))
  * Base + ext.&scaled:     [Xn, Wm, SXTW #imm]     M(Xn, scaled(sxtw(Wm)))
  * --------------------
  * Exotic & rarely used cases:
  * unscaled imm offset:    ldur Rt, [Xn, #imm]     ldr(Rt, M(UNSCALED, Xn, imm))
  * base + SXTX:            [Xn, Xm, SXTX]          M(Xn, sxtx(Xm))
  * base + SXTX scaled:     [Xn, Xm, SXTX #imm]     M(Xn, scaled(sxtx(Xm)))
  * }}}
  *
  * @author paul
  */
object Arg {
  ////////////////////////////////////////////////////////////////////////////////
  // API for defining shifted/extended register arguments

  def R(rm: IRegister, mode: ShiftMode, amount: Int): ShiftedReg = new ShiftedReg(rm, mode, amount)

  def R(rm: IRegister, mode: ExtendMode, shift: Int): ExtendedReg = new ExtendedReg(rm, mode, shift)

  def R(rm: IRegister, mode: ExtendMode): ExtendedReg = R(rm, mode, 0)

  ////////////////////////////////////////////////////////////////////////////////
  // API for defining memory arguments (load/store addressing modes)

  def M(mode: MemAddrMode, rn: IRegister.X, offset: Int): MemRI = new MemRI(mode, rn, offset)
  def M(rn: IRegister.X, offset: Int): MemRI = M(REG_IMM, rn, offset)
  def M(rn: IRegister.X): MemRI = M(REG_IMM, rn, 0)

  def uxtw(rm: IRegister.W): MemIndex = new MemIndex(rm, false)
  def sxtw(rm: IRegister.W): MemIndex = new MemIndex(rm, true)
  def sxtx(rm: IRegister.X): MemIndex = new MemIndex(rm, true)

  def scaled(rm: IRegister.X): MemIndex = new MemIndex(rm, false, true)
  def scaled(ex: MemIndex): MemIndex = new MemIndex(ex.rm, ex.signExt, true)

  def M(rn: IRegister.X, rm: IRegister.X): MemRR = new MemRR(rn, rm)
  def M(rn: IRegister.X, ex: MemIndex): MemRR = new MemRR(rn, ex.rm, ex.signExt, ex.scaled)

  ////////////////////////////////////////////////////////////////////////////////
  // Implementation: shifted/extended register

  /** Right argument of add/sub instruction: register, optionally shifted or extended. */
  trait RArith

  /** Right argument of logical instruction: optionally shifted register. */
  trait RLogical

  final class ShiftedReg(val rm: IRegister, val mode: ShiftMode, val amount: Int) extends RArith with RLogical

  final class ExtendedReg(val rm: IRegister, val mode: ExtendMode, val amount: Int) extends RArith

  ////////////////////////////////////////////////////////////////////////////////
  // Implementation: load/store addressing modes

  abstract sealed class Mem(val mode: MemAddrMode, val rn: IRegister.X)

  final class MemRI(_mode: MemAddrMode, _rn: IRegister.X, val offset: Int) extends Mem(_mode, _rn) {
    assert(mode != REG_REG)
  }

  final class MemRR(_rn: IRegister.X, val rm: IRegister, val signExt: Boolean = false, val scaled: Boolean = false)
    extends Mem(REG_REG, _rn)

  final class MemIndex private[Arg](val rm: IRegister, val signExt: Boolean, val scaled: Boolean = false)
}