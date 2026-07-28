/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.compiler.bytecode.ArithOp._

/** DSL to create arithmetic nodes. */
trait ArithNodesDSL extends IRBuilderDSL {

  // Naming inconsistency is due to historical accident
  def iarg: Param = Param(IntType, 7)
  def larg: Param = Param(LongType, 5)
  def farg: Param = Param(FloatType, 3)
  def darg: Param = Param(DoubleType, 1)

  def ic(c: Int): Node    = IConst(c)
  def lc(c: Long): Node   = LConst(c)
  def fc(c: Float): Node  = FConst(c)
  def dc(c: Double): Node = DConst(c)
  def neg(x: Node): Node  = Neg(x.tpe)(x)

  def nullAddr = IntegralConst(AddrType)(0)

  def add(x: Node, y: Node): Node = Add(x, y)
  def sub(x: Node, y: Node): Node = Sub(x, y)

  def mul(x: Node, y: Node): Node = Mul(x, y)

  def lsl(x: Node, y: Node): Node  = Shift(LSL, x, y)
  def asr(x: Node, y: Node): Node  = Shift(ASR, x, y)
  def lsr(x: Node, y: Node): Node = Shift(LSR, x, y)

  def mulh(x: Node, y: Node): Node  = MulH(x, y)
  def umulh(x: Node, y: Node): Node = UMulH(x, y)

  def div(x: Node, y: Node): Node  = IDiv(x.tpe)(entryBlock, x, y)
  def udiv(x: Node, y: Node): Node = UDiv(x.tpe)(entryBlock, x, y)

  def rem(x: Node, y: Node): Node  = IRem(x.tpe)(entryBlock, x, y)
  def urem(x: Node, y: Node): Node = URem(x.tpe)(entryBlock, x, y)

  def and(x: Node, y: Node): Node = And(x, y)
  def or(x: Node, y: Node): Node = Or(x, y)
  def xor(x: Node, y: Node): Node = Xor(x, y)
}
