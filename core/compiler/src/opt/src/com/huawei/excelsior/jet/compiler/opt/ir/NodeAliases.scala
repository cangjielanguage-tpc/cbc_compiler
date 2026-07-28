/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.bytecode.ArithOp

import scala.PartialFunction.condOpt

/**
 * Handy aliases for some nodes.
 *
 * @author paul
 * @author cypok
 * @author kit
 * @author alexm
 * @author liontiger
 */
trait NodeAliases { self: Universe with Nodes with Types =>

  object PowerOfTwo {
    import java.lang.Long.{numberOfLeadingZeros => nlz}
    def apply(i: Int): Long = 1L << i
    def unapply(x: Long): Option[Int] =
      if (((x & (x-1)) == 0) && (x > 0)) Some(63 - nlz(x)) else None
  }

  object NegPowerOfTwo {
    def apply(i: Int): Long = -1L << i
    def unapply(x: Long): Option[Int] = condOpt(-x) {
      case Long.MinValue => 63
      case PowerOfTwo(p) => p
    }
  }

  def lsl(v: Node, c: Int) = Shift(ArithOp.LSL, v, IConst(c))
  def asr(v: Node, c: Int) = Shift(ArithOp.ASR, v, IConst(c))
  def lsr(v: Node, c: Int) = Shift(ArithOp.LSR, v, IConst(c))
  def and(v: Node, c: Long) = And(v, IntegralConst(v.tpe)(c))
  def imulh(a: Node, c: Long) = MulH(a, IntegralConst(a.tpe)(c))
  def umulh(a: Node, c: Long) = UMulH(a, IntegralConst(a.tpe)(c))
}
