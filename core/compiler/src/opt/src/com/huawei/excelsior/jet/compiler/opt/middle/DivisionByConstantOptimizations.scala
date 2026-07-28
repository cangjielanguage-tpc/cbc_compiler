/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.ir.{NodeAliases, Universe}
import com.huawei.excelsior.jet.compiler.opt.util.DivByConstMagicNumberComputation
import com.huawei.excelsior.jet.compiler.options.BoolOption

import scala.PartialFunction.cond

/** Various optimizations for division by constant.
  *
  * The whole algorithm is described in following paper:
  * ''"Hacker's Delight" by Henry S. Warren, Jr. (2002);
  * Chapter 10 "Integer Division by Constants"''.
  *
  * @author paul
  * @author liontiger
  */
trait DivisionByConstantOptimizations extends DivByConstMagicNumberComputation with NodeAliases { self: Universe =>

  def getOptimizedDivRemByConst(op: IDivRemOp, d: Long): Node = {
    if (-1 <= d && d <= 1) { // +-1 cases are handled in identities
      return null
    }

    if (targetArch == CBC) {
      return null
    }

    val (tpe, n) = (op.tpe, op.l)
    (op.isUnsigned, op.isDiv, d) match {
      case (false, true,  PowerOfTwo(p))    => optIDivByPowerOfTwo(tpe)(n, p)
      case (false, true,  NegPowerOfTwo(p)) => Neg(tpe)(optIDivByPowerOfTwo(tpe)(n, p))
      case (false, true,  _)                => optIDivByConst(tpe)(n, d)

      case (false, false, NegPowerOfTwo(p)) => optIRemByPowerOfTwo(tpe)(n, p)
      case (false, false, PowerOfTwo(p))    => optIRemByPowerOfTwo(tpe)(n, p)
      case (false, false, _)                => optIRemByConst(tpe)(n, d)

      case (true, true, Int.MinValue) if tpe == IntType => lsr(n, 31)
      case (true, true, Long.MinValue)                  => lsr(n, 63)
      case (true, true, PowerOfTwo(p))                  => lsr(n, p)
      case (true, true, _)                              => optUDivByConst(tpe)(n, d)

      case (true, false, Int.MinValue) if tpe == IntType => and(n, Int.MaxValue)
      case (true, false, Long.MinValue)                  => and(n, Long.MaxValue)
      case (true, false, PowerOfTwo(_))                  => and(n, d - 1)
      case (true, false, _)                              => optURemByConst(tpe)(n, d)

      case _ => null
    }
  }

  /** Handling negative argument for div/rem by power of two replacement.
    * See "Hacker's Delight" Chapter 10, Section 10-1.
    */
  private def divremByPowerOfTwoPrepare(tpe: Type)(n: Node, p: Int): Node = {
    assert(p > 0)
    val w = typeSizeInBits(tpe)
    val mask = lsr(asr(n, p - 1), w - p) // mask = if(n < 0) 2**p - 1 else 0
    Add(n, mask)
  }


  /** IDiv replacement when divider is power of two.
    * See "Hacker's Delight" Chapter 10, Section 10-1.
    */
  private def optIDivByPowerOfTwo(tpe: Type)(n: Node, p: Int) = {
    val x = divremByPowerOfTwoPrepare(tpe)(n, p)
    asr(x, p)
  }

  /** IRem replacement when divider is power of two.
    * See "Hacker's Delight" Chapter 10, Section 10-2.
    */
  private def optIRemByPowerOfTwo(tpe: Type)(n: Node, p: Int) = {
    val x = divremByPowerOfTwoPrepare(tpe)(n, p)
    val y = and(x, -PowerOfTwo(p))
    Sub(n, y)
  }


  /** IDiv replacement when divider is const and not power of two. */
  private def optIDivByConst(tpe: Type)(n: Node, d: Long) = {
    assert(!tpe.isFloatingPointType)

    // Word size
    val w: Int = tpe match {
      case IntType => 32
      case LongType => 64
    }

    // m - multiplicative inverse to 1/d
    // s - shift amount to avoid overflow
    val (m: Long, s: Int) = computeSignedMagicNumber(w, d)


    var q: Node = imulh(n, m)
    // Handling signed division properly
    if (d > 0 && m < 0) {
      q = Add(q, n)
    } else if (d < 0 && m > 0) {
      q = Sub(q, n)
    }

    q = asr(q, s)

    val u = if (d > 0) n else q
    val t = lsr(u, w - 1) // if (u < 0)
    q = Add(q, t)         // q = q + 1

    q
  }

  /** IRem replacement when divider is const and not power of two. */
  private def optIRemByConst(tpe: Type)(n: Node, d: Long) = {
    Sub(n, Mul(IntegralConst(tpe)(d), optIDivByConst(tpe)(n, d)))
  }


  /** UDiv replacement when divider is const and not power of two. */
  private def optUDivByConst(tpe: Type)(n: Node, d: Long) = {
    assert(!tpe.isFloatingPointType)

    // Word size
    val w: Int = tpe match {
      case IntType => 32
      case LongType => 64
    }

    // m - multiplicative inverse to d
    // a - indicator that tells whether generate Add instruction or not
    // s - shift amount to avoid overflow
    val (m: Long, a: Boolean, s: Int) = computeUnsignedMagicNumber(w, d)


    var q: Node = umulh(n, m)
    // Handling unsigned division properly
    if (a) {
      var t = Sub(n, q)
      t = lsr(t, 1)
      t = Add(t, q)
      q = lsr(t, s - 1)
    } else {
      q = lsr(q, s)
    }

    q
  }

  /** URem replacement when divider is const and not power of two. */
  private def optURemByConst(tpe: Type)(n: Node, d: Long) = {
    Sub(n, Mul(IntegralConst(tpe)(d), optUDivByConst(tpe)(n, d)))
  }
}
