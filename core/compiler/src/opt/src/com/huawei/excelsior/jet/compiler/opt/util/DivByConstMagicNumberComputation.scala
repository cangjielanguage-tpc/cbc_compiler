/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.util

import xscala.util.{UInt, ULong}

import scala.math.abs

/** Computes magic number for Division by Constant Optimization.
  *
  * Let `w` be the word size (32 or 64) and `d` be divisor (signed or unsigned).
  * Magic number for `d` is such integer `M` that for all `n` of word size the following equation is satisfied:
  * {{{ floor(n / d) = (n * M) >>> w }}}
  * Note: in signed case the sign of `M` depends on the sign of `d`.
  *
  * The algorithm is described in following paper:
  * ''"Hacker's Delight" by Henry S. Warren, Jr. (2002);
  * Chapter 10 "Integer Division by Constants";
  * Sections 10-3 through 10-10''.
  *
  * @author rust
  */
trait DivByConstMagicNumberComputation {

  /** Computes magic number for signed case of division by const optimization.
    *
    * @param w word size
    * @param d signed divisor
    * @return (M, s) - magic number and shift amount needed for optimization.
    */
  def computeSignedMagicNumber(w: Int, d: Long): (Long, Int) = w match {
    case 32 => computeInt(d.toInt)
    case 64 => computeLong(d)
  }
  
  private def computeInt(d: Int): (Long, Int) = {
    var p = 31

    val two31: UInt = 1 << 31

    val ad: UInt = abs(d)
    val t: UInt = two31 + (d >>> 31)
    val anc: UInt = t - 1 - (t % ad)

    var q1 = two31 / anc
    var r1 = two31 - q1 * anc
    var q2 = two31 / ad
    var r2 = two31 - q2 * ad

    def update(q: UInt, r: UInt, d: UInt): (UInt, UInt) = {
      var nq = 2 * q
      var nr = 2 * r
      if (nr >= d) {
        nq += 1
        nr -= d
      }
      (nq, nr)
    }

    while {
      p = p + 1
      
      val res1 = update(q1, r1, anc)
      q1 = res1._1
      r1 = res1._2

      val res2 = update(q2, r2, ad)
      q2 = res2._1
      r2 = res2._2

      val delta = ad - r2
      q1 < delta || (q1 == delta && r1 == UInt(0))
    } do ()

    var M: Int = (q2 + 1).toInt

    if (d < 0) {
      M = -M
    }

    (M, p - 32)
  }

  private def computeLong(d: Long): (Long, Int) = {
    var p = 63

    val two63: ULong = 1L << p

    val ad: ULong = abs(d)
    val t: ULong = two63 + (d >>> p)
    val anc = t - 1 - (t % ad)

    var q1 = two63 / anc
    var r1 = two63 - q1 * anc
    var q2 = two63 / ad
    var r2 = two63 - q2 * ad

    var delta: ULong = 0

    def update(q: ULong, r: ULong, d: ULong): (ULong, ULong) = {
      var nq: ULong = 2 * q
      var nr: ULong = 2 * r

      if (nr >= d) {
        nq += 1
        nr -= d
      }

      (nq, nr)
    }

    while {
      p = p + 1

      val res1 = update(q1, r1, anc)
      q1 = res1._1
      r1 = res1._2

      val res2 = update(q2, r2, ad)
      q2 = res2._1
      r2 = res2._2

      delta = ad - r2
      q1 < delta || (q1 == delta && r1 == ULong(0))
    } do ()

    var M: Long = (q2 + 1).toLong

    if (d < 0) {
      M = -M
    }

    (M, p - 64)
  }

  /** Computes magic number for unsigned case of division by const optimization.
    *
    * @param w word size
    * @param d unsigned divisor
    * @return (M, a, s) - magic number, addition flag and shift amount needed for optimization.
    */
  def computeUnsignedMagicNumber(w: Int, d: Long): (Long, Boolean, Int) = w match {
    case 32 => computeUInt(UInt(d.toInt))
    case 64 => computeULong(d)
  }

  private def computeUInt(d: UInt): (Long, Boolean, Int) = {
    var p = 31
    val two31: UInt = 1 << p

    var a = false
    val nc: UInt = UInt(-1) - UInt(-d.toInt) % d

    var q1 = two31 / nc
    var r1 = two31 - q1 * nc
    var q2 = (two31 - 1) / d
    var r2 = (two31 - 1) - q2 * d

    while {
      p = p + 1
      assert(!a)

      if (r1 >= nc - r1) {
        q1 = 2 * q1 + 1
        r1 = 2 * r1 - nc
      } else {
        q1 = 2 * q1
        r1 = 2 * r1
      }

      if (r2 + 1 >= d - r2) {
        if (q2 >= (two31 - 1)) {
          a = true
        }
        q2 = 2 * q2 + 1
        r2 = 2 * r2 + 1 - d
      } else {
        if (q2 >= two31) {
          a = true
        }
        q2 = q2 * 2
        r2 = 2 * r2 + 1
      }

      val delta = d - 1 - r2
      (p < 64) && (q1 < delta || (q1 == delta && r1 == UInt(0)))
    } do ()

    ((q2 + 1).toLong, a, p - 32)
  }

  private def computeULong(d: ULong): (Long, Boolean, Int) = {
    var p = 63
    val two63: ULong = ULong(1) << 63

    var a = false
    val nc: ULong = ULong(-1) - ULong(-d.toLong) % d


    var q1 = two63 / nc
    var r1 = two63 - q1 * nc
    var q2 = (two63 - 1) / d
    var r2 = (two63 - 1) - q2 * d

    while {
      p = p + 1
      assert(!a)

      if (r1 >= nc - r1) {
        q1 = 2 * q1 + 1
        r1 = 2 * r1 - nc
      } else {
        q1 = 2 * q1
        r1 = 2 * r1
      }

      if (r2 + 1 >= d - r2) {
        if (q2 >= (two63 - 1)) {
          a = true
        }
        q2 = 2 * q2 + 1
        r2 = 2 * r2 + 1 - d
      } else {
        if (q2 >= two63) {
          a = true
        }
        q2 = 2 * q2
        r2 = 2 * r2 + 1
      }

      val delta: ULong = d - 1 - r2
      (p < 128) && (q1 < delta || (q1 == delta && r1 == ULong(0)))
    } do ()

    ((q2 + 1).toLong, a, p - 64)
  }
}
