/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.util

import xscala.util.MathUtils.{mulh, umulh}
import xscala.util.{UInt, ULong}

trait DivByConstMagicNumberComputationToolbox extends DivByConstMagicNumberComputation {

  def substDivInt(n: Int, d: Int, m: Int, s: Int): Int = {
    var q = mulhs(n,m)        // mulhs  q, n, m
    if (d > 0 && m < 0) {
      q = q + n               // add    q, q, n
    } else if (d < 0 && m > 0) {
      q = q - n               // sub    q, q, n
    }
    q = q >> s                // shrsi  q, q, s
    if (q < 0) {              // shri   t, q, 31
      q += 1                  // add    q, q, t
    }
    q
  }

  def substDivLong(n: Long, d: Long, m: Long, s: Int): Long = {
    var q = mulh(n,m)        // mulhs  q, n, m
    if (d > 0 && m < 0) {
      q = q + n               // add    q, q, n
    } else if (d < 0 && m > 0) {
      q = q - n               // sub    q, q, n
    }
    q = q >> s                // shrsi  q, q, s
    if (q < 0) {              // shri   t, q, 31
      q += 1                  // add    q, q, t
    }
    q
  }

  def substDivUInt(n: Int, d: Int, m: Int, a: Boolean, s: Int): Int = {
    var q = mulhu(n,m)        // mulhu  q, n, m
    if (a) {
      var t = n - q           // sub    t, n, q
      t = t >>> 1             // shri   t, t, 1
      t = t + q               // add    t, t, q
      q = t >>> (s-1)         // shri   q, t, (s-1)
    } else {
      q = q >>> s             // shri   q, q, s
    }
    q
  }

  def substDivULong(n: Long, d: Long, m: Long, a: Boolean, s: Int): Long = {
    var q = umulh(n,m)        // mulhu  q, n, m
    if (a) {
      var t = n - q           // sub    t, n, q
      t = t >>> 1             // shri   t, t, 1
      t = t + q               // add    t, t, q
      q = t >>> (s-1)         // shri   q, t, (s-1)
    } else {
      q = q >>> s             // shri   q, q, s
    }
    q
  }

  def udiv(n: Int, d: Int): Int = udiv(UInt(n).toLong, UInt(d).toLong).toInt
  def udiv(n: Long, d: Long): Long = (ULong(n) / ULong(d)).toLong

  def urem(n: Int, d: Int): Int = urem(UInt(n).toLong, UInt(d).toLong).toInt
  def urem(n: Long, d: Long): Long = (ULong(n) % ULong(d)).toLong

  def mulhs(x: Int, y: Int): Int = mulh32(x.toLong, y.toLong)
  def mulhu(x: Int, y: Int): Int = mulh32(UInt(x).toLong, UInt(y).toLong)
  def mulh32(x: Long, y: Long): Int = ((x * y) >> 32).toInt

}
