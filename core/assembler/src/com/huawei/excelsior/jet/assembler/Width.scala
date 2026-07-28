/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import xscala.util.MathUtils.{isPowerOf2, log2}

/** Data width with machine-independent names.
  *
  * @author conwor
  * @author paul
  */
enum Width(private val _nbytes: Int) extends Ordered[Width] {
  case W0   extends Width(0)
  case W8   extends Width(1)
  case W16  extends Width(2)
  case W32  extends Width(4)
  case W64  extends Width(8)
  case W80  extends Width(10)
  case W128 extends Width(16)

  /** Platform-specific pointer/address width, currently only 8 bytes wide platforms are supported. */
  case WPTR extends Width(8)

  /** Special value; for internal use only. */
  case WNONE extends Width(-2)

  /** Returns the number of bytes. */
  def nbytes = _nbytes ensuring (_ >= 0, this)

  /** Returns the number of bits. */
  def nbits = nbytes * 8

  private val _log2bytes = if ((_nbytes > 0) && isPowerOf2(_nbytes)) log2(_nbytes) else -1

  /** Returns binary logarithm of size: `log_2(nbytes)` */
  def log2bytes = _log2bytes ensuring (_ >= 0)

  /** Returns binary logarithm of size in bits: `log_2(nbits)` */
  def log2bits = log2bytes + 3

  override def compare(that: Width) = this.nbytes compare that.nbytes
}

object Width {
  /** 1-byte width. */
  val BYTE = W8

  def apply(nbytes: Int) = nbytes match {
    case 0 => W0
    case 1 => W8
    case 2 => W16
    case 4 => W32
    case 8 => W64
    case 10 => W80
    case 16 => W128
  }
}
