/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.jet.assembler.arm64.CC.fromOrdinal
import xscala.util.MathUtils.isNBits

/** ARM64 condition codes.
  *
  * @author orangebyte256
  */
enum CC {
  case EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, NV

  assert(isNBits(encoding, 4))

  def encoding = ordinal

  /** Returns opposite condition code. (X cond Y) == !(X cond.opposite Y). */
  def opposite: CC = fromOrdinal(ordinal ^ 1)
}

object CC {

  /** HS condition code is identical to [[# CS]] in ARM64 architecture. */
  val HS = CS

  /** LO condition code is identical to [[# CC]] in ARM64 architecture. */
  val LO = CC
}
