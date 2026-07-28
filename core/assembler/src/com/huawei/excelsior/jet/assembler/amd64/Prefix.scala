/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.amd64

/** One-byte instruction prefixes
  * REX, VEX, XOP prefixes listed elsewhere
  *
  * @param code The prefix code.
  * @author paul
  * @author cypok
  */
enum Prefix(val code: Int) {
  // lock prefix
  case LOCK extends Prefix(0xf0)

  // string instructions prefixes
  case REPNZ extends Prefix(0xf2)
  case REPNE extends Prefix(REPNZ)

  case REP extends Prefix(0xf3)
  case REPZ extends Prefix(REP)
  case REPE extends Prefix(REP)

  // segment override prefixes
  case CS extends Prefix(0x2e)
  case DS extends Prefix(0x3e)
  case ES extends Prefix(0x26)
  case SS extends Prefix(0x36)
  case FS extends Prefix(0x64)
  case GS extends Prefix(0x65)

  // operand size override prefix
  case OP_SIZE extends Prefix(0x66)
  // address size override prefix
  case ADR_SIZE extends Prefix(0x67)

  def this(another: Prefix) = this(another.code)

  /** Returns whether this prefix is a segment override prefix. */
  def isSegmentOverride = this match {
    case CS | DS | ES | SS | FS | GS => true
    case _ => false
  }
}
