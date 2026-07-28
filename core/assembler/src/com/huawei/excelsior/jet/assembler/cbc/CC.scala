/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

/** CBC condition codes. */
enum CC {
  case EQ, NE, LT, GT, LE, GE

  /** Returns opposite condition code. */
  def opposite = this match {
    case EQ => NE
    case NE => EQ
    case LT => GE
    case GT => LE
    case LE => GT
    case GE => LT
  }
}
