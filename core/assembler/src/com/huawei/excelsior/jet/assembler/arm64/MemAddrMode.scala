/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

/** ARM64 memory addressing modes. */
enum MemAddrMode {
  case REG_REG, REG_IMM, PRE_IDX, POST_IDX, UNSCALED

  def text = this match {
    case REG_REG  => "[Xn|SP, (Xm|Wm){, extend/LSL {amount}}]"
    case REG_IMM  => "[Xn|SP, #imm]"
    case UNSCALED => "[Xn|SP, #simm]"
    case PRE_IDX  => "[Xn|SP, #simm]!"
    case POST_IDX => "[Xn|SP], #simm"
  }

  def isWBack = this == PRE_IDX || this == POST_IDX
}