/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Arithmetic bytecode operations
  *
  * @author paul
  */
enum ArithOp {
  case ADD, SUB, NEG
  case MUL, DIV, REM
  case LSL, ASR, LSR
  case AND, OR, XOR
  case CMP, CMPL, CMPG

  def isCmp = (this == CMP) || (this == CMPL) || (this == CMPG)

  def isShift = (this == LSL) || (this == ASR) || (this == LSR)
}
