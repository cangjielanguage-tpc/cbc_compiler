/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.ir

/** Constants and methods related to offsets in bytecode.
  *
  * @author ijorch
  */
object BytecodeOffset {

  /** Special value representing missing information about bytecode position. */
  val INVALID = -1

  /** Special value representing bytecode position of artificially-generated code (without corresponding java-bytecode). */
  val SYNTHETIC = -2

  /** Construct synthetic offset by given real bytecode position. */
  def makeSynthetic(offset: Int) = {
    assert(offset >= 0)
    (SYNTHETIC - offset - 1) ensuring isValid _
  }

  // TODO: make LineNumber.INVALID and BytecodeOffset.INVALID equal and remove MIN_VALUE check
  def isValid(offset: Int) = offset != INVALID && offset != Integer.MIN_VALUE

  def isSynthetic(offset: Int) = offset == SYNTHETIC
}