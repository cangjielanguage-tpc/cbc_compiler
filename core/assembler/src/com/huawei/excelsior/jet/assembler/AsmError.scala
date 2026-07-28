/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

/**
  * Common assembler errors
  *
  * @author cypok
  * @author paul
  */
class AsmError(_message: String) extends Error(_message)

object AsmError {
  def error(message: String) = throw new AsmError(message)
  def require(condition: Boolean, message: String): Unit = if (!condition) error(message)
}
