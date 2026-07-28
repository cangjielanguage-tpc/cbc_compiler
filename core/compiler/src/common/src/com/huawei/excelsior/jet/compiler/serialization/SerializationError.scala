/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.serialization

class SerializationError(msg: String) extends Exception(msg)

object SerializationError {
  def apply(msg: => String) =
    throw new SerializationError(msg)
  def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) apply(msg)
  }
}
