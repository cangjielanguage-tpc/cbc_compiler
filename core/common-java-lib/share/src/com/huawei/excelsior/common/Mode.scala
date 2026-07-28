/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.util.StringOps.*

/** Represents "work" (i.e. "debugging") vs. "enduser" (i.e. "production") configurations of JET framework. */
enum Mode {
  case WORK, ENDUSER

  override def toString = productPrefix.asciiToLowerCase
}

object Mode {
  def apply(name: String) = Mode.valueOf(name.trim.asciiToUpperCase)
}
