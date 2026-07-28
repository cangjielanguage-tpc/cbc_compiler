/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.util.StringOps.*

/** Represents presence of dynamic capabilities (JIT etc.) in a given configuration of JET framework. */
enum DynamicBundle {
  case ON, OFF, NOT_SPECIFIED

  /** Returns whether the dynamic capabilities are enabled. */
  def enabled = this == ON
}

object DynamicBundle {
  def apply(name: String): DynamicBundle = if (name == null) {
    NOT_SPECIFIED
  } else {
    DynamicBundle.valueOf(name.trim.asciiToUpperCase)
  }
}
