/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

/** The kind of edges between IR nodes. */
enum Tag {
  case CONTROL
  case XCONTROL
  case MEMORY
  case VALUE

  /** Returns the unique integer ID of this tag. */
  def id = ordinal

  /** Returns the bit mask with only the bit corresponding to this tag set. */
  def asMask = 1 << id

  /** Checks whether the given mask contains a bit for this tag. */
  def containsInMask(mask: Int) = (asMask & mask) != 0
}

object Tag {

  val VALUES = values

  /** The mask with all possible tag bits set. */
  val ALL_VALID_MASK = (1 << VALUES.length) - 1
}
