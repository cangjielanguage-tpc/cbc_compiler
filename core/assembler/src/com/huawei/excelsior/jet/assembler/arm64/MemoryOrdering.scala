/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

/** Helper enum for setting memory ordering semantics in atomic instructions
  *
  * @author orangebyte256
  */
enum MemoryOrdering {
  case NONE
  case ACQUIRE
  case RELEASE
  case ACQUIRE_RELEASE

  def a = ordinal & 0x1 // TODO: 0b1
  def r = ordinal >> 1
}
