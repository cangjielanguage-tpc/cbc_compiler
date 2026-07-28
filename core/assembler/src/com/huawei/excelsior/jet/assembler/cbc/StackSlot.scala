/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Symbol
import xscala.util.MathUtils.isNBits

object StackSlot {
  case class Untyped(slot: Int) {
    require(isNBits(slot, 16))
  }
  case class Typed(idx: Int) {
    require(isNBits(idx, 16))
  }
  case class OffHeapMemory(idx: Int) extends Symbol {
    require(isNBits(idx, 16))
  }
}
