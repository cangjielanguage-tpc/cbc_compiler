/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.jet.assembler.Fixup

/** Allows to align data by specified [[_align]] in bytes uses provided [[filler]].
  *
  * @author ijorch
  */
final class DataAlignment(_align: Int, filler: Int) extends Alignment(_align) {
  override protected def resolveImpl(): Unit = {
    val count = size
    val pos = position
    for (i <- 0 until count) {
      segment.setByte(pos + i, filler)
    }
  }

  override protected def guts = Fixup.seq(alignment, filler)
}
