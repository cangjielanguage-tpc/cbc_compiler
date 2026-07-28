/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.jet.assembler.Fixup
import com.huawei.excelsior.jet.assembler.Segment
import xscala.util.MathUtils.alignUp

/** Allows to align code or data by specified [[alignment]] in bytes.
  * For code alignment emits a sequence of NOPs, and for data alignment uses provided `filler`.
  *
  * @author ijorch
  */
abstract class Alignment protected(val alignment: Int) extends Fixup(true, 0) {
  override protected[assembler] def expectedSize = alignUp(position, alignment) - position

  protected def resolveImpl(): Unit

  override def resolve(converter: Relocation.Converter): Unit = {
    assert(isBound)
    val segAlign = segment.getAlignment
    assert(segAlign != Segment.UNSPECIFIED_ALIGNMENT)
    assert(alignment <= segAlign)
    resolveImpl()
  }

  override protected def guts = Fixup.seq(alignment)
}