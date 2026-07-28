/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Fixup

abstract class FixedSizeFixup protected(_initialSize: Int) extends Fixup(false, _initialSize) {
  // There is no need to hide size() for fixed-size fixups.
  override def size: Int = super.size

  /** Size that this fixup would have in current context. */
  override protected[assembler] final def expectedSize: Int = shouldNotCallThis()
}
