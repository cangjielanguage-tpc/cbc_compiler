/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.fixups

import com.huawei.excelsior.jet.assembler.Fixup
import com.huawei.excelsior.jet.assembler.Label
import com.huawei.excelsior.jet.assembler.Symbol

/** Base implementation for control transfer fixups.
  *
  * We use platform-independent terminology:
  *
  *  1. `Branch` - conditional control transfer inside segment. Target of branch can be only [[Label]].
  *     Each implementation of branch defines platform-specific conditional code and possibly additional data
  *
  *  1. `Jump` - unconditional control transfer inside segment or out of it. Target of jump can be [[Label]]
  *     or any external [[Symbol]]. Jump can be with or without `link`, means that it saved return address
  *     in some platform-specific resource.
  *
  * If target is [[Label]] (transfer is inside segment), fixup should be resolved and can be variable-length.
  * Otherwise it should be fixed-length and it's resolve sends some relocation to external environment.
  *
  * @author conwor
  */
abstract class ControlFixup protected(protected val target: Symbol, _isVariable: Boolean, _initialSize: Int) extends Fixup(_isVariable, _initialSize) {
  protected val isLocal = target.isInstanceOf[Label]
  assert(!isVariable || isLocal)

  protected def this(target: Symbol, isVariable: Boolean, initialSize: Int, nonVariableSize: Int) =
    this(target, isVariable, if (isVariable) initialSize else nonVariableSize)

  protected def targetDistance: Int = {
    assert(isLocal)
    val target = this.target.asInstanceOf[Label]
    assert(target.segment == segment)
    target.position - position
  }
}
