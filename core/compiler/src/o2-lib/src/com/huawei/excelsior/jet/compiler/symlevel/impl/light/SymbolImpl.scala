/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.o2lib.fe.pc
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment._

class SymbolImpl(override val o2object: pc.Symbol, private val description: String = null) extends Symbol with SymLevelObject {
  override def toString = "symbol " + (if (description != null) description else o2name(o2object))

  override def equals(thatPar: Any): Boolean = {
    val that = thatPar.asInstanceOf[AnyRef]
    if (this eq that) return true
    if (that == null || (getClass ne that.getClass)) return false
    this.o2object == that.asInstanceOf[SymbolImpl].o2object
  }

  override def hashCode = o2object.hashCode

  override def ownsSegment = o2object.ownsSegment
}
