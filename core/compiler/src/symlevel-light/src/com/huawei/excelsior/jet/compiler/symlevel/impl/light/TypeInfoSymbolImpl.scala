/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcOModule}
import com.huawei.excelsior.jet.compiler.symlevel.TypeInfoSymbol

abstract class TypeInfoSymbolImpl(override val tpe: TypeImpl, obj: pc.Symbol) extends SymbolImpl(obj) with TypeInfoSymbol {
  override def equals(that: Any) = super.equals(that) && this.tpe == that.asInstanceOf[TypeInfoSymbol].tpe
  override def hashCode = super.hashCode * 31 + tpe.hashCode
  override def toString = super.toString + " of " + tpe.toString
}
