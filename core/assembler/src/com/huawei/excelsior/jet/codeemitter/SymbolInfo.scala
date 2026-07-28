/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind.DIRECT
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind.FAR
import com.huawei.excelsior.jet.assembler.Symbol

/** Provides an information how to access symbols. */
trait SymbolInfo {

  /** Returns the access kind for the given symbol. */
  def accessKind(symbol: Symbol): SymbolInfo.AccessKind

  /** Returns whether the given symbol can be accessed directly, i.e. its
    * access kind is [[SymbolInfo.AccessKind.DIRECT]].
    */
  def isDirectAccess(symbol: Symbol) = accessKind(symbol) == DIRECT

  /** Returns whether the given symbol '''cannot''' be accessed directly, i.e. its
    * access kind is [[SymbolInfo.AccessKind.FAR]].
    */
  def isFarAccess(symbol: Symbol) = accessKind(symbol) == FAR
}

object SymbolInfo {

  /** Access kind determines how the symbol can be accessed from code & data. */
  enum AccessKind {

    /** Symbol can be accessed directly as a target of load/store/jump/call fixup. */
    case DIRECT

    /** Symbol cannot be accessed directly; full address loading in register (via amd64-mov or arm-literal) is required. */
    case FAR
  }
}
