/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.assembler.Symbol

import scala.PartialFunction.condOpt

/** Permanent mirror of a member that survives between compilation sessions. */
trait PermanentMember extends Symbol {
  def get: Member
  override final def toString = get.toString
}

object PermanentMember {
  def unapply(symbol: Symbol): Option[Member] = condOpt(symbol) {
    case pm: PermanentMember => pm.get
  }
}
