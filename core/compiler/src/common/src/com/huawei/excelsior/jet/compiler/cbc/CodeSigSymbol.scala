/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type}

case class CodeSigSymbol(sig: SignatureType) extends Symbol {

  def containsTypeVariables: Boolean = sig.containsTypeVariables
}

object CodeSigSymbol {
  def apply(tpe: Type) = new CodeSigSymbol(SignatureType.fromSymType(tpe))
}
