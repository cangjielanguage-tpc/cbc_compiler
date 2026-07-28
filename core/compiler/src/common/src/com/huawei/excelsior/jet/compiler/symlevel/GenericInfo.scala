/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.compiler.symlevel.GenericInfo.Constraint
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.LocalTypeVariable

case class GenericInfo(constraints: Seq[Constraint]) {
  def ++(that: GenericInfo) = GenericInfo(constraints ++ that.constraints.map { c => Constraint(LocalTypeVariable(c.typeVariable.idx + constraints.size), c.upperBounds) })
}

object GenericInfo {
  case class Constraint(typeVariable: SignatureType.LocalTypeVariable, upperBounds: Seq[SignatureType])

  def none: GenericInfo = null
}