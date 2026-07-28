/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.symlevel.{GenericInfo, MethodReference, SignatureType}

/** FIXME-UG: get rid of this, use [[com.huawei.excelsior.jet.compiler.symlevel.InstantiatedMethodReference]] instead */
case class InstantiatedGenericMethod(mr: MethodReference, instantiatedTypeParameters: Seq[SignatureType]) extends Symbol {
  assert(genericInfo.exists(_.constraints.size == instantiatedTypeParameters.size))

  def containsTypeVariables: Boolean = instantiatedTypeParameters.exists(_.containsTypeVariables)

  private def genericInfo: Option[GenericInfo] =
    Option.when(mr.method.isUniversalGeneric)(mr.method.getGenericInfo) orElse
      Option.when(mr.method.getDeclaringClass.isUniversalGeneric)(mr.method.getDeclaringClass.getGenericInfo)
}
