/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JBCReference, JavaArray, NonNullableWrapper, NullableWrapper, Primitive}

object JBCSignature {
  def apply(sig: Signature): String = (sig: @unchecked) match {
    case Primitive(kind)              => kind.getBCSignatureChar.toString
    case sig: JBCReference            => s"L${sig.name};"
    case NonNullableWrapper(baseType) => JBCSignature(baseType)
    case JavaArray(baseType, dimNum)  => s"${"[" * dimNum}${JBCSignature(baseType)}"

    case MethodSignature(retType, paramTypes) => s"(${paramTypes.map(JBCSignature.apply).mkString})${JBCSignature(retType)}"
  }
}
