/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

/** Compiler representation for primitive types. */
final case class PrimitiveType private[types] (val sigType: SignatureType) extends CompiledType {
  require(sigType.isPrimitive)
  
  def isFinal = true
}

object PrimitiveType extends CompiledType.Companion[PrimitiveType]
