/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, ClassType as SymClassType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.Approximation
import com.huawei.excelsior.jet.compiler.types.Approximation.CC

/** Compiler representation for Cangjie records. */
final class RecordType private[types] (val sigType: SignatureType) extends Approximation with CompiledType {
  require(sigType.isRecord)
  override val symType: SymClassType = asClassType(super.symType)

  override def equals(that: Any) = that match {
    case that: AnyRef if this eq that => true
    case that: RecordType =>
      this.symType == that.symType ||
        this.sigType.isArraySliceLike && that.sigType.isArraySliceLike
    case _ => false
  }

  override def hashCode = if (sigType.isArraySliceLike) 13 /* Any constant works */ else symType.hashCode

  def isFinal = true

  // Approximation for records consists of separate incomparable elements with no top or bottom.
  override def compare(that: Approximation) = if (this == that) CC.Equal else CC.Incomparable
  override def intersect(that: Approximation) = shouldNotCallThis()
  override def union(that: Approximation): Approximation = shouldNotCallThis()
  override def isEmpty = false
}

object RecordType extends CompiledType.Companion[RecordType]
