/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{CangjieEnumWrapper, Primitive}

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat
import scala.annotation.tailrec

object ConstValues {
  sealed abstract class ConstValue

  case class IntValue(value: Int) extends ConstValue
  case class LongValue(value: Long) extends ConstValue
  case class FloatValue(value: Float) extends ConstValue
  case class DoubleValue(value: Double) extends ConstValue
  case class StringValue(value: XString) extends ConstValue

  @tailrec
  def apply(sig: SignatureType, value: Long): ConstValue = (sig: @unchecked) match {
    case Primitive(TypeKind.LONG) => ConstValues.LongValue(value)
    case Primitive(TypeKind.CHAR) => ConstValues.IntValue(value.toChar)
    case Primitive(kind) if kind.isIntegral => ConstValues.IntValue(value.toInt)

    case Primitive(TypeKind.FLOAT) => ConstValues.FloatValue(intBitsToFloat(value.toInt))
    case Primitive(TypeKind.DOUBLE) => ConstValues.DoubleValue(longBitsToDouble(value))
    case CangjieEnumWrapper(base: Primitive, _) => apply(base, value)
  }
}
