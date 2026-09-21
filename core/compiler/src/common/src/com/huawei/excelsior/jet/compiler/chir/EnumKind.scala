/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

enum EnumKind(val id: Int) {
  case ZeroSized                  extends EnumKind(0)
  case PrimitiveBased             extends EnumKind(1)
  case OptionLike(tpe: CHIR.Type) extends EnumKind(2)
  case UnionBased                 extends EnumKind(3)
  case ClassBased                 extends EnumKind(4)
}

object EnumKind {
  def fromId(id: Int): EnumKind = id match {
    case 0 => ZeroSized
    case 1 => PrimitiveBased
    case 2 => OptionLike(null) // null because sym level doesn't need frontend info
    case 3 => UnionBased
    case 4 => ClassBased
  }
}
