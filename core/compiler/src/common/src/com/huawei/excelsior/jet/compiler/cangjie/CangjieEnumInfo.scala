/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

case class CangjieEnumInfo(kind: CangjieEnumInfo.Kind, constructors: Seq[CangjieEnumInfo.Constructor])

object CangjieEnumInfo {
  case class Constructor(params: Seq[SignatureType])
  enum Kind {
    case ZeroSized, PrimitiveBased, OptionLike, UnionBased, ClassBased
  }
}
