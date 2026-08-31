/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

sealed abstract class EnumKind
object EnumKind {
  case object ZeroSized extends EnumKind
  case object PrimitiveBased extends EnumKind
  case class OptionLike(tpe: CHIR.Type) extends EnumKind
  case object UnionBased extends EnumKind
  case object ClassBased extends EnumKind
}