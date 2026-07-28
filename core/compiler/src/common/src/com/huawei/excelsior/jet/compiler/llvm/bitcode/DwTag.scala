/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

enum DwTag(val value: Int):
  case DW_TAG_array_type           extends DwTag(1)
  case DW_TAG_enumeration_type     extends DwTag(4)
  case DW_TAG_member               extends DwTag(13)
  case DW_TAG_pointer_type         extends DwTag(15)
  case DW_TAG_reference_type       extends DwTag(16)
  case DW_TAG_structure_type       extends DwTag(19)
  case DW_TAG_typedef              extends DwTag(22)
  case DW_TAG_inheritance          extends DwTag(28)
  case DW_TAG_ptr_to_member_type   extends DwTag(31)
  case DW_TAG_const_type           extends DwTag(38)
  case DW_TAG_friend               extends DwTag(42)
  case DW_TAG_volatile_type        extends DwTag(53)
  case DW_TAG_restrict_type        extends DwTag(55)
  case DW_TAG_atomic_type          extends DwTag(71)

object DwTag:
  private lazy val value2tag = DwTag.values.map(t => (t.value, t)).toMap
  def byValue(value: Int) = value2tag(value)
