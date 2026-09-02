/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode


/** Field access kind. Corresponds to one of bytecode instructions. */
enum FieldAccessKind(statik: Boolean, write: Boolean) {
  case GETFIELD extends FieldAccessKind(false, false)
  case PUTFIELD extends FieldAccessKind(false, true)
  case GETSTATIC extends FieldAccessKind(true, false)
  case PUTSTATIC extends FieldAccessKind(true, true)

  def isStatic = statik
  def isInstance = !isStatic
  def isWrite = write
  def isRead = !isWrite
}

object FieldAccessKind {
  val FIELD_ACCESS_KINDS = FieldAccessKind.values
}
