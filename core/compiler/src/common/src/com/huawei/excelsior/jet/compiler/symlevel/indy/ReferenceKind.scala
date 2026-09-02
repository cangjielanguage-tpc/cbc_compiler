/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.indy

import com.huawei.excelsior.jet.compiler.bytecode.{FieldAccessKind, MethodAccessKind}

import scala.annotation.nowarn

/** Constant pool reference-kind codes, as used by CONSTANT_MethodHandle CP entries.
  *
  * @author liontiger
  */
enum ReferenceKind {
  case REF_NONE // null value
  case REF_getField
  case REF_getStatic
  case REF_putField
  case REF_putStatic
  case REF_invokeVirtual
  case REF_invokeStatic
  case REF_invokeSpecial
  case REF_newInvokeSpecial
  case REF_invokeInterface
  case REF_LIMIT

  def isValid = REF_NONE.ordinal < this.ordinal && this.ordinal < REF_LIMIT.ordinal

  def isInvoke = REF_invokeVirtual.ordinal <= this.ordinal && this.ordinal < REF_LIMIT.ordinal

  @nowarn("msg=match may not be exhaustive")
  def asFieldAccessKind = this match {
    case REF_getField => FieldAccessKind.GETFIELD
    case REF_getStatic => FieldAccessKind.GETSTATIC
    case REF_putField => FieldAccessKind.PUTFIELD
    case REF_putStatic => FieldAccessKind.PUTSTATIC
  }

  @nowarn("msg=match may not be exhaustive")
  def asMethodAccessKind = this match {
    case REF_invokeVirtual => MethodAccessKind.VIRTUAL
    case REF_invokeStatic => MethodAccessKind.STATIC
    case REF_invokeSpecial | REF_newInvokeSpecial => MethodAccessKind.SPECIAL
    case REF_invokeInterface => MethodAccessKind.INTERFACE
  }
}

object ReferenceKind {
  def fromBytecode(refKind: Int) = ReferenceKind.fromOrdinal(refKind)
}
