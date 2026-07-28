/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.Symbol

case class FieldReference(kind: FieldReference.Kind, field: Symbol, refType: Symbol, fieldType: Symbol, tk: CbcTypeKind) extends Symbol {
  assert(field != null)

  def isGeneric: Boolean = kind != FieldReference.Kind.NonGenericType
  def isGenericVLT: Boolean = kind == FieldReference.Kind.GenericVST || kind == FieldReference.Kind.GenericVariableType
  def isGenericVST: Boolean = kind == FieldReference.Kind.GenericVST
}

object FieldReference {
  enum Kind {
    case GenericVST, GenericVariableType, GenericConcreteType, NonGenericType
  }

  def forGenericVSTField(refType: Symbol, field: Symbol, fieldType: Symbol, tk: CbcTypeKind): FieldReference = {
    FieldReference(FieldReference.Kind.GenericVST, field, refType, fieldType, tk)
  }

  def forGenericVariableFieldType(refType: Symbol, field: Symbol, fieldType: Symbol, tk: CbcTypeKind): FieldReference = {
    FieldReference(FieldReference.Kind.GenericVariableType, field, refType, fieldType, tk)
  }

  def forGenericConcreteFieldType(refType: Symbol, field: Symbol, fieldType: Symbol, tk: CbcTypeKind): FieldReference = {
    FieldReference(FieldReference.Kind.GenericConcreteType, field, refType, fieldType, tk)
  }

  def forNonGenericFieldType(refType: Symbol, field: Symbol, tk: CbcTypeKind): FieldReference = {
    FieldReference(FieldReference.Kind.NonGenericType, field, refType, null, tk)
  }
}