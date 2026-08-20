/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.assembler.Symbol

sealed trait CangjieFieldReference extends Symbol {
  def refType: SignatureType
  def fieldType: SignatureType
  def field: Option[Field]
}

case class SymLevelBasedFieldReference private[symlevel](_field: Field, refType: SignatureType, fieldType: SignatureType) extends CangjieFieldReference {
  def idx = _field.getFieldIndex
  def field = Some(_field)
}

case class IndexBasedFieldReference private[symlevel](idx: Long, refType: SignatureType, fieldType: SignatureType) extends CangjieFieldReference {
  def field = None
}

case class SignatureBasedFieldReference private[symlevel](refType: SignatureType, fieldType: SignatureType) extends CangjieFieldReference {
  def field = None
}

object CangjieFieldReference {

  def newSymLevelBased(field: Field, refType: SignatureType, fieldType: SignatureType): CangjieFieldReference = {
    SymLevelBasedFieldReference(field, refType, fieldType)
  }

  def newIndexBased(idx: Long, refType: SignatureType, fieldType: SignatureType): CangjieFieldReference = {
    IndexBasedFieldReference(idx, refType, fieldType)
  }

  def newSigBased(refType: SignatureType, fieldType: SignatureType): CangjieFieldReference = {
    SignatureBasedFieldReference(refType, fieldType)
  }
}