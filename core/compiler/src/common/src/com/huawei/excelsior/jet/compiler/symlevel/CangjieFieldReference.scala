/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.assembler.Symbol

import scala.annotation.tailrec

case class CangjieFieldReference(idx: Long, field: Option[Field], refType: SignatureType, fieldType: SignatureType) extends Symbol

object CangjieFieldReference {
  def apply(idx: Long, field: Field, refType: SignatureType, fieldType: SignatureType): CangjieFieldReference =
    new CangjieFieldReference(idx, Some(field), refType, fieldType)

  def apply(field: Field, refType: SignatureType, fieldType: SignatureType): CangjieFieldReference =
    new CangjieFieldReference(field.getFieldIndex.toLong, Some(field), refType, fieldType)

  def apply(refType: SignatureType, fieldType: SignatureType): CangjieFieldReference =
    new CangjieFieldReference(0, None, refType, fieldType)
}

case class CangjieIndexReference(idx: Long, refType: SignatureType, fieldType: SignatureType) extends Symbol {
  require(checkConstIndexRefType(refType), refType)
}

// Note: no Options here!
@tailrec
private def checkConstIndexRefType(t: SignatureType): Boolean = t match {
  case _: (SignatureType.Tuple | SignatureType.VArray | SignatureType.CangjieArray) => true
  case t: SignatureType.Box => checkConstIndexRefType(t.base)
  case _ => false
}
