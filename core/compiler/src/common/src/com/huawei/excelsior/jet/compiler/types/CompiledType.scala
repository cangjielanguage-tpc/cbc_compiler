/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.symlevel

import scala.collection.mutable
import scala.ref.SoftReference
import scala.reflect.ClassTag

/** Compiler representation for any type from compilation set. */
trait CompiledType {
  def sigType: SignatureType
  @Deprecated // TODO: make protected
  def symType: symlevel.Type = sigType.symType(ReferenceTypes.typeProvider)

  /** True if there can be no subtype of this. */
  def isFinal: Boolean
}

object CompiledType {

  // Signature type contains more precise info than symlevel type
  // but lacks classloaderID which is located in symType.
  private val cache = mutable.HashMap.empty[(SignatureType, symlevel.Type), SoftReference[CompiledType]]
  def apply(sigType: SignatureType): CompiledType = if (sigType == null) null else {
    val key = (sigType, sigType.symType(ReferenceTypes.typeProvider))
    cache.get(key).flatMap(_.get).getOrElse {
      val tpe: CompiledType = if (sigType.isPrimitive) {
        new PrimitiveType(sigType)
      } else if (sigType.isRecord) {
        new RecordType(sigType)
      } else {
        ReferenceType.create(sigType)
      }
      cache.put(key, SoftReference(tpe))
      tpe
    }
  }

  def apply(symType: symlevel.Type): CompiledType = apply(SignatureType.fromSymType(symType))

  trait Companion[T <: CompiledType : ClassTag] {
    def apply(symType: symlevel.Type): T = apply(SignatureType.fromSymType(symType))
    def apply(sigType: SignatureType): T = CompiledType(sigType).asInstanceOf[T]
  }
}
