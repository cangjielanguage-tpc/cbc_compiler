/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{TypeEnumKind, TypeFlag}
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.cangjie.CangjieEnumInfo
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc

/** Keep the enum representation chosen by CHIR in the CBC definition as well as its signatures. */
private[cbc] object CbcEnumEncoding {
  def isValueEnum(info: CangjieEnumInfo): Boolean = info.kind != CangjieEnumInfo.Kind.ClassBased

  def write(eInfo: CangjieEnumInfo, builder: CbcFileFormat.Type.Builder)(implicit typeProvider: TypeProvider): Unit = {
    if (isValueEnum(eInfo)) builder.addFlag(TypeFlag.ENUM)
    def isNullableOption(base: SignatureType): Boolean = base match {
      case _: SignatureType.OptionLikeEnum => false
      case _ => base.isReference
    }
    val ctors = eInfo.constructors.map(_.params)
    (eInfo.kind, ctors) match {
      case (CangjieEnumInfo.Kind.OptionLike, Seq(Seq(base), Seq())) =>
        builder.setEnumKind(TypeEnumKind.Option0)
        val enumType = if (isNullableOption(base)) {
          // Erase to avoid infinite recursion.
          ReferenceType.cangjieStdCoreObject.sigType.toCbc
        } else {
          base.toCbc
        }
        builder.setSuperOrEnumType(enumType)
      case (CangjieEnumInfo.Kind.OptionLike, Seq(Seq(), Seq(base))) =>
        builder.setEnumKind(TypeEnumKind.Option1)
        val enumType = if (isNullableOption(base)) {
          // Erase to avoid infinite recursion.
          ReferenceType.cangjieStdCoreObject.sigType.toCbc
        } else {
          base.toCbc
        }
        builder.setSuperOrEnumType(enumType)
      case (CangjieEnumInfo.Kind.ZeroSized, _) =>
        builder.setEnumKind(TypeEnumKind.Primitive)
        builder.setSuperOrEnumType(CbcFileFormat.BuiltinSignature.Unit)
      case (CangjieEnumInfo.Kind.PrimitiveBased, _) =>
        builder.setEnumKind(TypeEnumKind.Primitive)
        builder.setSuperOrEnumType(CbcFileFormat.BuiltinSignature.I32)
      case (CangjieEnumInfo.Kind.UnionBased, _) =>
        builder.setEnumKind(TypeEnumKind.Union)
        builder.setUnionFields(ctors.map(ps => CbcFileFormat.Tuple(Seq(CbcFileFormat.BuiltinSignature.U32) ++ ps.map(_.toCbc))))
      case (CangjieEnumInfo.Kind.ClassBased, _) => // consider as class
      case _ => shouldNotReachHere(eInfo)
    }
  }
}
