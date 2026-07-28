/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{DirectCallAotData, FieldFlag, InstanceFieldAotData, InterfaceCallAotData, MethodRefFlag, MethodRefFlags, StaticFieldAotData, StringLiteral}
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.SymbolAdapter
import com.huawei.excelsior.jet.assembler.cbc.{CbcFileFormat, FieldReference, RawData}
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator.env
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.*
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait CbcSymbolAdapter extends SymbolAdapter {
  implicit val typeProvider: TypeProvider = env.getTypeProvider
  
  def adapt(symbol: Symbol): CbcFileFormat.BytecodeReference = symbol match {
    case symbol: CodeSigSymbol => symbol.sig.toCbc
    case symbol: MethodReference =>
      val declaringClass = symbol.method.getDeclaringClass
      val refType = if (declaringClass.isCangjiePackage) {
        if (symbol.method.getCHIRDef.nonEmpty) {
          // Force reference to alt definition (see CbcFileEncoderAdapter.TypeWrapper)
          CbcFileFormat.TypeSignature.ref(CbcFileEncoderAdapter.cbcPackageName(declaringClass.getName))
        } else {
          CbcFileFormat.AotTypeSignature.ref(declaringClass.getName)
        }
      } else {
        symbol.refType.sigType.toCbc
      }
      val aotData = Option.when(symbol.method.getCHIRDef.isEmpty) {
        symbol.accessKind match {
          case STATIC | SPECIAL | MUT => DirectCallAotData(symbol.method.getExportedName.toString)
          case VIRTUAL => InterfaceCallAotData(symbol.explicitVNum.get) // TODO: improve if needed
          case _ => notImplemented(symbol.accessKind)
        }
      }
      val signature = symbol.method.getSignature.toCbc
      val flags = Option.when(symbol.methodType.hasRetByValParameter)(MethodRefFlag.SRET)
      CbcFileFormat.MethodReference(symbol.method.getName, refType, signature, MethodRefFlags(flags), aotData)
    case symbol: CangjieFieldReference =>
      // Only symlevel field references supported here.
      // Index-based references should be handled outside.
      val field = symbol.field.get
      val aotData = Option.when(field.getCHIRDef.isEmpty) {
        if (field.isStatic) {
          StaticFieldAotData(field.getExportedName.toString)
        } else {
          val idx = field.getDeclaringClass.getFields.indexOf(field)
          InstanceFieldAotData(idx)
        }
      }
      val declaringClass = field.getDeclaringClass
      val refType = if (declaringClass.isCangjiePackage) {
        if (field.getCHIRDef.nonEmpty) {
          // Force reference to alt definition (see CbcFileEncoderAdapter.TypeWrapper)
          CbcFileFormat.TypeSignature.ref(CbcFileEncoderAdapter.cbcPackageName(declaringClass.getName))
        } else {
          CbcFileFormat.AotTypeSignature.ref(declaringClass.getName)
        }
      } else {
        symbol.refType.toCbc
      }
      CbcFileFormat.FieldReference(name = field.getName,
        refType = refType, fieldType = field.getType.toCbc, aotData = aotData)
    case symbol: ConstStringSymbol => StringLiteral(symbol.value.toString)
    case symbol: RawData => CbcFileFormat.RawData(ArraySeq.from(symbol.data))
  }
}

object CbcSignatureAdapter {
  extension (s: Signature) {
    def toCbc: CbcFileFormat.Signature = adaptSignature(s)
  }

  implicit private val typeProvider: TypeProvider = env.getTypeProvider

  private def adaptSignature(signature: Signature): CbcFileFormat.Signature = signature match {
    case SignatureType.Void          => CbcFileFormat.BuiltinSignature.Void
    case SignatureType.Unit          => CbcFileFormat.BuiltinSignature.Unit
    case SignatureType.Nothing       => CbcFileFormat.BuiltinSignature.Nothing
    case SignatureType.Boolean       => CbcFileFormat.BuiltinSignature.Boolean
    case SignatureType.Int8          => CbcFileFormat.BuiltinSignature.I8
    case SignatureType.UInt8         => CbcFileFormat.BuiltinSignature.U8
    case SignatureType.Int16         => CbcFileFormat.BuiltinSignature.I16
    case SignatureType.UInt16        => CbcFileFormat.BuiltinSignature.U16
    case SignatureType.Int32         => CbcFileFormat.BuiltinSignature.I32
    case SignatureType.UInt32        => CbcFileFormat.BuiltinSignature.U32
    case SignatureType.UnicodeChar32 => CbcFileFormat.BuiltinSignature.UChar32
    case SignatureType.Int64         => CbcFileFormat.BuiltinSignature.I64
    case SignatureType.UInt64        => CbcFileFormat.BuiltinSignature.U64
    case SignatureType.AddrInt       => CbcFileFormat.BuiltinSignature.IAddr
    case SignatureType.AddrUInt      => CbcFileFormat.BuiltinSignature.UAddr
    case SignatureType.BString       => CbcFileFormat.BuiltinSignature.BString
    case SignatureType.Float16       => CbcFileFormat.BuiltinSignature.F16
    case SignatureType.Float32       => CbcFileFormat.BuiltinSignature.F32
    case SignatureType.Float64       => CbcFileFormat.BuiltinSignature.F64

    case mt: MethodSignature => CbcFileFormat.Functional(mt.parameterTypes.map(_.toCbc), mt.returnType.toCbc)

    case sig: SignatureType.Record =>
      assert(!asClassType(sig).isUniversalGeneric, s"erased signature type: ${sig.toJETSignature}")
      if (!sig.symType.isCHIRDef) CbcFileFormat.AotTypeSignature.rec(sig.name)
      else CbcFileFormat.TypeSignature.rec(sig.name)

    case sig: SignatureType.CangjieReference =>
      assert(!asClassType(sig).isUniversalGeneric, s"erased signature type: ${sig.toJETSignature}")
      if (!sig.symType.isCHIRDef) CbcFileFormat.AotTypeSignature.ref(sig.name)
      else CbcFileFormat.TypeSignature.ref(sig.name)

    case sig: SignatureType.InstantiatedType   =>
      if (!sig.symType.isCHIRDef) CbcFileFormat.AotTypeSignature(sig.name, sig.instantiatedTypeParameters.map(_.toCbc), sig.isReference)
      else CbcFileFormat.TypeSignature(sig.name, sig.instantiatedTypeParameters.map(_.toCbc), sig.isReference)

    case sig: SignatureType.NullableWrapper    => CbcFileFormat.Nullable(sig.baseType.toCbc)
    case sig: SignatureType.NonNullableWrapper => CbcFileFormat.NonNullable(sig.baseType.toCbc)
    case sig: SignatureType.CangjieArray       => CbcFileFormat.CangjieArray(sig.elemType.toCbc)
    case sig: SignatureType.CPointer           => CbcFileFormat.CPointer(sig.pointee.toCbc)
    case sig: SignatureType.VArray             => CbcFileFormat.VArray(sig.elemType.toCbc, sig.length)
    case sig: SignatureType.LocalTypeVariable  => CbcFileFormat.FuncTypeVariable(sig.idx)
    case sig: SignatureType.ClassTypeVariable  => CbcFileFormat.ClassTypeVariable(sig.idx)

    case sig: SignatureType.Tuple => CbcFileFormat.Tuple(sig.params.map(_.toCbc))
    case sig: SignatureType.Box => notImplemented(sig)

    case sig: SignatureType.ArraySlice => notImplemented(sig)
    case sig: SignatureType.JavaArray => notImplemented(sig)
    case sig: SignatureType.CangjieEnumWrapper => notImplemented(sig)
  }
}
