/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.preparation

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.assembler.AsmType.*
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol}
import com.huawei.excelsior.jet.assembler.cbc.{CbcTypeKind, FieldReference}
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.cbc.CodeSigSymbol
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseIsa12
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{InstantiatedType, TypeVariable}
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, CangjieFieldReference, Field, SignatureType}

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.ListBuffer

/** An analysis for record's field access operations that split them into the pair of a host object
  * (traceable reference, stack alloc, static variable, or unknown) and a sequence of subfields
  * from this host object to the given field. Scan goes recursively from the current field access operation
  * to preceding consecutive accesses.
  * 
  * @author ikireev
  * @author arxdukalis
  */
trait FieldChains { self: Universe =>

  case class FieldRef(instantiatedRefType: Option[SignatureType], field: Field | BitcodeFieldReference, instantiatedFieldType: Option[SignatureType]) {

    def isStatic: Boolean = field match {
      case f: Field => f.isStatic
      case b: BitcodeFieldReference => b.isStatic
    }

    def refType: Type = field match {
      case _ if isStatic => VoidType
      case f: Field if instantiatedRefType.isDefined => ValueType(instantiatedRefType.get)
      case f: Field => InstanceFieldOperation.declaringClassType(f)
      case b: BitcodeFieldReference => ValueType(b.refType)
    }

    def fieldType: SignatureType = instantiatedFieldType.getOrElse(field match {
      case f: Field => f.getType
      case b: BitcodeFieldReference => b.fieldType
    })

    def valueType: ValueType = ValueType.fromSig(fieldType, instantiateRich = true)

    def toFieldRef: FieldReference = {
      FieldRef.createFieldRef(field, instantiatedRefType, instantiatedFieldType)
    }
  }

  object FieldRef {
    def apply(field: Field | BitcodeFieldReference): FieldRef = FieldRef(None, field, None)

    def createFieldRef(field: Field | BitcodeFieldReference,
                       instantiatedRefType: Option[SignatureType],
                       instantiatedFieldType: Option[SignatureType]): FieldReference = {
      val f = field match {
        case f: Field => f.getPermanent
        case f: BitcodeFieldReference => f
      }
      (instantiatedRefType, instantiatedFieldType) match {
        case (Some(refType), Some(fieldType)) if fieldType.isVariableSizeType =>
          assert(refType.isVariableLayoutType) // VST field can only be present in VLT type as they must share some type parameters
          FieldReference.forGenericVSTField(CodeSigSymbol(refType), f, CodeSigSymbol(fieldType), CbcTypeKind(fieldType.toAsm))

        case (Some(refType), Some(fieldType)) if refType.isVariableLayoutType =>
          FieldReference.forGenericVariableFieldType(CodeSigSymbol(refType), f, CodeSigSymbol(fieldType), CbcTypeKind(fieldType.toAsm))

        case (Some(refType), Some(fieldType)) if refType.isUniversalGeneric =>
          FieldReference.forGenericConcreteFieldType(CodeSigSymbol(refType), f, CodeSigSymbol(fieldType), CbcTypeKind(fieldType.toAsm))

        case _ =>
          val refType = field match {
            case f: Field => SignatureType.fromSymType(f.getDeclaringClass) // information loss is ok here as type is non-generic
            case f: BitcodeFieldReference => f.refType
          }
          val fieldType = field match {
            case f: Field => f.getType
            case b: BitcodeFieldReference => b.fieldType
          }
          FieldReference.forNonGenericFieldType(CodeSigSymbol(refType), f, CbcTypeKind(fieldType.toAsm))
      }
    }
  }

  type FieldRead = GetField | BitcodeDeferred.GetField | UniversalGeneric.GetField
  type FieldWrite = PutField | BitcodeDeferred.PutField | UniversalGeneric.PutField

  def permanent(chain: List[FieldRef]): List[FieldReference] = {
    if (chain.isEmpty) return List.empty

    val permanents = chain.map(_.toFieldRef)
    permanents.tail.foldLeft(ListBuffer(permanents.head)) {
      (result, curr) =>
        (result.last, curr) match {
          case (prev, _) =>
            assert(prev.isGenericVLT || !curr.isGenericVLT, (prev, curr))
            result :+ curr
        }
    }.toList
  }

  @tailrec
  final def collectOneChain(obj: Node, chain: List[FieldRef]): Option[(Node, List[FieldRef])] = obj match {
    case ReinterpretCast(RecordAddrType(_: SignatureType.VArray), RecordAddrType(_: SignatureType.VArray), arg) =>
      collectOneChain(arg, chain)

    case GetField(prevField, _, _, prevObj) =>
      assert(prevField.getType.isRecord)
      val fieldRef = FieldRef(prevField)
      if (prevField.getDeclaringClass.isRecord) {
        collectOneChain(prevObj, fieldRef :: chain)
      } else {
        Some((prevObj, fieldRef :: chain))
      }

    case op: BitcodeDeferred.GetField =>
      val prevField = op.fieldRef
      val fieldRef = FieldRef(prevField)
      if (!prevField.isStatic) {
        assert(prevField.fieldType.isRecord)
        if (prevField.refType.isRecord) {
          collectOneChain(op.obj, fieldRef :: chain)
        } else {
          Some((op.obj, fieldRef :: chain))
        }
      } else {
        Some((Void(), fieldRef :: chain))
      }

    case op: UniversalGeneric.GetField =>
      val prevField = op.field
      assert(op.instantiatedFieldType.isRecord)
      val fieldRef = FieldRef(Some(op.instantiatedRefType), prevField, Some(op.instantiatedFieldType))
      if (op.instantiatedRefType.isRecord) {
        collectOneChain(op.obj, fieldRef :: chain)
      } else {
        Some((op.obj, fieldRef :: chain))
      }

    case x: UniversalGeneric.FromHolder => collectOneChain(x.arg, chain)

    case _: StackAlloc | _: Param | _: LoadTailParam => // param-passed structs are always stack allocated in caller frame
      assert(obj.tpe.isRecordAddrType)
      Option.when(chain.nonEmpty || env.enabled(UseIsa12))((obj, chain))

    case GetStatic(staticField, _, _) =>
      Some((Void(), FieldRef(staticField) :: chain)) // TODO: replace Void with some specific marker or at least NoValue

    case m: MutFunc.Combine =>
      Option.when(chain.nonEmpty || env.enabled(UseIsa12))((m, chain))

    case arrayGet: ArrayGet if env.enabled(UseIsa12) && arrayGet.arrayType.isRecordArray =>
      Some((arrayGet, chain))

    case _ => None
  }
}
