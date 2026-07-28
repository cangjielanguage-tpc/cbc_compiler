/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc.preparation

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.cbc.CodeSigSymbol
import com.huawei.excelsior.jet.compiler.opt.backend.preparation.FieldChains
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseIsa12
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, Field, SignatureType}

import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait FieldChainsCBC extends FieldChains { self: Universe =>

  private def transformObjForChainReplacement(obj: Node): Node = obj match {
    case arrayGet: ArrayGet if arrayGet.arrayType.isRecordArray =>
      assert(env.enabled(UseIsa12))
      RecordArrayGet(arrayGet.arrayType)(arrayGet.array, arrayGet.idx)

    case _ => obj
  }

  def replaceChainRead(origin: FieldRead, obj: Node, chain: List[FieldRef]): Unit = {
    origin.replaceBy(FieldChainRead.proto(permanent(chain), chain.head.refType, chain.last.valueType)
      .withExplicitArgs(origin.inCtrl, origin.inMemory, transformObjForChainReplacement(obj)))
  }

  def replaceChainWrite(origin: FieldWrite, obj: Node, value: Node, chain: List[FieldRef]): Unit = replaceByCode(origin) {
    FieldChainWrite(permanent(chain), chain.head.refType, chain.last.valueType)(transformObjForChainReplacement(obj), value)
  }

  def replaceOneChain(origin: Node, obj: Node, chain: List[FieldRef]): Unit = origin match {
    case origin: GetField =>
      replaceChainRead(origin, obj, chain)
    case origin: BitcodeDeferred.GetField if !origin.fieldRef.isStatic =>
      replaceChainRead(origin, obj, chain)
    case origin: UniversalGeneric.GetField =>
      replaceChainRead(origin, obj, chain)

    case origin: PutField =>
      replaceChainWrite(origin, obj, origin.inValue0, chain)
    case origin: BitcodeDeferred.PutField if !origin.fieldRef.isStatic =>
      replaceChainWrite(origin, obj, origin.inValue, chain)
    case origin: UniversalGeneric.PutField =>
      replaceChainWrite(origin, obj, origin.value, chain)
  }

  protected def collectFieldChains(): Unit = withRecordConversion {
    // Look up from the end of field access chain to the root object
    for {
      fieldOp <- (all[GetField] ++ all[PutField]).toList
      field = fieldOp.field if field.getDeclaringClass.isRecord && !field.getType.isRecord
      (obj, chain) <- collectOneChain(fieldOp.obj, List(FieldRef(field)))
    } {
      replaceOneChain(fieldOp, obj, chain)
    }

    for {
      fieldOp <- all[BitcodeDeferred.FieldOp].filterNot(_.fieldRef.isStatic).toList
      field = fieldOp.fieldRef if field.refType.isRecord && !field.fieldType.isRecord
      (obj, chain) <- collectOneChain(fieldOp.obj, List(FieldRef(field)))
    } {
      replaceOneChain(fieldOp, obj, chain)
    }

    for {
      fieldOp <- (all[UniversalGeneric.GetField] ++ all[UniversalGeneric.PutField]).toList
      field = fieldOp.field if fieldOp.instantiatedRefType.isRecord && !fieldOp.instantiatedFieldType.isRecord
      (obj, chain) <- collectOneChain(fieldOp.obj, List(FieldRef(Some(fieldOp.instantiatedRefType), field, Some(fieldOp.instantiatedFieldType))))
    } {
      replaceOneChain(fieldOp, obj, chain)
    }

    for {
      copy <- all[CopyStructure].toList
      (dstObj, dstChain) <- collectOneChain(copy.dst, List.empty)
      (srcObj, srcChain) <- collectOneChain(copy.src, List.empty)
    } {
      replaceByCode(copy) {
        (dstObj, srcObj) match {
          case (_: ArrayGet, _: ArrayGet) | (_: Void, _: Void) if env.enabled(UseIsa12) => // cannot generate with one instruction - split into copies through local
            val localCopy = StackAlloc.Local(copy.structureType, workaroundForNonZeroedTraceableRecords = true)
            if (localCopy.zeroed) {
              ZeroRefs(localCopy)
            }
            CopyStructureCBC(copy.structureType, ValueType(copy.structureType), srcObj.tpe,
              List.empty, permanent(srcChain), false, srcChain.headOption.exists(_.isStatic))(localCopy, transformObjForChainReplacement(srcObj))
            CopyStructureCBC(copy.structureType, dstObj.tpe, ValueType(copy.structureType),
              permanent(dstChain), List.empty, dstChain.headOption.exists(_.isStatic), false)(transformObjForChainReplacement(dstObj), localCopy)

          case _ =>
            CopyStructureCBC(copy.structureType, dstObj.tpe, srcObj.tpe,
              permanent(dstChain), permanent(srcChain), dstChain.headOption.exists(_.isStatic), srcChain.headOption.exists(_.isStatic))(
              transformObjForChainReplacement(dstObj), transformObjForChainReplacement(srcObj))
        }

      }
    }
  }

  private def convertRecord(tpe: Type, n: Node): Node = (n.tpe, tpe) match {
    case (from, to) if from == to => n

    case (RecordAddrType(x), RecordAddrType(y)) if x.isArraySliceLike && y.isArraySliceLike => n

    case (from: RecordAddrType, to: RecordAddrType) =>
      // Such casts can happen when the same record is instantiated in different packages with different mangled names.
      // TODO: check actual layout of records or better -- prohibit such casts at all
      assert(from.sigType.getRawObjectSize == to.sigType.getRawObjectSize, s"inconsistent record type size: cast $from -> $to")
      ReinterpretCast(from, to)(n)

    case (from @ (_: RecordAddrType | AddrType), to @ (_: RecordAddrType | AddrType)) =>
      // Such casts are needed to convert @C structs to/from C pointers.
      ReinterpretCast(from, to)(n)

    case _ => n
  }

  private def withRecordConversion[T](action: => T): T = {
    Node.withImplicitArgConversion(convertRecord) {
      action
    }
  }
}
