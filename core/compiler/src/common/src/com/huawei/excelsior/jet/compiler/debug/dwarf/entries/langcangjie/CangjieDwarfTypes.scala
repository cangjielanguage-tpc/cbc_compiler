/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langcangjie

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Names.*
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Types.sigType
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf.typeProvider
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.Types
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.AbbreviationsElements.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.*
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*

import scala.annotation.tailrec

/** Cangjie-specific types description for DWARF debug format.
  *
  * @author orangebyte256
  * @author gatimosh
  * @author conwor
  */
object CangjieDwarfTypes {
  @tailrec def isDeref(tpe: DebugType): Boolean = tpe match {
    case dt: DTStaticField => isDeref(dt.baseType)
    case DTConst(baseType) => isDeref(baseType)
    case dt: DTOption => isDeref(dt.payload)
    case _: DTArraySlice | _: DTRecord | _: DTCustom | _: DTEnumeration | _: DTPayloadEnumHeir => false
    case _: DTPayloadEnumeration | _: DTArray | _: DTClass | _: DTPointer | _: DTInterface => true
  }
}

final class CangjieDwarfTypes(implicit typeProvider: TypeProvider) extends Types {
  // CJDB demands some types to be declared outside of compilation unit namespace, while all the others go inside
  override def isOutsideNamespaceType(`type`: DebugType) = `type` match {
    case _: DTArray | DTPayloadEnumHeir(_,_) | DTPayloadEnumeration(_,_) | DTRecord(DTCompound.ObjectName, DTCompound.ObjectName) => true
    case _ => false
  }

  private val EnumPayloadsOffsShift = 16 + 8 // 16 - object header, 8 - EnumId

  private def calcFieldOffset(host: DTCompound, symHost: Option[ClassType], field: DTInstanceField, fieldNumber: Int): Int =
    if (symHost.isEmpty || field.baseType == DTUnit()) 0 else {
      val fieldSymName = constructFieldName(host, field, fieldNumber)
      val symField = host match {
        case _: DTPayloadEnumHeir if field.name.equals2("EnumId$$") => asClassType(symHost.get.getSuperClassSig).findDeclaredFieldOrNull(fieldSymName) // TODO super add to SignatureType?
        case _ => symHost.get.findDeclaredFieldOrNull(fieldSymName)
      }
      // symField can be null when field Type is Unit or it is a compound-of-unit
      if (symField == null) 0 else symField.getInstanceFieldOffset
    }

  override protected def emitType(debugType: DebugType, entry: Dwarf.Entry): Unit = debugType match {
    case DTCustom(DTUnit.name, sizeInBytes, 0) =>
      assert(sizeInBytes == 1)
      toPubnames.add(debugType)
      entry.abbreviation(CangjieVoidType)(DTUnit.name, sizeInBytes)

    case DTCustom(name, sizeInBytes, dwarfEncoding) =>
      toPubnames.add(debugType)
      entry.abbreviation(AbbrDTCustomByteSized)(name, dwarfEncoding, sizeInBytes)

    case DTConst(base)                => entry.abbreviation(AbbrDTConst)(label(base))
    case DTPointer(target)            => entry.abbreviation(AbbrDTPointer)(label(target))
    case DTInterface(name, _)         => entry.abbreviation(CangjieVoidType)(name, 1)

    case DTArray(name, elemType) =>
      toPubnames.add(debugType)
      entry.abbreviationScope(AbbrDTArray)(name, 16) {
        entry.abbreviation(TypeMember)(XString("_header0"), label(LONG), 0)
        entry.abbreviation(TypeMember)(XString("_header8"), label(INT),  8)
        entry.abbreviation(TypeMember)(XString("size"),     label(INT),  12)
        entry.abbreviation(TypeMember)(XString("elements"), label(elemType), 16)
      }

    case dt @ DTEnumeration(name, _, baseType) =>
      toPubnames.add(debugType)
      entry.abbreviationScope(AbbrDTEnumeration)(label(baseType), name, baseType.sizeInBytes / 8) {
        for (DTEnumerator(name, value) <- dt.elements) {
          entry.abbreviation(AbbrDTEnumerator) (name, value)
        }
      }

    case debugType: DTCompound =>
      toPubnames.add(debugType)

      val symType = sigType(debugType, typeProvider).map(_.symType(typeProvider).asInstanceOf[ClassType])
      val objSize = symType.map(_.getRawObjectSize).getOrElse(0)

      val (abbr, params) = debugType match {
        case _: DTClass | _: DTPayloadEnumeration => (ClassTypeSpec, List(DW_CC_PASS_BY_REF, debugType.name, objSize))
        case _ => (AbbrDTRecord, List(debugType.name, objSize))
      }

      entry.abbreviationScope(abbr)(params) {
        debugType.superTypes.foreach { ancestor => entry.abbreviation(TypeBase)(label(ancestor), 0) }

        var fieldNumber = 0
        for (elem <- debugType.elements) {
          elem match {
            case field @ DTInstanceField(name, isFake) =>
              val abbreviationForm = if (isFake) FakeTypeMember else TypeMember

              debugType match {
                case DTPayloadEnumHeir(_,_) if name.equals2("EnumClass$") =>
                  // field type (_type subtype for Enum-constructor) better be generated inside EnumHeir DIE
                  val innerType = field.baseType
                  val innerTypeLabel = entry.newLabel

                  // EnumClass$ field offset is always EnumPayloadsOffsShift
                  entry.abbreviation(abbreviationForm)(name, innerTypeLabel, EnumPayloadsOffsShift)

                  assert(symType.nonEmpty)
                  assert(innerType.isInstanceOf[DTCompound])
                  assert(innerType.name.equals2("_type"))
                  entry.bind(innerTypeLabel)
                  // _type subtype for enum-constructor has the same identifier as enum-constructor
                  // so type size and member's offsets should be taken from symType and shifted by EnumPayloadsOffsShift
                  val innerTypeSize = objSize - EnumPayloadsOffsShift
                  assert(innerTypeSize >= 0)

                  entry.abbreviationScope(AbbrDTRecord)(List(innerType.name, innerTypeSize)) {
                    var innerFieldNumber = 1
                    for (elem <- innerType.asInstanceOf[DTCompound].elements) {
                      elem match {
                        case innerField @ DTInstanceField(innerFieldName, false) =>
                          var innerFieldOffset = calcFieldOffset(debugType, symType, innerField, innerFieldNumber)

                          // do the shift
                          innerFieldOffset = if (innerFieldOffset > 0) innerFieldOffset - EnumPayloadsOffsShift else 0
                          assert(innerFieldOffset >= 0)

                          entry.abbreviation(TypeMember)(innerFieldName, label(innerField.baseType), innerFieldOffset)
                        case _ =>
                      }
                      innerFieldNumber += 1
                    }
                  }
                case _ =>
                  val offset = calcFieldOffset(debugType, symType, field, fieldNumber)
                  entry.abbreviation(abbreviationForm)(name, label(field.baseType), offset)
              }

              fieldNumber += 1

            case _ => // static fields are not generated into containing type
          }
        }
      }
  }
}