/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.cangjie

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{Environment, TypeProvider}
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Names.{compoundTypeNameDeprecated, debugTypeName}
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf.linkageName
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CommonToolbox.*
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.llvm.bitcode.{Bitcode, DIFlag, DwTag}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.FailOnUnknownDebugTypeInCBC
import com.huawei.excelsior.jet.compiler.symlevel.{Field, Method, SignatureType}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{ArraySlice, fromSymType}

import scala.annotation.nowarn

/** Box of cangjie-specific tools for debug support.
  *
  * TODO-DWARF: should be reviewed and simplified
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
@nowarn("msg=match may not be exhaustive")
object CangjieDebugToolbox {

  object Types {
    private def pointerWrapper(tpe: DebugType): DebugType = tpe match {
      case _: DTArray | _: DTClass | _: DTPayloadEnumHeir | _: DTPayloadEnumeration => DTPointer(tpe)
      case _ => tpe
    }

    private def bitcodeTypeOfElementsToDebugType(scope: DTCompound, elements: Array[MDItem]): (Array[DebugType], Array[DTField]) = {
      elements.map(x => x.resolve()).filter(x => x.isInstanceOf[DIDerivedType]).map { case tpe: DIDerivedType => tpe }
        .foldLeft((Array[DebugType](), Array[DTField]()))((elemTypes, tpe) => tpe.tag match {
          case DwTag.DW_TAG_inheritance =>
            (elemTypes._1 :+ bitcodeTypeToDebugType(tpe.baseType), elemTypes._2)
          case DwTag.DW_TAG_member =>
            val name = XString(tpe.name)
            var baseType = bitcodeTypeToDebugType(tpe.baseType)
            val res = if (tpe.flags.contains(DIFlag.FlagStaticMember)) {
              // TODO-DWARF think about wrapping with pointer for static members
              DTStaticField(name)
            } else {
              baseType = if (scope.isInstanceOf[DTOption]) baseType else pointerWrapper(baseType)
              DTInstanceField(name)
            }
            res.baseType = baseType
            res.scope = scope
            (elemTypes._1, elemTypes._2 :+ res)
          case tag => shouldNotReachHere("DIDerivedType " + tpe.name + "has unsupported tag " + tag)
        })
    }

    private val NameOfUnitType = XString("Unit")
    private val TypePostfix = XString(".Type")
    private val EmptyName = XString.empty
    private val PayloadEnumHeirFirstFieldTypeName = "Int64"

    private val convertedBitcodeTypes = collection.mutable.Map[String, DebugType]()

    private def getBitcodeTypeName(tpe: MDItem): String = tpe.resolve() match {
      case tpe: Bitcode.DIBasicType => tpe.name
      case tpe: Bitcode.DIDerivedType => tpe.name
      case tpe: Bitcode.DICompositeType => tpe.identifier
      case _: Bitcode.DISubroutineType | MDItem.INVALID => "Unit"
    }

    def bitcodeTypeToDebugType(tpe: MDItem): DebugType = {
      var postpondAction: () => Unit = () => ()

      def addPostponedAction[T <: DebugType](actor: T, action: T => Unit): DebugType = {
        postpondAction = () => action(actor)
        actor
      }

      def isPayloadEnumeration(tpe: Bitcode.DICompositeType): Boolean = {
        val fieldType = tpe.elements.elts(0).asInstanceOf[Bitcode.DIDerivedType].baseType
        fieldType.isInstanceOf[Bitcode.DICompositeType] &&
          fieldType.asInstanceOf[Bitcode.DICompositeType].tag == DwTag.DW_TAG_enumeration_type
      }

      def isPayloadEnumHeir(tpe: Bitcode.DICompositeType): Boolean = {
        val fieldType = tpe.elements.elts(0).asInstanceOf[Bitcode.DIDerivedType].baseType
        fieldType.isInstanceOf[Bitcode.DIBasicType] &&
          fieldType.asInstanceOf[Bitcode.DIBasicType].name.equals(PayloadEnumHeirFirstFieldTypeName)
      }

      def isArraySlice(tpe: Bitcode.DICompositeType): Boolean = {
        val fieldType = tpe.elements.elts(0).asInstanceOf[Bitcode.DIDerivedType].baseType
        fieldType.isInstanceOf[Bitcode.DICompositeType] && isRawArray(fieldType.asInstanceOf[Bitcode.DICompositeType])
      }

      // it is agreed with frontend that DICompositeType + FlagArtificial always means raw-array
      def isRawArray(tpe: Bitcode.DICompositeType): Boolean = tpe.flags.contains(DIFlag.FlagArtificial)

      val foundElement = convertedBitcodeTypes.get(getBitcodeTypeName(tpe))
      if (getBitcodeTypeName(tpe) != null && foundElement.isDefined) {
        return foundElement.get
      }

      val resultDebugType: DebugType = tpe.resolve() match {
        case tpe: Bitcode.DIBasicType =>
          assert(tpe.sizeInBits % 8 == 0)
          DTCustom.applyWithBasicTypes(XString(tpe.name), tpe.sizeInBits / 8, tpe.encoding)

        case tpe: Bitcode.DIDerivedType =>
          if (tpe.flags.contains(DIFlag.FlagStaticMember)) {
            bitcodeTypeToDebugType(tpe.baseType)
          } else {
            (bitcodeTypeToDebugType(tpe.baseType), tpe.tag) match {
              case (baseType, DwTag.DW_TAG_pointer_type) => DTPointer(baseType)
              case (baseType, DwTag.DW_TAG_const_type) => DTConst(baseType)
              case (baseType, _) => baseType
            }
          }

        case tpe: Bitcode.DICompositeType if tpe.tag == DwTag.DW_TAG_structure_type =>
          (XString(tpe.name), if (tpe.identifier == null) EmptyName else XString(tpe.identifier),
            tpe.elements.elts.filter(item => !item.isInstanceOf[DISubprogram])) match {
            case (NameOfUnitType, identifier, _) if identifier.endsWith(TypePostfix) =>
              // TODO-DWARF implement other cases
              DTUnit()
            case (name, _, Array(_, elements: DIDerivedType)) if isRawArray(tpe) =>
              addPostponedAction(DTArray(name),
                (actor: DTArray) => actor.elemType = pointerWrapper(bitcodeTypeToDebugType(elements.baseType)))
            case (name, identifier, elts) =>
              (tpe.flags.contains(DIFlag.FlagTypePassByReference), elts) match {
                case (true, Array()) =>
                  DTInterface(name, identifier)
                case (passByReference, elts) =>
                  assert(!passByReference || elts(0).asInstanceOf[DIDerivedType].tag == DwTag.DW_TAG_inheritance)
                  val fieldNames = elts.map(x => x.resolve()).filter(x => x.isInstanceOf[DIDerivedType]).map(field => field.asInstanceOf[DIDerivedType].name)
                  val dt = fieldNames match {
                    case Array("rawptr") if isArraySlice(tpe) => DTArraySlice(name, identifier)
                    case Array("EnumId$") | Array("EnumId$", "EnumClass$") if isPayloadEnumHeir(tpe) => DTPayloadEnumHeir(name, identifier)
                    case Array("constructor", "val") if tpe.name.startsWith("Option") => DTOption(name, identifier)
                    case Array("constructor", _*) if isPayloadEnumeration(tpe) => DTPayloadEnumeration(name, identifier)
                    case _ => if (passByReference) DTClass(name, identifier) else DTRecord(name, identifier)
                  }
                  addPostponedAction(dt, (actor: DTCompound) => {
                    val (ancestors, fields) = bitcodeTypeOfElementsToDebugType(actor, elts)
                    actor.superTypes = ancestors
                    actor.elements = actor match {
                      case _: DTArraySlice =>
                        assert(fields.length == 1)
                        val start = DTInstanceField(XString("start"), DTCustom.Int64Instance, actor)
                        val len = DTInstanceField(XString("len"), DTCustom.Int64Instance, actor)
                        fields :+ start :+ len
                      case _ => fields
                    }
                    if (actor.isInstanceOf[DTPayloadEnumeration]) {
                      assert(fields.length == 1 && fields(0).baseType.isInstanceOf[DTEnumeration])
                    }
                    if (actor.isInstanceOf[DTPayloadEnumHeir]) {
                      assert(fields.length >= 1 && fields(0).baseType.name.equals2(PayloadEnumHeirFirstFieldTypeName))
                    }
                  })
              }
          }

        case tpe: Bitcode.DICompositeType if tpe.tag == DwTag.DW_TAG_enumeration_type =>
          val enumenators = tpe.elements.elts.map {
            case enumerator: DIEnumerator => DTEnumerator(XString(enumerator.name), enumerator.value)
          }
          assert(tpe.baseType.isInstanceOf[Bitcode.DIBasicType])
          addPostponedAction(DTEnumeration(XString(tpe.name), XString(tpe.identifier), bitcodeTypeToDebugType(tpe.baseType).asInstanceOf[DTCustom]),
            (actor: DTEnumeration) => actor.elements = enumenators
          )

        // TODO-DWARF implement it
        case tpe: Bitcode.DICompositeType if tpe.tag == DwTag.DW_TAG_array_type =>
          DTUnit()

        // TODO-DWARF implement it
        case _: Bitcode.DISubroutineType | MDItem.INVALID =>
          DTUnit()
      }

      // There is no guarantee that DIDerivedType has unique name, it's better to create new type from scratch
      if (!tpe.isInstanceOf[DIDerivedType]) {
        convertedBitcodeTypes(getBitcodeTypeName(tpe)) = resultDebugType
      }

      postpondAction()

      resultDebugType
    }

    def sigType(debugType: DebugType, env: Environment): Option[SignatureType] =
      sigType(debugType, env.getTypeProvider, env.enabled(FailOnUnknownDebugTypeInCBC))

    def sigType(debugType: DebugType, typeProvider: TypeProvider, failOnUnknown: Boolean = false): Option[SignatureType] = {
      def unknown(): Option[SignatureType] = {
        if (failOnUnknown) {
          shouldNotReachHere(s"unknown debug type for CBC:\n  name: ${debugType.name}\n  identifier: ${debugType.identifier}")
        }
        None
      }

      val unwrappedDT = unwrapDebugType(debugType)
      val tpe = unwrappedDT match {
        case unwrappedDT: DTCustom =>
          DTCustom.toSignaturePrimitive(unwrappedDT)

        case DTEnumeration(_, _, baseType) =>
          DTCustom.toSignaturePrimitive(baseType) // TODO tune this when signature type for enumeration gets available

        case unwrappedDT: DTArraySlice =>
          unwrapDebugType(unwrappedDT.elements.head.baseType) match {
            case baseArr: DTArray => sigType(baseArr.elemType, typeProvider, failOnUnknown) map ArraySlice.apply
          }

        case _: DTCompound | _: DTInterface =>
          Option(typeProvider.findClass(debugTypeName(unwrappedDT))) map fromSymType

        case _ => None
      }

      tpe orElse { // last hope - to be dropped some time later
        debugType match {
          case x: DTCompound => Option(typeProvider.findClass(compoundTypeNameDeprecated(x))) map fromSymType
          case _ => None
        }
      } orElse unknown()
    }
  }


  object Names {
    def methodPublicName(unitName: XString, method: Method): Option[XString] = {
      val sourceName = method.getSourceName

      // gen public name for methods with line number info
      // some methods (like _ZN7default06ExtendC_ZN7default7Clazz_1E3fooj) may have malformed SourceFullName
      // produce for them pubnames like default::foo
      if (sourceName != null && method.getSourceLine > 1) { // TODO-DWARF: why 1???
        val sourceFullName = method.getSourceFullName
        if (sourceFullName.endsWith(sourceName)) {
          Some(XString(sourceFullName.platformToString.replace(".", "::"))) // good sourceFullName
        } else {
          Some(constructPubName(unitName, sourceName))
        }
      } else {
        None
      }
    }

    private val NameOfDefaultUnit = XString("default")
    private val NameOfMain = XString("main")

    def fieldDwarfLinkageName(field: Field) = {
      val cppLinkageName = field.getCPPLinkageName
      if (cppLinkageName != null) cppLinkageName else linkageName(field.getStaticFieldSymbol)
    }

    def methodDwarfLinkageName(method: Method): XString = {
      val cppName = method.getCPPLinkageName
      if (cppName != null) cppName else linkageName(method)
    }

    def methodIsMain(unit: XString, name: XString): Boolean = NameOfDefaultUnit == unit && NameOfMain == name

    def staticFieldPublicName(unitName: XString, field: Field): XString = {
      constructPubName(unitName, memberSourceName(field))
    }

    // TODO-DWARF test that front provides correct type-name <=> identifier pairs and get rid of it
    def compoundTypeNameDeprecated(tpe: DTCompound): XString = {
      var identifier = tpe.identifier
      val usePrefix = !tpe.isInstanceOf[DTPayloadEnumeration] && !tpe.isInstanceOf[DTPayloadEnumHeir]
      if (identifier.startsWith(XString("$"))) {
        identifier = identifier.substring(1)
      }
      if (usePrefix) tpe.prefix.concat(identifier) else identifier
    }

    def debugTypeName(tpe: DebugType): XString = {
      var identifier = tpe match {
        case _: DTCompound | _: DTInterface => tpe.identifier
        case _ => shouldNotReachHere("mangleDebugTypeName is supposed to be used only for DTCompound | DTInterface")
      }
      if (identifier.startsWith(XString("$"))) {
        identifier = identifier.substring(1)
      }
      identifier
    }

    def constructFieldName(host: DTCompound, field: DTInstanceField, fieldNumber: Int) = host match {
      case _: DTClass | _: DTRecord | _: DTOption         => field.name
      case _: DTPayloadEnumHeir | _: DTPayloadEnumeration => XString.ascii(String.format("fld$$%d", fieldNumber))
      case _: DTArraySlice                                => XString.ascii(String.format("_%d", fieldNumber))
    }

    // DWARF or cbc debug-info should not be generated for @main(i32 %argc, i8** %argv)
    def isArtificial(method: Method): Boolean =
      method.getDebugType == null && method.getDeclaringClass.isCangjiePackage && method.getName == "main"
  }
}