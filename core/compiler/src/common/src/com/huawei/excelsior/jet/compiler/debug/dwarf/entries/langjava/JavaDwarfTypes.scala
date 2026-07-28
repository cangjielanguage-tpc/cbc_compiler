/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.{Label, Location}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf.typeProvider
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfEmitter.ExprLoc
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CommonToolbox.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.Types
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.Types.pointerWrapper
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava.JavaDwarfTypes.typeToDebugType
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.*
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.LocalVarLabel
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.ir.LineNumber
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{ArraySlice, Box, BString, CPointer, CangjieArray, CangjieEnumWrapper, InstantiatedRecord, InstantiatedReference, JavaArray, Primitive, Record, Reference, ThisTypeInfo, Tuple, TypeVariable, VArray, fromSymType}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, SignatureType, Type}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*

import scala.collection.mutable

case class FieldInfo(publicName: XString, abbr: Abbreviation, params: Seq[Any])
case class LocalInfo(abbr: Abbreviation, params: Seq[Any])
case class MethodInfo(publicName: Option[XString], abbr: Abbreviation, params: Seq[Any], locals: Seq[LocalInfo])

object JavaDwarfTypes {
  def typeToDebugType(tpe: SignatureType)(implicit tp: TypeProvider): DebugType = SignatureType.Wrapper.skip(tpe) match {
    case Primitive(kind) => DTCustom(kind)
    case _: Reference => typeToDebugType(tpe.symType)
    case JavaArray(baseType, dimNum) => DTUnit() // FIXME: figure out array dimensions
    case _: Record | _: ArraySlice | BString | _: CPointer | _: VArray | _: CangjieArray |
         _: InstantiatedReference | _: InstantiatedRecord | _: Tuple | _: TypeVariable | ThisTypeInfo | _: Box =>
      shouldNotReachHere(s"unsupported type $tpe")
  }

  def typeToDebugType(tpe: Type)(implicit tp: TypeProvider): DebugType = {
    if (tpe.isPrimitive)
      DTCustom(tpe.getKind)
    else if (tpe.isAJArray || tpe.isCangjieArray || tpe.isJavaArray)
      DTArray(tpe.getXName, pointerWrapper(typeToDebugType(tpe.getArrayElemType)))
    else if (tpe.isDeferred)
      DTCustom(VOID)
    else if (tpe.isClassOrInterface)
      DTClass(tpe.getXName, tpe.getXName)
    else
      shouldNotReachHere(s"unsupported type ${tpe.getName}")
  }
}

final class JavaDwarfTypes(unit: JavaCompilationUnit)(implicit tp: TypeProvider) extends Types {
  val typeBodiesCache = new mutable.LinkedHashMap[Label, Dwarf.Entry]

  def appendMethodSpec(record: CodeRecord): Label = {
    val dtpe = JavaDwarfTypes.typeToDebugType(record.scope.getDeclaringClass)
    val tpeLabel = label(dtpe)
    val typeBody = this.typeBodiesCache.getOrElseUpdate(tpeLabel, new Dwarf.Entry())
    val MethodInfo(_, abbr, params, _) = extractMethodSpecInfo(record)
    val methodSpecLabel = typeBody.newBoundLabel
    typeBody.abbreviationScope(abbr)(params){ /* no need to repeat locals in the -spec part */ }
    methodSpecLabel
  }

  def getObjectSize(tpe: Type): Int = if (tpe.isErroneousOrAbsent) 0 else tpe.getRawObjectSize

  override protected def emitType(`type`: DebugType, entry: Dwarf.Entry): Unit = `type` match {
    case DTCustom(name, sizeInBytes, dwarfEncoding) =>
      entry.abbreviation(AbbrDTCustomByteSized)(name, dwarfEncoding, sizeInBytes)

    case DTPointer(target) =>
      entry.abbreviation(AbbrDTPointer)(label(target))

    case DTArray(name, elemType) =>
      entry.abbreviationScope(AbbrDTArray)(name, 24) {
        entry.abbreviation(TypeMember)(XString("_header0"),  label(LONG), 0)
        entry.abbreviation(TypeMember)(XString("_header8"),  label(LONG), 8)
        entry.abbreviation(TypeMember)(XString("_header16"), label(INT),  16)
        entry.abbreviation(TypeMember)(XString("size"),      label(INT),  20)
        entry.abbreviation(TypeMember)(XString("elements"),  label(elemType), 24)
      }

    case DTClass(nm, id) =>
      toPubnames.add(`type`)

      val symType = sigType(`type`, typeProvider).map(_.symType(typeProvider).asInstanceOf[ClassType])
      val objSize = symType.map(_.getRawObjectSize).getOrElse(0)
      assert(symType.nonEmpty)

      val (abbr, params) = (JavaClassType, List(`type`.name, objSize, 1)) // TODO-DWARF 1 - is decl_file
      entry.abbreviationScope(abbr)(params) {
        for (field <- symType.get.getDeclaredFields) {
          // TODO-DWARF wrap JavaArray with DTPointer explicitly until it is properly wrapped in Types.pointerWrapper
          val debugType = if (field.getType.symType.isJavaArray) DTPointer(JavaDwarfTypes.typeToDebugType(field.getType))
          else Types.pointerWrapper(JavaDwarfTypes.typeToDebugType(field.getType))

          if (field.isStatic) {
            val labelKey = constructPubName(field.getDeclaringClass.getXName, memberSourceName(field))
            entry.bind(staticFieldLabel(labelKey, typeToDebugType(field.getDeclaringClass)))
            entry.abbreviation(TypeStaticMember)(field.getXName, label(debugType), 1)
          } else {
            entry.abbreviation(TypeMember)(field.getXName, label(debugType), field.getInstanceFieldOffset)
          }
        }

        typeBodiesCache.get(label(`type`)).foreach(typeBody => entry.include(typeBody))
      }

    case _ => shouldNotReachHere("Unsupported")
  }

  // TODO: unify with CangjieDebugToolbox.sigType
  def sigType(debugType: DebugType, typeProvider: TypeProvider, failOnUnknown: Boolean = false): Option[SignatureType] = {
    def unknown(): Option[SignatureType] = {
      if (failOnUnknown) {
        shouldNotReachHere(s"unknown debug type for:\n  name: ${debugType.name}\n  identifier: ${debugType.identifier}")
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
        Option(typeProvider.findClass(unwrappedDT.identifier)) map fromSymType

      case _ => None
    }

    tpe orElse { // last hope - to be dropped some time later
      debugType match {
        case x: DTCompound => Option(typeProvider.findClass(x.identifier)) map fromSymType
        case _ => None
      }
    } orElse unknown()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Transformation of incoming data (symlevel objects, segments) to intermediate (field, local and method info)

  private def extractLocalInfo(varLabel: LocalVarLabel): LocalInfo = {
    val LocalVarLabel(local @ DebugLocalVar(name, tpe, _, isPointer, decl), loc: Location, _) = varLabel
    var abbr = if (local.isArgument) FormalParameter else VarWithLoc
    val needDeref = isPointer || Types.isDeref(tpe)
    var params: List[Any] = List(name, ExprLoc(loc, needDeref), label(tpe))
    decl match {
      case Some(DebugDeclaration(file, line, _, _)) =>
        abbr = if (local.isArgument) FormalParameterWithDecl else VarWithLocAndDecl
        params = params ++ List(unit.sources.id(file), line)
      case _ =>
    }
    LocalInfo(abbr, params)
  }

  def extractLocalInfo(record: CodeRecord): Seq[LocalInfo] = {
    val locals = record.localVariables.toSeq.sortWith(LocalVarLabel.lessThan)
    locals map extractLocalInfo
  }

  private def extractMethodSpecInfo(record: CodeRecord): MethodInfo = {
    val method = record.scope
    val sourceName = method.getXName
    val sourceFile = if (method.hasSourceFile) unit.sources.id(method.getSourceFile) else 0
    val sourceLine = if (LineNumber.isKnown(method.getSourceLine)) method.getSourceLine else 1
    val returnType = if (method.getDebugType != null) method.getDebugType else DTCustom(method.getReturnType.symKindErased)
    val locals = Seq.empty // no need locals for -spec info
    val publicName = None // no need the public name for -spec info
    MethodInfo(publicName, JavaSubprogramSpec, List(sourceName, sourceFile, sourceLine, label(returnType)), locals)
  }

}
