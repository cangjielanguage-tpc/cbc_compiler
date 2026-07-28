/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langcangjie

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.stackPointer
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.CodeRecord.Interval
import com.huawei.excelsior.jet.compiler.debug.cangjie.CangjieDebugToolbox.Names.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfEmitter.ExprLoc
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CommonToolbox.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CompilationUnit
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.AbbreviationsElements.DW_AT_linkage_name
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.*
import com.huawei.excelsior.jet.compiler.debug.info.DebugLabels.LocalVarLabel
import com.huawei.excelsior.jet.compiler.debug.info.{DTCompUnit, DTCustom, DTField, DTStaticField, DTUnit, DebugDeclaration, DebugLocalVar}
import com.huawei.excelsior.jet.compiler.ir.LineNumber
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field}

/** Cangjie-specific compilation unit.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */
final class CangjieCompilationUnit(`package`: ClassType)(implicit tp: TypeProvider) extends CompilationUnit(`package`.getDebugType.asInstanceOf[DTCompUnit].cuInfo) {
  protected val types = new CangjieDwarfTypes


  /////////////////////////////////////////////////////////////////////////////
  // Transformation of incoming data (symlevel objects, segments) to intermediate (field, local and method info)

  private case class FieldInfo(publicName: XString, abbr: Abbreviation, params: Seq[Any])
  private case class LocalInfo(abbr: Abbreviation, params: Seq[Any])
  private case class MethodInfo(publicName: Option[XString], abbr: Abbreviation, params: Seq[Any], locals: Seq[LocalInfo], interval: Interval)

  private def extractStaticFieldOfUnitTypeInfo(dtSField: DTStaticField): FieldInfo = {
    val name = dtSField.name
    val ln = Option(dtSField.linkageName).getOrElse(name)
    val publicName = constructPubName(unitName(), name)
    val exprLoc = ExprLoc(Location.mem(stackPointer), false) // cjdb expects some readable memory for Unit
    val abbr = if (dtSField.isLocal) GlobalWithLocAndLn else GlobalExtWithLocAndLn
    val params = List(name, exprLoc, types.label(DTUnit()), ln)
    FieldInfo(publicName, abbr, params)
  }

  private def extractStaticFieldInfo(field: Field): FieldInfo = {
    assert(field.isStatic)
    val publicName = staticFieldPublicName(unitName(), field)
    val loc = Location.mem(field.getStaticFieldSymbol, field.getStaticFieldOffset)
    val `type` = field.getDebugType
    val exprLoc = ExprLoc(loc, CangjieDwarfTypes.isDeref(`type`))
    val sourceName = memberSourceName(field)

    def fieldInfoParts(ln: XString, abbrBase: Abbreviation, abbrWithLn: Abbreviation, paramsBase: List[Any]) = {
      assert(abbrBase.attributes.length + 1 == abbrWithLn.attributes.length)
      assert(abbrWithLn.attributes.last == DW_AT_linkage_name)
      if (ln != null) {
        (abbrWithLn, paramsBase :+ ln)
      } else {
        (abbrBase, paramsBase)
      }
    }

    def sfWithLn(f: DTField, ln: XString) = f match {
      case sf: DTStaticField => sf.linkageName == ln
      case _ => false
    }

    def sfIsGlobal(field: Field, ln: XString): Boolean = {
      if (!field.getDeclaringClass.isCangjiePackage) {
        return false
      }
      field.getDeclaringClass.getDebugType.asInstanceOf[DTCompUnit].elements.find(sfWithLn(_, ln)) match {
        case Some(DTStaticField(_, _, isLocal)) => !isLocal
        case _ => false
      }
    }

    // attribute DW_AT_external must be provided for non-local globals (static fields of module)
    val ln = fieldDwarfLinkageName(field)
    val isGlobal = sfIsGlobal(field, ln) // global static field needs 'external' attribute

    val (abbr, params) = field.getSourceFile match {
      case sourceFile if sourceFile != null =>
        val abbrBase = if (isGlobal) GlobalExtWithLocAndDecl else GlobalWithLocAndDecl
        val abbrWithLn = if (isGlobal) GlobalExtWithLocDeclAndLn else GlobalWithLocDeclAndLn
        val params = List(sourceName, exprLoc, types.label(`type`), sources.id(sourceFile), field.getSourceLine)
        fieldInfoParts(ln, abbrBase, abbrWithLn, params)
      case _ =>
        val abbrBase = if (isGlobal) GlobalExtWithLoc else GlobalWithLoc
        val abbrWithLn = if (isGlobal) GlobalExtWithLocAndLn else GlobalWithLocAndLn
        val params = List(sourceName, exprLoc, types.label(`type`))
        fieldInfoParts(ln, abbrBase, abbrWithLn, params)
    }

    FieldInfo(publicName, abbr, params)
  }

  private def extractLocalInfo(label: LocalVarLabel): LocalInfo = {
    val LocalVarLabel(local @ DebugLocalVar(name, tpe, _, isPointer, decl), loc: Location, _) = label
    var abbr = if (local.isArgument) FormalParameter else VarWithLoc
    val needDeref = isPointer || CangjieDwarfTypes.isDeref(tpe)
    var params: List[Any] = List(name, ExprLoc(loc, needDeref), types.label(tpe))
    decl match {
      case Some(DebugDeclaration(file, line, _, _)) =>
        abbr = if (local.isArgument) FormalParameterWithDecl else VarWithLocAndDecl
        params = params ++ List(sources.id(file), line)
      case _ =>
    }
    LocalInfo(abbr, params)
  }

  private def extractMethodInfo(record: CodeRecord): MethodInfo = {
    // TODO move the assert to append(CodeRecord)
    // assert(record.root.children.isEmpty) // TODO-DWARF-CANGJIE: enable inline
    val method = record.scope
    val seg = record.seg
    val publicName = methodPublicName(unitName(), method)
    val sourceName = memberSourceName(method)
    val linkageName = methodDwarfLinkageName(method)
    val sourceFile = if (method.hasSourceFile) sources.id(method.getSourceFile) else 0
    val sourceLine = if (LineNumber.isKnown(method.getSourceLine)) method.getSourceLine else 1
    val returnType = if (method.getDebugType != null) method.getDebugType else DTCustom(method.getReturnType.symKindErased)
    val locals = record.localVariables.toSeq.sortWith(LocalVarLabel.lessThan)
    val abbr = if (methodIsMain(unitName(), method.getSourceName)) {
      hasMainRecord = true
      CangjieSubprogramMain
    } else {
      CangjieSubprogram
    }

    MethodInfo(publicName, abbr,
      List(sourceName, linkageName, sourceFile, sourceLine, types.label(returnType), stackPointer, method, seg.length),
      locals map extractLocalInfo,
      record.root)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Intermediate data encoding

  private val body = new Dwarf.Entry()
  private var hasMainRecord = false

  `package`.getDebugType.asInstanceOf[DTCompUnit].elements foreach {
    case sf: DTStaticField if sf.baseType == DTUnit() =>
      val FieldInfo(publicName, abbr, params) = extractStaticFieldOfUnitTypeInfo(sf)
      pubName(publicName, body.newBoundLabel)
      body.abbreviation(abbr)(params)
    case sf: DTStaticField if `package`.getDebugType == sf.scope => // fields of module scope can also be in DTCompUnit
  }

  for (field <- `package`.getDeclaredFields if field.getDebugType != null) {
    val FieldInfo(publicName, abbr, params) = extractStaticFieldInfo(field)
    pubName(publicName, body.newBoundLabel)
    body.abbreviation(abbr)(params)
  }

  def generateInterval(interval: CodeRecord.Interval): Unit = {
    val lb = interval.context.lexBlock
    assert(lb != null) // for now we support ony pure lexcial blocks without inilining

    val method = interval.context.method
    body.abbreviationScope(LexicalBlock)(Location.mem(method, interval.start), interval.end - interval.start) {
      for (LocalInfo(abbr, params) <- interval.localVariables map extractLocalInfo) { // locals of the block
        body.abbreviation(abbr)(params)
      }
      interval.children.filter(_.containsSomeVariables) foreach { child => generateInterval(child) } // inner blocks
    }
  }

  override def append(record: CodeRecord): Unit = if (!isArtificial(record.scope)) {
    val MethodInfo(publicName, abbr, params, locals, rootInterval) = extractMethodInfo(record)
    publicName foreach { name =>
      pubName(name, body.newBoundLabel)
    }
    body.abbreviationScope(abbr)(params) {
      for (LocalInfo(abbr, params) <- locals) {
        body.abbreviation(abbr)(params)
      }
      rootInterval.children.filter(_.containsSomeVariables) foreach { child => generateInterval(child) }
    }
    super.append(record)
  }

  override def isMain: Boolean = hasMainRecord

  // Cangjie CU full body goes inside namespace
  override protected def finishedBodies(): NsInsideOutsidePartsProvider = () => (body, new Dwarf.Entry())
}
