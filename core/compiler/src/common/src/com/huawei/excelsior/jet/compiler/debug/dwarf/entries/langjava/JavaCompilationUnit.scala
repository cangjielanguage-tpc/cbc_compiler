/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava

import com.huawei.excelsior.jet.assembler.{Label, Location}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.stackPointer
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.debug.CodeRecord
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.DwarfEmitter.ExprLoc
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.CommonToolbox.*
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.langjava.JavaDwarfTypes.typeToDebugType
import com.huawei.excelsior.jet.compiler.debug.dwarf.entries.{CompilationUnit, Types}
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.DebugAbbrev.*
import com.huawei.excelsior.jet.compiler.debug.info.Language.LANG_Java
import com.huawei.excelsior.jet.compiler.debug.info.*
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field}

/** Java-specific compilation unit.
  *
  * @author conwor
  * @author gatimosh
  * @author orangebyte256
  */

// TODO-DWARF need proper values for root dir and producer for CU
final class JavaCompilationUnit(`class`: ClassType)(implicit tp: TypeProvider) extends CompilationUnit(CompilationUnitInfo(XString.ascii(`class`.getName), LANG_Java, XString("."), XString("JET compiler"))) { unit =>
  protected val types: JavaDwarfTypes = new JavaDwarfTypes(this)


  /////////////////////////////////////////////////////////////////////////////
  // Transformation of incoming data (symlevel objects, segments) to intermediate (field, local and method info)

  private def extractStaticFieldInfo(field: Field): FieldInfo = {
    assert(field.isStatic)
    val loc = Location.mem(field.getStaticFieldSymbol, field.getStaticFieldOffset)
    val exprLoc = ExprLoc(loc, Types.isDeref(field.getDebugType))
    val publicName = constructPubName(field.getDeclaringClass.getXName, memberSourceName(field))
    val fieldDeclLabel = types.staticFieldLabel(publicName, typeToDebugType(field.getDeclaringClass))
    FieldInfo(publicName, GlobalStatic, List(fieldDeclLabel, exprLoc))
  }

  private def extractMethodInstanceInfo(methodSpec: Label, record: CodeRecord): MethodInfo = {
    val method = record.scope
    val seg = record.seg
    val locals = types.extractLocalInfo(record)
    val publicName = constructPubName(method.getDeclaringClass.getXName, memberSourceName(method))
    MethodInfo(Some(publicName), JavaSubprogramInstance, List(method, seg.length, stackPointer, methodSpec), locals)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Intermediate data encoding

  private val body = new Dwarf.Entry()

  for (field <- `class`.getDeclaredFields if field.isStatic) {
    if (field.shouldBeGenerated) {
      if (field.getDebugType == null) {
        field.setDebugType(JavaDwarfTypes.typeToDebugType(field.getType))
      }
      val FieldInfo(publicName, abbr, params) = extractStaticFieldInfo(field)
      pubName(publicName, body.newBoundLabel)
      body.abbreviation(abbr)(params)
    }
  }

  override def append(record: CodeRecord): Unit = {
    val methodSpec = types.appendMethodSpec(record)
    val MethodInfo(publicName, abbr, params, locals) = extractMethodInstanceInfo(methodSpec, record)
    publicName foreach { name =>
      pubName(name, body.newBoundLabel)
    }
    body.abbreviationScope(abbr)(params) {
      // locals are really required to be in the -instance part
      for (LocalInfo(abbr, params) <- locals) {
        body.abbreviation(abbr)(params)
      }
    }

    super.append(record)
  }

  // Java CU body goes without any namespace i.e. outside
  override protected def finishedBodies(): NsInsideOutsidePartsProvider = () => (new Dwarf.Entry(), body)
}