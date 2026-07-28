/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.dotty.plugins.phases

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.{Annotations, Names}
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.CompleteJavaEnums

/** Transforms every enum annotated with `@javaFriendly` as described in [[README.md]] for this plugin. */
class CompleteAnnotatedEnums extends PluginPhase with JavaFriendlyContext {

  import tpd.*

  val phaseName = "completeAnnotatedEnums"

  override val runsAfter = Set(CompleteJavaEnums.name)
  override val changesMembers = true

  private val initEnumValuesName: Names.PreName = "$$initEnumValues"

  private def staticUnitMethod(owner: Symbol, name: Names.TermName, flags: FlagSet)(stats: Tree*)(using Context) = {
    val sym = newSymbol(owner, name, flags, MethodType(Nil, defn.UnitType))
    sym.addAnnotation(Annotations.Annotation(defn.ScalaStaticAnnot, sym.span))
    sym.entered

    DefDef(sym, Block(stats.toList, unitLiteral))
  }

  private def transformEnumClass(cls: Symbol, tmpl: Template)(using Context): Tree = {
    val (params, rest) = decomposeTemplateBody(tmpl.body)

    val moduleRef = ref(cls.companionModule)

    val initEnumValues = {
      val classScope = cls.info.decls

      val assignments = allEnumValuesInCompanion(cls).map { companionField =>
        val fieldName = companionField.name.asTermName

        val alreadyDefined = classScope.lookup(fieldName)
        if (alreadyDefined.exists) {
          report.error(s"member with name $fieldName is already defined in $cls", alreadyDefined.sourcePos)
        }

        val newField = newSymbol(cls, fieldName, staticEnumFieldFlags, companionField.info)
        newField.addAnnotation(Annotations.Annotation(defn.ScalaStaticAnnot, newField.span))
        newField.entered

        Assign(moduleRef.select(newField), moduleRef.select(companionField))
      }

      staticUnitMethod(cls, initEnumValuesName.toTermName, Synthetic | Method)(assignments: _*)
    }

    // TODO: ensure that there is no other <clinit> code
    val clinit = staticUnitMethod(cls, nme.STATIC_CONSTRUCTOR, Synthetic | Method | Private) {
      moduleRef.select(cls.companionClass)
    }

    cpy.Template(tmpl)(body = params ++ List(clinit, initEnumValues) ++ rest)
  }

  private def transformEnumCompanion(cls: Symbol, tmpl: Template)(using Context): Tree = {
    val moduleRef = ref(cls.companionModule)

    val initEnumValuesRef = cls.companionClass.info.decls.lookup(initEnumValuesName.toTermName) ensuring (_.exists)
    val initEnumValuesCall = Apply(moduleRef.select(initEnumValuesRef), Nil)

    cpy.Template(tmpl)(body = tmpl.body ++ List(initEnumValuesCall))
  }

  override def transformTemplate(tmpl: Template)(using Context): Tree = {
    val cls = tmpl.symbol.owner

    if (isJavaFriendlyEnum(cls)) {
      transformEnumClass(cls, tmpl)

    } else if (isJavaFriendlyEnum(cls.companionClass)) {
      transformEnumCompanion(cls, tmpl)

    } else tmpl
  }
}
