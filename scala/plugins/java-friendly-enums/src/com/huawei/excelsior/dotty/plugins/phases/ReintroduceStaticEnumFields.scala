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
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.{Annotations, Contexts, Names}
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.RestoreScopes

/** Bring back static enum field declarations after cleanup in [[RestoreScopes]] phase. */
class ReintroduceStaticEnumFields extends PluginPhase with JavaFriendlyContext {

  import tpd.*

  val phaseName = "reintroduceStaticEnumFields"

  override val runsAfter = Set(RestoreScopes.name)
  override val changesMembers = true

  override def transformTemplate(tmpl: Template)(implicit ctx: Context): Tree = {
    val cls = tmpl.symbol.owner

    if (isJavaFriendlyEnum(cls)) {
      for (enumValue <- allEnumValuesInCompanion(cls)) {
        val sym = newSymbol(cls, enumValue.name.asTermName, staticEnumFieldFlags, enumValue.info)
        sym.addAnnotation(Annotations.Annotation(defn.ScalaStaticAnnot, sym.span))
        sym.entered
      }
    }

    tmpl
  }
}
