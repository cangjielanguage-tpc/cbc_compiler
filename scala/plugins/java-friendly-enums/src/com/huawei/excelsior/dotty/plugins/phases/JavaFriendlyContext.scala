/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.dotty.plugins.phases

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.PluginPhase

trait JavaFriendlyContext {
  self: PluginPhase =>

  private val javaFriendlyName: Names.PreName = "com.huawei.excelsior.dotty.annot.javaFriendly"
  private var _javaFriendlyAnnot: ClassSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context) = {
    _javaFriendlyAnnot = requiredClass(javaFriendlyName)
    ctx
  }

  protected def isJavaFriendlyEnum(sym: Symbol)(using Context) = {
    sym.is(Enum) && sym.hasAnnotation(_javaFriendlyAnnot)
  }

  protected def allEnumValuesInCompanion(cls: Symbol)(using Context): List[Symbol] = {
    cls.companionClass.info.decls.filter(_.isAllOf(EnumValue))
  }

  protected val staticEnumFieldFlags = JavaStatic | JavaEnumValue | Synthetic
}
