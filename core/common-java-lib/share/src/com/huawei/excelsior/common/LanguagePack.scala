/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere

/** Enumerates possible combinations of [[Language]]s supported by a given configuration of JET framework. */
enum LanguagePack(private val name: String, languages: Language*) {
  case NONE extends LanguagePack("none", Language.AJ)
  case JAVA extends LanguagePack("java", Language.AJ, Language.JAVA)
  case CANGJIE extends LanguagePack("cangjie", Language.AJ, Language.CANGJIE)
  case CANGJIE_JAVA extends LanguagePack("cangjie-java", Language.AJ, Language.CANGJIE, Language.JAVA)
  case SCALA extends LanguagePack("scala", Language.AJ, Language.SCALA)

  /** Returns whether a given language is supported by this language pack. */
  def supports(lang: Language): Boolean = languages contains lang

  override def toString = name
}

object LanguagePack {
  def apply(name: String): LanguagePack = VALUES
    .find(_.name.equalsIgnoreCase(name))
    .getOrElse(shouldNotReachHere(s"Unknown language pack: $name"))

  private val VALUES = values
}
