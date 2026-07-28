/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.options.Option.allByName
import xscala.util.StringOps.*

import scala.collection.mutable

/** Compiler option.
  *
  * @author paul
  * @author conwor
  */
trait Option[T >: Null <: Any] { self: Product =>
  def register(option: Option[_]): Unit = {
    val name = option.name.asciiToLowerCase
    assert(!allByName.contains(name))
    allByName(name) = option
  }

  /** Returns default value of this option in given `env`. */
  def defaultValueOrNull(env: Environment): T

  def smartKind: Option.SmartKind

  def name: String = productPrefix

  def parse(value: String): Any

  def isAlias = false
}

object Option {
  val allByName = new mutable.HashMap[String, Option[_]]

  def ensureAllClinited(): Unit = {
    BoolOption.values
    NumOption.values
    StrOption.values
  }

  def byName(name: String): Option[?] = {
    ensureAllClinited()
    allByName.getOrElse(name.asciiToLowerCase, null)
  }

  // TODO: remove it
  enum SmartKind {
    case RuntimeRecompile extends SmartKind // changing the option triggers the whole Java Runtime re-compilation
    case AffectsCode      extends SmartKind // changing the option results in possibly different object file
    case Checked          extends SmartKind // changing the option disables smart-zero (re-link only)
    case Unchecked        extends SmartKind // does not affect compilation (may affect resource processing or linking)
  }
}
