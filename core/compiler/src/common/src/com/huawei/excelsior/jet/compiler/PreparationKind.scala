/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.symlevel.Type

/** Encapsulates the type of run-time data structure to be prepared and how or when preparation should take place.
  *
  * @param `lazy`        Whether a type is prepared via explicit check in generated code (lazy), or added to hosting class preparation closure (eager).
  * @param assertionOnly Never generate lazy preparation, only assertions are allowed.
  * @param forced        Whether the check is added with explicit intent and is never a subject to optimizations.
  * @param bootstrap     Bootstrap preparation via an iteration through linker-generated table in UnmanagedInit.
  * @author liontiger
  * @author b-andrew
  */
case class PreparationKind private(`lazy`: Boolean, assertionOnly: Boolean, forced: Boolean, bootstrap: Boolean = false) {

  def toBitmask: Byte = {
    var bits: Int = 0
    def bit(idx: Byte, value: Boolean): Unit = if (value) bits |= 1 << idx
    bit(0, `lazy`)
    bit(1, assertionOnly)
    bit(2, forced)
    bit(3, bootstrap)
    bits.toByte
  }

}

object PreparationKind {
  /** Lazy preparation of a type's RunTimeTypeInfo via explicit check in generated code.
    */
  val PROLOGUE_PREPARATION = PreparationKind(`lazy` = true, assertionOnly = false, forced = true)

  /** Lazy assertion of type's RunTimeTypeInfo preparation via explicit check in generated code.
    */
  val PROLOGUE_ASSERTION = PreparationKind(`lazy` = true, assertionOnly = true, forced = true)

  def apply(managedContext: Boolean, env: Environment): PreparationKind = {
    apply(managedContext, `lazy` = ProjectLogic.useLazyPreparation)
  }

  def apply(managedContext: Boolean, `lazy`: Boolean): PreparationKind = {
    val bootstrap = !managedContext
    val assertionOnly = bootstrap
    PreparationKind(`lazy` = `lazy`, assertionOnly = assertionOnly, forced = false, bootstrap = bootstrap)
  }

  def fromBitmask(bits: Byte): PreparationKind = {
    def bit(idx: Byte): Boolean = (bits & (1 << idx)) != 0
    PreparationKind(`lazy` = bit(0), assertionOnly = bit(1), forced = bit(2), bootstrap = bit(3))
  }
}