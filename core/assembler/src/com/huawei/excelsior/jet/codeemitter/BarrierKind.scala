/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import xscala.util.MathUtils.isBitSet
import scala.Predef.wrapRefArray
import scala.collection.immutable.Set

/** Enumerates supported kinds of memory access barriers. */
enum BarrierKind {
  case LOAD_LOAD, LOAD_STORE, STORE_LOAD, STORE_STORE, STRICT_MEM
}

object BarrierKind {

  /** Converts a number of barrier kinds to a bit mask. */
  def toMask(kinds: BarrierKind*) = {
    kinds.foldLeft(0) { case (acc, kind) => acc | maskOf(kind) }
  }

  private def maskOf(kind: BarrierKind) = 1 << kind.ordinal

  /** Converts a bit mask of barrier kinds to the corresponding [[EnumSet]]. */
  def toSet(mask: Int) = values.filter(kind => isBitSet(mask, kind.ordinal)).toSet

  /** A bit mask for [[# LOAD_LOAD]] barrier. */
  val LL_MASK = maskOf(LOAD_LOAD)

  /** A bit mask for [[# LOAD_STORE]] barrier. */
  val LS_MASK = maskOf(LOAD_STORE)

  /** A bit mask for [[# STORE_STORE]] barrier. */
  val SS_MASK = maskOf(STORE_STORE)

  /** A bit mask for [[# STORE_LOAD]] barrier. */
  val SL_MASK = maskOf(STORE_LOAD)

  /** A bit mask for [[# STRICT_MEM]] barrier. */
  val STRICT_MEM_MASK = maskOf(STRICT_MEM)
}
