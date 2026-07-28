/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.jet.assembler.Location.AnyReg

import scala.reflect.ClassTag

/** Descriptor of one register file in context of calling conventions.
  *
  * @author conwor
  */
final case class RegFile[R <: AnyReg : ClassTag](

  /** All available registers in ABI-specified order which is extremely important for exceptions rethrowing & GC to work.
    * See runtime/PLATFORM/asm/exceptions.asm.
    */
  availableInABIOrder: Array[R],

  /** Registers not preserved through call in arbitrary order. */
  volatiles: Array[R],

  /** Registers available to pass params in strict order. */
  headArea: Array[R]
) {

  // Pre-calculated collections for fast access
  val volatilesSet = volatiles.toSet
  val headAreaIndex = headArea.zipWithIndex.toMap
  val savedIndex = availableInABIOrder.zipWithIndex.toMap

  /** All available registers in order suitable for registers allocation - volatiles go first, non-volatiles next. */
  val available = { val (vs, ns) = availableInABIOrder partition volatilesSet; vs ++ ns }

  /** Returns [[RegFile]] with extra non-volatile registers except given ones. */
  def withAllNonVolatile(except: Array[AnyReg]): RegFile[R] = copy(volatiles = except collect { case r: R => r })

  /** Returns [[RegFile]] with given extra non-volatile registers. */
  def withExtraNonVolatile(extra: Set[AnyReg]): RegFile[R] = if (extra.isEmpty) this else copy(volatiles = volatiles filterNot extra)
}
