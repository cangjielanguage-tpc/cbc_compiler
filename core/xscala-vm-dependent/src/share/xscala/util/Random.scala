/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.util

import xscala.time.*

/** Duplicate of [[managedlib.aj.util.ThreadLocalRandomNatives]]. */
object Random {

  /** L'Ecuyer, Pierre (1999).
    * "Tables of Linear Congruential Generators of Different Sizes and Good Lattice Structure"
    */
  private val multiplier = 3202034522624059733L
  private val addend = 0x421L

  /** Linear Congruential Generator.
    *
    * If two instances of [[PRNG]] created with the same seed,
    * they will generate same sequence of numbers.
    */
  class PRNG(private var seed: Long = unixNanoseconds) {
    def next: Long = {
      val nextValue = multiplier * seed + addend
      seed = nextValue
      nextValue
    }
  }
}
