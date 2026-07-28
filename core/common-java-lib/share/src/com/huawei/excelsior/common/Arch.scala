/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.util.StringOps.*

/** The target architecture of the compiled code.
  *
  * Represents either an architecture of target CPU or an intermediate code format.
  */
enum Arch(val bitWidth: Int) {
  case AMD64 extends Arch(64)
  case ARM64 extends Arch(64)
  case CBC   extends Arch(64)

  assert(bitWidth == 32 || bitWidth == 64)

  /** The address size. */
  val addressSize = if (bitWidth == 32) 4 else 8

  /** Binary logarithm of the address size. */
  val addressLog2Size = if (bitWidth == 32) 2 else 3

  override def toString = productPrefix.asciiToLowerCase

  /** Returns the internal family name for the groups of architectures. */
  def familyName = this match {
    case AMD64 => "intel"
    case ARM64 => "arm"
    case CBC   => "cbc"
  }

  /** Returns the size of the stack slot in bytes. */
  def stackSlotSize = {
    // for current architectures stack slot size is equal to address size
    addressSize
  }

  /** Returns whether this architecture is 32-bit. */
  def is32Bit = bitWidth == 32

  /** Returns whether this architecture is 64-bit. */
  def is64Bit = bitWidth == 64
}

object Arch {
  def apply(name: String) = Arch.valueOf(name.asciiToUpperCase)
}
