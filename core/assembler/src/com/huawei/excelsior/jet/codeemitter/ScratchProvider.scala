/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.codeemitter

import com.huawei.excelsior.jet.assembler.Location.IReg

/** Interface between [[CodeEmitter]] and register allocation in compiler.
  *
  * @author conwor
  */
object ScratchProvider {
  class NoAvailableScratchError extends Error
}

abstract class ScratchProvider {
  def acquireScratch(): IReg
  def releaseScratch(scratch: IReg): Unit
  def available: Int

  def appendScratch(scratch: IReg): Unit
  def removeScratch(scratch: IReg): Unit

  /** Returns true iff `r` is one of [[allScratches]]. */
  def contains(r: IReg): Boolean

  /** Returns list of all scratches used in this provider. */
  def allScratches: Array[IReg]

  def has(count: Int) = available >= count

  def isEmpty = !(this has 1)
}
