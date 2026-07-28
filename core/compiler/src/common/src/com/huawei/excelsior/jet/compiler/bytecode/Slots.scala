/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Abstraction over all bytecode slots: locals and stack.
  *
  * Allows unified numbering of all slots using so called "slot index" which is obtained by methods:
  * [[Slots.localIdx]], [[Slots.stackIdx]].
  */
final class Slots(val localsCount: Int, val stackCount: Int) {

  val totalCount = localsCount + stackCount

  def localIdx(localNum: Int) = {
    assert(0 <= localNum && localNum < localsCount)
    localNum
  }

  def stackIdx(stackNum: Int) = {
    assert(0 <= stackNum && stackNum < stackCount)
    localsCount + stackNum
  }

  def slotToString(slotIdx: Int) = {
    if (0 <= slotIdx && slotIdx < localsCount) {
      s"local #$slotIdx"
    } else {
      assert(slotIdx < totalCount)
      s"stack #${slotIdx - localsCount}"
    }
  }
}
