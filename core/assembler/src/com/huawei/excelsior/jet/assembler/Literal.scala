/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

/** Data label that will be allocated immediately after generated code.
  *
  * Used to offload any data to the end of code segment when it can't be encoded in the current instruction
  * (e.g. fixup target is located too far from the instruction
  * or numerical constant doesn't fit instruction immediate value).
  *
  * @author conwor
  * @author liontiger
  * @author ikireev
  */
abstract class Literal protected(val alignment: Int) extends Label {
  protected def this(width: Width) = this(width.nbytes)

  def emit(): Unit
}
