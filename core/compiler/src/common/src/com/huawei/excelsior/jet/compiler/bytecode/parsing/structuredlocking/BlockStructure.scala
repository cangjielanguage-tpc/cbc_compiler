/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode.parsing.structuredlocking

object BlockStructure {
  case class Enter[EN, EX](enter: EN) extends BlockStructure[EN, EX] {
    override def toString = s"BlockStructure.Enter($enter)"
  }

  case class Exit[EN, EX](exit: EX) extends BlockStructure[EN, EX] {
    override def toString = s"BlockStructure.Exit($exit)"
  }

  object Error extends BlockStructure[Nothing, Nothing] {
    override def toString = "BlockStructure.Error"
  }

  object Empty extends BlockStructure[Nothing, Nothing] {
    override def toString = "BlockStructure.Empty"
  }
}

sealed abstract class BlockStructure[+EN, +EX]
