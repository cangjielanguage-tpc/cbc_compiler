/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

import com.huawei.excelsior.jet.assembler.Fixup.seq
import com.huawei.excelsior.jet.assembler.fixups.{Relocation, RelocationKind}

/** This class is a twin brother of [[Relocation]], except that it does not ask width of `kind` in constructor.
  * We use it to create relocation-like fixups to check the result of [[Relocation.Converter.send]]
  * from fixup resolution.
  */
final case class FakeRelocation(kind: RelocationKind, target: Symbol, addend: Int) extends Fixup(false, 0) {
  override def expectedSize = 0
  override def resolve(converter: Relocation.Converter): Unit = {}
  override protected def guts = seq(kind, target, addend)
}
