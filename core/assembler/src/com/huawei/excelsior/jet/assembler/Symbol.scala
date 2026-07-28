/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler

/** Symbol - an abstract target of Fixup. It serves as a common interface for heirs of two kinds:
  *
  *  - an heir is bound to a segment, or it is not, but will be bound in the future (like Label)
  * it allows the symbol to be a target of the internal fixup that will be resolved during compilation;
  *  - an heir owns a segment now or in the future, so the symbol becomes a part of an object file
  * and the symbol can be a target of external fixup that can not be resolved during compilation,
  * so it will be managed later by linker.
  *
  * TODO: to underline the difference these two groups should be split and each group should have its own ancestor.
  *
  * @author cypok
  * @author paul
  */
trait Symbol {
  def ownsSegment = false
}
