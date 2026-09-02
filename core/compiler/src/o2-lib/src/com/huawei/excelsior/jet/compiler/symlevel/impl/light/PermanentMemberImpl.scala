/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel.impl.light

import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule
import com.huawei.excelsior.jet.compiler.symlevel.PermanentMember

abstract class PermanentMemberImpl protected(protected val ref: pcOModule.MemberRef) extends PermanentMember {
  override final def hashCode = ref.hashCode

  override def equals(that: Any): Boolean = that match {
    case that: AnyRef if this eq that => true
    case that: PermanentMemberImpl => this.ref == that.ref
    case _ => false
  }
}
