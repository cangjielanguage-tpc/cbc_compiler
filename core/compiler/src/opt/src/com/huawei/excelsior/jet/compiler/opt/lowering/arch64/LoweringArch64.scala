/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.arch64

import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.lowering.Lowering
import com.huawei.excelsior.jet.compiler.opt.middle.Optimize

trait LoweringArch64 extends Lowering { self: Universe with Optimize =>

  override protected def genCheckRich(n: Node) = {
    val bits = ConcealRef(n)
    val enrichment = Shift(ArithOp.LSR, bits, IConst(enrichmentIMTOffsetShift))
    val checkRich = If(Cmp(AddrType, Condition.NE)(enrichment, addrNull))
    (checkRich, bits, enrichment)
  }

  override protected def useCacheForBackupWeakCast = false

  override protected def getIMTOffsetFromCIAO(ciao: Node): Node = ciao

}
