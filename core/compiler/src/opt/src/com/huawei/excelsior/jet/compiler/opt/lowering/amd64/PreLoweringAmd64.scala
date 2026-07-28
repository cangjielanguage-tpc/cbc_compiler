/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering.amd64

import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.codeemitter.amd64.CodeEmitterAmd64.isMemBarrierNop
import com.huawei.excelsior.jet.compiler.opt.lowering.PreLowering
import com.huawei.excelsior.jet.compiler.opt.middle.MemoryBarrierHalfDiamondOptimization

trait PreLoweringAmd64 extends PreLowering with MemoryBarrierHalfDiamondOptimization {
  override def optimizeNopMemoryBarriers(): Unit = {
    memoryBarrierDiamondElimination { barrier => isMemBarrierNop(BarrierKind.toMask(barrier.kinds.toSeq: _*)) }
  }
}
