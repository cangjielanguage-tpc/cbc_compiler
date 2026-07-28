/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64.frame

import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.amd64.GPR.RSP
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.amd64.FrameAmd64
import com.huawei.excelsior.jet.compiler.abi.frame.FrameDebug

trait FrameDebugAmd64 extends FrameDebug { self: FrameAmd64 =>

  override protected def initCallerFrameInfo(): Unit = if (genDebug) {
    assert(currentCallerSP == null && currentCallerRA == null)
    currentCallerSP = mem(PTR, RSP, stackSlotSize)
    currentCallerRA = mem(PTR, RSP, 0)
    genCallerFrameInfo()
  }
}
