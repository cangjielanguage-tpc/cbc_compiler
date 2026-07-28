/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64.frame

import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.Location.mem
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{LR, SP}
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.arm64.FrameArm64
import com.huawei.excelsior.jet.compiler.abi.frame.FrameDebug
import xscala.util.MathUtils.isAligned

trait FrameDebugArm64 extends FrameDebug { self: FrameArm64 =>

  override protected def initCallerFrameInfo(): Unit = if (genDebug) {
    assert(currentCallerSP == null)
    currentCallerSP = mem(PTR, SP, 0)
    currentCallerRA = null // to be initialized later just before LR spill at prologue
    genCallerFrameInfo(LR)
  }

  override protected def updateCallerFrameInfo(spAddend: Int): Unit = if (isCallerFrameInfoSupported) {
    if (currentCallerRA == null) {
      assert(isAligned(spAddend, stackSlotSize))
      currentCallerSP = currentCallerSP.disposed(spAddend)
      genCallerFrameInfo(LR)
    } else {
      super.updateCallerFrameInfo(spAddend)
    }
  }

  protected def initCallerFrameRA(spOffset: Int): Unit = if (genDebug) {
    assert(currentCallerRA == null)
    currentCallerRA = mem(PTR, SP, spOffset)
  }
}
