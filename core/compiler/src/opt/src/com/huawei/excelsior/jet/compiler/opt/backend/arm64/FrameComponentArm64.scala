/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.MemBased
import com.huawei.excelsior.jet.assembler.arm64.Register
import com.huawei.excelsior.jet.compiler.Env.stackPointer
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.abi.arm64.FrameArm64
import com.huawei.excelsior.jet.compiler.opt.backend.FrameComponent
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformDependentArm64
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseFramePointer

/** Arm64-specific frame. */
trait FrameComponentArm64 extends FrameComponent with PlatformDependentArm64 { self: Universe with BackEndArm64 =>

  override protected def makeFrame(): FrameArm64 = new FrameArm64(asm, env, symbolLinker, codeUnit.getFrameProperties, env.enabled(UseFramePointer), useSPAddressing = true)

  protected def makeFrameLayout(spoiledRegs: collection.Seq[Location.AnyReg], frameSlots: collection.Seq[FrameSlot]): Unit = {
    registerStackChecks()

    // JET-10170 fix: don't change code order in this block until `calculateAddressesForSlots()` is fixed
    {
      frameSlots foreach { fs => frame.addSlot(frame.newSlot(fs.size, fs.align)) }

      if (rootMethod.isCVarArgs) {
        frame.initCVarArgs()
        ensureFullFrame()
      }

      assert(frameShouldBeFull(frameSlots) || (frameSlots.isEmpty && all[Call].isEmpty))
    }

    val mode = if (frameShouldBeFull(frameSlots)) Frame.Mode.FULL else Frame.Mode.LIGHTWEIGHT
    frame.makeLayout(mode)

    calculateAddressesForSlots(frameSlots)
  }
}
