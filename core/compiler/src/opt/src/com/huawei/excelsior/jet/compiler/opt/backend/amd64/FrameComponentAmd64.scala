/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64

import com.huawei.excelsior.jet.assembler.Location
import com.huawei.excelsior.jet.assembler.Location.MemBased
import com.huawei.excelsior.jet.assembler.amd64.{GPR, XMM}
import com.huawei.excelsior.jet.compiler.Env
import com.huawei.excelsior.jet.compiler.Env.{stackPointer, targetOS}
import com.huawei.excelsior.jet.compiler.abi.Frame
import com.huawei.excelsior.jet.compiler.abi.amd64.FrameAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.FrameComponent
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.platforms.PlatformDependentAmd64
import com.huawei.excelsior.jet.compiler.options.BoolOption.UseFramePointer

/** Amd64-specific frame.
  *
  * @author conwor
  */
trait FrameComponentAmd64 extends FrameComponent with PlatformDependentAmd64 { self: Universe with BackEndAmd64 =>

  override protected def makeFrame(): FrameAmd64 = new FrameAmd64(asm, env, symbolLinker, codeUnit.getFrameProperties, env.enabled(UseFramePointer), useSPAddressing = true)

  protected def makeFrameLayout(spoiledRegs: collection.Seq[Location.AnyReg], frameSlots: collection.Seq[FrameSlot]): Unit = {
    registerStackChecks()

    val hasSavedXMMs = spoiledRegs exists { r => r.isInstanceOf[Location.FReg] && frame.abi.isNonVolatile(r) }
    // Actually, we can support lightweight frame in this case, but we don't want to (rare case, performance is not important).
    if (hasSavedXMMs) ensureFullFrame() //TODO: remove this

    // JET-10170 fix: don't change code order in this block until `calculateAddressesForSlots()` is fixed
    {
      frameSlots foreach { fs => frame.addSlot(frame.newSlot(fs.size, fs.align)) }

      if (targetOS.isLinux && rootMethod.isCVarArgs) {
        frame.initUnixVarArgs()
        ensureFullFrame()
      }

      assert(frameShouldBeFull(frameSlots) || (frameSlots.isEmpty && all[Call].isEmpty))
    }

    val mode = if (frameShouldBeFull(frameSlots)) Frame.Mode.FULL else Frame.Mode.LIGHTWEIGHT
    frame.makeLayout(mode)

    calculateAddressesForSlots(frameSlots)
  }
}
