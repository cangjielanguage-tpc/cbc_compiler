/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.amd64

import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.amd64.*
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.Frame.Slot
import com.huawei.excelsior.jet.compiler.abi.amd64.CallingConventionAmd64.unixRSASize
import com.huawei.excelsior.jet.compiler.abi.amd64.frame.{FrameCodeGenAmd64, FrameDebugAmd64}
import com.huawei.excelsior.jet.compiler.abi.{Frame, FrameProperties}
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}

/** Frame on amd64 platform.
  *
  * {{{
  * | ...                             |
  * |---------------------------------| <- SP after caller's frame build
  * | caller return address           |
  * |---------------------------------| <- SP after caller's call instruction
  * | saved general-purpose registers | <- FP (frame pointer register, optional)
  * | ...                             |
  * | ...                             |
  * |---------------------------------|
  * | saved float registers           |
  * | ...                             |
  * | ...                             |
  * |---------------------------------|
  * | frame slots (spill) &           |
  * | stack alloc results             |
  * | ...                             |
  * |---------------------------------| <- FMR (frame middle register, baseline only)
  * | alignment                       |
  * |---------------------------------|
  * | parameters on stack             |
  * |---------------------------------|
  * | shadow space (varargs, windows) |
  * |---------------------------------|
  * | frame descriptor                |
  * |---------------------------------| <- SP after frame build
  * | ...                             |
  * }}}
  *
  * @author paul
  * @author alexm
  * @author cypok
  * @author conwor
  */
final class FrameAmd64(protected val asm: Assembler, _env: Environment, _symbolLinker: SymbolLinker, _properties: FrameProperties, _useFramePointer: Boolean, useSPAddressing: Boolean)
  extends Frame[GPR, XMM, ABIAmd64](_env, _symbolLinker, _properties, _useFramePointer, useFMRAddressing = !useSPAddressing) with FrameCodeGenAmd64 with FrameDebugAmd64 {

  override protected def preHeaderSize: Int = stackSlotSize

  override protected def fRegsArePushable: Boolean = false

  override protected def framePointerSetupOffset: Int = stackSlotSize // caller FP
}
