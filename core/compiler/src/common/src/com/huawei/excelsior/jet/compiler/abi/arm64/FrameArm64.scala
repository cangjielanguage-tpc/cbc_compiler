/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi.arm64

import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.AsmType.PTR
import com.huawei.excelsior.jet.assembler.arm64.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.*
import com.huawei.excelsior.jet.compiler.Env.stackSlotSize
import com.huawei.excelsior.jet.compiler.abi.arm64.FrameArm64.*
import com.huawei.excelsior.jet.compiler.abi.arm64.frame.{FrameCodeGenArm64, FrameDebugArm64}
import com.huawei.excelsior.jet.compiler.abi.{Frame, FrameProperties}
import com.huawei.excelsior.jet.compiler.{Environment, SymbolLinker}
import xscala.util.MathUtils.*

/** Frame on arm64 platform.
  *
  * {{{
  * | ...                             |
  * |---------------------------------| <- caller's frame SP before callers's call instruction
  * | (nothing here)                  |    (these pointers are equal)
  * |---------------------------------| <- SP after callers's call instruction
  * | varargs, passed on registers    |
  * | ...                             |
  * | ...                             |
  * |---------------------------------|
  * | RA (caller return address)      |
  * | saved general-purpose registers | <- FP, optional
  * | ...                             |
  * |---------------------------------|
  * | saved float registers           |
  * | ...                             |
  * | ...                             |
  * |---------------------------------|
  * | frame slots (spill) &           |
  * | stack alloc results             |
  * | ...                             |
  * |---------------------------------| <- FMR
  * | alignment                       |
  * |---------------------------------|
  * | parameters on stack             |
  * |---------------------------------|
  * |                                 |
  * |                                 |
  * | frame descriptor                |
  * |---------------------------------| <- SP after frame build
  * | ...                             |
  * }}}
  *
  * @author gatimosh
  */
object FrameArm64 {
  val FP_VARARG_SIZE = 16
  val FP_VARARGS_ALIGNMENT = 16
}

final class FrameArm64(protected val asm: Assembler, _env: Environment, _symbolLinker: SymbolLinker, _properties: FrameProperties, _useFramePointer: Boolean, useSPAddressing: Boolean)
  extends Frame[IRegister.X, VFPRegister.D, ABIArm64](_env, _symbolLinker, _properties, _useFramePointer, useFMRAddressing = !useSPAddressing) with FrameCodeGenArm64 with FrameDebugArm64 {

  // reversed for proper order at spill
  protected val vaOccupiedIRegs = abi.getVarArgsOccupiedIRegs.reverseIterator.toIndexedSeq
  protected val vaOccupiedFRegs = abi.getVarArgsOccupiedFRegs.reverseIterator.toIndexedSeq

  override protected def preHeaderSize = {
    // vr_top must be quad-aligned
    alignUp(vaOccupiedIRegs.length * stackSlotSize, FP_VARARGS_ALIGNMENT) + vaOccupiedFRegs.length * FP_VARARG_SIZE
  }

  override protected def fRegsArePushable: Boolean = true

  override protected def framePointerSetupOffset: Int = 2 * stackSlotSize // LR + caller FP
}
