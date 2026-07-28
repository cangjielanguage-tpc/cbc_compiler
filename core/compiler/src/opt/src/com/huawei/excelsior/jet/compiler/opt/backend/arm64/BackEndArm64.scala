/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64

import com.huawei.excelsior.jet.assembler.Width
import com.huawei.excelsior.jet.assembler.arm64.IRegister
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{IP0, IP1, LR}
import com.huawei.excelsior.jet.assembler.arm64.immediates.{BitMaskImm, ShiftedImm12}
import com.huawei.excelsior.jet.compiler.abi.arm64.CallingConventionArm64
import com.huawei.excelsior.jet.compiler.opt.backend.{BackEnd, BackEndMach}
import com.huawei.excelsior.jet.compiler.opt.backend.arm64.codegen.CodeGeneratorArm64
import com.huawei.excelsior.jet.compiler.opt.backend.arm64.preparation.PreparationArm64
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.setOf
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import scala.PartialFunction.condOpt

/** Arm64-specific backend. */
trait BackEndArm64 extends BackEnd with BackEndMach with FrameComponentArm64 with NodesDescriptionArm64 with MachineDescriptionArm64
  with CodeGeneratorArm64 with PreparationArm64 { self: Universe =>

  private var codegen: CodeGeneratorImplArm64 = _

  override protected def makeCodeGeneratorImpl() = {
    assert(codegen == null)
    codegen = new CodeGeneratorImplArm64
    codegen
  }

  // stdCodeEmitterScratch is not yet initialized
  override protected def makeAllIRegsSet() = super.makeAllIRegsSet() &~ setOf(IP0)

  val stdTmp = IP1

  // `IP0` is used as CodeEmitter's scratch.
  // Needed as workaround for JET-17887.
  val stdCodeEmitterScratch = IP0

  val ip0Set                                = setOf(IP0)
  val stdTmpSet                             = setOf(stdTmp)
  val allParamIRegsExceptStdTmp             = allParamIRegsSet &~ stdTmpSet
  val allParamIRegsExceptAlwaysVolatile     = allParamIRegsSet &~ setOf(CallingConventionArm64.alwaysVolatile)

  // TODO-ARM64: find suitable place for this stuff

  object ShiftedImm12Node {
    def unapply(n: Node) = condOpt(n) {
      case DWordConst(imm) if ShiftedImm12.canEncode(imm) => imm
      case _: AnyNull if ShiftedImm12.canEncode(0) => 0
    }
  }

  object BitMaskNode {
    def unapply(n: Node) = condOpt(n) {
      case IntegralConst(imm) if BitMaskImm.canEncode(imm, ValueType.width(n.tpe)) => imm
      case _: AnyNull if BitMaskImm.canEncode(0, Width.WPTR) => 0
    }
  }
}
