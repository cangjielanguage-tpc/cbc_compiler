/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.arm64

import com.huawei.excelsior.jet.assembler.Location.MemBased
import com.huawei.excelsior.jet.assembler.arm64.Register
import com.huawei.excelsior.jet.compiler.opt.backend.NodesDescription
import com.huawei.excelsior.jet.compiler.opt.backend.arm64.BackEndArm64
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.Arm64CASBackupPath
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.VOID

import scala.annotation.nowarn


/** Nodes description for arm64 platform.
  */
@nowarn("msg=match may not be exhaustive")
trait NodesDescriptionArm64 extends NodesDescription { self: Universe with BackEndArm64 =>

  override protected def nodeClassFormImpl(node: Node): NodeForm = node match {
    case _: CAS       => new CustomForm(Seq(casArg, casArg, casArg))
    case _: MemAtomic => new CustomForm(Seq(atomicArg, atomicArg))
    case _: Throw     => new CustomForm(Seq(allParamIRegsExceptStdTmp))

    case _ => super.nodeClassFormImpl(node)
  }


  ///////////////////////////////////////////////////////////////////////////////////////////

  private val atomicArg: Edge => ResourceSet = { _ =>
    if (env.enabled(Arm64CASBackupPath)) allParamIRegsExceptStdTmp else allParamIRegsSet
  }

  private val casArg: Edge => ResourceSet = { _ =>
    if (env.enabled(Arm64CASBackupPath)) allParamIRegsExceptStdTmp else allParamIRegsSet
  }

  //// Call / Return

  protected override def indirectCallTargetSet(call: Call): ResourceSet = {
    // TODO: unify copy-paste with other archs (amd64)
    val baseSet = if (call.abi.spoilsCallerFrameDescriptor(rootMethod.getMethodType)) {
      allParamIRegsExceptStdTmp
    } else if (call.gcActions.checkGCSafeState) {
      // Look at CallGenerator.gcSafeStateAssert
      allParamIRegsExceptAlwaysVolatile
    } else {
      allParamIRegsSet
    }
    baseSet -- call.abi.usedArgumentIRegs
  }
}
