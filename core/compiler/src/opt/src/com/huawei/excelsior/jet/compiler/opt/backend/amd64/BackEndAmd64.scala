/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.assembler.amd64.GPR.*
import com.huawei.excelsior.jet.assembler.amd64.XMM.*
import com.huawei.excelsior.jet.compiler.abi.amd64.CallingConventionAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.BackEndMach
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.bgcm.PreferredAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.codegen.CodeGeneratorAmd64
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.preparation.PreparationAmd64
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.graph.Loops

/** Amd64-specific backend.
  *
  * @author conwor
  */
trait BackEndAmd64 extends BackEnd with BackEndMach with FrameComponentAmd64 with NodesDescriptionAmd64 with MachineDescriptionAmd64
  with CodeGeneratorAmd64 with PreparationAmd64 with PreferredAmd64 { self: Universe =>

  private var codegen: CodeGeneratorImplAmd64 = _

  override protected def makeCodeGeneratorImpl() = {
    assert(codegen == null)
    codegen = new CodeGeneratorImplAmd64
    codegen
  }

  ////////////////////////////////////////////////////////////////////////////////////////////

  val xmm0Set                         = setOf(XMM0)
  val raxSet                          = setOf(RAX)
  val rcxSet                          = setOf(RCX)
  val rdxSet                          = setOf(RDX)
  val allIRegsExceptRCXSet            = allIRegsSet &~ rcxSet
  val allParamIRegsExceptRDXAndRAXSet = allParamIRegsSet &~ (rdxSet | raxSet)
  val allParamIRegsExceptRAXSet       = allParamIRegsSet &~ raxSet


  ////////////////////////////////////////////////////////////////////////////////////////////

  override def requiredMethodAlignment: Int = {
    val hasDirectRecursion = all[Call] exists {
      case DirectCall(method) => method == rootMethod
      case _ => false
    }

    if (hasDirectRecursion ||          // check for direct recursion
         cfg.loops.nonEmpty ||         // check for loops
         rootMethod.isAJRTAllocator) { // check if method is allocator
      64
    } else {
      super.requiredMethodAlignment // align doesn't matter
    }
  }
}
