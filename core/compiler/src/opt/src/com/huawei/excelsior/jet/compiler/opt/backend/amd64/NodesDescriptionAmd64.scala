/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.amd64

import com.huawei.excelsior.jet.compiler.Env.targetOS
import com.huawei.excelsior.jet.compiler.opt.backend.NodesDescription
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/**
  * Nodes description for amd64 platform.
  *
  * @author conwor
  */
@nowarn("msg=match may not be exhaustive")
trait NodesDescriptionAmd64 extends NodesDescription { self: Universe with BackEndAmd64 =>

  override protected def nodeClassFormImpl(node: Node): NodeForm = node match {
    case IDiv() | UDiv() | _: MulH | _: UMulH | IRem() | URem() => new CustomForm(Seq(raxSet, allParamIRegsExceptRDXAndRAXSet))

    case _: Shift     => new CustomForm(Seq(shiftFirstArg, shiftSecondArg))
    case _: CAS       => new CustomForm(Seq(allParamIRegsExceptRAXSet, raxSet, allParamIRegsExceptRAXSet))
    case _: CmpCAS    => new CustomForm(Seq(allParamIRegsExceptRAXSet, raxSet, allParamIRegsExceptRAXSet))
    case _: MemAtomic => new CustomForm(Seq(allParamIRegsSet, allParamIRegsSet))
    case _: Throw     => new CustomForm(Seq(allParamIRegsExceptRAXSet))
    case _: CheckedOp => new CustomForm(Seq(checkedOpFirstArg, checkedOpSecondArg))

    case _ => super.nodeClassFormImpl(node)
  }


  ///////////////////////////////////////////////////////////////////////////////////////////

  private val checkedOpFirstArg: Edge => ResourceSet = { e => e.target match {
    case CheckedOp(CheckedOp.Kind.MUL, _, _) => raxSet
    case CheckedOp(CheckedOp.Kind.ADD | CheckedOp.Kind.SUB, _, _) => allParamIRegsExceptRAXSet
    case _ => argRegs(e)
  }}

  private val checkedOpSecondArg: Edge => ResourceSet = { e => e.target match {
    case CheckedOp(CheckedOp.Kind.MUL, _, _) => allParamIRegsExceptRDXAndRAXSet
    case _ => argRegs(e)
  }}


  ///////////////////////////////////////////////////////////////////////////////////////////

  override def temporalSlotsCount(node: Node): Int = {
    import Java.Lang.MathIntrinsic.*
    node match {
      case MathIntrinsic(D_SQRT | D_ABS | F_ABS) => 0
      case MathIntrinsic(_) => 1
      case _ => 0
    }
  }


  ///////////////////////////////////////////////////////////////////////////////////////////

  private val shiftFirstArg: Edge => ResourceSet = { e =>
    if (e.target.inEdge(1).source.isInstanceOf[IConst]) allIRegsSet else allIRegsExceptRCXSet
  }

  private val shiftSecondArg: Edge => ResourceSet = { e => e.source match {
    case _: IConst => emptySet
    case _ => rcxSet
  }}

  protected override def indirectCallTargetSet(call: Call): ResourceSet = {
    val withoutParams = allParamIRegsSet -- call.abi.usedArgumentIRegs // TODO: why exclude params?
    if (call.methodType.isCVarArgs && targetOS.isLinux) {
      // On unix/amd64 AL register is used before call for passing number of XMM register arguments
      withoutParams &~ raxSet
    } else {
      withoutParams
    }
  }
}
