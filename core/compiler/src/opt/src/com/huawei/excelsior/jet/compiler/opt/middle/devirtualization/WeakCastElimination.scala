/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.TypeFiltersAbsorption
import com.huawei.excelsior.jet.compiler.symlevel.ClassType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.Guards.{Guard, MethodGuard}

trait WeakCastElimination extends CallTargetInfos with TypeFiltersAbsorption { self: Universe =>

  object WeakCastElimination {
    sealed abstract class Result
    case class Impossible(reason: String) extends Result
    case object Possible extends Result
    case class Absorbable(guard: Guard) extends Result
  }

  import WeakCastElimination._

  def isWeakCastEliminationPossible(call: Call, guard: Guard): Result = {
    if (!call.target.isInstanceOf[InvokeInterfaceTarget]) {
      return Impossible("virtual call")
    }

    if (guard.isInstanceOf[MethodGuard]) {
      return Impossible("method test")
    }

    if (call.block.isCold) {
      return Impossible("cold code")
    }

    call.target match {
      case InvokeInterfaceTarget(wc: WeakCast) =>
        val interf = wc.targetType
        wc.obj match {
          case Deprive(`interf`, _) =>
            // more compact code because of EOPs
            Impossible("WeakCast from rich")

          case Deprive(richType, _) if richType doesImplement asClassType(interf) =>
            // upcasts to superinterfaces can be implemented more efficiently
            Impossible("WeakCast to superinterface")

          case _ =>
            if (!wc.hasDominatingCheck) {
              Possible
            } else {
              wc.dominatingCheck match {
                case check: CheckCast =>
                  selectGuardForInterfaceCastAbsorption(check, guard, call) match {
                    case Some(absorbingGuard) => Absorbable(absorbingGuard)
                    case None => Impossible("WeakCast with non-absorbable dominating CheckCast")
                  }
                case _: InstanceOf => Impossible("WeakCast with non-absorbable dominating InstanceOf")
                case x => shouldNotReachHere(s"unexpected dominating check $x")
              }
            }
        }

      case _ => Impossible("no WeakCast")
    }
  }
}
