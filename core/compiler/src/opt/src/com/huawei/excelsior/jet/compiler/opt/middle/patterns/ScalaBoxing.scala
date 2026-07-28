/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.compiler.bytecode.BytecodePosition
import com.huawei.excelsior.jet.compiler.symlevel.{JBCSignature, Method, MethodReferenceAccessKind as MAK}
import com.huawei.excelsior.jet.compiler.ir.BytecodeOffset
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import scala.PartialFunction.condOpt

/**
  * Optimize Scala boxing by wrapping `BoxesRunTime.equalsNumNum(x, y)` calls into
  * {{{
  *   if (x != null && y != null && x.td == y.td) x.equals(y)
  *   else equalsNumNum(x, y)
  * }}}
  * It allows to skip all the sophisticated logic of comparing two different numeric types
  * in popular case when `x` and `y` are of the same type.
  * The price for that is a comparison of these objects' `InstanceDescriptor`s,
  * everything else (null-checks and `InstanceDescriptor`s loading) is merged with neighbouring `instanceof`s.
  */
trait ScalaBoxing { self: Universe =>

  object ScalaBoxing {
    val equalsRef = typeProvider.getObjectType.getMethodRefToLocal(xstr("equals"), null, MAK.VIRTUAL)
    val boxesRunTime = typeProvider.getScalaBoxesRunTimeType

    def shouldEvenTry = boxesRunTime != null

    def isEqualsNumNum(method: Method) = {
      method.getDeclaringClass == boxesRunTime &&
        method.isStatic &&
        method.getName == "equalsNumNum" && // TODO: consider also allowing BoxesRunTime.equals2
        JBCSignature(method.getSignature) == "(Ljava/lang/Number;Ljava/lang/Number;)Z"
    }

    object EqualsNumNumInvoke {
      def unapply(call: Call) = condOpt(call) {
        case CallMethod(method, _, Seq(x, y)) if isEqualsNumNum(method) => (x, y)
      }
    }
  }

  def optimizeScalaBoxing(): Boolean = ScalaBoxing.shouldEvenTry && {
    var changed = false
    for (n @ ScalaBoxing.EqualsNumNumInvoke((x, y)) <- all[Call]) {
      import PredicateConstructor._
      replaceByDiamondWithFastPath(n)(nonNull(x) && nonNull(y) && equalType(x, y)) {
        withPos(n.pos match {
          case pos: BytecodePosition =>
            // inserted fast-path call needs distinct position to avoid clash with the position of cold-path call,
            // so that they could be moved to / out from Siberia independently based on their actual hotness
            pos.copy(offset = BytecodeOffset.makeSynthetic(pos.offset))
          case _ => shouldNotReachHere(n.pos)
        }) {
          Invoke(ScalaBoxing.equalsRef)(x, y)
        }
      }
      changed |= true
    }
    changed
  }
}
