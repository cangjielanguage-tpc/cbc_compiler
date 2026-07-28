/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method

import scala.PartialFunction.{cond, condOpt}

/** Compile-time evaluation of various [[java.lang.String]] methods.
  *
  * @author arxdukalis
  */
trait ConstantStringOptimizations { self: Universe =>

  def optimizeConstantStringInvokes(call: Call): Boolean = cond(call) {
    case CallMethod(target, _, args @ Seq(receiver: ConstString, _*)) if call.targetRef.hasReceiverParameter && (args forall (_.isInstanceOf[Constant])) =>

      val host = receiver.str.getHost

      val value = if (host.isJavaReference) {
        evaluateForJava(target, args)
      } else if (host.isAJManagedType || host.isCangjieType || host.isXScalaType) {
        // TODO: implement for preferred AJString methods
        None
      } else {
        shouldNotReachHere(s"unexpected const string host: $host")
      }

      cond(value) {
        case Some(v) =>
          stats.count(StatsKind.ConstStrings, s"${target.getName} evaluated", call)
          strikeOutWithValueUses(call, v)
          true
      }
  }

  private def boolToIConst(b: Boolean) = IConst(if (b) 1 else 0)

  private def evaluateForJava(method: Method, args: Seq[Node]): Option[Node] = condOpt(method, args) {
    case (Java.Lang.String.hashCodeMethod, Seq(ConstString(XStr(v)))) => IConst(v.hashCode())
    case (Java.Lang.String.equalsMethod, Seq(ConstString(l), ConstString(r))) => boolToIConst(l.equals(r))

    case (Java.Lang.String.startsWith, Seq(ConstString(XStr(r)), ConstString(XStr(prefix)))) => boolToIConst(r.startsWith(prefix))
    case (Java.Lang.String.startsWithFrom, Seq(ConstString(XStr(r)), ConstString(XStr(prefix)), IConst(toffset))) => boolToIConst(r.startsWith(prefix, toffset))

    case (Java.Lang.String.endsWith, Seq(ConstString(XStr(r)), ConstString(XStr(suffix)))) => boolToIConst(r.endsWith(suffix))

    case (Java.Lang.String.indexOfChar, Seq(ConstString(XStr(r)), IConst(ch))) => IConst(r.indexOf(ch))
    case (Java.Lang.String.indexOfCharFrom, Seq(ConstString(XStr(r)), IConst(ch), IConst(fromIndex))) => IConst(r.indexOf(ch, fromIndex))
    case (Java.Lang.String.indexOfStr, Seq(ConstString(XStr(r)), ConstString(XStr(str)))) => IConst(r.indexOf(str))
    case (Java.Lang.String.indexOfStrFrom, Seq(ConstString(XStr(r)), ConstString(XStr(str)), IConst(fromIndex))) => IConst(r.indexOf(str, fromIndex))

    case (Java.Lang.String.lastIndexOfChar, Seq(ConstString(XStr(r)), IConst(ch))) => IConst(r.lastIndexOf(ch))
    case (Java.Lang.String.lastIndexOfCharFrom, Seq(ConstString(XStr(r)), IConst(ch), IConst(fromIndex))) => IConst(r.lastIndexOf(ch, fromIndex))
    case (Java.Lang.String.lastIndexOfStr, Seq(ConstString(XStr(r)), ConstString(XStr(str)))) => IConst(r.lastIndexOf(str))
    case (Java.Lang.String.lastIndexOfStrFrom, Seq(ConstString(XStr(r)), ConstString(XStr(str)), IConst(fromIndex))) => IConst(r.lastIndexOf(str, fromIndex))
  }

}
