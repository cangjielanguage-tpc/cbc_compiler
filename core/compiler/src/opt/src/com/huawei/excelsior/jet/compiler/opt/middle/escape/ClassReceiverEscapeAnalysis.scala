/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.escape

import com.huawei.excelsior.jet.compiler.ir.CallEscapeKind.Empty
import com.huawei.excelsior.jet.compiler.ir.NewEscapeKind.NoEscape
import com.huawei.excelsior.jet.compiler.ir.{EscapeKind, EscapeKindTuple, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{Method, ClassType as SymClassType}
import com.huawei.excelsior.jet.compiler.util.Log

/** Analysis of escape kind of receiver (this parameter) in all methods of given class.
  * Initially described in JET-9206.
  *
  * @author cypok
  */
private[escape] trait ClassReceiverEscapeAnalysis { self: Universe with EscapeAnalysis =>

  private def log = Log(Log.Kind.EscapeAnalysis)

  def mayBeCalledVirtually(method: Method): Boolean = {
    if (method.isStatic) {
      return false
    }
    // constructors are always called directly
    if (method.isConstructor) {
      return false
    }
    // all j.l.Object's final methods are always called directly
    // (even interface calls to these methods are always devirtualized)
    if (method.getDeclaringClass.isJavaLangObject && method.isFinal) {
      return false
    }
    true
  }

  def classReceiverEscape(klass: SymClassType): EscapeKind = {
    assert(currentPhase > CompilerPhase.InterProceduralAnalysis,
      "class receiver escape must be calculated only after inter-procedural analysis to prevent parsing cycles with inaccurate results")
    assert(klass.isClassOrInterface)

    // This information can always be recalculated so we use non-serialized cache.
    classReceiverEscapeInfo get klass getOrElse {
      log.inSession(s"class receiver escape for ${klass.getName}") {
        val esc = analyze(klass)
        assert(esc.containsEscape || (esc == NoEscape || esc == Empty || esc == EscapeKindTuple(Empty, NoEscape)), esc.toString)
        if (!esc.containsEscape) {
          log(s"result: $esc")
        }
        classReceiverEscapeInfo.put(klass, esc)
        esc
      }
    }
  }

  private def analyze(klass: SymClassType): EscapeKind = {
    var esc: EscapeKind = NewEscapeKind.NoEscape

    for (superType <- klass.getDeclaredSuperTypes) {
      esc = esc /\ classReceiverEscape(superType)
      if (globalLikeEscape(esc)) {
        log(s"GlobalEscape because of super type ${superType.getName}")
        return NewEscapeKind.PotentialEscape
      }
    }

    // Note that class may override some bad methods (with escaping this) with good ones (with non-escaping this).
    // However we do not handle such a situation.
    // This may be done via saving sequence of bad methods in extra info instead of single flag.
    for (method <- klass.getDeclaredMethods.filter(m => !m.isAbstract && mayBeCalledVirtually(m))) {
      val rcvEscape = methodEscapeInfo(method) map (_(method.getReceiverArgIdx)) getOrElse NewEscapeKind.GuaranteeEscape
      esc = esc /\ rcvEscape

      if (globalLikeEscape(esc)) {
        return NewEscapeKind.PotentialEscape
      }
    }

    assert(!globalLikeEscape(esc))
    if (esc.containsReceiverEscape) {
      // RcvEscape is a nop for analyzing receiver escape
      esc = esc.transformReceiverEscapeTo(NoEscape)
    }
    esc
  }

  private def globalLikeEscape(e: EscapeKind): Boolean =
    // Note that for this analysis RetEscape is equivalent to GlobalEscape
    e.containsEscape || e.containsPotentialEscape || e.containsRetEscape

}
