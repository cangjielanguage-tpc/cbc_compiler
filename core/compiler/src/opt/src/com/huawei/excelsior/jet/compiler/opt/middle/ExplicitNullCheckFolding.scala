/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption.FoldExplicitNullChecks
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodReferenceAccessKind}
import com.huawei.excelsior.jet.compiler.{Domain, Env, RTSProc, StatsKind}
import com.huawei.excelsior.jet.util.Closure

import scala.PartialFunction.cond

/** Eliminates explicit null tests with operation throwing `NullPointerException` (or its AJ analogue) on the false branch by replacing them with nullcheck.
  *
  * Supported throwing operations include:
  * - `throw new NullPointerException()`
  * - `throw new AJNullPointerException()`
  * - `JavaStandardExceptions.throwNullPointerException()`
  * - `AJStandardExceptions.throwNullPointerException()`
  * - `JR_ThrowNullPointerException()`
  * - `JR_ThrowAJNullPointerException()`
  *
  * Example:
  * {{{
  *             A                         A
  *             |                         |
  *       If(x != null)      ----->     NC(x)
  *        T/       \F                    |
  *        /         \                    B
  *       B       throw(NPE)
  * }}}
  *
  * Use cases of this optimization are:
  * - `java.util.Objects.requireNonNull(obj)` and similar handwritten code patterns
  * - null tests with `JR_ThrowNullPointerException` on backup path arising after [[com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.LoopPredication]]
  *
  * @author arxdukalis
  * @author cypok
  */
trait ExplicitNullCheckFolding { self: Universe =>

  protected def replaceByNullCheck(throwBlock: BBlock, xContext: SpinalNode, posContext: Node, domain: Domain): Boolean = withIncrementalGCM {
    val handlerUsesNodesFromBlock = xContext.hasXHandler && {
      val nodes = Closure(throwBlock.nodes)(_.uses filter (_.block == throwBlock))
      nodes flatMap (_.outEdges) exists {
        case TaggedEdge(Tag.CONTROL, _: XPoint, _) => false
        case TaggedEdge(Tag.MEMORY | Tag.VALUE | Tag.CONTROL, _, dest) if xContext.xHandler dominates upperPoint(dest) => true
        case _ => false
      }
    }
    if (handlerUsesNodesFromBlock) {
      return false
    }

    var changed = false
    for (If.Exit(branch @ IfNull(obj, BlockExit(_, `throwBlock`), nonNullExit)) <- throwBlock.inputs.toList) {
      Block.splitBefore(branch)
      insertCode(ctrlBefore = branch.inCtrl, xContext, useDefaultHandler = false, posContext) {
        NullCheck(trusted = false, domain)(depriveIfNeeded(obj))
      }
      replaceByGoto(nonNullExit)
      stats.count(StatsKind.ExplicitNullCheckFolding, "If replaced by NullCheck", branch)
      changed = true
    }
    changed
  }

  private object ThrowBlockSpine {
    def unapplySeq(b: BBlock) = {
      def unimportant(n: SpinalNode): Boolean = n match {
        case _: PreparationCheck => true
        case _ => SpinalNode.sideEffectFree(n)
      }

      Some((b.spineForward filterNot unimportant).toSeq)
    }
  }

  private def tryOptimizeThrowNewNPE(throwNode: Throw): Boolean = {
    if (rootDeclaringClass.isCangjieType) {
      cond(throwNode.inValue) {
        case newNode @ New(Cangjie.Std.Core.NoneValueException()) =>
          cond(throwNode.block) {
            case throwBlock @ ThrowBlockSpine(
                    `newNode`,
                    initCall @ CallMethod(Cangjie.Std.Core.NoneValueException.`init`, MethodReferenceAccessKind.SPECIAL, Seq(`newNode`)),
                    `throwNode`) =>

              replaceByNullCheck(throwBlock, throwNode, initCall, Domain.CANGJIE)
          }
      }

    } else {
      cond(throwNode.inValue) {
        case newNode @ New(Java.Lang.NullPointerException()) =>
          cond(throwNode.block) {
            case throwBlock @ ThrowBlockSpine(
            Clinit(Java.Lang.NullPointerException()),
            `newNode`,
            initCall @ CallMethod(Java.Lang.NullPointerException.`init`, MethodReferenceAccessKind.SPECIAL, Seq(`newNode`)),
            `throwNode`) =>

              replaceByNullCheck(throwBlock, throwNode, initCall, Domain.JAVA)
          }

        case newNode @ New(Com.Huawei.Excelsior.Aj.Lang.AJNullPointerException()) =>
          cond(throwNode.block) {
            case throwBlock @ ThrowBlockSpine(
            `newNode`,
            initCall @ CallMethod(Com.Huawei.Excelsior.Aj.Lang.AJNullPointerException.`init`, MethodReferenceAccessKind.SPECIAL, Seq(`newNode`)),
            `throwNode`) =>

              replaceByNullCheck(throwBlock, throwNode, initCall, Domain.AJ)
          }
      }
    }
  }

  private def tryOptimizeStdExNPE(call: Call): Boolean = {
    def optimize(method: Method, domain: Domain) = {
      cond(call) {
        case CallMethod(`method`, MethodReferenceAccessKind.STATIC, Seq()) =>
          cond(call.block) {
            case throwBlock @ ThrowBlockSpine(`call`) =>
              replaceByNullCheck(throwBlock, call, call, domain)
          }
      }
    }
    optimize(RT.AJStandardExceptions.throwNPE, Domain.AJ) ||
      (languagePack.supports(JAVA) && optimize(RT.JavaStandardExceptions.throwNPE, Domain.JAVA))
  }

  private def tryOptimizeErrorRTSCallNPE(rtsCall: ErrorRTSCall): Boolean = {
    cond(rtsCall) {
      case ErrorRTSCall(RTSProc.JR_ThrowNullPointerException | RTSProc.JR_ThrowAJNullPointerException |
                        RTSProc.JR_ThrowCJNoneValueException | RTSProc.JR_ThrowScalaNullPointerException) =>
        cond(rtsCall.block) {
          case throwBlock @ ThrowBlockSpine(`rtsCall`) =>
            val domain = (rtsCall.proc: @unchecked) match {
              case RTSProc.JR_ThrowNullPointerException => Domain.JAVA
              case RTSProc.JR_ThrowAJNullPointerException => Domain.AJ
              case RTSProc.JR_ThrowCJNoneValueException => Domain.CANGJIE
              case RTSProc.JR_ThrowScalaNullPointerException => Domain.SCALA
            }
            replaceByNullCheck(throwBlock, rtsCall, rtsCall, domain)
        }
    }
  }

  def foldExplicitNullChecks(): Boolean = {
    if (!env.enabled(FoldExplicitNullChecks)) {
      return false
    }

    var changed = false
    for (x <- all[Throw]) {
      changed |= tryOptimizeThrowNewNPE(x)
    }
    for (x <- all[Call]) {
      changed |= tryOptimizeStdExNPE(x)
    }
    for (x <- all[ErrorRTSCall]) {
      changed |= tryOptimizeErrorRTSCallNPE(x)
    }
    changed
  }
}
