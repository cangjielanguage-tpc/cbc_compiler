/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.escape

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenStackAlloc
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenStackAllocJavaCBC
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.{isStandalone, targetArch}
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.ir.{EscapeKind, EscapeKindTuple, NewEscapeKind}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.InstantiatedType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.References.{Cone, ReferenceApprox}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.Log

import scala.PartialFunction.condOpt

trait StackAllocAnalysis extends EscapeAnalysis { self: Universe =>

  val MaxArraySizeInBytes = 256

  object StackAllocAnalysis {
    sealed abstract class Result { def asBoolean: Boolean }
    object Success extends Result { def asBoolean = true }
    case class Guarded(guard: GuardKey, invokes: Seq[(Call, Cone)]) extends Result { def asBoolean = true }
    case class Failure(msg: String | EscapeKind) extends Result { def asBoolean = false }
  }

  import StackAllocAnalysis._

  /** Returns result and message for stats. */
  def mayBeAllocatedOnStack(node: AnyNew): Result =
    mayBeAllocatedOnStackImpl(node, isSuitableForStackAlloc, allowGuarded = true)

  def mayBeAllocatedOnStack(node: Node, allocType: SymType): Boolean =
    mayBeAllocatedOnStackInContext(node, allocType, isSuitableForStackAlloc, allowGuarded = true).asBoolean

  private def mayBeAllocatedOnStackInOurCaller(node: AnyNew): Boolean =
    mayBeAllocatedOnStackImpl(node, isSuitableForStackAllocInOurCaller, allowGuarded = false).asBoolean

  private def mayBeAllocatedOnStackInOurCaller(node: Node, allocType: SymType): Boolean =
    mayBeAllocatedOnStackInContext(node, allocType, isSuitableForStackAllocInOurCaller, allowGuarded = false).asBoolean

  private def mayBeAllocatedOnStackImpl(node: AnyNew, isSuitableEscape: (EscapeKind) => Boolean, allowGuarded: Boolean): Result =
    mayBeAllocatedOnStackContextFree(node) match {
      case Success => mayBeAllocatedOnStackInContext(node, node.allocType.symType, isSuitableEscape, allowGuarded)
      case x: Failure => x
      case _: Guarded => shouldNotReachHere()
    }

  private def isSuitableForStackAlloc(esc: EscapeKind): Boolean = esc == NewEscapeKind.NoEscape

  private def isSuitableForStackAllocInOurCaller(esc: EscapeKind): Boolean = {
    // RcvEscape is ok for now, will be checked later
    esc.containsRetEscape
  }

  /** Method has generalized new type `T` if:
    * 1) method contains _caller stack allocatable_ `new T`
    * 2) method contains _caller stack allocatable_ another generalized new with type `T`
    * Operation is _caller stack allocatable_ if after inline to some context it may be allocated on stack.
    *
    * Originally described in JET-9211.
    */
  def calcGeneralizedNewTypes(): Seq[SymType] = {
    object ReturnedNew {
      // TODO: stack alloc key strings
      def unapply(n: AnyNew): Option[SymType] = condOpt(n) {
        case _: New | _: NewArray if mayBeAllocatedOnStackInOurCaller(n) => n.allocType.symType
      }
    }

    object ReturnedGeneralizedNew {
      def unapply(n: Node): Option[Seq[SymType]] = n match {
        case AnyDirectCall(target) if n.tpe.isTraceableRefType =>
          globallyAnalyzeMethod(target) map { info =>
            info.generalizedNewTypes filter (mayBeAllocatedOnStackInOurCaller(n, _))
          }

        case _ => None
      }
    }

    val types = (allNodes collect {
      case ReturnedNew(t) => Seq(t)
      case ReturnedGeneralizedNew(ts) => ts
    }).flatten.toList.distinct

    if (types.nonEmpty) {
      val log = Log(Log.Kind.GeneralizedNew)
      if (log.isEnabled) {
        log.inSession(s"code unit $codeUnit") {
          for (t <- types) {
            log(s"- ${t.getName}")
          }
        }
      }
    }

    types
  }

  private def mayBeAllocatedOnStackContextFree(node: AnyNew): Result = {
    lazy val allocType = node.allocType.symType

    if (isStandalone) {
      Failure(s"StackAlloc disabled in standalone mode")

    } else if (isO1Compiled || !env.enabled(GenStackAlloc)) {
      Failure(s"-$GenStackAlloc")

    } else if (allocType.isClass) {
      assert(node.isInstanceOf[New])

      if (allocType.isJavaReference && targetArch == CBC && !env.enabled(GenStackAllocJavaCBC)) {
        Failure("Java class in CBC")

      } else if (node.allocType.isInstanceOf[InstantiatedType]) {
        Failure("Instantiated type") // TODO: support stack alloc of concrete generic types

      } else if (implicitlyEscapedType(allocType)) {
        Failure("implicitly escaped type")

      } else if (allocType.getHeapObjectSize > RTConst.SmallAllocConfig.MAX_SMALL_OBJ_SIZE.intValue) {
        Failure("not SMALL")

      } else {
        Success
      }

    } else if (allocType.isArray) {
      // TODO: support stack allocation for arrays
      if (targetArch != CBC) {
        node.asInstanceOf[NewArray].lengths match {
          case Seq(IntegralConst(len)) =>
            if (len < 0) {
              Failure("negative array length")

            } else if (allocType.getArrayObjectSize(len, false) > MaxArraySizeInBytes) {
              Failure("too big array size")

            } else {
              Success
            }

          case Seq(_) =>
            Failure("non-constant array length")

          case _ =>
            Failure("multi-dimensional array")
        }
      } else {
        Failure("stack allocated arrays in CBC not supported")
      }

    } else {
      shouldNotReachHere()
    }
  }

  private def mayBeAllocatedOnStackInContext(node: Node, allocType: SymType, isSuitableEscape: (EscapeKind) => Boolean, allowGuarded: Boolean) = {
    val res = escapeKindOfGeneralizedNew(node, allocType, allowGuarded)
    if (isO1Compiled || !env.enabled(GenStackAlloc)) {
      Failure(s"-$GenStackAlloc")

    } else if (!isSuitableEscape(res.escape)) {
      Failure(res.escape)

    } else if (mayBeUsedOnBackEdge(node, typeApproxFromAllocType(allocType), allowGuarded)) {
      // TODO: implement less conservative analysis, see JET-9697
      Failure("used on back edge")

    } else {
      res match {
        case EscapeResult.Plain(_) => Success
        case EscapeResult.Guarded(_, guard, invokesAndTypes) => Guarded(guard, invokesAndTypes)
      }
    }
  }

  /** Check if this generalized new may be used on back edge. */
  def mayBeUsedOnBackEdge(node: Node, typeApprox: ReferenceApprox, allowGuarded: Boolean): Boolean = {
    val noUsesInOuterLoops = checkAllPossibleUsesOfNonEscapedNodeValue(node, typeApprox, allowGuarded) {
      case n: Phi =>
        // newOp is used on back edge of some outer loop
        // equivalent: phi is located in header of some outer loop
        // equivalent: phi's block dominates newOp
        !(n.block dominates node.block)
      case _ =>
        true
    }
    !noUsesInOuterLoops
  }
}
