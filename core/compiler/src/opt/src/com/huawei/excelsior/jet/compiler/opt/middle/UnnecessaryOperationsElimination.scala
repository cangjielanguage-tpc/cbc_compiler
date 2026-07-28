/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.options.BoolOption.OptimizeGetFlatThin
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.StatsKind.TypeOpt
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Domain
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.PrimitiveType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.*
import com.huawei.excelsior.jet.compiler.types.References.*

import PartialFunction.cond

/**
 * Optimization of elimination of operations:
 *   1) NullCheck,
 *   2) Clinit,
 *   3) CheckCast,
 *   4) InstanceOf.
 *
 * @author conwor, cypok
 */
trait UnnecessaryOperationsElimination { self: Universe =>
  import com.huawei.excelsior.jet.compiler.types.Approximation.CC

  private def optimizeNullCheck(n: NullCheck) = {
    // types are not used here because
    // nullchecks may be optimized easier later
    cond(n.obj) {
      case _: AnyNull =>
        replaceCheckByThrow(n)
        true
    }
  }

  private def optimizeThinNullCheck(n: ThinNullCheck): Boolean = {
    // if Thin is obtained from a @Flat field of ExecEnv, it cannot be null

    // TODO: this optimization should be replaced with normal GetFlatThin nodes and checks.
    // For more details look at comment near AJCallKind.GET_FLAT_THIN_INTRINSIC implementation in DataFlow.

    object ExecEnvWithDisp {
      def unapply(x: Node) = cond(x) {
        case _: ExecEnv | _: StackDescriptor => true
        case Add(_: ExecEnv | _: StackDescriptor, IntegralConst(disp)) if disp > 0L => true
      }
    }

    cond(n.obj) {
      case _: AnyNull =>
        replaceCheckByFatal(n)
        true
      case ReinterpretCast(_, _, ExecEnvWithDisp()) if !env.enabled(OptimizeGetFlatThin) =>
        strikeOut(n)
        true
    }
  }

  private def optimizeInstanceOf(n: InstanceOf): Boolean = {
    assert(!n.targetType.isDeferred)

    val argType = nodeType(n.obj)
    val checkedType = OpenCone(ReferenceType(asClassType(n.targetType)), mayBeNull = false)

    cond(checkedType compare argType) {
      case CC.Greater | CC.Equal =>
        // InstanceOf nodes aren't optimized in context types, because they are data-flow nodes,
        // we may optimize their control uses only, e.g. If. Thus they may be optimized here.
        stats.count(TypeOpt, "instanceof replaced by True", n)
        replaceTransitively(n, IConst(1))
        true

      case CC.Incomparable =>
        stats.count(TypeOpt, "instanceof replaced by False", n)
        replaceTransitively(n, IConst(0))
        true

      case CC.PartiallyEqual | CC.Less
        if argType.mayBeNull && (checkedType >= argType.withoutNull) =>
        // (x instanceof T) => (x != null) ? 1 : 0
        stats.count(TypeOpt, "instanceof replaced by null test", n)
        n replaceBy CondVal(Cmp(TRefType, Condition.NE)(n.obj, Null()))
        true
    }
  }

  private def optimizeCheckCast(n: CheckCast): Boolean = {
    assert(!n.targetType.isDeferred)

    val argType = nodeType(n.obj)
    val checkedType = OpenCone(ReferenceType(asClassType(n.targetType)), mayBeNull = true)

    cond(checkedType compare argType) {
      case CC.Incomparable =>
        stats.count(TypeOpt, "checkcast replaced by throw", n)
        replaceCheckByThrow(n)
        true

      case CC.PartiallyEqual
        if argType.mayBeNull && ((checkedType compare argType.withoutNull) == CC.Incomparable) && !n.trusted =>
        // (T)x => if (x != null) throw ClassCastException
        stats.count(TypeOpt, "checkcast replaced by null test", n)
        replaceCheckCastByThrowIfNonNull(n)
        true
    }
  }

  private def optimizeArrayStoreCheck(n: ArrayStoreCheck) = {
    n.value match {
      case get: ArrayGet if get.array == n.array =>
        stats.count(TypeOpt, "arraystorecheck is striked out (same array)", n)
        strikeOut(n)
        true

      case value if nodeTypeAt(value, n) == RefNull =>
        stats.count(TypeOpt, "arraystorecheck is striked out (null)", n)
        strikeOut(n)
        true

      case _ => cond(nodeTypeAt(n.array, n)) {
        case Point(arrType: JavaArrayType, _)
          if OpenCone(arrType.arrayElement.asInstanceOf[ReferenceType], mayBeNull = true) >= nodeTypeAt(n.value, n) =>
          stats.count(TypeOpt, "arraystorecheck is striked out (assign compatible)", n)
          strikeOut(n)
          true

        case UpperBounded(arrType: JavaArrayType, _)
          if OpenCone(arrType.arrayElement.asInstanceOf[ReferenceType], mayBeNull = true) incomparable nodeTypeAt(n.value, n) =>
          stats.count(TypeOpt, "arraystorecheck is replaced by throw", n)
          replaceCheckByThrow(n)
          true

        case arrApprox @ Point(arrType: JavaArrayType, _) if !n.hasFastPathInfo && !n.trusted => cond(nodeTypeAt(n.value, n)) {
          case ProbableType(OpenCone(t: InterfaceType, _))
            if !JavaArrayType.isSupertype(arrType.arrayElement) && (arrType.arrayElement.asInstanceOf[ReferenceType] >= t) =>
            n.arrayTypeForFastPath = arrApprox
            n.valueRelaxedType = t
            stats.count(TypeOpt, "arraystorecheck is fastpathed by rich check", n)
            false
        }

        case arrApprox: Cone if !n.hasFastPathInfo && !n.trusted =>
          (arrApprox.probableType match { // calculate formal array element
            case UpperBounded(arrType @ JavaReferenceArrayType(elem), _)
              if elem == ReferenceType.javaLangObject || !JavaArrayType.isSupertype(arrType.base) => Some(elem)
            case _ => None
          }) flatMap { arrayElement => nodeTypeAt(n.value, n) match { // check value assign compatibility with array element
            case ProbableType(Cone(t: InterfaceType, _)) if arrayElement >= t => Some(t)
            case t if OpenCone(arrayElement, mayBeNull = true) >= t => Some(null)
            case _ => None
          }} match {
            case Some(t) =>
              n.arrayTypeForFastPath = arrApprox
              n.valueRelaxedType = t
              stats.count(TypeOpt, s"arraystorecheck is fastpathed by elem check (value rich check needed = ${n.valueHasRelaxedType})", n)
            case None =>
          }
          false

        case OpenCone(arrType: JavaArrayType, _) if arrType.base.isInstanceOf[PrimitiveType] => shouldNotReachHere()
      }
    }
  }

  private def optimizeColdCodeMarker(n: ColdCodeMarker): Boolean = {
    val duplicates = collect[ColdCodeMarker](n.block.spine).filter(_ != n).toList
    duplicates foreach strikeOut
    duplicates.nonEmpty
  }

  private def optimizeConvertDomain(n: ConvertDomain): Boolean = {
    assert(n.valueArgs.length == 1)
    val exceptionArg = n.obj
    val exceptionArgType = nodeTypeAt(exceptionArg, n)

    // If all uses are throws, no conversion is needed: the next handler will do it.
    val allUsesAreThrows = Phi.transitiveValueUses(n) forall (_.isInstanceOf[Throw])

    // If the exception already has matching domain, no conversion is needed.
    // TODO: Support Cangjie exceptions/errors, if/when they get types in the compiler.
    val exceptionArgHasMatchingDomain = n.domain match {
      case Domain.AJ   => OpenCone(ReferenceType.ajLangAJThrowable, mayBeNull = false) >= exceptionArgType
      case Domain.JAVA => OpenCone(ReferenceType.javaLangThrowable, mayBeNull = false) >= exceptionArgType

      case _ => false
    }

    if (allUsesAreThrows || exceptionArgHasMatchingDomain) {
      strikeOutWithValueUses(n, exceptionArg)
      true
    } else {
      false
    }
  }


  def eliminateUnnecessaryOperations(): Boolean = {
    var wasChanged = false

    for (n <- allNodes) {
      wasChanged |= cond(n) {
        case n: NullCheck => optimizeNullCheck(n)
        case n: ThinNullCheck => optimizeThinNullCheck(n)
        case n: InstanceOf => optimizeInstanceOf(n)
        case n: CheckCast => optimizeCheckCast(n)
        case n: ArrayStoreCheck => optimizeArrayStoreCheck(n)
        case n: ColdCodeMarker => optimizeColdCodeMarker(n)
        case n: ConvertDomain => optimizeConvertDomain(n)
      }
    }

    wasChanged
  }

}
