/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.StatsKind.{Evacuation, NewOptimization}
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, LogsKind, Nodes, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.escape.StackAllocOptimization
import com.huawei.excelsior.jet.compiler.opt.middle.inline.InlineOptimization
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.LambdaCommonSuperclass
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type}
import com.huawei.excelsior.jet.util.ScalaCollections.groupBy
import com.huawei.excelsior.jet.compiler.{CodeUnit, Environment, RTConst, RTSProc, Stage, Stats, symlevel}
import com.huawei.excelsior.jet.util.{Closure, Worklist}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Evacuation analysis allows you to stack-allocate some of those objects
  * that were not stack-allocated as a result of escape analysis.
  *
  * In addition, it places [[Evacuation]] before escape uses
  * (determined by the method [[TypesToAnalyze.TypeFunctions.isEscapeUse]]).
  * @author qq
  */
trait EvacuateAnalysis {
  self: Universe =>

  /**
    *  1. RequiresEvacuation needs to be covered by [[Evacuate]].
    *  1. Evacuated should affect the [[EvacuateAnalysis.isEscapeUse]] result, but don't need [[Evacuate]].
    *  1. Safe should not affect the result of the analysis.
    */
  private[EvacuateAnalysis] enum EvacuateResult {
    case Safe
    case Evacuated
    case RequiresEvacuation
  }
  import EvacuateResult.*

  private[EvacuateAnalysis] object EvacuateResult {
    def apply(cond: Boolean): EvacuateResult = if (cond) RequiresEvacuation else Safe
  }

  // Escape use in value is considered:
  //   * PutMemoryOperation: if value is `putValue`
  //   * Call: if the call accepts a value with erased typical information (for example, lambda's type is not LambdaCommon),
  //   * Return: if object is stack-allocated and created inside current method.
  private def isEscapeUse(edge: Edge, source: Node): EvacuateResult = edge.target match {
    case put: PutMemoryOperation if put.isPutValue(edge) => RequiresEvacuation
    
    case call: Call =>
      val tpe = call.methodType.parameterType(call.invokeArgIdx(edge))
      EvacuateResult(
        !tpe.symType.isEvacuatedType || (!call.targetRef.method.isAbstract && {
          locallyAnalyzeMethod(call.targetRef.method) match {
            case Some(info) if info.alwaysEvacuatedParams(call.invokeArgIdx(edge)) => return Evacuated
            case _ => false
          }
        })
      )

    case r: Return => EvacuateResult(!source.isInstanceOf[Param] ||
      !rootMethod.getMethodType.returnType.symType.isEvacuatedType)

    case _: EscapeWriteBarrier.Instance | _: EscapeWriteBarrier.Static => RequiresEvacuation // see JET-16633

    case _ => Safe
  }

  def paramHasUnconditionalEscape(p: Param): Boolean = p.formalType.symType.isEvacuatedType &&
                                                       computeTransitiveUses(p).exists(
                                                         use => isEscapeUse(use, p) != Safe &&
                                                         Return.unique.exists(use.usePoint.strictDominates)
                                                       )

  def newHasOnlyEscapeUses(n: AnyNew): Boolean =
    computeTransitiveUses(n).forall(isEscapeUse(_, n) != Safe)

  // Transitively calculates all usages through Phi-functions and EOPConverts.
  private def computeTransitiveUses(value: ProducesValue): collection.Set[Edge] =
    Closure(value.valueOutEdges) {
      _.target match {
        case n: EOPConvert => n.valueOutEdges
        case phi: Phi => phi.valueOutEdges
        case _ => Iterator.empty
      }
    }.filter(n => !n.target.isInstanceOf[Phi] && !n.target.isInstanceOf[EOPConvert])

  def placeEvacuation(): Boolean = {
    if (!env.enabled(BoolOption.Evacuation)) {
      return false
    }

    var changed = false

    // Node will be analyzed if it's type is marked as `EvacuatedType` and it is a:
    //   * NewStackAllocated: if it was stack-allocated by evacuate analysis decision,
    //   * Param: if param is not a receiver of method,
    //   * Call: if the method potentially returns the value it received as an argument.
    val stackAllocated = allNodes.collect {
      case n: NewStackAllocated if n.stackAllocatedByEvacuateAnalysis && n.allocType.symType.isEvacuatedType => n
      case p: Param if p.formalType.symType.isEvacuatedType && !p.isReceiver => p
      case c: Call if c.targetRef.methodType.parameterTypes
        .exists(tpe => tpe == c.targetRef.methodType.returnType && tpe.symType.isEvacuatedType) => c
    }.toSeq

    for (sa <- stackAllocated) {
      val transitiveUses = computeTransitiveUses(sa)

      var placedEvacuation = 0
      val usesBySource = groupBy(transitiveUses.filter(isEscapeUse(_, sa) == RequiresEvacuation))(_.source)

      // TODO: Each group of uses (grouped by blocks and use's type (Plain or Eop)) creates its own evacuation, which
      //       can potentially lead to a large number of objects on the heap. Instead of this, we need to reassign the
      //       maximum possible use to a single evacuation. JET-16847
      for ((_, usesFromOneSource) <- usesBySource; (_, uses) <- groupBy(usesFromOneSource)(_.useBlock)) {
        // TODO: JET-16455: currently evacuation can only throw out a fatal error, instead of OOM or SOE. 
        val posToPlaceEvacuation = uses.map(_.usePoint).reduce(_ nearestDom _)
        val evacuate = insertCodeBefore(lowerPoint(posToPlaceEvacuation), useDefaultHandler = true)(Evacuate(uses.head.source))
        uses.foreach(_.source = evacuate)

        placedEvacuation += 1
        changed = true
      }

      if (transitiveUses.nonEmpty) stats.count(Evacuation, s"Evacuate placed for node $sa, in quantity $placedEvacuation, in method $rootMethod.")
    }
    changed
  }
}
