/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.symlevel.{MethodType, Type as SymType}
import com.huawei.excelsior.jet.compiler.options.BoolOption.*

/**
 * Utilities for enrichment support.
 *
 * @author conwor
 */
trait EnrichmentSupport { self: Universe =>

  /** True, iff enriched pointers used. */
  lazy val useEnrichedPointers: Boolean = RTConst.Eop.ENABLED.boolValue

  /** Exclusive index limit */
  lazy val enrichmentIndexLimit: Int = RTConst.Eop.IDX_LIMIT.intValue

  /** Inclusive imtOffset limit */
  lazy val enrichmentIMTOffsetLimit: Int = RTConst.Eop.OFFSET_LIMIT.intValue

  lazy val enrichmentIMTOffsetShift: Int = RTConst.Eop.ENRICHMENT_SHIFT.intValue

  lazy val enrichmentMask: Long = RTConst.Eop.ENRICHMENT_MASK.longValue

  /** Offset shift value of ciao */
  lazy val ciaoOffsetShift: Int = RTConst.CIAO.IMT_OFFSET_SHIFT.intValue

  /** True, iff enrichment contains incremented index of IMT slot */
  lazy val enrichmentContainsIncrementedIndex: Boolean = RTConst.Eop.INCREMENTED_IDX.boolValue

  sealed abstract class EnrichmentDecision {
    protected def symType: SymType

    def map[B](f: SymType => B): Option[B] = Option(symType).map(f)
    def isDefined: Boolean = symType != null
    def isEmpty: Boolean = !isDefined
    def toOption: Option[SymType] = map(Predef.identity)
    def get: SymType = if (isDefined) symType else throw new NoSuchElementException("EnrichmentDecision.get")

    // Workaround for JET-15020.
    // TODO: propose any better solution
    def <=(that: EnrichmentDecision): Boolean = EnrichmentDecision.lteq(this, that)
  }

  object EnrichmentDecision {
    case class Yes(symType: SymType) extends EnrichmentDecision

    case object No extends EnrichmentDecision {
      override def symType = null
    }
    case object DoNotKnow extends EnrichmentDecision {
      override def symType = null
    }

    private def lteq(x: EnrichmentDecision, y: EnrichmentDecision): Boolean = (x, y) match {
      case (x, y) if x == y => true
      case (No, DoNotKnow) => true
      case _ => false
    }
  }

  import EnrichmentDecision._

  /** Returns Some(t), if given `node` produces Rich(t) value, or None otherwise. */
  def producesRich(node: Node): EnrichmentDecision = node match {
    case _: NoValue => shouldNotReachHere()
    case x: TDBarrier => producesRich(x.obj)

    case BitcodeDeferred.Invoke(target) if !target.refClass.isJavaReference => DoNotKnow // TODO: workaround for JET-15803
    case x: BitcodeDeferred.FieldOp if !x.fieldRef.refType.isJavaReference => DoNotKnow // TODO: workaround for JET-15803

    case _ => isRich(node.tpe)
  }

  def isRich(tpe: Type): EnrichmentDecision = tpe match {
    case EopType.Eop(t) =>
      assert(!t.isDeferred)
      assert(!typeProvider.isManagedEopUnderlyingType(t))
      Yes(t)
    case EopType.Any => DoNotKnow
    case _ => No
  }

  def methodParamEnrichment(mt: MethodType, paramIdx: Int): EnrichmentDecision = {
    if (mt.isReceiverParameter(paramIdx) || mt.isVarArgs) {
      No
    } else {
      val t = mt.parameterType(paramIdx)
      if (t.isInterface) {
        if (t.isDeferred) No
        else if (typeProvider.isManagedEopUnderlyingType(t)) DoNotKnow
        else Yes(t.symType)
      } else No
    }
  }

  def depriveMethodArg(mt: MethodType, paramIdx: Int, n: Node): Node = {
    methodParamEnrichment(mt, paramIdx) ensuring (checkRichType(n, _)) map (t => Deprive(t)(n)) getOrElse n
  }

  def depriveMethodArgs(mt: MethodType, args: Seq[Node]): Seq[Node] =
    args.zipWithIndex map { case (n, paramIdx) => depriveMethodArg(mt, paramIdx, n) }

  def depriveIfNeeded(n: Node): Node = n match {
    case _: NoValue => n
    case _ => producesRich(n) map (t => Deprive(t)(n)) getOrElse n
  }

  def depriveUnsafe(n: Node): Node = isRich(n.tpe) match {
    case Yes(t) => Deprive(t)(n)
    case DoNotKnow | No => n
  }

  def enrichArg(allowTypeMismatch: Boolean = false)(tpe: Type, node: Node): Node = {
    if (!tpe.isValueType) return node

    isRich(tpe) match {
      case Yes(t) =>
        def enrich(n: Node) = {
          Enrich(t)(n, WeakCast(t)(n, WeakCast.NoCheck()))
        }

        if (allowTypeMismatch) {
          enrich(depriveUnsafe(node))
        } else {
          val doEnrich = node match {
            case _: NoValue => false
            case _ => producesRich(node) match {
              case Yes(objT) => assert(objT == t); false
              case No => true
              case DoNotKnow => false
            }
          }
          if (doEnrich) {
            enrich(node)
          } else {
            node
          }
        }
      case _ => node
    }
  }

  private def checkRichType(node: Node, rt: EnrichmentDecision) = node match {
    case _: NoValue => true
    case _ => producesRich(node) <= rt
  }
}
