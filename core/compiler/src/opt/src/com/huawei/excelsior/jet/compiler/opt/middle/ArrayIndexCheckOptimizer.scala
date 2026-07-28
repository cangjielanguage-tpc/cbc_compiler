/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.util.ScalaCollections.{groupBy, maximalElements, partialOrderingBy, singleton}
import com.huawei.excelsior.jet.util.graph.Loops

/** Optimization of unnecessary ArrayIndexChecks. */
trait ArrayIndexCheckOptimizer extends ValueRangeAnalysis with CountedLoopsRecognizer { self: Universe =>

  private def log = Log(Log.Kind.AICOpt)

  def withArrayIndexCheckOptimizer[T](action: ArrayIndexCheckOptimizer => T): T = {
    log.inSession("AIC optimization", codeUnit) {
      val optimizer = new ArrayIndexCheckOptimizer
      action(optimizer)
    }
  }

  // Support iteration with Int32 inductive variable over Int64-indexed array (i.e. Cangjie or AJ array).
  // See JET-15749.
  private def skipExtend(n: Node) = n match {
    case BitFieldExtract.SignExtend(x) => x
    case BitFieldExtract.ZeroExtend(x) => x
    case n => n
  }

  class ArrayIndexCheckOptimizer {
    lazy val loops = cfg.loops
    lazy val inductiveVariables: collection.SeqMap[Phi, Seq[InductiveVariable]] = {
      val inductive = loops.seq flatMap (findInductiveVariables(_))
      groupBy(inductive)(_.index)
    }

    def ranges(n: ArrayIndexCheck, extended: Boolean = false) =
      calcValueRanges(skipExtend(n.idx), n, inductiveVariables.getOrElse(_, Nil), extended) // lambda is used for laziness

    def strikeOutCheck(check: ArrayIndexCheck, evidence: ControlNode, becauseMsg: String): Unit = {
      log(s"check removed$becauseMsg", check)
      if (evidence == null) {
        // This AIC is unnecessary in the whole IR.
        strikeOut(check)
      } else {
        // This AIC is unnecessary in IR part dominated by evidence only. We can not remove check at all,
        // because controlled nodes, dependent from it, may move out of this IR part (JET-10418, JET-10419). Instead of this we
        // make this check trusted.
        // See workaround for ArrayGet at ContextTypesMap.topmostValidPoint.
        // TODO: try to move check to evidence, try to delegate check dependencies to evidence
        assert(evidence dominates check)
        replaceByCode(check) { ArrayIndexCheck(check.arrayType, trusted = true)(check.valueArgs.toSeq: _*) }
      }
    }

    def versioningBound(check: ArrayIndexCheck) = ArrayIndexCheckOptimizer.VersioningBound(check.array, check.arrayType, ranges(check).toSeq: _*)
  }

  object ArrayIndexCheckOptimizer {

    sealed abstract class VersioningBound(val pred: PredicateConstructor)

    object VersioningBound {
      // TODO: unit-tests
      def apply(array: Node, arrayType: SignatureType, ranges: ValueRange*): VersioningBound = {
        val singleBounds = ranges map {
          case ConstValueRange(tpe, from, to, _) =>
            assert(from >= 0)
            Constant(tpe, to, array, arrayType)
          case HalfSymbolicValueRange(tpe, from, to, toAddend, _) =>
            assert(from >= 0)
            HalfSymbolic(tpe, to, toAddend, array, arrayType)
          case SymbolicValueRange(tpe, from, fromAddend, to, toAddend, _) =>
            Symbolic(tpe, from, fromAddend, to, toAddend, array, arrayType)
          case EmptyValueRange(_) =>
            shouldNotReachHere()
        }

        singleton(singleBounds) match {
          case Some(x) => x
          case None => Multiple(singleBounds)
        }
      }

      import PredicateConstructor._

      sealed abstract class Single(pred: PredicateConstructor) extends VersioningBound(pred) {
        def array: Node
      }

      def longArrayLength(tpe: Type, inCtrl: UpperPoint, array: Node, arrayType: SignatureType): Node = {
        // Note that we don't support unsigned value range analysis, so we use sign-extension here.
        // TODO: JET-13050
        array match {
          case getField: GetField if getField.field.getDeclaringClass.isArraySlice && getField.field.getName == "base" =>
            val sizeField = getField.field.getDeclaringClass.findDeclaredFieldOrNull(xstr("size"))
            BitFieldExtract.Extend(tpe, AsmType.I32, signExtension = true,
              GetField.proto(sizeField)(inCtrl, inCtrl.memoryAfter, getField.obj))  // TODO: JET-16759
          case _ =>
            BitFieldExtract.Extend(tpe, AsmType.I32, signExtension = true, ArrayLength(arrayType)(inCtrl, array))  // TODO: JET-16759
        }
      }

      /** Represents versioning base for `IConst(limit) < array.length` predicate. */
      case class Constant(tpe: Type, limit: Long, array: Node, arrayType: SignatureType) extends Single(
        atom(inCtrl => Cmp(tpe, Condition.LT)(IntegralConst(tpe)(limit), longArrayLength(tpe, inCtrl, array, arrayType)))
      )

      /** Represents versioning base for `0 <= limit + IConst(addend) < array.length` predicate. */
      case class HalfSymbolic(tpe: Type, limit: Node, addend: Long, array: Node, arrayType: SignatureType) extends Single(
        atom(inCtrl => Cmp(tpe, Condition.ULT)(Add(limit, IntegralConst(tpe)(addend)), longArrayLength(tpe, inCtrl, array, arrayType)))
      )

      /** Represents versioning base for `IConst(0) <= start + IConst(startAddend) <= limit + IConst(limitAddend) < array.length` predicate. */
      case class Symbolic(tpe: Type, start: Node, startAddend: Long, limit: Node, limitAddend: Long, array: Node, arrayType: SignatureType) extends Single(
        atom(inCtrl => Cmp(tpe, Condition.ULT)(Add(limit, IntegralConst(tpe)(limitAddend)), longArrayLength(tpe, inCtrl, array, arrayType))) &&
          atom(inCtrl => Cmp(tpe, Condition.ULE)(Add(start, IntegralConst(tpe)(startAddend)), Add(limit, IntegralConst(tpe)(limitAddend))))
      )

      /** Represents versioning base for `bounds(0) || bounds(1) || ... || bounds(n)` predicate. */
      case class Multiple(bounds: Seq[Single]) extends VersioningBound(bounds map (_.pred) reduceLeft (_ || _))

      implicit def ordering[T <: VersioningBound]: PartialOrdering[T] = partialOrderingBy[T](VersioningBound.absorbs).reverse
      private def absorbs(l: VersioningBound, r: VersioningBound): Boolean = (l, r) match {
        case (l: Constant, r: Constant) => l.array == r.array && l.limit >= r.limit
        case (l: HalfSymbolic, r: HalfSymbolic) => l.array == r.array && l.limit == r.limit && l.addend >= r.addend
        case (l: Symbolic, r: Symbolic) => l.array == r.array && l.start == r.start && l.limit == r.limit &&
          l.startAddend <= r.startAddend && l.limitAddend >= r.limitAddend
        case (l: Symbolic, r: HalfSymbolic) => l.array == r.array && l.limit == r.limit && l.limitAddend >= r.addend
        case (l: Single, Multiple(bounds)) => bounds exists (absorbs(l, _))
        case (Multiple(bounds), r) => bounds forall (absorbs(_, r))
        case (_, _) => false
      }

      def joinedPredicate(bounds: Seq[VersioningBound]): PredicateConstructor = {
        def collectArrays(x: VersioningBound): Seq[Node] = x match {
          case x: Single => Seq(x.array)
          case x: Multiple => x.bounds flatMap collectArrays
        }
        val arrayPreds = bounds.iterator.flatMap(collectArrays).distinct map nonNull
        val boundPreds = maximalElements(bounds) map (_.pred)
        val preds = arrayPreds ++ boundPreds
        assert(preds.nonEmpty)

        preds reduceLeft (_ && _)
      }
    }
  }

  def optimizeArrayIndexChecks(): Boolean = {
    withArrayIndexCheckOptimizer { optimizer =>

      def limitLessThanLength(limit: Node, limitAddend: Long, length: Node): Boolean = {
        val extraAddend: Long = (limit, length) match {
          case (`length`, _) => 0
          case (BitFieldExtract.Truncate(`length`), _) => 0 // Iteration with In32 variable over Int64-indexed array (JET-15749).
          case (AddWithIntegralConst(`length`, c), _) => c
          case (_, AddWithIntegralConst(`limit`, c)) => -c
          // Feel free to add more cases if you need.
          case _ => return false // Incomparable nodes.
        }
        checkAdd(limit.tpe, limitAddend, extraAddend) && {
          val distanceFromLimitToLength = limitAddend + extraAddend
          // Note that we know for sure that range(length) = [0, Int.MaxValue]
          // So if distance is negative then limit is less than length.
          // Note that in general case it doesn't hold
          // (e.g. (x - 10) not less than (x) if x = Int.MinValue + 5).
          distanceFromLimitToLength < 0
        }
      }

      var changed = false

      for (check <- all[ArrayIndexCheck].toList if !check.trusted) {
        // Iterate all calculated value ranges and stop if we successfully optimized check.
        changed |= optimizer.ranges(check, extended = true).iterator exists { x => (x, check.length) match {
          case (ConstValueRange(_, from, to, evidence), IntegralConst(len)) if 0 <= from && to < len =>
            optimizer.strikeOutCheck(check, evidence, s" because it's value is in range [$from, $to] and array length is $len")
            true

          case (HalfSymbolicValueRange(_, from, to, toAddend, evidence), len) if 0 <= from && limitLessThanLength(to, toAddend, len) =>
            optimizer.strikeOutCheck(check, evidence, s"")
            true

          case _ => false
        }}
      }

      changed
    }
  }

}
