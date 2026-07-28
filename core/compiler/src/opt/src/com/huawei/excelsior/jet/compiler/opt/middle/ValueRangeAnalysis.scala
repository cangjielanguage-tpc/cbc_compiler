/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

import xscala.util.MathUtils

trait ValueRangeAnalysis extends ValueRanges { this: Universe with CountedLoopsRecognizer =>

  /** Calculate ranges of `node` values at given `point`.
    *
    * In current implementation loop index may have multiple value ranges
    * in case of multiple loop exits
    * (e.g. `for (i = 0; (i < x) && (i < y); ...)` gives following ranges for `i`: `{[0, x), [0, y)}`).
    *
    * @param extendedRanges indicates whether calculation should return additional (possibly conservative) value ranges
    *                       calculated using [[ValueRangeFilter]] information and symbolic range expansion.
    */
  // TODO: make cache
  def calcValueRanges(node: Node, point: LowerPoint,
                      inductiveVariables: Phi => Iterable[InductiveVariable],
                      extendedRanges: Boolean = false): Iterable[ValueRange] = {
    calcValueRangesImpl(node, point, inductiveVariables, extendedRanges, Nil)
  }

  private def calcValueRangesImpl(node: Node, point: LowerPoint,
                                  inductiveVariables: Phi => Iterable[InductiveVariable],
                                  extendedRanges: Boolean, alreadyProcessed: List[Node]): Iterable[ValueRange] = {
    if (alreadyProcessed contains node) {
      return Iterable.empty
    }

    val tpe = node.tpe

    val ranges = node match {

      case IntegralConst(v) => Seq(ConstValueRange(tpe, v, v, null))

      // TODO: support LongType
      case bfx @ BitFieldExtract(_, size, sx, _) if bfx.tpe == IntType =>
        if (sx || size == 32) {
          Seq(ConstValueRange(tpe, ~MathUtils.rightNBits32(size - 1), MathUtils.rightNBits32(size - 1), null))
        } else {
          Seq(ConstValueRange(tpe, 0, MathUtils.rightNBits32(size), null))
        }

      // TODO: support LongType
      case AndWithIConst(_, mask) if mask > 0 => Seq(ConstValueRange(tpe, 0, mask, null))

      case AddWithIntegralConst(x, v) => calcValueRangesImpl(x, point, inductiveVariables, extendedRanges, node :: alreadyProcessed) flatMap {
        case ConstValueRange(tpe, from, to, evidence) if checkAdd(tpe, from, v) && checkAdd(tpe, to, v) =>
          Seq(ConstValueRange(tpe, from + v, to + v, evidence))

        case HalfSymbolicValueRange(tpe, from, to, toAddend, evidence) if checkAdd(tpe, from, v) && checkAdd(tpe, toAddend, v) =>
          Seq(HalfSymbolicValueRange(tpe, from + v, to, toAddend + v, evidence))

        case SymbolicValueRange(tpe, from, fromAddend, to, toAddend, evidence) if checkAdd(tpe, fromAddend, v) && checkAdd(tpe, toAddend, v) =>
          Seq(SymbolicValueRange(tpe, from, fromAddend + v, to, toAddend + v, evidence))

        case _ => Nil
      }

      case node: Phi =>
        // Note that we pragmatically analyze value ranges only of inductive variables
        // however simple phi functions also might give us information about variable value range
        // (e.g. range(Phi(Iconst(2), IConst(3))) = [2,3]).
        inductiveVariables(node) flatMap (calcValueRangesOfInductiveVariable(_, point))

      case _ => Nil
    }

    if (extendedRanges) {
      // Collect information from value range filters.
      val filteredRanges = node.valueUses.toSeq collect {
        case x: ValueRangeFilter if x.filteredValue == node && (x.filteredValueCtrl dominates point) => x.filteredValueRange
      }
      // Symbolic value range expansion.
      // Try to expand all collected symbolic value ranges to half symbolic ones,
      // by combining them with value ranges of lower bounds (e.g. for RMACombining).
      // TODO: support more cases if needed
      val expandedRanges = (ranges ++ filteredRanges) collect {
        case SymbolicValueRange(_, from, fromAddend, to, toAddend, evidence) =>
          calcValueRangesImpl(from, point, inductiveVariables, extendedRanges, node :: alreadyProcessed) collect {
            case ConstValueRange(_, newFrom, _, _) if checkAdd(tpe, newFrom, fromAddend) =>
              HalfSymbolicValueRange(tpe, newFrom + fromAddend, to, toAddend, evidence)
            case HalfSymbolicValueRange(_, newFrom, _, _, _) if checkAdd(tpe, newFrom, fromAddend) =>
              HalfSymbolicValueRange(tpe, newFrom + fromAddend, to, toAddend, evidence)
          }
      }
      ranges ++ filteredRanges ++ expandedRanges.flatten
    } else {
      ranges
    }
  }

  /** Calculate constant range of `node` values at given `point`. */
  def calcConstValueRanges(node: Node, point: LowerPoint,
                           inductiveVariables: Phi => Iterable[InductiveVariable]): Iterable[ConstValueRange] = node match {
    case _: ArrayLength =>
      assert(calcValueRanges(node, point, inductiveVariables).isEmpty, "ensure that ArrayLength is not handled")
      Seq(ConstValueRange(node.tpe, 0, maxValue(node.tpe), null))

    case _ =>
      calcValueRanges(node, point, inductiveVariables) collect { case x: ConstValueRange => x }
  }

  private def calcValueRangesOfInductiveVariable(iv: InductiveVariable, point: LowerPoint): Iterable[ValueRange] = {
    val evidence = iv.continueEdge
    if (evidence dominates point) {
      // Note that we pragmatically analyze value ranges only after successful limit check
      // however failed check also might give us information about variable value range
      // (e.g. for (i = ...; i < 10; ...) { ... }; range(i, after loop) = [10, Int.MaxValue]).
      calcValueRangeOfInductiveVariable(iv)
    } else {
      None
    }
  }

  def calcValueRangeOfInductiveVariable(iv: InductiveVariable): Option[ValueRange] = {
    val step = iv.step
    val stepAbs = step.abs
    val cond = iv.cond
    val evidence = iv.continueEdge
    val preIncrement = iv.incrementIsCompared

    val tpe = iv.tpe

    val _preIncrementPadding = if (preIncrement) stepAbs else 0

    /** Actual maximum value of index may be less (greater) than end value
      * because of strict condition and/or pre-increment (pre-decrement) before comparing.
      */
    def getEndPadding(getCondPadding: PartialFunction[Condition, Long]): Option[Long] = {
      getCondPadding lift cond flatMap { condPadding =>
        assert(condPadding >= 0)
        if (checkAdd(tpe, condPadding, _preIncrementPadding)) {
          Some(condPadding + _preIncrementPadding)
        } else {
          None
        }
      }
    }

    (iv.start, iv.limit) match {
      case (IntegralConst(_start), IntegralConst(_end)) =>
        val start = _start
        val preIncrementPadding = math.signum(step) * _preIncrementPadding
        if (!checkAdd(tpe, _end, -preIncrementPadding)) return None
        val end = _end - preIncrementPadding

        cond match {
          case Condition.LT if step < 0 => return Option.when(start >= end)(EmptyValueRange(tpe))
          case Condition.LE if step < 0 => return Option.when(start >  end)(EmptyValueRange(tpe))
          case Condition.GT if step > 0 => return Option.when(start <= end)(EmptyValueRange(tpe))
          case Condition.GE if step > 0 => return Option.when(start <  end)(EmptyValueRange(tpe))

          case Condition.NE if step > 0 && start > end => return None
          case Condition.NE if step < 0 && start < end => return None

          case _ =>
        }

        val _condPadding = cond match {
          case Condition.LT | Condition.GT => 1L
          case Condition.LE | Condition.GE => 0L
          case Condition.NE if (start - end).abs % stepAbs == 0 => 1L
          case _ => return None
        }

        val condPadding = math.signum(step) * _condPadding
        if (!checkAdd(tpe, end, -condPadding)) return None
        val (fromRaw, toRaw) = if (step > 0) {
          (start, end - condPadding)
        } else {
          (end - condPadding, start)
        }

        if (!checkRange(tpe, fromRaw) || !checkRange(tpe, toRaw)) {
          return None
        }

        if (fromRaw > toRaw) {
          return Some(EmptyValueRange(tpe))
        }

        val sizeRaw = toRaw - fromRaw + 1L
        val overstep = if (sizeRaw % stepAbs == 0) {
          sizeRaw
        } else {
          sizeRaw - sizeRaw % stepAbs + stepAbs
        }

        val (from, to) = if (step > 0 && checkRange(tpe, fromRaw + overstep)) {
          (fromRaw, fromRaw + overstep - stepAbs)
        } else if (step < 0 && checkRange(tpe, toRaw - overstep)) {
          (toRaw - overstep + stepAbs, toRaw)
        } else {
          return None
        }
        Some(ConstValueRange(tpe, from, to, evidence))

      case (lowExpr, highExpr) if step == 1 =>
        getEndPadding {
          case Condition.LT | Condition.NE => 1
          case Condition.LE => 0
        } flatMap { highPadding => lowExpr match {
          case IntegralConst(lowValue) if lowValue == maxValue(tpe) =>
            None
          case IntegralConst(lowValue) =>
            Some(HalfSymbolicValueRange(tpe, lowValue, highExpr, -highPadding, evidence))
          case _ =>
            Some(SymbolicValueRange(tpe, lowExpr, 0, highExpr, -highPadding, evidence))
        }}

      case (highExpr, lowExpr) if step == -1 =>
        getEndPadding {
          case Condition.GT | Condition.NE => 1
          case Condition.GE => 0
        } map { lowPadding => lowExpr match {
          case IntegralConst(lowValue) if checkAdd(tpe, lowValue, lowPadding) =>
            HalfSymbolicValueRange(tpe, lowValue + lowPadding, highExpr, 0, evidence)
          case _ =>
            SymbolicValueRange(tpe, lowExpr, lowPadding, highExpr, 0, evidence)
        }}

      case _ => None
    }
  }

  /** Returns true, iff `a` is in `tpe` range. */
  def checkRange(tpe: Type, a: Long): Boolean = tpe match {
    case LongType => a.isValidLong
    case IntType => a.isValidInt
    case _ => shouldNotReachHere(tpe)
  }

  /** Returns true, iff add of given `a`, `b` do not lead to overflow. */
  def checkAdd(tpe: Type, a: Long, b: Long): Boolean = tpe match {
    // Overflow may occur only if `a` and `b` have same signs.
    case IntType  => !((a.sign == b.sign) && (a.sign != (a.toInt + b.toInt).sign))
    case LongType => !((a.sign == b.sign) && (a.sign != (a + b).sign))
    case _ => shouldNotReachHere(tpe)
  }

  def maxValue(tpe: Type): Long = tpe match {
    case LongType => Long.MaxValue
    case IntType => Int.MaxValue
    case _ => shouldNotReachHere(tpe)
  }

  def minValue(tpe: Type): Long = tpe match {
    case LongType => Long.MinValue
    case IntType => Int.MinValue
    case _ => shouldNotReachHere(tpe)
  }

  def unsignedMaxValue(tpe: Type): Long = tpe match {
    case LongType => MathUtils.ULONG_MAX
    case IntType => MathUtils.UINT_MAX
    case _ => shouldNotReachHere(tpe)
  }

  def unsignedMinValue(tpe: Type): Long = tpe match {
    case LongType => MathUtils.ULONG_MIN
    case IntType => MathUtils.UINT_MIN
    case _ => shouldNotReachHere(tpe)
  }

}
