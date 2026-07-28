/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Universe

trait ValueRanges { self: Universe =>

  sealed abstract class ValueRange {
    /** This range is valid only in points dominated by this `evidence` if `evidence` is non-null. */
    def evidence: ControlNode

    def tpe: Type

    def isEmpty: Boolean = this.isInstanceOf[EmptyValueRange]
    def nonEmpty: Boolean = !isEmpty
  }

  object ValueRange {

    /** Returns a [[ValueRange]] encapsulating `[from, to]` range given by inclusive bounds `from` and `to`. */
    def apply(from: Node, to: Node, evidence: ControlNode): ValueRange = {
      val tpe = from.tpe
      (from, to) match {
        case (IntegralConst(from), IntegralConst(to)) => if (to < from) EmptyValueRange(tpe) else ConstValueRange(tpe, from, to, evidence)
        case (IntegralConst(from), _)                 => HalfSymbolicValueRange(tpe, from, to, 0, evidence)
        case (_, _)                                   => SymbolicValueRange(tpe, from, 0, to, 0, evidence)
      }
    }

    /** Returns a pair of inclusive bounds `(from, to)` for the given `range` encapsulating `[from, to]` range. */
    def bounds(range: ValueRange): (Node, Node) = range match {
      case ConstValueRange(tpe, from, to, _)                          => (IntegralConst(tpe)(from),                  IntegralConst(tpe)(to))
      case HalfSymbolicValueRange(tpe, from, to, toAddend, _)         => (IntegralConst(tpe)(from),                  Add(to, IntegralConst(tpe)(toAddend)))
      case SymbolicValueRange(tpe, from, fromAddend, to, toAddend, _) => (Add(from, IntegralConst(tpe)(fromAddend)), Add(to, IntegralConst(tpe)(toAddend)))
      case EmptyValueRange(_) => shouldNotReachHere()
    }

    /** Returns the size of the given `range` as unsigned integer. */
    def size(range: ValueRange): Node = range match {
      case range: ConstValueRange => IntegralConst(range.tpe)(range.size)
      case _ =>
        val (from, to) = bounds(range)
        val tpe = range.tpe
        Add(Sub(to, from), IntegralConst(tpe)(1))
    }
  }

  case class EmptyValueRange(tpe: Type) extends ValueRange {
    def evidence = null
  }

  /** E.g. `for (i = 3; i < 20; i++)` gives following range for `i`: `CVR(3, 19, ...)`. */
  case class ConstValueRange(tpe: Type, from: Long, to: Long, evidence: ControlNode) extends ValueRange {
    require(from <= to)

    /** Returns the size of this range as unsigned integer. */
    def size: Long = to - from + 1 // TODO: JET-13031
  }

  /** E.g. `for (i = 0; i < foo(); i++)` gives following range for `i`: `HSVR(0, foo(), -1, ...)`. */
  case class HalfSymbolicValueRange(tpe: Type, from: Long, to: Node, toAddend: Long, evidence: ControlNode) extends ValueRange

  /** E.g. `for (i = foo(); i < bar(); i++) gives following range for `i`: `SVR(foo(), 0, bar(), -1, ...)`.
    * E.g. `for (i = foo(); i > bar(); i--) gives following range for `i`: `SVR(bar(), 1, foo(),  0, ...)`.
    */
  case class SymbolicValueRange(tpe: Type, from: Node, fromAddend: Long, to: Node, toAddend: Long, evidence: ControlNode) extends ValueRange

}
