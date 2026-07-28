/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.types.Approximation.CC

import scala.math.Ordering.Implicits.infixOrderingOps

/** Lattice of integral intervals, including infinite intervals.
  *
  * Single Interval instance represents either closed (no infinities), left-closed (with right infinity) or
  * right-closed (left infinity) intervals, arranged as lattice with both join (union) and meet (intersect) operators.
  *
  * Lattice includes every continuous interval of integrals with or without infinity symbols in any position.
  * `Top` element is [-inf, +inf] interval, which includes all other intervals by definition of lattice.
  * `Bot` element is [_, _] or empty interval, which is included in all other intervals by definition.
  *
  * Partial order of intervals `(x, y)` defined like:
  *     1. If `x` contains `y`, then `y <= x`.
  *     1. If `y` contains `x`, then `x <= y`.
  *     1. Else `x` and `y` are incomparable.
  *
  * @author julian
  */
object Intervals {
  sealed trait IntervalApprox extends Approximation {
    def equals(obj: Any): Boolean

    def contains(that: IntervalApprox): Boolean = that == (this intersect that)

    override def compare(that: Approximation): Approximation.CC = that match {
      case that: IntervalApprox =>
        if (this == that) return CC.Equal

        this intersect that match {
          case res if res == this => CC.Less
          case `that` => CC.Greater
          case Empty => CC.Incomparable
          case _ => CC.PartiallyEqual
        }
    }

    override def union(that: Approximation): Approximation = that match {
      case that: IntervalApprox => IntervalApprox.union(this, that)
    }

    override def intersect(that: Approximation): Approximation = that match {
      case that: IntervalApprox => IntervalApprox.intersect(this, that)
    }

    override def isEmpty: Boolean = this eq Empty
  }

  case class NonEmpty private[Intervals](from: Bound, to: Bound) extends IntervalApprox {
    require(from <= to)
  }

  object Infinity extends NonEmpty(MinusInf, PlusInf)
  object Empty extends IntervalApprox

  object IntervalApprox {
    def apply(from: Bound, to: Bound): IntervalApprox = {
      if (to < from) return Empty
      if (from == MinusInf && to == PlusInf) Infinity else NonEmpty(from, to)
    }

    def apply(from: Long, to: Long): IntervalApprox = IntervalApprox(Finite(from), Finite(to))
    def apply(x: Long): IntervalApprox = IntervalApprox(Finite(x), Finite(x))
    def from(x: Long): IntervalApprox = IntervalApprox(Finite(x), PlusInf)
    def to(x: Long): IntervalApprox = IntervalApprox(MinusInf, Finite(x))

    def intersect(x: IntervalApprox, y: IntervalApprox): IntervalApprox = (x, y) match {
      case (Infinity, _) => y
      case (_, Infinity) => x
      case (Empty, _) => Empty
      case (_, Empty) => Empty

      case (NonEmpty(xFrom, xTo), NonEmpty(yFrom, yTo)) =>
        IntervalApprox(xFrom max yFrom, xTo min yTo)
    }

    /** This is conservative and not strict implementation of union.
      * For example: [0, 1] | [4, 5] = [0, 5], and not {0, 1, 4, 5}.
      */
    def union(x: IntervalApprox, y: IntervalApprox): IntervalApprox = (x, y) match {
      case (Infinity, _) => Infinity
      case (_, Infinity) => Infinity
      case (Empty, _) => y
      case (_, Empty) => x

      case (NonEmpty(xFrom, xTo), NonEmpty(yFrom, yTo)) =>
        IntervalApprox(xFrom min yFrom, xTo max yTo)
    }
  }

  sealed trait Bound extends Ordered[Bound] {
    override def compare(that: Bound): Int = (this, that) match {
      case (MinusInf, MinusInf) => 0
      case (MinusInf, _) => -1
      case (_, MinusInf) => 1
      case (PlusInf, PlusInf) => 0
      case (PlusInf, _) => 1
      case (_, PlusInf) => -1
      case (Finite(x), Finite(y)) => java.lang.Long.compare(x, y)
    }
  }

  object PlusInf extends Bound
  object MinusInf extends Bound
  case class Finite(n: Long) extends Bound
}
