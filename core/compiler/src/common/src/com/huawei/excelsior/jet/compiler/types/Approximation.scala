/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.types

import com.huawei.excelsior.jet.compiler.types.Approximation.{CC, compareToPartiallyOrdered}
import com.huawei.excelsior.jet.compiler.types.Approximation.CC.{Equal, Greater, Incomparable, Less, PartiallyEqual}

/** Value set approximation of an IR node. */
abstract class Approximation extends PartiallyOrdered[Approximation] {
  override def tryCompareTo[B >: Approximation : AsPartiallyOrdered](that: B) = that match {
    case that: Approximation => compareToPartiallyOrdered(that)(compare)
    case _ => None
  }

  def compare(that: Approximation): CC

  def union(that: Approximation): Approximation

  /** Returns pair (r, s), where `r` is the result of intersection, and `s` is true iff result strict,
    * i.e. same as theoretical result. Not strict result should be >= than theoretical result.
    */
  def weakIntersect(that: Approximation): (Approximation, Boolean) = (intersect(that), true)

  /** Returns approximation, which is same as theoretical result.
    *
    * We expect [[intersect]] to return strict result.
    * Depending on implementation of your subclass choose to override either of [[intersect]] or [[weakIntersect]].
    */
  def intersect(that: Approximation): Approximation = {
    val (result, isStrict) = weakIntersect(that)
    assert(isStrict)
    result
  }

  def isEmpty: Boolean
}

object Approximation {

  /** Condition code: comparison result of two approximations. */
  enum CC {
    case // (left compare right) is equal to ...
      Equal,          // if left is fully equal to right
      Less,           // if right contains all values from left and something additional
      Greater,        // if left contains all values from right and something additional
      Incomparable,   // if left and right contain some values but nothing in common
      PartiallyEqual  // if left and right contain some common values and some uncommon

    def inverse = this match {
      case Less => Greater
      case Greater => Less
      case _ => this
    }
  }

  def compareToPartiallyOrdered[A](that: A)(compare: A => CC): Option[Int] = compare(that) match {
    case Equal => Some(0)
    case Less => Some(-1)
    case Greater => Some(1)
    case Incomparable | PartiallyEqual => None
  }
}
