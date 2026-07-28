/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.types

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox.{intersect, *}
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.types.Approximation
import com.huawei.excelsior.jet.compiler.types.Approximation.CC

object LoweredReferences {

  /** Lattice of [[LoweredReferenceApprox]].
    * {{{
    * Greater ->    Nullable
    *                /    \
    *            Null      NonNull
    *                \    /
    * Less    ->     Empty
    * }}}
    */
  sealed abstract class LoweredReferenceApprox extends Approximation {
    import LoweredReferenceApprox._

    override final def compare(that: Approximation): CC = that match {
      case that: LoweredReferenceApprox =>
        (this, that) match {
          case (`that`, `that`) => CC.Equal
          case (LoweredRefNullable, _)    => CC.Greater
          case (LoweredRefEmpty, _)       => CC.Less
          case (_, LoweredRefNullable)    => CC.Less
          case (_, LoweredRefEmpty)       => CC.Greater
          case (_, _)           => CC.Incomparable
        }
      case _ => shouldNotReachHere()
    }

    final override def weakIntersect(that: Approximation): (LoweredReferenceApprox, Boolean) = that match {
      case that: LoweredReferenceApprox => (LoweredReferenceApprox.intersect(this, that), true)
      case _ => shouldNotReachHere()
    }

    final def subtract(that: LoweredReferenceApprox): (LoweredReferenceApprox, Boolean) = (this, that) match {
      case (`that`, `that`)                        => (LoweredRefEmpty,  true)
      case (_, LoweredRefNullable)                 => (LoweredRefEmpty,   true)
      case (LoweredRefNullable, LoweredRefNull)    => (LoweredRefNonNull, true)
      case (LoweredRefNullable, LoweredRefNonNull) => (LoweredRefNull,    true)
      case (_, _)                                  => (this,    true)
    }

    final override def union(that: Approximation): Approximation = (this, that) match {
      case (`that`, _)           => this
      case (LoweredRefEmpty, _)  => that
      case (_, LoweredRefEmpty)  => this
      case (_, _)                => LoweredRefNullable
    }

    override def isEmpty = this eq LoweredRefEmpty
  }

  object LoweredReferenceApprox {
    case object LoweredRefEmpty extends LoweredReferenceApprox
    case object LoweredRefNull extends LoweredReferenceApprox
    case object LoweredRefNonNull extends LoweredReferenceApprox
    case object LoweredRefNullable extends LoweredReferenceApprox

    def fromReferenceApproximation(tpe: ReferenceApprox): LoweredReferenceApprox = tpe match {
      case RefEmpty => LoweredRefEmpty
      case RefNull => LoweredRefNull
      case _ => if (tpe.mayBeNull) LoweredRefNullable else LoweredRefNonNull
    }

    private def intersect(t1: LoweredReferenceApprox, t2: LoweredReferenceApprox): LoweredReferenceApprox = (t1, t2) match {
      case (`t2`, `t2`)   => t1
      case (_, LoweredRefNullable)  => t1
      case (LoweredRefNullable, _)  => t2
      case (_, _)         => LoweredRefEmpty
    }
  }
}
