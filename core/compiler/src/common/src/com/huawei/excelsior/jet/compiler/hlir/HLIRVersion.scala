/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir

import scala.PartialFunction.cond
import math.Ordered.orderingToOrdered
import scala.language.implicitConversions

case class HLIRVersion(major: Int, minor: Int, patch: Int) {
  def pretty: String = s"$major.$minor.$patch"

  def isSupported: Boolean = cond(this) {
    case HLIRVersion(1, 3, 0 | 1 | 2 | 3 | 4) => true
  }

  def hasVArray: Boolean = this >= HLIRVersion(1, 3, 1)

  def hasReferenceHashCode: Boolean = this >= HLIRVersion(1, 3, 1)

  def hasUncheckedArrayOperations: Boolean = this >= HLIRVersion(1, 3, 2)

  def hasUniversalGenerics: Boolean = this >= HLIRVersion(1, 3, 3)

  def hasUniversalGenericsForFusion: Boolean = this >= HLIRVersion(1, 3, 4)
}

object HLIRVersion {
  implicit val ord: Ordering[HLIRVersion] = Ordering by { v =>
    (v.major, v.minor, v.patch)
  }
}
