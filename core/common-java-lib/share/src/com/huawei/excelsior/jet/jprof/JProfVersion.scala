/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import scala.PartialFunction.cond
import math.Ordered.orderingToOrdered
import scala.language.implicitConversions

case class JProfVersion(major: Int, minor: Int, patch: Int) {

  def pretty: String = s"$major.$minor.$patch"

  def isSupported: Boolean = this >= JProfVersion.minimal
}

object JProfVersion {
  implicit val ord: Ordering[JProfVersion] = Ordering by { v =>
    (v.major, v.minor, v.patch)
  }

  def fromString(version: String): JProfVersion = {
    val Array(major, minor, patch) = version.split('.')
    JProfVersion(major.toInt, minor.toInt, patch.toInt)    
  }

  /** Minimal supported version of JProf format. */
  private val minimal = JProfVersion(2, 1, 0)

  /** Current version of JProf format. */
  private[jprof] val current = JProfVersion(2, 1, 0)

}
