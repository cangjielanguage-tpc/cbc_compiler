/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie

import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodSignature, SignatureType}

object CangjieMath {
  val float64PowerName = "CJ_CORE_CPow"
  val float64PowerSignature = MethodSignature(SignatureType.Float64, Seq(SignatureType.Float64, SignatureType.Float64))
  val float64PowerInt64Name = "CJ_CORE_FastPowerDoubleInt64"
  val float64PowerInt64Signature = MethodSignature(SignatureType.Float64, Seq(SignatureType.Float64, SignatureType.Int64))
  val powerMethods = Seq(float64PowerName -> float64PowerSignature, float64PowerInt64Name -> float64PowerInt64Signature)

  /** Returns the colon-separated foreign library list to record in the CBC file.
    *
    * When `methods` contains a Cangjie foreign power helper with a matching name
    * and signature, adds `cangjie-std-core` to the current list and removes duplicate
    * entries.
    */
  def updateForeignLibsList(currentlyConfigured: Option[String], methods: Iterator[Method]): Option[String] = {
    val needsRuntime = methods.exists { m =>
      m.isCangjieForeign && powerMethods.exists { case (name, sig) =>
        m.getName == name && m.getSignature == sig
      }
    }
    if (needsRuntime) {
      Some((currentlyConfigured.toSeq.flatMap(_.split(":")) :+ "cangjie-std-core").distinct.mkString(":"))
    } else {
      currentlyConfigured
    }
  }
}
