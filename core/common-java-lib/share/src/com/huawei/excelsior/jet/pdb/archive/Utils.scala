/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.pdb.archive

object Utils {
  def sizeStr(v: Long): String = {
    val t = 1000
    val k = 1024

    def k10th(x: Long): Double = x * 10 / k / 10.0

    if (v >= k * k) s"${k10th(v / k)}m"
    else if (v > 100 * k) s"${v / k}k"
    else if (v > 10 * t) s"${k10th(v)}k"
    else s"$v"
  }
}
