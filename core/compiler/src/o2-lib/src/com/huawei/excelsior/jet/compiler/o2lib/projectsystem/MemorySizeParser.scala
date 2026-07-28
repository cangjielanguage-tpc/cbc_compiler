/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule as js

object MemorySizeParser {

  def parseMemorySize(sPar: XString): Long = {
    var s = sPar
    var factor: Int = 0
    var hassuffix: Boolean = false

    if (s == null || s.isEmpty) {
      return -1
    }
    s.charAt(s.length - 1) match {
      case 'G' |
           'g' =>
        factor = 1024 * 1024 * 1024
        hassuffix = true
      case 'M' |
           'm' =>
        factor = 1024 * 1024
        hassuffix = true
      case 'K' |
           'k' =>
        factor = 1024
        hassuffix = true
      case _ =>
        factor = 1
        hassuffix = false
    }

    if (hassuffix) {
      s = s.substring(0, s.length - 1)
    }

    val i = js.parseULong(s)
    if (i >= 0) {
      i * factor.toLong
    } else {
      -1
    }
  }

}
