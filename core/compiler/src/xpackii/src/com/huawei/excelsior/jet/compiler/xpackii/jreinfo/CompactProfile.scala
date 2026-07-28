/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import xscala.util.StringOps.*

enum CompactProfile(val packagesProperty: String) {
  case COMPACT1 extends CompactProfile("compact1Packages")
  case COMPACT2 extends CompactProfile("compact2Packages")
  case COMPACT3 extends CompactProfile("compact3Packages")
  case FULL extends CompactProfile("fullPackages")
}

object CompactProfile {

  def fromString(str: String): CompactProfile = {
    assert(str != null)
    val strUC = str.asciiToUpperCase
    if ("FULLJRE" == strUC) {
      return FULL
    }
    valueOf(strUC)
  }
}
