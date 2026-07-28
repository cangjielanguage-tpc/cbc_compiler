/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.xpackii.jreinfo

import xscala.util.StringOps.*
import com.huawei.excelsior.jet.compiler.xpackii.ProgressLogger

enum RTOptionalComponent {
  case
    JDK_TOOLS,
    API_CLASSES,
    JCE,
    ACCESSIBILITY,
    JAVAFX,
    NASHORN,
    CLDR,
    DNSNS,
    ZIPFS
}

object RTOptionalComponent {

  def fromString(optionalComponent: String, logger: ProgressLogger) = {
    assert(optionalComponent != null)
    try {
      valueOf(optionalComponent.asciiToUpperCase.replace('-', '_'))
    } catch {
      case e: IllegalArgumentException =>
        logger.fatalError("Unknown optional component \"" + optionalComponent + "\"")
        throw e // unreachable
    }
  }
}
