/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.ConvertableInt
import xscala.util.Set32
import xscala.util.StringOps.asciiToUpperCase

object DecorParser {

  def parseDecor(): Unit = {
    if (env.config.option("SilentCompilation")) {
      env.decor = Set32.of(env.dc_silent.toUByte)
      return
    }
    val s = env.config.equation("DECOR")
    if (s == null) {
      env.decor = Set32.of(env.dc_header.toUByte, env.dc_tailer.toUByte, env.dc_compiler.toUByte, env.dc_report.toUByte, env.dc_progress.toUByte)
      return
    }
    env.decor = Set32.empty
    for (ch <- s) {
      ch.toChar.asciiToUpperCase match {
        case 'H' =>
          env.decor += env.dc_header.toUByte
        case 'T' =>
          env.decor += env.dc_tailer.toUByte
        case 'C' =>
          env.decor += env.dc_compiler.toUByte
        case 'P' =>
          env.decor += env.dc_progress.toUByte
        case 'R' =>
          env.decor += env.dc_report.toUByte
        case 'W' =>
          env.decor += env.dc_warnings.toUByte
        case 'S' =>
          env.decor += env.dc_silent.toUByte
        case _ =>
      }
    }
  }


}
