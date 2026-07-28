/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.jprof

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.jprof.JProfManagerModule.JProfManager
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.{MultipleJProfs, PGO, PlainJProfile}
import com.huawei.excelsior.jet.compiler.options.StrOption.{JProfile, JProfileDir}

object JProf {

  var manager: JProfManager = _

  def initJProf(): Unit = {
    val pgo = O2Env.env.enabled(PGO)

    try {
      val jprofDir = XString(O2Env.env.valueOfOrNull(JProfileDir))
      var jprofName = XString(O2Env.env.valueOfOrNull(JProfile))  // reconsider using an option

      if (jprofDir != null) {
        if (!O2Env.env.enabled(MultipleJProfs) && jprofName != null) {
          env.errors.fault(ErrMsg681)
        }
        if (jprofName == null) {
          jprofName = FS.makeFileName(jprofDir, env.config.equation("OutputName"), js.newJString("jprof"))
          env.config.setEquation2(s"$JProfile", jprofName)
        } else {
          assert(O2Env.env.enabled(MultipleJProfs))
        }
      }

      if (manager == null) {
        if (jprofName != null) {
          manager = new JProfManager

          if (!xfs.sys.exists(jprofName)) {
            env.errors.fault(ErrMsg680, jprofName)
          }

          assert(O2Env.env.enabled(PlainJProfile), "Encrypted profiles are not supported anymore")
          manager.init(jprofName)

          if (pgo && !manager.hasBlameProfile) {
            env.errors.fault(ErrMsg689, jprofName)
          }
        } else {
          if (pgo) {
            env.errors.fault(ErrMsg685)
          }
        }
      }

      env.config.setOption(BoolOption.GenerateMarkedRegions.name, !pgo)
    } catch {
      case e: OutOfMemoryError => throw e
      case e: Throwable =>
        env.errors.fault(ErrMsg686, XString.ascii(e.getMessage))
    }
  }
}
