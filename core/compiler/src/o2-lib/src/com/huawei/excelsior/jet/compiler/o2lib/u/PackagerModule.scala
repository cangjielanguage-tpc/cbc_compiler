/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.XString
import xscala.io.TextOutput

import java.io.IOException

object PackagerModule {
  class Packager {
    @throws[IOException]
    def pack(logOut: TextOutput, logErr: TextOutput): Unit =
      impl.pack(logOut, logErr)

    def initForTomcat(executable: XString, jetJreHome: XString, outputFile: XString, compactProfile: XString, optRTFiles: XString, locales: XString, hasSplash: Boolean, appDir: XString, packedBundles: XString, toHideClassesBundles: XString, binDir: XString): Unit =
      impl.initForTomcat(executable, jetJreHome, outputFile, compactProfile, optRTFiles, locales, hasSplash, appDir, packedBundles, toHideClassesBundles, binDir)

    def init(executable: XString, jetJreHome: XString, outputFile: XString, compactProfile: XString, optRTFiles: XString, locales: XString, hasSplash: Boolean, extraFiles: XString): Unit =
      impl.init(executable, jetJreHome, outputFile, compactProfile, optRTFiles, locales, hasSplash, extraFiles)
  }

  private var impl: PackagerModule.Packager = null

  def setImpl(impl: PackagerModule.Packager): Unit = PackagerModule.impl = impl
}
