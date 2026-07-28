/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.PackagerModule
import com.huawei.excelsior.jet.compiler.options.BoolOption.CleanTarget
import com.huawei.excelsior.jet.compiler.options.StrOption.TargetDir
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.xpackii.{Packager, ProgressLogger}
import xscala.io.TextOutput

object PackagerImpl extends PackagerModule.Packager {

  private enum AppType {
    case PLAIN, TOMCAT
  }

  private var appType: AppType = _
  private var executable: String = _
  private var jetJreHome: String = _
  private var outputFile: String = _
  private var compactProfile: String = _
  private var optRTFiles: String = _
  private var locales: String = _
  private var hasSplash = false
  private var tomcatBinDir: String = _
  private var extraFiles: String = _
  private var packedBundles: String = _
  private var toHideClassesBundles: String = _
  private var appHome: String = _
  private var targetDir: String = _
  private var cleanTarget = false

  override def pack(logOut: TextOutput, logErr: TextOutput): Unit = {
    val logger = new ProgressLogger(logOut, logErr)

    appType match {
      case AppType.TOMCAT =>
        Packager.packTomcat(executable, jetJreHome, outputFile, compactProfile, optRTFiles,
          locales, hasSplash, appHome, packedBundles, toHideClassesBundles, tomcatBinDir, logger)
      case AppType.PLAIN =>
        Packager.pack(executable, jetJreHome, outputFile, compactProfile, optRTFiles,
          locales, hasSplash, extraFiles, logger)
    }

    if (targetDir != null) {
      Packager.unzip(outputFile, targetDir, cleanTarget, logger)
    }
  }

  private def init(executable: XString, jetJreHome: XString, outputFile: XString, compactProfile: XString,
                   optRTFiles: XString, locales: XString, hasSplash: Boolean): Unit = {
    this.executable = executable.toString
    this.jetJreHome = if (jetJreHome == null) null else jetJreHome.toString
    this.outputFile = outputFile.toString
    this.compactProfile = compactProfile.toString
    this.optRTFiles = optRTFiles.toString
    this.locales = locales.toString
    this.hasSplash = hasSplash
    val env = LightweightEnvironment.getInstance
    targetDir = env.valueOfOrNull(TargetDir)
    cleanTarget = env.enabled(CleanTarget)
  }

  private def initCustomClassLoadersCommonParams(appHome: XString, packedBundles: XString, toHideClassesBundles: XString): Unit = {
    this.appHome = appHome.toString
    this.packedBundles = packedBundles.toString
    this.toHideClassesBundles = toHideClassesBundles.toString
  }

  override def initForTomcat(executable: XString, jetJreHome: XString, outputFile: XString, compactProfile: XString,
                             optRTFiles: XString, locales: XString, hasSplash: Boolean, appHome: XString,
                             packedBundles: XString, toHideClassesBundles: XString, binDir: XString): Unit = {
    appType = PackagerImpl.AppType.TOMCAT
    init(executable, jetJreHome, outputFile, compactProfile, optRTFiles, locales, hasSplash)
    initCustomClassLoadersCommonParams(appHome, packedBundles, toHideClassesBundles)
    tomcatBinDir = binDir.toString
  }

  override def init(executable: XString, jetJreHome: XString, outputFile: XString, compactProfile: XString,
                    optRTFiles: XString, locales: XString, hasSplash: Boolean, extraFiles: XString): Unit = {
    appType = PackagerImpl.AppType.PLAIN
    init(executable, jetJreHome, outputFile, compactProfile, optRTFiles, locales, hasSplash)
    this.extraFiles = extraFiles.toString
  }
}
