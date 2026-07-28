/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import xscala.util.UByte

object CPEntryModes {
  private val err_bad_classloader_type = ErrMsg510

  type CPEntryMode = UByte

  val cpe_app: CPEntryMode = UByte(0) // cp entry for app classloader
  // that comes from !classpathentry directive.
  val cpe_appclassloader: CPEntryMode = UByte(1) // cpe entry for app classloader
  // that comes from !classloaderentry directive.
  // We need two modes to distinguesh directives
  // while project reading:
  // for classpathentry path is relative to project location
  // for app classloaderentry path is relative to appdir
  val cpe_equinox: CPEntryMode = UByte(2) // cp entry for Equinox OSGi bundle
  val cpe_tomcat: CPEntryMode = UByte(3)  // cp entry for Tomcat classloaders
  // (common, catalina, shared)
  val cpe_webapp: CPEntryMode = UByte(4)  // cp entry for Tomcat's WebappClassloader
  val cpe_idea: CPEntryMode = UByte(5)
  val cpe_ideaplugin: CPEntryMode = UByte(6)
  val cpe_springboot: CPEntryMode = UByte(7)
  val cpe_error: CPEntryMode = UByte(9)

  def toCpeMode(cpeModeStringPar: XString): CPEntryMode = {
    var cpeModeString = cpeModeStringPar

    cpeModeString = cpeModeString.toUpperCase
    if (cpeModeString.equals2("EQUINOX")) {
      cpe_equinox
    } else if (cpeModeString.equals2("APP")) {
      cpe_appclassloader
    } else if (cpeModeString.equals2("TOMCAT")) {
      cpe_tomcat
    } else if (cpeModeString.equals2("WEBAPP")) {
      cpe_webapp
    } else if (cpeModeString.equals2("IDEA")) {
      cpe_idea
    } else if (cpeModeString.equals2("IDEAPLUGIN")) {
      cpe_ideaplugin
    } else if (cpeModeString.equals2("SPRINGBOOT")) {
      cpe_springboot
    } else {
      env.errors.envError(err_bad_classloader_type, cpeModeString)
      cpe_error
    }
  }

}
