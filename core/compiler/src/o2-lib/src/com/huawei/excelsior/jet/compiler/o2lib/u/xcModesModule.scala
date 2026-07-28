/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule as env
import com.huawei.excelsior.o2s.runtime.*
import xscala.util.StringOps.*
import xscala.util.UByte

object xcModesModule {
  type Jobs = UByte
  val nothing: Jobs = UByte(0)
  val pro: Jobs = UByte(1)
  val make: Jobs = UByte(2)

  var job: Jobs = _
  var help: Boolean = _
  var tomcat: Boolean = _ /*tomcat delta mode*/
  var idea: Boolean = _ /*idea experemental mode */
  var clean: Boolean = _
  def workerMode: Boolean = env.config.equation("worker") != null

  def isModeSpecifier(s: XString): Boolean = {
    s.nonEmpty && s.charAt(0) == '='
  }

  def setMode(s: String): Unit = {
    def compare(a: String, b: String, n: Int): Boolean =
      (b.asciiToUpperCase startsWith a.asciiToUpperCase) && (a.length > n)

    def setJob(j: Jobs): Unit = {
      if (job == nothing) {
        job = j
      } else {
        env.errors.fault(ErrMsg416)
      }
    }

    if (compare(s, "=PROJECT", 1)) {
      setJob(pro)
    } else if (compare(s, "=MAKE", 1)) {
      setJob(make)
    } else if (compare(s, "=TOMCAT", 6)) {
      tomcat = true
    } else if (compare(s, "=IDEA", 4)) {
      idea = true
    } else if (compare(s, "=ALL", 1)) {
      // nothing to do
    } else if (compare(s, "=HELP", 1)) {
      help = true
    } else if (compare(s, "=CLEAN", 2)) {
      clean = true
    } else {
      env.errors.fault(ErrMsg415, s)
    }
  }

  def init(): Unit = {
    job = nothing

    help = false

    if (!workerMode) {
      // if main PDB is opened, then discard it
      xPDB.resetMainPDB()
    }
  }
}
