/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.{Environment, JetDirs}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.targetArch
import xscala.io.{Files, Path}

object WritablePathsModule {

  def getProfileWritablePath(profileName: XString): XString = {
    val jetString = s"jet${Environment.JET_VERSION}-$targetArch"
    val path = JetDirs.userHome / "ProfilePDB" / jetString / s"$profileName"
    Files.makeDir(path)
    XString(path.absolutePath.toString)
  }
}
