/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.{xcMakeModule as mk, xcModesModule as xcModes, xiFilesModule as xfs, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS

object xcCompModule {

  def isCompilable(mPar: mk.File, reuse: Boolean = xcModes.workerMode): mk.CState = {
    val m = mPar

    if (m.mode != mk.md_jbc || reuse) {
      // no classes to be parsed
      return mk.none
    }

    if (pcNames.isLambdaClassName(m.name)) {
      return mk.compilable
    }

    if (reuse) {
      assert(m.host != null)
      mk.compilable
    } else {
      // no reuse
      if (mk.checkConflict(m.getProject, m.name, null)) {
        return mk.none
      }
      mk.compilable
    }
  }

}
