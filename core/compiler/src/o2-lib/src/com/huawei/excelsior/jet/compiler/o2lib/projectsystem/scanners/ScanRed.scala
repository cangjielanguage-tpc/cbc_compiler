/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners

import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.msg_syntax_error
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule as xfs

class ScanRed extends Scan {

  override def Do(): Boolean = {
    if (this.linebf.length > 0) {
      val ps = xfs.sys.parseRedAtLevel(this.getLinebfAsString, xfs.RED_LEVEL_REDFILE)
      if (ps >= 0) {
        wrongSyntax(this.in, this.lineno, ps, msg_syntax_error)
      }
    }
    false
  }

}
