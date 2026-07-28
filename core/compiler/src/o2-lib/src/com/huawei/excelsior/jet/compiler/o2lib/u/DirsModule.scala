/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule as xfs

object DirsModule {
  /** The system independent file separator character. */
  private val separator: Char = '/'

  /**
     Gets the name of the parent directory.
     Returns: the parent directory, or null if one is not found.
  */
  private def getParent(f: XString): XString = {
    val i = f.lastIndexOf(separator)
    if (i <= 0) {
      null
    } else {
      f.substring(0, i)
    }
  }

  /**
     Creates a directory and returns a boolean indicating the
     success of the creation.
  */
  private def mkdir(f: XString): Boolean = xfs.sys.createDir(f)

  /**
     Creates all directories in this path.  This method 
     returns true if all directories in this path are created.
  */
  def mkdirs(fPar: XString): Boolean = {
    var f = fPar

    if (xfs.sys.exists(f)) { // patch: ignore case and dir 
      return true
    }

    if (f.length > 0 && f.charAt(f.length - 1) == separator) {
      f = f.substring(0, f.length - 1)
    }

    val p = getParent(f)
    if (p != null) {
      mkdirs(p) && mkdir(f)
    } else {
      mkdir(f)
    }
  }

}
