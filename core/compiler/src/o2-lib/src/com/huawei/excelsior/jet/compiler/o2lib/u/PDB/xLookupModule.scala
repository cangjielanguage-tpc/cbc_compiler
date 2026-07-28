/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule as xfs
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS

object xLookupModule {

  def lookup(name: XString, lookInCurrentDir: Boolean): xfs.FileDescriptor = {
    // printf("xLooking for `%S`", name); 
    if (xPDB.manager != null) {
      val place = xPDB.findPlaceToReadFrom(xPDB.getNameByPlaceName(name), xPDB.getTypeByExt(name))
      if (place != null) {
        // printf(" success\n"); 
        return place.getFileDescriptor
      }
    }

    // printf(" failure\n"); 
    xfs.sys.lookup(name, lookInCurrentDir)
  }
}
