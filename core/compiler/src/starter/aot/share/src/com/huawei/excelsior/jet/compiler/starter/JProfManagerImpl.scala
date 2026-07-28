/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.starter

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.jprof.JProfManager
import com.huawei.excelsior.jet.compiler.jprof.JProfManager.ClassNameAndCLID
import com.huawei.excelsior.jet.compiler.o2lib.jprof.JProfManagerModule
import com.huawei.excelsior.jet.compiler.o2lib.jprof.JProfManagerModule.{BlameDataEntry, BlameDataMethodEntry}
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile
import com.huawei.excelsior.jet.jprof.JProfFormat

/** Implementation of JProfManager.
  *
  * @author xappymah
  * @author ijorch
  */
class JProfManagerImpl extends JProfManagerModule.JProfManager {
  override def getUSGDataEntries = {
    JProfManager.main.getUSGEntries.iterator.map { e =>
      new JProfManagerModule.USGDataEntry(XString(e.name), e.mask)
    }.toArray
  }

  override def init(jprofName: XString): Unit = JProfManager.initMain(new JProfManager(jprofName.toString))

  override def hasBlameProfile = JProfManager.main.getSectionsByType(JProfFormat.SectionType.BLAME_PROF).nonEmpty

  override def getClassesFromExecutionProfile =
    Profile.blame.allClasses.map(getBlameDataEntry).toArray

  override def getOptimizedClasses =
    Profile.blame.optimizedClasses.map(getBlameDataEntry).toArray

  private def getBlameDataEntry(e: ClassNameAndCLID): BlameDataEntry = {
    val clid = e.classLoaderSID
    new BlameDataEntry(XString(clid), XString(e.className))
  }
}
