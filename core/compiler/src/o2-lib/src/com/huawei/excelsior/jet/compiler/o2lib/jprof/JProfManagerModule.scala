/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.jprof

import com.huawei.excelsior.jet.common.*

object JProfManagerModule {
  var impl: JProfManager = null

  class JProfManager {
    def getUSGDataEntries: Array[USGDataEntry] = impl.getUSGDataEntries
    def hasBlameProfile: Boolean = impl.hasBlameProfile
    def init(jprofName: XString): Unit = impl.init(jprofName)
    def getClassesFromExecutionProfile: Array[BlameDataEntry] = impl.getClassesFromExecutionProfile
    def getOptimizedClasses: Array[BlameDataEntry] = impl.getOptimizedClasses
  }

  class BlameDataEntry(val classLoaderSID: XString, val className: XString)
  class USGDataEntry(val name: XString, val mask: Int)
  class BlameDataMethodEntry(val clazz: BlameDataEntry, val name: XString, val sig: XString)
}
