/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.u.JStringsModule
import com.huawei.excelsior.jet.compiler.xminizip.Minizip

class ZipEntry(val zentry: Minizip.ZipEntry) {
  def getSize: Int = zentry.size

  def getTime: Int = zentry.time.toInt

  /** Returns XString in UTF-8 encoding (as entries in zip are encoded in UTF-8) */
  def getName: XString = JStringsModule.newJString(zentry.name)

  def isDirectory: Boolean = zentry.isDir
}

