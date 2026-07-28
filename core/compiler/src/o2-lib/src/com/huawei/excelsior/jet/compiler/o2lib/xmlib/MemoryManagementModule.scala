/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import xscala.management.Management

object MemoryManagementModule {
  def compactHeap(): Unit = {
    System.gc()
    System.runFinalization()
    System.gc()
  }

  def getTotalGCTime: Int = {
    // TODO: fully implement
    val totalGCTime: Long = Management.get.getTotalCollectionTime
    assert(totalGCTime <= Integer.MAX_VALUE)
    totalGCTime.toInt
  }

  def isGCThrashWarning = GCThreshWarning.check

  def printMem(): Unit = {
    // TODO: implement
  }

  def setGCThrashWarningHandled(): Unit = GCThreshWarning.threshWarningHandled()
}
