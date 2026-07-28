/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.xmlib

import xscala.management.Management
import xscala.time.unixMilliseconds

/** This class allows one to check so called <i>GC thrashing</i>, that is, substantial increase of GC ratio of
  * the checked period time.
  *
  * @author vitvit
  */
object GCThreshWarning {
  private val THRESHABLE_GC_RATIO_PERCENT = 50
  private var lastCheckedTime = unixMilliseconds
  private var first = true

  private def getLastCheckedPeriodTime: Long = {
    val currentTime = unixMilliseconds
    val res = currentTime - lastCheckedTime
    lastCheckedTime = currentTime
    res
  }

  private var lastTotalGCTime: Long = 0

  private def getTotalGCTimeForLastCheckedPeriod: Long = {
    val totalGCTime: Long = Management.get.getTotalCollectionTime
    val res = totalGCTime - lastTotalGCTime
    lastTotalGCTime = totalGCTime
    res
  }

  /** @return if GC takes more than {@link # THRESHABLE_GC_RATIO_PERCENT} in the time period since last call to this method
    *         (or since startup for the first call to it).
    */
  def check: Boolean = {
    val gcTime = getTotalGCTimeForLastCheckedPeriod
    if (gcTime == 0) {
      return false //skip periods with no gc
    }
    val periodTime = getLastCheckedPeriodTime
    if (first) {
      first = false
      false
    } else {
      gcTime > (periodTime * THRESHABLE_GC_RATIO_PERCENT / 100)
    }
  }

  /** We handled thresh warning. To not take into account last GC to provoke another thresh warning,
    * just advance our computations.
    */
  def threshWarningHandled(): Unit = {
    System.gc()
    System.runFinalization()
    System.gc()
    check
  }
}
