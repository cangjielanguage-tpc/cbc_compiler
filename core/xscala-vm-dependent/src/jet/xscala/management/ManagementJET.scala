/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.management

import xscala.management.ManagementJET.*

import scala.annotation.static

private[xscala] final class ManagementJET extends Management {

  def getTotalCores: Int = getTotalCores0

  def getTotalCollectionTime: Long = getTotalCollectionTime0

  def getTotalPhysicalMemorySize: Long = getTotalPhysicalMemorySize0

  def getSystemLoadAverage: Double = {
    val loadavg = new Array[Double](1)
    if (getLoadAverage0(loadavg, 1) == 1) {
      loadavg(0)
    } else {
      -1.0
    }
  }

  def getSystemCpuLoad: Double = getSystemCpuLoad0

}

private object ManagementJET {
  @native @static private def getTotalCores0: Int
  @native @static private def getTotalCollectionTime0: Long
  @native @static private def getTotalPhysicalMemorySize0: Long
  @native @static private def getLoadAverage0(loadavg: Array[Double], nelems: Int): Int
  @native @static private def getSystemCpuLoad0: Double
}
