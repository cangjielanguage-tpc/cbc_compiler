/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.management

private[xscala] final class ManagementJDK extends Management {

  // Direct calls instead of reflection: this file is already JDK-specific
  // (see directory name), and reflection creates registration burden for
  // AOT compilation of the compiler itself (e.g. GraalVM native-image).
  def getTotalCores: Int =
    Runtime.getRuntime.availableProcessors

  def getTotalCollectionTime: Long = {
    var result: Long = 0
    val beans = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans
    val iter = beans.iterator()
    while (iter.hasNext) {
      result += iter.next().getCollectionTime
    }
    result
  }

  def getTotalPhysicalMemorySize: Long =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean
      .asInstanceOf[com.sun.management.OperatingSystemMXBean].getTotalPhysicalMemorySize

  def getSystemLoadAverage: Double =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean
      .asInstanceOf[com.sun.management.OperatingSystemMXBean].getSystemLoadAverage

  def getSystemCpuLoad: Double =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean
      .asInstanceOf[com.sun.management.OperatingSystemMXBean].getSystemCpuLoad

}
