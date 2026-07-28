/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.management

private[xscala] final class ManagementJDK extends Management {

  lazy val runtimeClass = Class.forName("java.lang.Runtime")
  lazy val managementFactoryClass = Class.forName("java.lang.management.ManagementFactory")
  lazy val operatingSystemMXBeanClass = Class.forName("com.sun.management.OperatingSystemMXBean")
  lazy val garbageCollectorMXBeanClass = Class.forName("java.lang.management.GarbageCollectorMXBean")

  def getTotalCores: Int = {
    val runtime = runtimeClass.getMethod("getRuntime").invoke(null)
    runtimeClass.getMethod("availableProcessors").invoke(runtime).asInstanceOf[Int]
  }

  def getTotalCollectionTime: Long = {
    var result: Long = 0
    val beans = managementFactoryClass.getMethod("getGarbageCollectorMXBeans").invoke(null).asInstanceOf[java.util.List[_]]
    val iter = beans.iterator()
    while (iter.hasNext) {
      val elem = iter.next()
      val collectionTime = garbageCollectorMXBeanClass.getMethod("getCollectionTime").invoke(elem).asInstanceOf[Long]
      result += collectionTime
    }
    result
  }

  def getTotalPhysicalMemorySize: Long = {
    val bean = managementFactoryClass.getMethod("getOperatingSystemMXBean").invoke(null)
    operatingSystemMXBeanClass.getMethod("getTotalPhysicalMemorySize").invoke(bean).asInstanceOf[Long]
  }

  def getSystemLoadAverage: Double = {
    val bean = managementFactoryClass.getMethod("getOperatingSystemMXBean").invoke(null)
    operatingSystemMXBeanClass.getMethod("getSystemLoadAverage").invoke(bean).asInstanceOf[Double]
  }

  def getSystemCpuLoad: Double = {
    val bean = managementFactoryClass.getMethod("getOperatingSystemMXBean").invoke(null)
    operatingSystemMXBeanClass.getMethod("getSystemCpuLoad").invoke(bean).asInstanceOf[Double]
  }

}
