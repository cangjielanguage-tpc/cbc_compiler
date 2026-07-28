/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.management

import xscala.vm.VMDependent

protected trait Management {
  def getTotalCores: Int
  def getTotalCollectionTime: Long
  def getTotalPhysicalMemorySize: Long
  def getSystemLoadAverage: Double
  def getSystemCpuLoad: Double
}

object Management extends VMDependent[Management]
