/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.properties

import xscala.vm.VMDependent

private trait Properties {
  def userDir(): String
  def osName(): String
  def arch(): String
  def tmpDir(): String
  def jetExeDir(): String
  def userHome(): String
}

object Properties extends VMDependent[Properties]
