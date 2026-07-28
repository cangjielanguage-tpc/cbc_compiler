/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.process

import xscala.vm.VMDependent

trait ProcessVMDependent {
  def start(command: Array[String]): Object
  def getOutputStream(process: Object): Object
  def getInputStream(process: Object): Object
  def getErrorStream(process: Object): Object
  def waitFor(process: Object): Int
}

object ProcessVMDependent extends VMDependent[ProcessVMDependent]
