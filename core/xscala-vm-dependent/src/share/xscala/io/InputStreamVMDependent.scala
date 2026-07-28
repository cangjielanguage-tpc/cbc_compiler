/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.vm.VMDependent

trait InputStreamVMDependent {
  def getStdin(): Object
  def readByte(stream: Object): Int
  def bytesAvailable(stream: Object): Int
  def close(stream: Object): Unit
}

object InputStreamVMDependent extends VMDependent[InputStreamVMDependent]
