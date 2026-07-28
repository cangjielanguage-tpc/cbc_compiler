/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.vm.VMDependent

trait OutputStreamVMDependent {
  def getStdout(): Object
  def getStderr(): Object
  def writeByte(stream: Object, b: Int): Unit
  def close(stream: Object): Unit
  def flush(stream: Object): Unit
}

object OutputStreamVMDependent extends VMDependent[OutputStreamVMDependent]
