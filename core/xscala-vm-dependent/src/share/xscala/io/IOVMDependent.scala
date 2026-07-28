/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.vm.VMDependent

trait IOVMDependent {
  def printStackTrace(ex: Throwable, out: TextOutput): Unit

  def createRandomAccessFile(path: String, readOnly: Boolean): RandomAccess
}

object IOVMDependent extends VMDependent[IOVMDependent]
