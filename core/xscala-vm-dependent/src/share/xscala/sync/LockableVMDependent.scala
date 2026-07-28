/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.sync

import xscala.vm.VMDependent
import Sync.Lock

private trait LockableVMDependent {
  def newLock: Lock
}

object LockableVMDependent extends VMDependent[LockableVMDependent]