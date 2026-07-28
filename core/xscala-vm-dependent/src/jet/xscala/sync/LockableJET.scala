/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.sync

import xscala.internal.*
import xscala.sync.LockableJET.*

import scala.annotation.static

private[xscala] final class LockableJET extends LockableVMDependent {
  override def newLock: LockJET = new LockJET(newLockable0)
}

private object LockableJET {
  @native @static private[sync] def newLockable0: ForeignRef0
  @native @static private[sync] def await0(ref: ForeignRef0): Unit
  @native @static private[sync] def signalAll0(ref: ForeignRef0): Unit
}

class LockJET private[xscala](val lock: ForeignRef0) extends Sync.Lock {
  override def sync[T](action: => T): T = lock.asInstanceOf[AnyRef].synchronized { action }
  override def await(): Unit = await0(lock)
  override def signalAll(): Unit = signalAll0(lock)
}
