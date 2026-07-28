/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.sync

private[xscala] class LockableJDK extends LockableVMDependent {
  override def newLock: LockJDK = new LockJDK(new Object())
}

class LockJDK private[xscala](val lock: AnyRef) extends Sync.Lock {
  override def sync[T](action: => T): T = lock.synchronized { action }
  override def await(): Unit = lock.wait()
  override def signalAll(): Unit = lock.notifyAll()
}