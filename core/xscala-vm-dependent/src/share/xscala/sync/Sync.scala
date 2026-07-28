/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.sync

import annotation.static

/**
  * After introducing compact headers (JET-17150) it became necessary to move LockWord
  * from object layout for memory optimization. Now ScalaObj may be interpreted as AJObject,
  * not LockableAJObject, so there's need for having some abstraction for synchronization.
  */
object Sync {
  /**
    * This interface is used to encapsulate synchronization logic to VM specific types,
    * which are wrapped around by Lock and on which synchronization happens (see JET-17226).
    * <p>
    * Use `newLock()` to create a Lock object for specific VM as a new synchronization point.
    */
  abstract class Lock {
    def sync[T](action: => T): T
    def signalAll(): Unit
    def await(): Unit
  }

  /**
    * Returns new Lock object on which synchronization will happen.
    */
  def newLock(): Lock = LockableVMDependent.get.newLock
}

