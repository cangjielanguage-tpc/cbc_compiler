/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import xscala.sync.Sync.{Lock, newLock}

object SynchronizedHashtableModule {
  def newHashtable: Hashtable = new Hashtable() {
      private val lock = newLock()
      override def values: Hashtable.Iterator = lock.sync { super.values }
      override def keys: Hashtable.Iterator = lock.sync { super.keys }
      override def clear(): Unit = lock.sync { super.clear() }
      override def remove(key: AnyRef): AnyRef = lock.sync { super.remove(key) }
      override def put(key: AnyRef, value: AnyRef): AnyRef = lock.sync { super.put(key, value) }
      override def get(key: AnyRef): AnyRef = lock.sync {  super.get(key) }
      override def containsKey(key: AnyRef): Boolean = lock.sync { super.containsKey(key) }
      override def contains(value: AnyRef): Boolean = lock.sync { super.contains(value) }
      override def size: Int = lock.sync { super.size }
    }
}
