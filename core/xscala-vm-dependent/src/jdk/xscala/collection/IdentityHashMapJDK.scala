/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.collection

final class IdentityHashMapImpl[K <: AnyRef, V] extends IdentityHashMap[K, V] {
  private val impl = new java.util.IdentityHashMap[K, V]

  override def get(key: K) = Option(impl.get(key))

  override def addOne(elem: (K, V)) = { impl.put(elem._1, elem._2); this }

  override def subtractOne(elem: K) = { impl.remove(elem); this }

  override def clear(): Unit = impl.clear()

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    private val iter = impl.entrySet().iterator()

    def hasNext: Boolean = iter.hasNext

    def next(): (K, V) = if (hasNext) {
      val nextEntry = iter.next()
      (nextEntry.getKey, nextEntry.getValue)
    } else Iterator.empty.next()
  }
}

private [xscala] class IdentityHashMapJDK extends IdentityHashMapVMDependent {
  override def empty[K <: AnyRef, V]: IdentityHashMap[K, V] = new IdentityHashMapImpl[K, V]
}
