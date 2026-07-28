/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.compiler.o2lib.u.Hashtable.{Entry, Iterator}
import com.huawei.excelsior.o2j.runtime.*

class Hashtable private(initialCapacity: Int, loadFactor: Float) {

  private var count = 0
  private var threshold = (initialCapacity.toFloat * loadFactor).toInt
  private var table = new Array[Entry](initialCapacity)
  private var modCount = 0

  assert(initialCapacity > 0 && loadFactor > 0.0f)

  def this() = this(11, 0.75f)
  def this(initialCapacity: Int) = this(initialCapacity, 0.75f)

  def keys: Iterator = new Iterator(this, true)
  def values: Iterator = new Iterator(this, false)

  def isEmpty = count == 0
  def size = count

  def clear(): Unit = {
    modCount += 1
    for (i <- table.indices) {
      table(i) = null
    }
    count = 0
  }

  private def getIndex(hash: Int) =
    O2JSupport.mod(hash & 0x7FFFFFFF, table.length)

  def remove(key: AnyRef): AnyRef = {
    if (key == null) return null
    var prev: Entry = null
    val hash = key.hashCode
    var e = table(getIndex(hash))
    while (e != null) {
      if (hash == e.hash && e.key == key) {
        modCount += 1
        if (prev != null) {
          prev.next = e.next
        } else {
          table(getIndex(hash)) = e.next
        }
        count -= 1
        return e.obj
      }
      prev = e
      e = e.next
    }
    null
  }

  def put(key: AnyRef, value: AnyRef): AnyRef = {
    if (key == null) return null
    val hash = key.hashCode
    var e = table(getIndex(hash))
    while (e != null) {
      if (hash == e.hash && e.key == key) {
        val old = e.obj
        e.obj = value
        return old
      }
      e = e.next
    }
    modCount += 1
    if (count == threshold) {
      rehash()
    }
    val index = getIndex(hash)
    e = new Entry(hash, key, value, table(index))
    table(index) = e
    count += 1
    null
  }

  def rehash(): Unit = {
    val old: Array[Entry] = table
    table = new Array[Entry](old.length * 2 + 1)
    modCount += 1
    threshold = (table.length.toFloat * loadFactor).toInt
    for (i <- old.indices) {
      var e = old(i)
      while (e != null) {
        val o = e
        e = e.next
        val hash = o.key.hashCode
        o.next = table(getIndex(hash))
        table(getIndex(hash)) = o
      }
    }
  }

  def get(key: AnyRef): AnyRef = {
    if (key == null) return null
    val hash = key.hashCode
    var e = table(getIndex(hash))
    while (e != null) {
      if (hash == e.hash && e.key == key) return e.obj
      e = e.next
    }
    null
  }

  def containsKey(key: AnyRef): Boolean = {
    if (key == null) return false
    val hash = key.hashCode
    var e = table(getIndex(hash))
    while (e != null) {
      if (hash == e.hash && e.key == key) return true
      e = e.next
    }
    false
  }

  def contains(value: AnyRef): Boolean = {
    if (value == null) return false
    for (i <- table.indices) {
      var e = table(i)
      while (e != null) {
        if (e.obj == value) return true
        e = e.next
      }
    }
    false
  }

}

object Hashtable {
  private class Entry(val hash: Int, val key: AnyRef, var obj: AnyRef, var next: Entry) {}

  class Iterator(val h: Hashtable, val keys: Boolean) extends scala.collection.Iterator[AnyRef] {
    private var e: Entry = null
    private var pos = 0
    private var lastReturned: Entry = null
    private var expectedModCount = h.modCount

    def remove(): Unit = {
      assert(lastReturned != null)
      assert(!isRotten)
      val removedObj = h.remove(lastReturned.key)
      assert(removedObj eq lastReturned.obj)
      expectedModCount += 1
      lastReturned = null
    }

    def isRotten = expectedModCount != h.modCount

    override def next(): AnyRef = {
      var en: Entry = null
      if (expectedModCount != h.modCount)
        throw new AssertionError("assert" + " #" + 911 + " at src/u/Hashtable.ob2:438")
      while (e == null && pos < h.table.length) {
        e = h.table(pos)
        pos += 1
      }
      if (e == null) {
        return null
      }
      en = e
      e = e.next

      lastReturned = en
      if (keys) en.key else en.obj
    }

    override def hasNext: Boolean = {
      if (expectedModCount != h.modCount) {
        throw new AssertionError("assert" + " #" + 911 + " at src/u/Hashtable.ob2:416")
      }
      if (e != null) return true
      while (pos < h.table.length) {
        e = h.table(pos)
        pos += 1
        if (e != null) return true
      }
      false
    }
  }

}
