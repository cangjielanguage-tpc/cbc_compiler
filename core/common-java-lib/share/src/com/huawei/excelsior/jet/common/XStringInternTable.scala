/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import MyPredef.*
import com.huawei.excelsior.jet.common.XString.*
import com.huawei.excelsior.jet.common.XStringInternTable.*
import com.huawei.excelsior.jet.common.XStringInternTable.XInternedString
import xscala.sync.Sync.{Lock, newLock}
import java.lang.ref.WeakReference

object XStringInternTable {
  /** String in the intern table.
    * A separate type is used for interned strings in order to avoid bloating of
    * ordinary [[XString]] objects with extra fields.
    */
  private class XInternedString (tableID: Int, chars: Array[Byte], offset: Int, count: Int, _hash: Int)
    extends XString(chars, offset, count, copyChars = false) {
    override protected[common] def internTableID: Int = tableID
    this.hash = _hash
  }

  private class Entry(str: XString, var nextEntry: Entry = null) extends WeakReference[XString](str)

  private lazy val lock = newLock()
  private val DEFAULT_CAPACITY = 1024
  private val DEFAULT_LOAD_FACTOR = 0.75f

  private var lastID = 0

  private def allocateID() = lock.sync {
    lastID += 1
    lastID
  }

  /** Copy of [[MathUtils.nextPowerOf2]]. Written to not use [[MyPredef.*]] in [[MathUtils]]. */
  private def nextPowerOf2(x: Int) = {
    val result = if (x <= 1) 1 else Integer.highestOneBit(x - 1) << 1
    assert(x > 0 && result >= x)
    result
  }
}

final class XStringInternTable(initialCapacity: Int = DEFAULT_CAPACITY, loadFactor: Float = DEFAULT_LOAD_FACTOR) {
  if (initialCapacity < 0) throw new IllegalArgumentException(s"Illegal initial capacity: $initialCapacity")
  if (loadFactor <= 0) throw new IllegalArgumentException(s"Illegal load factor: $loadFactor")

  private val id = allocateID()
  private var table: Array[Entry] = new Array[Entry](nextPowerOf2(initialCapacity))
  private var threshold = (table.length * loadFactor).toInt
  private var _size = 0
  private lazy val lock = newLock()

  def size: Int = _size
  def capacity = table.length

  /** Iterates `i`-th bucket entries and apply `action` to live ones (if action returns true, iteration stops).
    * Dead entries removed from bucket. If `killBucket` is true all bucket removed from table. */
  private def iterateBucket(i: Int, killBucket: Boolean)(action: (Entry, XString) => Boolean): XString = {
    var e = table(i)
    if (killBucket) table(i) = null

    var prev: Entry = null
    while (e != null) {
      val n = e.nextEntry
      val s = e.get()
      if (s == null) {
        if (!killBucket) {
          if (prev == null) { table(i) = n } else { prev.nextEntry = n }
        }
        e.nextEntry = null
        _size -= 1
      } else {
        prev = e
        if (action(e, s)) {
          return s
        }
      }
      e = n
    }
    null
  }

  /** cached interned empty string. */
  val internedEmptyString: XString = putWithoutCopy(new Array[Byte](0), 0, 0)

  /** Adds `str` into the table, if it is not added yet, and returns interned version of it. */
  def put(str: XString): XString = str.internTableID match {
    case `id` => str
    case 0 => put(str, 0, str.length)
  }

  /** Adds region of `str` into the table, if it is not added yet, and returns interned version of it. */
  def put(str: XString, startIndex: Int, endIndex: Int): XString = {
    if (startIndex < 0)        throw new StringIndexOutOfBoundsException(startIndex)
    if (endIndex > str.length) throw new StringIndexOutOfBoundsException(endIndex)
    if (startIndex > endIndex) throw new StringIndexOutOfBoundsException(endIndex - startIndex)

    val strID = str.internTableID
    val isInterned = strID != 0

    if (isInterned && (startIndex == 0) && (endIndex == str.length)) {
      guarantee(strID == id)
      return str
    }

    if (startIndex == endIndex) {
      internedEmptyString
    } else {
      val offset = unsafeGetOffset(str)
      val value = unsafeGetValue(str)
      val shouldReuseImmutableChars = isInterned || ((offset + startIndex == 0) && (endIndex >= value.length - 1))
      put0(value, offset + startIndex, endIndex - startIndex, shouldReuseImmutableChars)
    }
  }

  /** Adds string of `chars` into the intern table, if it is not added yet, and returns interned version of it. */
  def put(chars: Array[Byte], count: Int) = put0(chars, 0, count, false)

  /** Adds string with given characters into the intern table, if it is not added yet, and returns interned version of it.
    *
    * Characters are not copied.
    * Attention: this method is unsafe and should be used with care.
    */
  private def putWithoutCopy(chars: Array[Byte], offset: Int, count: Int) = put0(chars, offset, count, true)

  /** Adds string with given characters into the intern table, if it is not added yet, and returns interned version of it.
    *
    * `shouldReuseImmutableChars` indicates if character array is immutable and the string can be created without copying.
    * NOTE: it is unsafe and should be used with care.
    */
  private def put0(chars: Array[Byte], offset: Int, count: Int, shouldReuseImmutableChars: Boolean): XString = lock.sync {
    val hash = computeHashCode(chars, offset, count)
    val index = hash & (table.length - 1)

    val found = iterateBucket(index, killBucket = false) { (_, s) =>
      hash == s.hashCode() && s.contentEquals(chars, offset, count)
    }
    if (found != null) return found

    val s = if (shouldReuseImmutableChars) {
      new XInternedString(id, chars, offset, count, hash)
    } else {
      new XInternedString(id, chars.slice(offset, offset + count), 0, count, hash)
    }

    table(index) = new Entry(s, table(index))
    _size += 1

    if (_size >= threshold) cleanAndResize()
    s
  }

  /** Remove all dead entries from all buckets and resize table if its size is too big or too small. */
  def cleanAndResize(): Unit = lock.sync {
    // Remove all dead entries and update `size`
    for (i <- table.indices) iterateBucket(i, killBucket = false)((_, _) => false)

    val capacity = if (_size > threshold) {
      table.length * 2
    } else if (_size < threshold / 4) {
      table.length / 4
    } else {
      return
    }

    val newTable = new Array[Entry](capacity)
    for (i <- table.indices) {
      iterateBucket(i, killBucket = true) { (e, s) =>
        val newIndex = s.hashCode() & (newTable.length - 1)
        e.nextEntry = newTable(newIndex)
        newTable(newIndex) = e
        false
      }
    }

    table = newTable
    threshold = (capacity * loadFactor).toInt
  }
}
