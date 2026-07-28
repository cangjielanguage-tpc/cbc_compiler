/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import java.util.ConcurrentModificationException

/**
  * Worklist is a sequence of elements that can be traversed in FIFO order.
  * New elements can be added to the worklist during traverse.
  * At any time no duplicate elements can present in the worklist.
  *
  * @author paul
  */
final class Worklist[A] extends mutable.Growable[A] with mutable.Shrinkable[A] {
  import Worklist.*

  private var sentinel = newSentinel
  private var entries = mutable.Map.empty[A, Entry]
  private var modCount = 0

  private def setContents(sentinel: Entry, entries: mutable.Map[A, Entry]): Unit = {
    this.sentinel = sentinel
    this.entries = entries
    modCount += 1
  }

  override def toString = iterator.mkString("Worklist{", ", ", "}")

  def size = entries.size
  def isEmpty = entries.isEmpty
  def nonEmpty = entries.nonEmpty
  def contains(x: A) = entries contains x

  def head = { assert(nonEmpty); sentinel.next.value[A] }
  def last = { assert(nonEmpty); sentinel.prev.value[A] }

  def succ(x: A): Option[A] = {
    val next = entries(x).next
    if (next != sentinel) Some(next.value[A]) else None
  }
  
  def pred(x: A): Option[A] = {
    val prev = entries(x).prev
    if (prev != sentinel) Some(prev.value[A]) else None
  }
  
  def clear(): Unit = {
    sentinel.next = sentinel
    sentinel.prev = sentinel
    entries.valuesIterator foreach (_.markAsDead())
    entries.clear()
    modCount += 1
  }

  /** O(1) contents swap of two worklists.
    * Active tracking iterators behave as if both worklists are cleared
    * and then filled with new contents.
    */
  def swap(that: Worklist[A]): Unit = {
    val (list, map) = (this.sentinel, this.entries)
    this.setContents(that.sentinel, that.entries)
    that.setContents(list, map)
  }


  /** Appends an element to the worklist.
    * @return `true` iff the element was not present in the worklist
    */
  def append(x: A): Boolean = {
    require(x.asInstanceOf[AnyRef] ne null)
    if (contains(x)) false else {
      entries(x) = newEntryBefore(x, sentinel)
      modCount += 1
      true
    }
  }

  /** Appends all the elements from `xs` to the worklist.
    * @return `true` if at least one element of `xs` was not present in the worklist
    */
  def appendAll(xs: IterableOnce[A]): Boolean = {
    val oldSize = size
    this ++= xs
    size != oldSize
  }

  def addOne(x: A): this.type = { append(x); this }

  /** Prepends an element to the worklist.
    * @return `true` iff the element was not present in the worklist
    */
  def prepend(x: A): Boolean = {
    require(x.asInstanceOf[AnyRef] ne null)
    if (contains(x)) false else {
      entries(x) = newEntryBefore(x, sentinel.next)
      modCount += 1
      true
    }
  }

  /** Prepends all the elements from `xs` to the worklist.
    * @return `true` if at least one element of `xs` was not present in the worklist
    */
  def prependAll(xs: IterableOnce[A]): Boolean = {
    val oldSize = size
    xs.iterator foreach prepend
    size != oldSize
  }

  /** Removes an element from the worklist.
    * @return `true` iff the element was present in the worklist
    */
  def remove(x: A): Boolean = entries.remove(x) match {
    case None => false
    case Some(e) =>
      e.next.prev = e.prev
      e.prev.next = e.next
      e.markAsDead()
      modCount += 1
      true
  }

  /** Removes all the elements from `xs` from the worklist.
    * @return `true` if at least one element of `xs` was present in the worklist
    */
  def removeAll(xs: IterableOnce[A]): Boolean = {
    val oldSize = size
    xs.iterator foreach remove
    size != oldSize
  }

  def subtractOne(x: A): this.type = { remove(x); this }


  /** Removes `src` from worklist and puts `dst` on it's place. */
  def replace(src: A, dst: A): Unit = {
    require( (src.asInstanceOf[AnyRef] ne null)
          && (dst.asInstanceOf[AnyRef] ne null) )
    assert(!contains(dst))
    val Some(e) = entries.remove(src)
    e.rawValue = dst
    entries(dst) = e
    modCount += 1
  }

  /** Moves given `element` to point before given `before`. */
  def moveBefore(before: A, element: A): Unit = {
    remove(element) ensuring { _ == true }
    insertBefore(before, element)
  }

  private def insert(before: Entry, element: A): Unit = {
    require(element.asInstanceOf[AnyRef] ne null)
    assert(!contains(element))
    entries(element) = newEntryBefore(element, before)
    modCount += 1
  }

  /** Inserts given `element` to point before given `before`. */
  def insertBefore(before: A, element: A): Unit = insert(entries(before), element)

  /** Inserts given `element` to point after given `after`. */
  def insertAfter(after: A, element: A): Unit = insert(entries(after).next, element)

  def iterator = checkedIterator()

  def reverseIterator = checkedIterator(reverseOrder = true)

  def foreach[U](action: A => U): Unit = iterator foreach action

  def find(p: A => Boolean): Option[A] = iterator find p

  /** Non-destructive tracking iterator over this worklist.
    * Worklist may be safely mutated during iteration.
    * See also [[Worklist.swap]].
    */
  def track = trackingIterator(sentinel)

  /** Traverse this worklist with accumulation of results.
    * Useful for building transitive closures.
    * Example of use: `for (x <- wl.accumulate) { wl ++= f(x) }`.
    */
  def accumulate = track

  /** Non-destructive tracking iterator over this worklist, started from element, immediately after given `x`.
    * Worklist may be safely mutated during iteration.
    */
  def trackAfter(x: A) = trackingIterator(entries(x))

  /** Non-destructive tracking iterator over this worklist, started from given `x`.
   * Worklist may be safely mutated during iteration.
   */
  def trackFrom(x: A) = trackingIterator(entries(x).prev)

  /** Destructive iterator over this worklist.
    * Each element produced by the iterator removed from the worklist before being processed.
    * Worklist may be safely mutated during iteration.
    */
  val drain = new Iterator[A] {
    def hasNext = entries.nonEmpty
    def next() = if (hasNext) { val h = head; remove(h); h } else Iterator.empty.next()
  }

  /** `x.drainTo(y)` is equivalent to `y ++= x.drain` but may work more efficiently */
  def drainTo(dst: Worklist[A]): Unit = {
    if (dst.isEmpty) this.swap(dst) else dst ++= this.drain
  }

  /** Returns current elements of the worklist. */
  def snapshot: collection.Seq[A] = {
    val buf = new ArrayBuffer[A](entries.size)
    buf ++= iterator
    buf
  }

  /** Checked iterator over this worklist.
    * Throws an exception if worklist was mutated during iteration.
    */
  private def checkedIterator(reverseOrder: Boolean = false): Iterator[A] = new Iterator[A] {
    var prev = sentinel
    val version = modCount

    def check(): Unit = { if (version != modCount) throw new ConcurrentModificationException() }
    def curr: Entry = if (reverseOrder) prev.prev else prev.next

    def hasNext = { check(); curr ne sentinel }
    def next() = if (hasNext) { prev = curr; prev.value[A] } else Iterator.empty.next()
  }

  /** Non-destructive iterator over this worklist.
    * Worklist may be safely mutated during iteration.
    * See also `Worklist#swap`.
    */
  private def trackingIterator(startAfter: Entry): Iterator[A] = new Iterator[A] {
    var myList = sentinel
    var prev = startAfter

    @tailrec def curr: Entry = if (!prev.isDead) prev.next else { prev = prev.prev; curr }
    def checkSwap(): Unit = { if (myList ne sentinel) { myList = sentinel; prev = myList } }

    def hasNext = { checkSwap(); curr ne myList }
    def next() = if (hasNext) { prev = curr; prev.value[A] } else Iterator.empty.next()
  }
}

object Worklist {
  def empty[A] = new Worklist[A]
  def apply[A](xs: A*) = from(xs)
  def from[A](xs: IterableOnce[A]) = { val r = empty[A]; r ++= xs; r }

  private final class Entry(private[Worklist] var rawValue: Any, var prev: Entry, var next: Entry) {
    def isDead = rawValue.asInstanceOf[AnyRef] eq null
    def markAsDead(): Unit = { rawValue = null }
    def value[A] = { assert(!isDead); rawValue.asInstanceOf[A] }
  }

  private final val sentinelValue = new Object

  private def newSentinel = {
    val e = new Entry(sentinelValue, null, null)
    e.prev = e
    e.next = e
    e
  }

  private def newEntryBefore[A](value: A, point: Entry) = {
    val e = new Entry(value, point.prev, point)
    e.next.prev = e
    e.prev.next = e
    e
  }
}
