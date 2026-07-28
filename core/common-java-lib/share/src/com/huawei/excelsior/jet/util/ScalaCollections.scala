/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere

import scala.collection.{AbstractIterator, mutable}
import scala.collection.mutable.ArrayBuffer

/**
 * Additional standard operations with Scala collections.
 *
 * @author cypok
 * @author conwor
 */
object ScalaCollections { //TODO: replace by implicit wrapper class

  /** Returns first element from the collection wrapped into `Option`.
    */
  def firstElement[A](xs: IterableOnce[A]): Option[A] = {
    val it = xs.iterator
    if (it.hasNext) Some(it.next()) else None
  }

  /** Returns last element from the collection wrapped into `Option`.
    */
  def lastElement[A](xs: IterableOnce[A]): Option[A] = {
    val it = xs.iterator
    if (it.isEmpty) None else {
      while (true) {
        val elem = it.next()
        if (!it.hasNext) {
          return Some(elem)
        }
      }
      shouldNotReachHere()
    }
  }

  /** If `xs` has elements and all of them are equal return head element
    * wrapped into `Option`, otherwise return `None`.
    */
  def uniqueValue[A](xs: IterableOnce[A]): Option[A] = {
    val it = xs.iterator
    if (it.isEmpty) None else {
      val elem = it.next()
      while (it.hasNext) {
        if (it.next() != elem) return None
      }
      Some(elem)
    }
  }

  /** If `xs` contains exactly one element return it
    * wrapped into `Option`, otherwise return `None`.
    */
  def singleton[A](xs: IterableOnce[A]): Option[A] = {
    val it = xs.iterator
    if (it.isEmpty) None else {
      val elem = it.next()
      if (it.nonEmpty) None else Some(elem)
    }
  }

  /** Returns an element from the collection containing exactly one element.
    * @throws IllegalArgumentException if `xs` is empty or contains more than one element
    */
  def singleElement[A](xs: IterableOnce[A]): A = {
    val it = xs.iterator
    if (it.isEmpty) {
      throw new IllegalArgumentException("#singleElement was called for empty collection")
    }
    val elem = it.next()
    if (it.nonEmpty) {
      throw new IllegalArgumentException("#singleElement was called for collection with many elements")
    }
    elem
  }

  /** Returns true, iff all elements of given `xs` have the same `property`. */
  def haveSame[T1, T2](xs: IterableOnce[T1])(property: T1 => T2): Boolean = {
    val it = xs.iterator
    if (it.isEmpty) true else {
      val value = property(it.next())
      while (it.hasNext) {
        if (property(it.next()) != value) return false
      }
      true
    }
  }

  /** Returns the sum of `xs` mapped by given `f`. */
  def sumBy[A, B](xs: IterableOnce[A])(f: A => B)(implicit num: Numeric[B]): B = {
    val it = xs.iterator
    it.foldLeft(num.zero) { (v, x) => num.plus(v, f(x)) }
  }

  /** Stable alternative to [[scala.collection.IterableOps.groupMap()]]. */
  def groupMap[T, K, V](xs: IterableOnce[T])(key: T => K)(value: T => V): mutable.SeqMap[K, Seq[V]] = {
    // The key feature is to use LinkedHashMap instead of HashMap.
    val resBuilder = mutable.LinkedHashMap.empty[K, ArrayBuffer[V]]
    for (x <- xs.iterator) {
      resBuilder.getOrElseUpdate(key(x), ArrayBuffer.empty[V]) += value(x)
    }
    val res = mutable.LinkedHashMap.empty[K, Seq[V]]
    for ((k, buf) <- resBuilder) {
      res += (k -> buf.toSeq)
    }
    res
  }

  /** Stable alternative to [[scala.collection.IterableOps.groupBy()]]. */
  def groupBy[A, B](xs: IterableOnce[A])(f: A => B): mutable.SeqMap[B, Seq[A]] =
    groupMap(xs)(f)(x => x)

  /** Convert collection of pairs into map grouped by first element of pair. */
  def toMultiMap[A, B](xs: IterableOnce[(A, B)]): mutable.SeqMap[A, Seq[B]] =
    groupMap(xs)(_._1)(_._2)

  /** Stable alternative to [[scala.collection.IterableOps.groupMapReduce()]]. */
  def groupMapReduce[T, K, B](xs: IterableOnce[T])(key: T => K)(f: T => B)(reduce: (B, B) => B): mutable.SeqMap[K, B] = {
    val m = mutable.LinkedHashMap.empty[K, B]
    for (elem <- xs.iterator) {
      val k = key(elem)
      val v =
        m.get(k) match {
          case Some(b) => reduce(b, f(elem))
          case None => f(elem)
        }
      m.put(k, v)
    }
    m
  }

  /** Splits given list into sublists
    * where all consequent elements of each sublist satisfy given predicate.
    *
    * Example:
    * {{{
    *   aggregate(List(1,2,3,1,2,3))(_ < _) == List(List(1,2,3),List(1,2,3))
    * }}}
    *
    * Implementation is roughly equivalent the following code in Haskell:
    *
    * {{{
    *   aggregate :: (a -> a -> Bool) -> [a] -> [[a]]
    *   aggregate p [] = []
    *   aggregate p xs = first : aggregate p rest
    *    where
    *     (first, rest) = aggregateFirst p xs
    * }}}
    */
  def aggregate[T](xs: Seq[T])(p: (T, T) => Boolean): Seq[Seq[T]] = xs match {
    case Seq() => Seq.empty
    case xs  =>
      val (first, rest) = aggregateFirst(xs)(p)
      first +: aggregate(rest)(p)
  }

  /** Splits given list into prefix and suffix pair
    * where all consequent elements of prefix satisfy given predicate.
    *
    * Example:
    * {{{
    *   aggregateFirst(List(1,2,3,1,2,3))(_ < _) == (List(1,2,3),List(1,2,3))
    * }}}
    *
    * Implementation is roughly equivalent the following code in Haskell:
    *
    * {{{
    *   aggregateFirst :: (a -> a -> Bool) -> [a] -> ([a], [a])
    *   aggregateFirst _ [] = ([], [])
    *   aggregateFirst _ [x] = ([x], [])
    *   aggregateFirst p (x : y : xs)
    *     | p x y = (x : next, rest)
    *     | otherwise = ([x], y : xs)
    *    where
    *     (next, rest) = aggregateFirst p (y : xs)
    * }}}
    */
  def aggregateFirst[T](xs: Seq[T])(p: (T, T) => Boolean): (Seq[T], Seq[T]) = xs match {
    case Seq()          => (Seq.empty, Seq.empty)
    case Seq(x)         => (Seq(x), Seq.empty)
    case Seq(x, y, xs*) =>
      if (p(x, y)) {
        val (next, rest) = aggregateFirst(y +: xs)(p)
        (x +: next, rest)
      } else {
        (Seq(x), y +: xs)
      }
  }

  /** Collect values if all are defined.
    * (It's a special case of Haskell's function
    * `sequence :: Iterable t, Monad m => t (m a) -> m (t a)`.)
    */
  def sequence[A](xs: Seq[Option[A]]): Option[Seq[A]] =
    if (xs exists (_.isEmpty)) None else Some(xs.flatten)

  /** Creates iterator that repeatedly applies given function to the previous result
    * until the function returns `null`. Same as `Iterator.iterate(start)(step) takeWhile (_ != null)`.
    */
  def iterateUntilNull[T](start: T)(step: T => T): Iterator[T] = new Iterator[T] {
    private var curr: T = start
    def hasNext: Boolean = curr != null
    def next(): T = if (hasNext) { val r = curr; curr = step(r); r } else Iterator.empty.next()
  }

  /** Creates iterator that repeatedly applies given function until it returns `null`.
    * Same as `iterateUntilNull(peek())(_ => peek())`.
    */
  def peekUntilNull[T](peek: () => T): Iterator[T] = iterateUntilNull(peek())(_ => peek())

  /** Creates iterator that repeatedly applies given function to the previous result
    * until the function returns `None`. Same as `Iterator.iterate(start)(_ flatMap step) takeWhile (_.isDefined) map (_.get)`.
    */
  def iterateUntilNone[T](start: Option[T])(step: T => Option[T]): Iterator[T] = new Iterator[T] {
    private var curr: Option[T] = start

    def hasNext: Boolean = curr.isDefined

    def next(): T = curr match {
      case Some(r) =>
        curr = step(r)
        r
      case _ => Iterator.empty.next()
    }
  }

  /** Inserts the given element into the given iterable collection at the given index.
    */
  def insertAt[T](xs: IterableOnce[T], idx: Int, elem: T): Iterator[T] = {
    val (prefix, suffix) = xs.iterator.splitAt(idx)
    prefix ++ Iterator.single(elem) ++ suffix
  }

  /** Removes the element at the given index from the given iterable collection.
    */
  def removeAt[T](xs: IterableOnce[T], idx: Int): Iterator[T] = {
    xs.iterator.patch(idx, Iterator.empty, 1)
  }

  /** Returns maximal elements in given `xs` according to given partial ordering `ord`.
    * Note that the order and any duplicate elements are preserved.
    */
  def maximalElements[T](xs: IterableOnce[T])(implicit ord: PartialOrdering[T]): collection.Seq[T] = {
    val buf = ArrayBuffer.empty[T]
    for (x <- xs.iterator if !(buf exists (ord.lt(x, _)))) {
      buf.filterInPlace(!ord.lt(_, x))
      buf += x
    }
    buf
  }

  /** Returns minimal elements in given `xs` according to given partial ordering `ord`.
    * Note that the order and any duplicate elements are preserved.
    */
  def minimalElements[T](xs: IterableOnce[T])(implicit ord: PartialOrdering[T]): collection.Seq[T] = {
    maximalElements(xs)(ord.reverse)
  }

  /** Creates partial ordering induced by given `_lteq` relation. */
  def partialOrderingBy[T](_lteq: (T, T) => Boolean): PartialOrdering[T] = new PartialOrdering[T] {
    override def tryCompare(x: T, y: T) = {
      if (equiv(x, y)) Some(0)
      else if (lt(x, y)) Some(-1)
      else if (lt(y, x)) Some(1)
      else None
    }

    override def lteq(x: T, y: T) = _lteq(x, y)
  }

  /** Returns ordered map with pairs (K, f(K)) for each K from `ks`. */
  def mapWith[K, V](ks: IterableOnce[K])(f: K => V): collection.SeqMap[K, V] = {
    val map = mutable.LinkedHashMap.empty[K, V]
    for (k <- ks.iterator) map(k) = f(k)
    map
  }

  /** Collects duplicate values from `xs` (i.e. returns all except the first occurrence of each identical element). */
  def collectDuplicates[T](xs: IterableOnce[T]): Iterator[T] = collectDuplicatesBy(xs)(x => x)

  /** Collects duplicate values from `xs` mapped by given `f` (i.e. returns all except the first occurrence of each identical element). */
  def collectDuplicatesBy[A, B](xs: IterableOnce[A])(f: A => B): Iterator[A] = {
    val unique = mutable.HashSet.empty[B]
    for (x <- xs.iterator if !unique.add(f(x))) yield x
  }

  /** Returns iterator over results of `f` applied to pair elements from `xs` and `ys` without pair objects allocation. */
  def zipMap[A, B, C](xs: IterableOnce[A], ys: IterableOnce[B])(f: (A, B) => C): Iterator[C] = new AbstractIterator[C] {
    val xsIter = xs.iterator
    val ysIter = ys.iterator

    override def knownSize = xsIter.knownSize min ysIter.knownSize
    def hasNext = xsIter.hasNext && ysIter.hasNext
    def next() = f(xsIter.next(), ysIter.next())
  }

  trait OrderedEnum[Self <: scala.reflect.Enum] extends Ordered[Self] { self: Self =>
    def compare(that: Self): Int = this.ordinal compare that.ordinal
  }
}
