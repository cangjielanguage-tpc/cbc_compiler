/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import scala.collection.mutable

/** Disjoint-set data structure.
  *
  * @author cypok
  * @author liontiger
  */
abstract class DisjointSet[A] extends Equiv[A] {

  protected type Elem <: DisjointSet.Element[Elem]

  protected def elem(x: A): Elem

  protected def value(x: Elem): A

  /** Returns iterator over all elements of all disjoint sets. */
  def iterator: Iterator[A]

  /** Determine which set a particular element `x` is in. */
  final def find(x: A): A = value(elem(x).find())

  /** Merge set that contains `x` into set that contains `y`. */
  final def union(x: A, y: A): Unit = elem(x).union(elem(y))

  /** Merge all sets that contain given elements. */
  final def unionAll(xs: IterableOnce[A]): Unit = {
    val it = xs.iterator
    if (it.hasNext) {
      val x = it.next()
      it foreach (union(_, x))
    }
  }

  /** Returns representatives of all equivalence classes of this set. */
  final def equivClasses: Iterator[A] = iterator.map(find).distinct

  /** Returns members of the same equivalence class as `x` (i.e. equivalent elements to `x` including `x` itself). */
  final def equivElements(x: A): Iterator[A] = iterator.filter(equiv(_, x))

  /** Returns true iff `x` and `y` belong to the same equivalence class. */
  final def equiv(x: A, y: A) = elem(x).find() == elem(y).find()
}

object DisjointSet {

  /** Disjoint-set of Int elements from `0` to `(size - 1)`. */
  class ofInt(size: Int) extends DisjointSet[Int] {
    protected type Elem = Element
    protected class Element(val num: Int) extends DisjointSet.Element[Element]

    private val elems = Array.tabulate[Elem](size)(new Elem(_))

    def iterator = (0 until size).iterator

    protected def elem(x: Int) = elems(x)
    protected def value(x: Elem) = x.num
  }

  /** Empty mutable disjoint-set data structure. */
  def empty[A]: DisjointSet[A] = new DisjointSet[A] {
    protected type Elem = Element
    protected class Element(val value: A) extends DisjointSet.Element[Element]

    private val elems = mutable.LinkedHashMap.empty[A, Elem]

    def iterator = elems.keysIterator

    protected def elem(x: A) = elems.getOrElseUpdate(x, new Elem(x))
    protected def value(x: Elem) = x.value
  }

  /** Builds mutable disjoint-set data structure from given elements using provided equivalence relation.
    *
    * Note: the resulting disjoint-set will always induce a proper equivalence relation
    *       (i.e. reflexive, symmetric and transitive one), even if the provided equivalence is not a proper one.
    */
  def from[A](_xs: IterableOnce[A])(implicit eq: Equiv[A]): DisjointSet[A] = {
    val set = empty[A]
    val xs = _xs.iterator.toSeq
    xs foreach set.find // ensure all elements are added
    for (Seq(x, y) <- xs combinations 2 if eq.equiv(x, y)) {
      set.union(x, y)
    }
    set
  }

  /** Disjoint-set element data structure. */
  trait Element[E <: Element[E]] { self: E =>

    /** Sets are implemented as trees: all elements of one tree are in one set and have one common root. */
    private[Element] var parent: E = this

    /** Determine which set this element is in. */
    def find(): E = {
      if (this != parent) {
        // Path compression: attach this node directly to root
        parent = parent.find()
      }
      parent
    }

    /** Merge set that contains this element into set that contains another element. */
    def union(another: E): Unit = {
      val thisRoot = this.find()
      val anotherRoot = another.find()

      if (thisRoot != anotherRoot) {
        // merge sets
        thisRoot.parent = anotherRoot
      }
    }
  }
}