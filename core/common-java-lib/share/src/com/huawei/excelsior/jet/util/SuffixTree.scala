/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import scala.collection.mutable

/** SuffixTree is a tree of elements.
  * Sequences of elements could be added to the tree.
  * Suffixes of sequences are merged if they are equal.
  *
  * Usually, all sequences are appended to the root (created by empty constructor of [[SuffixTree]]).
  *
  * Example: if we append the following sequences to the empty tree:
  *
  *  - 1, 2, 3
  *  - 4, 2, 3
  *  - 2, 3, 5
  *
  * the tree will look like:
  *
  * {{{
  *   1  4    2
  *   \ /    /
  *    2    3
  *    |    |
  *    3    5
  *    |   /
  *    root
  * }}}
  *
  * @author conwor
  * @author cypok
  */
object SuffixTree {
  def newRoot[T >: Null]() = new SuffixTree[T](null, null)
}

final class SuffixTree[T](
  /** It is equal to `null` for the root of a tree. */
  val elem: T,
  val parent: SuffixTree[T]
) { self =>
  private val children = mutable.LinkedHashMap.empty[T, SuffixTree[T]]

  def isRoot = parent == null

  /** Returns suffix tree containing `elem`. */
  def prepend(elem: T): SuffixTree[T] = {
    if (elem == null) {
      throw new NullPointerException
    }

    children.getOrElse(elem, {
      val next = new SuffixTree(elem, this)
      children += (elem -> next)
      next
    })
  }

  /** Returns suffix tree containing the first element of `elems` sequence. */
  def prepend(elems: collection.IndexedSeq[T]): SuffixTree[T] = {
    var current = this
    for (elem <- elems.reverseIterator) {
      current = current.prepend(elem)      
    }
    current
  }

  /** Removes all subtrees whose element is not accepted by `filter` predicate. */
  def retainAll(filter: T => Boolean): Unit =
    children filterInPlace ((k, _) => filter(k)) foreach { case (_, v) => v.retainAll(filter) }

  /** Iterator from current tree element to it's root. */
  def toRoot: Iterator[T] = new Iterator[T]() {
    private var curr = self
    override def hasNext: Boolean = !curr.isRoot
    override def next(): T = {
      if (!hasNext) {
        throw new NoSuchElementException
      }
      val res = curr.elem
      curr = curr.parent
      res
    }
  }

  def getRoot = {
    var current = this
    while (!current.isRoot) {
      current = current.parent
    }
    current
  }

  def getChildren: Iterable[SuffixTree[T]] = children.values

  def toRootToString = toRoot.mkString("", " -> ", " -> root")
}
