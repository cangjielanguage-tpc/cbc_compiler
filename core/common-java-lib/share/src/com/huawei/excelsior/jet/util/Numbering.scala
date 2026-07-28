/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

/**
 * Total ordering which allow iteration over all the objects in direct or reverse order.
 * Every object has unique number in range [0..n) where `n` is overall number of objects.
 * 
 * @author paul
 */
trait Numbering[T] extends Ordering[T] { outer =>
//TODO: consider replacing `extends Ordering` by `extends PartialOrdering`

  /** All objects of this ordering in direct order. */
  def order: collection.IndexedSeq[T]

  /** Checks if an argument is contained in this numbering. */
  def contains(x: T): Boolean

  /** Unique number of the object. `order(number(x)) == x`. */
  def number(x: T): Int

  def compare(x: T, y: T) = {
    val nx = number(x)
    val ny = number(y)
    if (nx < ny) -1 else if (nx == ny) 0 else 1
  }

  override def reverse: Numbering[T] = new Numbering[T] {
    override def reverse = outer
    val order = outer.order.reverse
    def number(x: T) = order.length - outer.number(x)
    def contains(x: T) = outer.contains(x)
  }

  def foreach[U](f: T => U): Unit = { order foreach f }

  /** Given a function T => U, creates Numbering[U]. */
  def map[U](f: T => U): Numbering[U] = Numbering(order map f)
}

object Numbering {
  /** Return numbering based on given sequence of objects. */
  def apply[A](seq: IterableOnce[A]): Numbering[A] = apply(seq.iterator.toIndexedSeq)

  /** Return numbering based on given sequence of objects. */
  def apply[A](seq: collection.IndexedSeq[A]): Numbering[A] = new Numbering[A] {
    val order = seq
    private val numMap = Map.from[A, Int](order.zipWithIndex)
    def number(x: A) = numMap(x)
    def contains(x: A) = numMap.contains(x)
  }
}