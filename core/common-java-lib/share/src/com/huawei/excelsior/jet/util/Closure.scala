/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import xscala.util.MathUtils.nextPowerOf2

import scala.collection.mutable

/**
 * Processes elements of set from given `from` elements. Each element is visited not more than once.
 * Process of each element executed in three steps:
 * <ol>
 * <li>calling `preAction`</li>
 * <li>processing all elements, returned from `succs`</li>
 * <li>calling `postAction`</li>
 * </ol>
 *
 * @author cypok
 * @author conwor
 * @author paul
 */
object Closure {

  def apply[A](from: A*)(succs: A => IterableOnce[A]): collection.Set[A] = {
    apply(from)(succs)
  }

  def apply[A](from: IterableOnce[A])(succs: A => IterableOnce[A]): mutable.LinkedHashSet[A] = {
    val set = mutable.LinkedHashSet.empty[A]
    collect(set, from)(succs)
    set
  }

  def collect[A](set: mutable.Set[A], from: IterableOnce[A])(succs: A => IterableOnce[A]): Unit = {
    withActions(set, from)(succs)(_ => ())(_ => ())
  }

  def withPreAction[A](set: mutable.Set[A], from: IterableOnce[A])(succs: A => IterableOnce[A])(preAction: A => Unit): Unit = {
    withActions(set, from)(succs)(preAction)(_ => ())
  }

  def withPostAction[A](set: mutable.Set[A], from: IterableOnce[A])(succs: A => IterableOnce[A])(postAction: A => Unit): Unit = {
    withActions(set, from)(succs)(_ => ())(postAction)
  }

  def withActions[A](set: mutable.Set[A], from: IterableOnce[A])(succs: A => IterableOnce[A])(preAction: A => Unit)(postAction: A => Unit): Unit = {
    new Worker(set, from.iterator, succs, preAction, postAction).run()
  }

  private class Worker[A](val set: mutable.Set[A],
                          val from: Iterator[A],
                          val succs: A => IterableOnce[A],
                          val preAction: A => Unit,
                          val postAction: A => Unit) {

    // NOTE: it is more natural to implement set processing in a recursive way.
    // However, Closure can be used to traverse large sets (for example, to calculate topsort of all nodes),
    // and recursive implementation uses too much stack and causes StackOverflowError failures.

    final val MAX_RECURSION_DEPTH = 256

    var recursionDepth = 0
    var bufferDepth = 0
    var buffer: Array[AnyRef] = _

    def ensureCapacity(cap: Int): Unit = {
      val oldLen = if (buffer eq null) 0 else buffer.length
      if (cap > oldLen) {
        val newLen = nextPowerOf2(cap)
        if (buffer == null) {
          buffer = new Array(newLen)
        } else {
          buffer = Array.copyOf(buffer, newLen)
        }
      }
    }

    def update(idx: Int, elem: AnyRef, it: AnyRef): Unit = {
      assert(buffer.length > idx*2 + 1)
      buffer(idx*2) = elem
      buffer(idx*2 + 1) = it
    }

    def evacuateOne(idx: Int, elem: A, successors: Iterator[A]): Int = {
      update(idx, elem.asInstanceOf[AnyRef], successors)
      idx
    }

    def bufferedElem(idx: Int) = buffer(idx*2).asInstanceOf[A]
    def bufferedSuccs(idx: Int) = buffer(idx*2 + 1).asInstanceOf[Iterator[A]]

    /** Returns `-1` on common case: recursive processing can be continued normally.
      * Returns `i >= 0` when recursion goes too far and current state was evacuated to i-th element of the buffer.
      */
    def process(x: A): Int = {
      if (!set(x)) {
        preAction(x)
        set += x
        val it = succs(x).iterator

        recursionDepth += 1
        if (recursionDepth >= MAX_RECURSION_DEPTH) {
          bufferDepth += recursionDepth
          recursionDepth = 0
          ensureCapacity(bufferDepth * 2)
          return evacuateOne(bufferDepth - 1, x, it)
        }

        while (it.hasNext) {
          val idx = process(it.next())
          if (idx >= 0) {
            assert(idx != 0)
            return evacuateOne(idx - 1, x, it)
          }
        }
        postAction(x)
        recursionDepth -= 1
      }
      return -1
    }

    def run(): Unit = {
      while (from.hasNext) {
        val root = from.next()
        process(root) ensuring { idx => (idx < 0) == (bufferDepth == 0) }

        while (bufferDepth > 0) {
          val it = bufferedSuccs(bufferDepth - 1)
          if (it.hasNext) {
            process(it.next())
          } else {
            postAction(bufferedElem(bufferDepth - 1))
            bufferDepth -= 1
            update(bufferDepth, null, null)
          }
        }
      }
      assert(bufferDepth == 0 && recursionDepth == 0)
    }
  }

}
