/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.util

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.util.ScalaCollections.singleElement

import scala.annotation.nowarn
import scala.collection.mutable.ArrayBuffer

/** Sieve implements tree of predicates and sort functions, used to divide collection of elements by
  * `leaves`, which are sorted from leftmost to rightmost leaf (leftmost is the `best`).
  *
  * During sorting filters applied as lazy as possible. For example, if you want to select 1 best element from
  * collection, and the head is the best, Sieve will not apply any filters to tail.
  *
  * @author conwor
  */
trait SieveUtil {

  class Sieve[T](filter: T => Int, succs: Seq[Sieve[T]]) {
    override def clone(): Sieve[T] = new Sieve[T](filter, succs map {_.clone()})
    def *:[K] (f: K => T): Sieve[K] = new Sieve[K]({ k => filter(f(k)) }, succs map { s => f *: s })

    protected val queue = new ArrayBuffer[T]()
    private var qi = 0
    private var si = 0


    ///////////////////////////////////////////////////////////////////////////
    // One-element operations
    // TODO: refactor CodeOrdering to massive operations, then hide one-element operations in private or remove them

    /** Put `x` to tree. Returns Some(`x`) if `x` is the best element (was sifted to leftmost leaf). Otherwise returns None.
      * If `x` is the best element, it will not be saved in tree. Otherwise it will be saved and can be received later. */
    def sift(x: T): Option[T] = {
      val index = filter(x)
      if (index == 0) succs.head.sift(x) else { succs(index).queue += x; None }
    }

    /** Returns best available element (saved in tree after sieve operations) and removes it from tree. */
    def get(): Option[T] = {
      var result: Option[T] = None
      while (result.isEmpty && qi < queue.size) { result = sift(queue(qi)); qi += 1 }
      while (result.isEmpty && si < succs.size) { result = succs(si).get(); if (result.isEmpty) si += 1 }
      result
    }

    /** Returns all equally-best available elements (saved in tree after sieve operations) and removes them from tree. */
    def getAllBest(): Seq[T] = {
      var result = Seq.empty[T]
      while (qi < queue.size) { result ++= sift(queue(qi)); qi += 1 }
      while (result.isEmpty && si < succs.size) { result = succs(si).getAllBest(); if (result.isEmpty) si += 1 }
      result
    }

    /** Clear tree, removes all saved elements. */
    def clear(): Unit = { queue.clear(); qi = 0; si = 0; succs foreach { _.clear() } }


    ///////////////////////////////////////////////////////////////////////////
    // Massive operations, remove elements from tree before return

    /** Returns best `n` elements from `from`. Clears tree after using */
    def selectFrom(from: IterableOnce[T], n: Int): Seq[T] = {
      val buf = ArrayBuffer.empty[T]
      val it = from.iterator
      while (buf.size < n && it.hasNext) buf ++= sift(it.next())
      while (buf.size < n) buf ++= (get() ensuring { _.nonEmpty })
      clear()
      buf.toSeq
    }

    /** Returns all equally-best elements from `from`. Clears tree after using */
    def allBestFrom(from: IterableOnce[T]) = {
      val buf = ArrayBuffer.empty[T]
      from.iterator foreach { x => buf ++= sift(x) }
      if (buf.isEmpty) buf ++= getAllBest()
      clear()
      buf.toSeq
    }

    def printTree(level: Int = 0): Unit = {
      println()
      for (i <- 0.until(level)) print("---")
      print("qi = " + qi + "; si = " + si + "; remaining queue: ")
      for (x <- queue.takeRight(queue.size - qi)) {
        print(x)
      }
      println()
      for (s <- succs) {
        print("|")
        s.printTree(level + 1)
      }
    }
  }

  private class Leaf[T] extends Sieve[T]({ _ => shouldNotCallThis() }, Seq.empty) {
    override def clone(): Sieve[T] = new Leaf[T]
    override def *:[K] (f: K => T): Sieve[K] = new Leaf[K]
    override def sift(x: T): Option[T] = Some(x)
  }


  ///////////////////////////////////////////////////////////////////////////
  // Sieve build DSL implementation

  object Sieve {

    abstract class Elem(val arity: Int)

    private class LeafElem[T](val sieve: Sieve[T]) extends Elem(0)
    private class NodeElem[T](val f: T => Int, val l: Int) extends Elem(l)
    case class DupElem[T](s: Sieve[T], count: Int) extends Elem(0)

    class Builder[T]() {
      private val levels = ArrayBuffer.empty[ArrayBuffer[Elem]]
      private def newLevel: Builder[T] = { levels += ArrayBuffer.empty; this }

      def | (f: T => Int, l: Int):  Builder[T] = this | new NodeElem[T](f, l)
      def | (f: T => Boolean):      Builder[T] = this | ({ x => if (f(x)) 0 else 1 }, 2)
      def | (s: Sieve[T]):          Builder[T] = this | new LeafElem[T](s.clone())

      def || (e: Elem):             Builder[T] = newLevel | e
      def || (f: T => Int, l: Int): Builder[T] = newLevel | (f, l)
      def || (f: T => Boolean):     Builder[T] = newLevel | f
      def || (s: Sieve[T]):         Builder[T] = newLevel | s

      def | (e: Elem): Builder[T] = {
        e match {
          case `leaf`         => levels.last += new LeafElem(new Leaf[T])
          case DupElem(s, c)  => levels.last ++= Seq.fill(c)(new LeafElem(s.clone()))
          case _              => levels.last += e
        }
        this
      }

      private def buildLevel(level: Int, reqSize: Int): Seq[Sieve[T]] = {
        if (level == levels.size) return Seq.fill(reqSize)(new Leaf[T])
        var next = buildLevel(level + 1, levels(level).map(_.arity).sum)

        (levels(level) map {
          case le: LeafElem[T] => le.sieve
          case ne: NodeElem[T] => val succs = next.take(ne.arity); next = next.drop(ne.arity); new Sieve[T](ne.f, succs)
        }).toSeq: @nowarn("msg=cannot be checked at runtime")
      }

      private [Sieve] def build: Sieve[T] = singleElement(buildLevel(0, 1))
    }

    def root[T](f: T => Int, l: Int): Builder[T] = new Builder[T] || (f, l)
    def root[T](f: T => Boolean): Builder[T] = new Builder[T] || f

    val leaf = new Elem(0) {}

    def dup[T](s: Sieve[T], count: Int) = new DupElem[T](s, count)

    def apply[T](builder: Builder[T]): Sieve[T] = builder.build
  }
}
