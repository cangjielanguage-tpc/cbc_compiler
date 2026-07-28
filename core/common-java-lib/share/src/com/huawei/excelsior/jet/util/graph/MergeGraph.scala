/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.jet.util.graph.MergeGraph.Node
import com.huawei.excelsior.jet.util.{DisjointSet, Worklist}
import xscala.util.MathUtils.nextPowerOf2

import scala.collection.mutable.ArrayBuffer

/** UGraph with nodes "merge" function.
  *
  * @author conwor
  * @author paul
  */
class MergeGraph[N >: Null <: Node[N]] extends UGraph[N] { self =>

  /** Returns delegate of given `node`. Delegate is a node, to which given is merged,
    * or `node` itself, if it was not merged to any other. */
  final def delegate(node: N): N = node.find()

  final def isGroopRoot(node: N) = delegate(node) eq node

  final def group(node: N): Iterator[N] = new Iterator[N] {
    val last = delegate(node)
    var curr = last.gnext

    def hasNext = curr ne null
    def next() = {
      assert(hasNext)
      val r = curr
      curr = if (r eq last) null else r.gnext
      r
    }
  }

  private val nstorage = new MergeGraph.NeighboursStorage[N]

  /** Returns delegates of all neighbours of given `node`'s group. */
  final def neighbours(node: N): Iterator[N] = {
    assert(isGroopRoot(node))
    new NeighboursIterator(node)
  }

  /** Returns true, iff given `x` and `y` nodes are adjacent. */
  final override def adjacent(x: N, y: N): Boolean = {
    assert(isGroopRoot(x) && isGroopRoot(y))
    if (x.nlength <= y.nlength) (neighbours(x) contains y) else (neighbours(y) contains x)
  }

  /** Create edge between nodes `x` and `y`. */
  final def connect(x: N, y: N): Unit = {
    assert(activeFocus eq null)
    assert(isGroopRoot(x) && isGroopRoot(y))
    nstorage.add(x, y)
    nstorage.add(y, x)
  }

  /** Create edge between nodes `x` and all nodes from `ys`. */
  final def connect(x: N, ys: IterableOnce[N]): Unit = {
    ys.iterator foreach (connect(x, _))
  }

  /** Merges given `node` to given `to`. All nodes from `node` group moved to `to` group. */
  final def merge(to: N, node: N): Unit = {
    assert(activeFocus eq null)
    assert(!adjacent(node, to))
    mergeImpl(to, node)
  }

  private def mergeImpl(to: N, node: N): Unit = {
    assert(isGroopRoot(to) && isGroopRoot(node))
    assert(to != node)

    nstorage.moveAll(node, to)

    node.union(to)

    val x = to.gnext
    to.gnext = node.gnext
    node.gnext = x
  }

  private def traverseNeighbours(n: N)(action: N => Boolean): Unit = {
    val it = new nstorage.RawIterator(n)
    while (!it.finished) {
      it.advance(action(it.current))
    }
  }

  private final val X_mask: Long = 0x1L // excluded node; adjacent to root or was merged to root
  private final val T_mask: Long = 0x2L // node is part of root's 2-neighbourhood
  private final val XT_mask: Long = (X_mask | T_mask)

  private var timestamp: Long = 0
  private var epoch: Long = 0

  private def tick() = {
    timestamp += (XT_mask + 1)
    assert(timestamp > 0)
    timestamp
  }

  private def stamp(n: N) = n.marks & ~XT_mask

  private def bits(n: N) = {
    val m = n.marks
    if (m >= epoch) m & XT_mask else 0
  }

  private def hasAnyOf(n: N, mask: Long) = (bits(n) & mask) != 0


  private var activeFocus: Focus = _

  final class Focus private[MergeGraph](val root: N) { thisFocus =>
    assert(isGroopRoot(root))

    private val n2buf = ArrayBuffer.empty[N] // 2-neighbourhood of `root`

    private def excluded(n: N) = hasAnyOf(n, X_mask)

    private def foreachNeighbour[U](n: N)(action: N => U): Unit = {
      val t = tick()
      traverseNeighbours(n) { x => if (t == stamp(x)) true else {
        x.marks = t | bits(x)
        action(x)
        false
      }}
    }

    private def updateNeighbours(x: N): Unit = {
      assert(!excluded(x)) // x is not adjacent to root nor was merged to root before

      val t = tick()
      x.marks = t | X_mask

      traverseNeighbours(x) { n =>
        if (excluded(n)) true else { n.marks = t | X_mask; false }
      }

      traverseNeighbours(x) { n =>
        foreachNeighbour(n) { n2 =>
          if (!hasAnyOf(n2, XT_mask)) {
            n2.marks |= T_mask
            n2buf += n2
          }
        }
        false
      }
    }

    updateNeighbours(root)

    /** Iterator over vertices located at a distance 2 from `root` without repetition.
      * Last iterated node can be safely merged to `root` by `Focus#merge()`.
      */
    def neighbours2: Iterator[N] = new Iterator[N] {
      var idx = 0

      def hasNext: Boolean = {
        assert(activeFocus eq thisFocus)
        while (idx < n2buf.size) {
          val b = bits(n2buf(idx))
          if (b == T_mask) return true
          assert(b == X_mask)
          idx += 1 // skip excluded nodes
        }
        false
      }

      def next() = { assert(hasNext); idx += 1; n2buf(idx - 1) }
    }

    
    /** For `n2` that belongs to 2-neighbourhood of `root` returns count of
      * separating vertices of `n2` and `root`, i.e. vertices which are neighbours of both `n2` and `root`.
      */
    def separatingCount(n2: N) = {
      assert(activeFocus eq thisFocus)
      assert(bits(n2) == T_mask) // `n2` belongs to 2-neighbourhood of `root`
      var count = 0
      foreachNeighbour(n2) { x => if (excluded(x)) count += 1 }
      count
    }

    /** Merges `x` node to `root` node. */
    def merge(x: N): Unit = {
      assert(activeFocus eq thisFocus)
      assert(bits(x) == T_mask) // `x` belongs to 2-neighbourhood of `root`
      updateNeighbours(x)
      mergeImpl(root, x)
    }
  }

  final def focused[R](root: N)(action: Focus => R): R = {
    assert(activeFocus eq null)
    epoch = tick()
    activeFocus = new Focus(root)
    try action(activeFocus) finally {
      activeFocus = null
      epoch = 0
    }
  }

  private final class NeighboursIterator(n: N) extends nstorage.RawIterator(n) with Iterator[N] {
    val t = tick()

    def hasNext = {
      if (t == timestamp) {
        while (!finished && t == stamp(current)) { advance(true) } // remove duplicates
      }
      !finished
    }

    def next() = {
      assert(hasNext)
      val x = current
      if (t == timestamp) { x.marks = t | bits(x) }
      advance(false)
      x
    }
  }

}

object MergeGraph {
  class Node[N <: Node[N]] extends DisjointSet.Element[N] { self: N =>
    private[MergeGraph] var gnext = this      // next member of the group
    private[MergeGraph] var nlast: Int = 0    // last neighbour in circular list
    private[MergeGraph] var nlength: Int = 0  // length of neighbours list
    private[MergeGraph] var marks: Long = 0
  }

  private final class NeighboursStorage[N >: Null <: Node[N]] {
    private[this] val initialCapacity: Int = 16

    private[this] var nrefs: Array[AnyRef] = new Array[AnyRef](initialCapacity)
    private[this] var nnext: Array[Int] = new Array[Int](initialCapacity)
    private[this] var nsize: Int = 1

    {
      nrefs(0) = null
      nnext(0) = 0
    }

    private def ensureCapacity(cap: Int): Unit = {
      if (cap > nrefs.length) {
        val len = nextPowerOf2(cap)
        nrefs = Array.copyOf(nrefs, len)
        nnext = Array.copyOf(nnext, len)
      }
    }

    /** Add `target` to the end of circular list of `host`'s neighbours. */
    def add(host: N, target: N): Unit = {
      val nlast = host.nlast
      val pos = nsize
      nsize += 1
      ensureCapacity(nsize)
      nrefs(pos) = target
      if (nlast == 0) {
        nnext(pos) = pos
      } else {
        nnext(pos) = nnext(nlast)
        nnext(nlast) = pos
      }
      host.nlast = pos
      host.nlength += 1
    }

    /** Move all 'from`'s neighbours to the end of neighbours list of `to`. */
    def moveAll(from: N, to: N): Unit = {
      val flast = from.nlast
      if (flast > 0) {
        val tlast = to.nlast
        if (tlast > 0) {
          val thead = nnext(tlast)
          val fhead = nnext(flast)
          nnext(flast) = thead
          nnext(tlast) = fhead
        }
        to.nlast = flast
        to.nlength += from.nlength
        from.nlast = 0
        from.nlength = 0
      }
    }

    class RawIterator(n: N) {
      private var prev = n.nlast

      final def finished = (prev == 0)

      final def current: N = {
        assert(!finished)
        nrefs(nnext(prev)).asInstanceOf[N].find()
      }

      final def advance(removeCurrent: Boolean): Unit = {
        assert(!finished)
        val c = nnext(prev)
        if (removeCurrent) {
          n.nlength -= 1
          nnext(prev) = nnext(c)
          if (c == n.nlast) {
            n.nlast = if (c == prev) 0 else prev
            prev = 0
          }
        } else {
          prev = if (c == n.nlast) 0 else c
        }
      }
    }
  }
}
