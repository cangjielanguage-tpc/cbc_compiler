/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.util.ScalaCollections
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Dominators {
  private def calculateIDoms[N](graph: BiGraph[N], ts: TopSort[N]): collection.Map[N, N] = {
    val idomMap = mutable.HashMap.empty[N, N]
    val processed = mutable.HashSet.empty[N]

    def intersect(node1: N, node2: N): N = {
      var n1 = node1
      var n2 = node2
      while (n1 != n2) {
        if (ts.lt(n1, n2)) {
          n2 = idomMap(n2)
        } else if (ts.gt(n1, n2)) {
          n1 = idomMap(n1)
        }
      }
      n1
    }

    val root = graph.start
    idomMap(root) = root
    processed(root) = true

    var changed = true
    while (changed) {
      changed = false
      for (node <- ts.order if node != root) {
        val newIDom = graph.preds(node) filter processed reduce intersect

        if (!processed(node) || idomMap(node) != newIDom) {
          idomMap(node) = newIDom
          processed(node) = true
          changed = true
        }
      }
    }

    idomMap
  }

  final private class Tree[N](val node: N, private val parent: Tree[N]) {
    var child: Tree[N] = _
    var next = if (parent eq null) null else {
      val n = parent.child
      parent.child = this
      n
    }

    val depth: Int = if (parent eq null) 0 else parent.depth + 1

    def isRoot = parent eq null

    override def toString = if (parent eq null) {
      s"$node, no idom"
    } else {
      s"$node, idom: ${parent.node}"
    }

    def idom = parent

    /** Returns iterator over node's parents.
     *  Iterator starts from immediate parent of tree node.
     */
    def strictDoms: Iterator[Tree[N]] = parent.doms

    /** Returns iterator over node and its parents.
     *  Iterator starts from this tree node.
     */
    def doms: Iterator[Tree[N]] = new Iterator[Tree[N]] {
      private var curr = Tree.this
      def hasNext: Boolean = !curr.isRoot
      def next(): Tree[N] = {
        if (!hasNext) {
          Iterator.empty.next()
        } else {
          val res = curr
          curr = curr.parent
          res
        }
      }
    }

    /** ancestorsCache(i) == ancestor(pow(2, i+2)) */
    private var ancestorsCache: Array[Tree[N]] = _

    private def ancestorOfExp(exp: Int): Tree[N] = {
      assert(exp >= 0 && depth >= (1 << exp))
      exp match {
        case 0 => parent
        case 1 => parent.parent
        case _ =>
          if (ancestorsCache eq null) {
            def log2(x: Int) = 31 - java.lang.Integer.numberOfLeadingZeros(x)
            ancestorsCache = new Array[Tree[N]](log2(depth) - 1)
          }
          val idx = exp - 2
          var result = ancestorsCache(idx)
          if (result eq null) {
            result = ancestorOfExp(exp - 1).ancestorOfExp(exp - 1)
            ancestorsCache(idx) = result
          }
          result
      }
    }

    /** Return k-th ancestor of node through strictly domination */
    def ancestor(k: Int): Tree[N] = {
      assert(k > 0 && k <= depth)

      val THRESHOLD = 4
      var curr = this
      var n = k
      if (n <= THRESHOLD) {
        while (n > 0) {
          curr = curr.parent
          n -= 1
        }
      } else {
        var exp = 0
        while (n > 0) {
          if ((n & 1) != 0) {
            curr = curr.ancestorOfExp(exp)
          }
          n >>>= 1
          exp += 1
        }
      }
      curr
    }

    def untie(): Unit = {
      assert(child eq null, "untie of nonleaf node in dominators tree")
      var pChild = parent.child
      if (pChild == this) {
        parent.child = this.next
      } else {
        while (pChild.next != this) {
          pChild = pChild.next
        }
        pChild.next = this.next
      }

    }
  }
}

/** Calculates dominators of nodes.<br/>
 *
 *  Detailed description of algorithm can be found in following paper:
 *  <i>A Simple, Fast Dominance Algorithm. Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001)</i>.
 *
 *  @author cypok
 *  @author conwor
 *  @author paul
 */
final class Dominators[N](graph: BiGraph[N]) extends PartialOrdering[N] {
  import Dominators.*

  private val treeRoot = new Tree(graph.invalidNode, null)

  private val trees: mutable.Map[N, Tree[N]] = mutable.HashMap.empty[N, Tree[N]]

  /** Full recalculation of dominators tree. */
  {
    val ts = graph.topSort
    val idoms = calculateIDoms(graph, ts)
    for (node <- ts.order) {
      val idom = idoms(node)
      makeTreeNode(node, idom)
    }
  }

  /** Returns immediate dominator of `node`.
   *  @return graph.invalidNode if `node` has no immediate dominator.
   */
  def idom(node: N) = trees(node).idom.node

  /** Returns dominators of `node` including `node` itself. Iteration would be in reversed dominators tree order. */
  def doms(node: N): Iterator[N] = trees(node).doms map { _.node }

  /** Returns strict dominators of `node`.
   *  Iteration starts from immediate dominator of `node`.
   */
  def strictDoms(node: N): Iterator[N] = trees(node).strictDoms map { _.node }

  /** Returns true, iff x strictly dominates y.
   *  Note that x strictly dominates y if y is unreachable.
   */
  def strictlyDominates(x: N, y: N) = {
    // Note that dominators tree does not contain unreachable nodes.
    if (trees.contains(y)) {
      if (trees.contains(x)) {
        val tx = trees(x)
        val ty = trees(y)
        val k = ty.depth - tx.depth
        (k > 0) && (ty.ancestor(k) eq tx)
      } else {
        // Unreachable node do not dominate any other node.
        false
      }
    } else {
      // Any node dominates unreachable node.
      true
    }
  }

  /** Returns iterator by nodes, for which idom(_) == `node`. */
  def children(node: N): Iterator[N] = ScalaCollections.iterateUntilNull(trees(node).child)(_.next) map (_.node)

  /** Returns true, iff x dominates y. */
  def dominates(x: N, y: N) = (x == y) || strictlyDominates(x, y)

  /** Returns true, iff x is dominator of y. */
  def lteq(x: N, y: N) = dominates(x, y)

  /** Returns iterator over all nodes, dominated by given node `x` (including `x`) */
  // TODO: refactor me
  def dominatedBy(x: N): Iterator[N] = {
    val xs = new ListBuffer[N]
    def add(tree: Tree[N]): Unit = {
      if (tree != null) {
        xs.append(tree.node)
        add(tree.next)
        add(tree.child)
      }
    }
    xs.append(x)
    add(trees(x).child)
    xs.iterator
  }

  def tryCompare(x: N, y: N): Option[Int] = {
    if (x == y) Some(0)
    else if (strictlyDominates(x, y)) Some(-1)
    else if (strictlyDominates(y, x)) Some(1)
    else None
  }

  /** Returns an integer whose sign communicates how x compares to y.
   * @see scala.Ordering#compare
   *
   * Throws an error if x and y are not comparable.
   */
  def compare(x: N, y: N): Int = tryCompare(x, y) match {
    case Some(result) => result
    case None => shouldNotReachHere("Nodes are not comparable")
  }

  /** Returns minimum of two nodes. */
  def min(x: N, y: N): N = if (compare(x, y) <= 0) x else y

  /** Returns maximum of two nodes. */
  def max(x: N, y: N): N = if (compare(x, y) >= 0) x else y

  /** Returns depth of given node in dominators tree. */
  def depth(n: N): Int = trees(n).depth

  def maxDepth = trees.values.maxBy(_.depth).depth

  /**
   * Nearest dominator of two given nodes.
   * `nearest(a, b)` is a dominator of both `a` and `b` which is dominated by any other their dominator.
   */
  def nearest(a: N, b: N): N = {
    if (a == graph.invalidNode) b else {
      var p1 = trees(a)
      var p2 = trees(b)
      (p1.depth - p2.depth) match {
        case 0 => // do nothing
        case k if k > 0 => p1 = p1.ancestor(k)
        case k if k < 0 => p2 = p2.ancestor(-k)
      }
      while (p1 ne p2) {
        p1 = p1.idom;
        p2 = p2.idom
      }
      p1.node
    }
  }

  /**
   * Returns iterator by nodes, that are dominators of 'from' node and dominated by 'to' node.
   * Iteration would be in reversed dominators tree order.
   */
  def range(from: N, to: N): Iterator[N] = {
    assert(lteq(to, from))
    val toT = trees(to)
    val fromT = trees(from)
    fromT.doms take (fromT.depth - toT.depth + 1) map { _.node }
  }

  /**
   * Returns iterator by nodes, that are dominators of 'from' node and dominated by 'to' node.
   * Iteration would be in dominators tree order.
   *
   * TODO: cypok, optimize me!
   */
  def reversedRange(to: N, from: N): Iterator[N] = {
    range(from, to).toSeq.reverse.iterator
  }

  override def toString = trees.values mkString("; ")

  /** Fast update of dominators subtree of `node`.
   *
   *  If `newIdom` is equal to `graph.invalidNode` immediate dominator
   *  of node is calculated by its preds,
   *  otherwise immediate dominator is supposed to be `newIdom`.
   *
   *  @param strict allows conservative dominators update if set to false
   *  @return if dominators tree have been updated successfully
   */
  def tryUpdateOne(node: N, newIdom: N = graph.invalidNode, strict: Boolean = true) = {
    if (graph.succs(node).nonEmpty) { // if `node` has successors we cannot safely update dominators
      if (strict) false else {
        assert(newIdom == null, "force set of idom should be used only for nodes without succs")
        // check that current idom conservatively suits as new idom of `node`
        val preds = graph.preds(node) filter trees.contains
        if (preds.isEmpty) true else {
          dominates(idom(node), preds reduce nearest)
        }
      }

    } else { // if `node` has no successors we may easily update dominators
      for (tree <- trees.get(node)) {
        tree.untie()
        trees -= node
      }

      if (newIdom != graph.invalidNode) {
        makeTreeNode(node, newIdom)
      } else {
        // first filter preds that does not present in dominators tree,
        // such preds are unreachable
        val preds = graph.preds(node) filter trees.contains
        if (preds.nonEmpty) {
          makeTreeNode(node, preds reduce nearest)
        }
      }
      true
    }
  }

  private def makeTreeNode(node: N, idom: N): Unit = {
    val parent = if (idom == node) treeRoot else trees(idom)
    trees(node) = new Tree(node, parent)
  }

  def contains(node: N) = trees contains node
}
