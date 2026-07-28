/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph

import com.huawei.excelsior.jet.util.graph.ordering.DepthFirstSearch
import com.huawei.excelsior.jet.util.{DisjointSet, ScalaCollections}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

enum LoopKind {
  case SELF, REDUCIBLE, IRREDUCIBLE
}

/** Loop representation.
  *
  * @author cypok
  */
final class Loop[N](val kind: LoopKind, val header: N, val body: mutable.Set[N]) {
  private var _outer: Loop[N] = _
  def outer = _outer

  def outer_=(another: Loop[N]): Unit = {
    _outer = another
    _depth = another.depth + 1
  }

  private var _depth = 1
  def depth = _depth

  def isOutermost = (_outer == null)

  var exits: collection.Set[N] = _

  override def toString = s"$kind-loop, header <$header>, body <${body.mkString(", ")}>, depth <$depth>"

  /** Returns true, iff `this` loop is inner of given `that` loop (maybe with some loops between them). */
  def isInnerOf(that: Loop[N]): Boolean = {
    var curr = this.outer
    while ((curr != null) && (curr != that)) curr = curr.outer
    curr == that
  }

  /** Returns outermost loop, which is outer for `this` loop and inner for `that` loop.
    * If `this` loop is not inner for `that`, returns null. */
  def outermostInnerFor(that: Loop[N]): Loop[N] = {
    var curr = this
    while ((curr != null) && (curr.outer != that)) curr = curr.outer
    curr
  }
}

/** Loop structure of a graph.
  */
abstract class Loops[N] {
  /** All loops detected in the graph.
    * Loops are ordered according to the nesting, so the outer loop is before all its inner loops.
    */
  def seq: Seq[Loop[N]]

  def iterator = seq.iterator
  def foreach[U](f: Loop[N] => U): Unit = { seq foreach f }

  def isEmpty = seq.isEmpty
  def nonEmpty = !isEmpty

  /** Mapping from graph node to its loop.
    * Returns `null` if node is not in any loop.
    */
  def loopOf(n: N): Loop[N]

  def isInLoop(n: N) = loopOf(n) ne null

  def inSameLoop(x: N, y: N) = loopOf(x) == loopOf(y)

  /** Iterate loops containing node n starting from the innermost loop to the outermost one.
    */
  def allLoopsOf(n: N): Iterator[Loop[N]] = ScalaCollections.iterateUntilNull(loopOf(n))(_.outer)

  /** Returns the number of loops in which given node lies. */
  def depth(n: N): Int = Loops.depth(loopOf(n))

  /** Returns maximal loop depth in the graph. */
  def maxDepth: Int = if (isEmpty) 0 else seq.maxBy(_.depth).depth
}

object Loops {
  /** Recognizes loops in graph.
    *
    *  Detailed description of algorithm can be found in following paper:
    *  'Nesting of Reducible and Irreducible Loops by Havlak, Paul (1997)'.
    *
    *  @author cypok
    *  @author paul
    */
  private def recognizeLoops[N](graph: BiGraph[N], startNodes: Iterator[N]): Loops[N] = {
    val dfs = DepthFirstSearch(graph, startNodes)

    /** Object that corresponds to each node in graph.
      * It has extra fields and methods specific to loops recognition.
      */
    final class NodeSupport(val node: N) extends DisjointSet.Element[NodeSupport] {
      val nonBackPreds = new ArrayBuffer[NodeSupport]
      var backPreds: ArrayBuffer[NodeSupport] = _ // most of nodes have no backPreds; so buffer created lazily
      var header: NodeSupport = _
      var ownLoop: Loop[N] = _

      def loop = {
        if (ownLoop ne null) ownLoop // this node is header of a loop
        else if (header ne null) header.ownLoop ensuring (_ ne null) // this node belongs to loop of `header`
        else null // this node lays outside of any loops
      }

      /** Checks whether one node is ancestor of another in the DFS tree. */
      def isAncestorOf(that: NodeSupport) = dfs.isAncestor(this.node, that.node)

      /** Split all predecessors into backPreds and nonBackPreds. */
      def processPreds(preds: Iterator[NodeSupport]): Unit = {
        for (pred <- preds) {
          if (this isAncestorOf pred) {
            if (backPreds eq null) {
              backPreds = new ArrayBuffer[NodeSupport]
            }
            backPreds += pred
          } else {
            nonBackPreds += pred
          }
        }
      }

      /** Try to recognize loop with this node as header. */
      def recognizeHeader(): Unit = {
        if (backPreds ne null) { // there is no need to process nodes without backPreds
          val (body, kind) = collectLoopBody()
          ownLoop = new Loop(kind, node, body)
        }
      }

      /** Collect all nodes that form body of loop with this node as header. */
      def collectLoopBody(): (mutable.Set[N], LoopKind) = {
        // used to collect nodes that will form the loop's body, starting from backPreds
        val workList = new ArrayBuffer[NodeSupport]

        def addToBody(n: NodeSupport): Unit = {
          // don't add nodes which are already in the loop's body or the loop header itself
          if (n != this && n.header != this) {
            assert(n.header == null)
            workList += n
            n.header = this
            n.union(this)
            assert(dfs.number(this.node) < dfs.number(n.node))
          }
        }

        for (pred <- backPreds if pred != this) {
          addToBody(pred.find())
        }

        var kind = LoopKind.REDUCIBLE

        if (workList.isEmpty) {
          assert(backPreds.nonEmpty && backPreds.forall(_ == this))
          kind = LoopKind.SELF

        } else {
          // workList(i) is the next node to process
          var i = 0
          // process nodes from workList one by one also adding their nonBackPreds to workList
          while (i < workList.size) {
            for (pred0 <- workList(i).nonBackPreds) {
              val pred = pred0.find()
              if (pred == this) {
                // skip header or already processed node
              } else if (this isAncestorOf pred) {
                // add all nonBackPreds to body's workList except header itself
                addToBody(pred)
              } else {
                // we found node that is in loop's body but is not descendant of its header
                kind = LoopKind.IRREDUCIBLE

                // this is done to interpret whole irreducible loop as loop with only one header
                nonBackPreds += pred
              }
            }
            i += 1
          }
        }

        workList += this // add header to loop body
        (mutable.LinkedHashSet.from(workList.iterator map(_.node)), kind)
      }

      def updateOuterLoops(): Unit = {
        assert(ownLoop ne null)
        if (header ne null) {
          var outer = header.ownLoop ensuring (_ ne null)
          ownLoop.outer = outer
          // Note: loops are collected by looking into headers in DFS order (see collectAllLoops), and loop header
          // always precedes the loop body in DFS order. So, by the moment of processing inner loop, all its outer
          // loops are already processed.
          while (outer ne null) {
            assert(dfs.number(outer.header) < dfs.number(ownLoop.header))
            outer.body ++= ownLoop.body
            outer = outer.outer
          }
        }
      }
    }

    // prepare data structures
    val node2NS = Map.from(dfs.order.iterator.map { n => (n, new NodeSupport(n))})

    for (n <- dfs.order) {
      // there could be unreachable preds (node2NS does not contain them), simply ignore them
      node2NS(n).processPreds(graph.preds(n) flatMap node2NS.get)
    }

    // try to process all nodes as loop's header
    for (w <- dfs.order.reverseIterator) {
      node2NS(w).recognizeHeader()
    }

    // collect results: collectAllLoops
    val loopSeq = ListBuffer.empty[Loop[N]]
    for (n <- dfs.order.iterator map node2NS if n.ownLoop != null) {
      loopSeq += n.ownLoop
      n.updateOuterLoops()
    }
    val loopByNode = node2NS map { case (n, ns) => (n, ns.loop) }

    // collect results: calculateLoopExits
    for (loop <- loopSeq) {
      loop.exits = mutable.LinkedHashSet.from(loop.body filterNot (graph.succs(_) forall loop.body))
    }

    new Loops.Full(loopSeq.toList, loopByNode)
  }

  private class Full[N](val seq: Seq[Loop[N]], map: Map[N, Loop[N]]) extends Loops[N] {
    def loopOf(n: N) = map.getOrElse(n, null)
  }

  private class Empty[N] extends Loops[N] {
    def seq = Nil
    def loopOf(n: N) = null
  }
  private val _empty = new Loops.Empty[Any]()

  def empty[N]: Loops[N] = _empty.asInstanceOf[Loops[N]]

  def apply[N](graph: BiGraph[N], startNodes: Iterator[N]) = {
    recognizeLoops(graph, startNodes)
  }

  def apply[N](graph: BiGraph[N]) = {
    if (graph.hasBackwardEdges) {
      recognizeLoops(graph, Iterator.single(graph.start))
    } else {
      empty[N]
    }
  }

  def depth[N](loop: Loop[N]): Int =
    if (loop != null) loop.depth else 0

  @tailrec
  def addToBody[N](loop: Loop[N], n: N): Unit = {
    if (loop != null) {
      loop.body += n
      addToBody(loop.outer, n)
    }
  }
}
