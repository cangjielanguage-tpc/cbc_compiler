/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.graph.ordering

import com.huawei.excelsior.jet.util.graph.*

import scala.collection.mutable

/**
  * Natural order of control-flow graph.
  * 1) Non-loop regions are top-sorted
  * 2) Every loop lies entirely in order
  * 3) All exits from loop lies in order after loop
  * 4) Loop oriented in order by given `loopOrientation`
  *
  * Used in backend layout and BGCM.
  *
  * @author paul
  */
object NaturalCFGOrder {

  enum LoopOrientation {
    case HEADER_FIRST     // the loop is oriented so that header placed the first in order
    case FALLTHROUGH_EXIT // the loop is oriented so that one of it's exit placed the last in order
  }

  def apply[N >: Null](graph: BiGraph[N], loopOrientation: LoopOrientation,
                                     processLoop: (Loop[N], collection.IndexedSeq[N]) => Unit = (_: Loop[N], _: collection.IndexedSeq[N]) => ()): collection.IndexedSeq[N] = {
    val loops = graph.loops
    val loopLayouts = mutable.Map.empty[Loop[N], collection.IndexedSeq[N]]

    class LoopCFG(loop: Loop[N]) extends ObjectGraph[N] {
      def headerOfInnerLoop(n: N, l: Loop[N]) = (l != null) && (l.outer == loop) && (l.header == n)

      def inThisLoop(n: N) = (loop == null) || loop.body(n)

      def delegate(n: N): N = loops.loopOf(n) match {
        case `loop` => n
        case inner => inner.outermostInnerFor(loop).header
      }

      def start: N = if (loop != null) loop.header else graph.start

      def succs(n: N): Iterator[N] = (loops.loopOf(n) match {
        case `loop` => graph.succs(n)
        case inner if headerOfInnerLoop(n, inner) => loopLayouts(inner).reverseIterator flatMap graph.succs
        case _ => Iterator.empty
      }) filter inThisLoop map delegate
    }

    // Returns layout of the loop, possibly rotated
    def rotatedLoop(loop: Loop[N], nextBlock: Option[N]): collection.IndexedSeq[N] = {
      val layout = loopLayouts(loop)

      loopOrientation match {
        case LoopOrientation.HEADER_FIRST =>
          assert(loop.header == layout.head)
          layout

        case LoopOrientation.FALLTHROUGH_EXIT =>
          // find pivot point for loop rotation
          val pivot = for {
          // if there is a block just after the loop...
            next <- nextBlock
            // ...and there is an exit from the loop to this block (take the last one of them)
            exit <- layout.reverseIterator find { x => graph.succs(x) contains next }
            idx = layout indexOf (exit ensuring loop.exits)
            //  ...and the exit is not last block in the layout of the loop
            if idx + 1 != layout.size
          } yield idx + 1 // then rotate the loop placing this exit last in the layout

          pivot match {
            case Some(idx) =>
              val (xs, ys) = layout splitAt idx
              ys ++ xs // rotate loop body
            case None => layout
          }
      }
    }

    def collectLoop(loop: Loop[N]): collection.IndexedSeq[N] = {
      val gloop = new LoopCFG(loop)
      val ts = gloop.topSort
      ts.order flatMap { b => loops.loopOf(b) match {
        case `loop` => List(b)
        case bl =>
          assert(gloop.headerOfInnerLoop(b, bl))
          val succ = ts.order.lift(ts.number(b) + 1) // immediate successor of loop `bl` or None
          val loopOrder = rotatedLoop(bl, succ)
          processLoop(bl, loopOrder)
          loopOrder
      }}
    }

    // Layout inner loops before outer, as inner loops are included into outer loops.
    for (loop <- loops.seq.reverseIterator) {
      loopLayouts(loop) = collectLoop(loop)
    }
    collectLoop(null) // make layout for whole `graph` including all the loops
  }

}
