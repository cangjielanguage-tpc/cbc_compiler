/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.opt.CompilerException
import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.Worklist

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * NodeReplaceOptimizer replace nodes with GVN & identity optimizations.
 * If node X replaced to node Y, then all nodes that have argument X should be
 * checked to be structurally equal with other nodes.
 *
 * @author paul
 * @author conwor
 */
trait NodeReplaceOptimizer { self: Universe =>

  class NodeReplacer {

    private val worklist = Worklist.empty[Node]
    private val replaced = Maps[Node].newMMap[AnyRef]

    private var entered = false

    def bulk(body: => Unit): Unit = {
      if (entered)
        body
      else {
        assert(worklist.isEmpty && replaced.isEmpty)
        entered = true
        try {
          body

          /** Tries to perform optimizations (value numbering, etc.) on usages of replaced nodes
            *  enclosing usage graph.
            */

          // Let some node X was replaced and it's use Y was added to the worklist by replace function.
          // Then node Y was replaced and decommitted.
          // In such situation we have decommitted node Y in the worklist
          // and it's clear that it should not be recommitted.

          for (x <- worklist.drain) {
            // Note: deserialization requires inCtrl on-commit optimization for some reason.
            // TODO: investigate and eliminate these crutches
            if (x.isCommitted && (currentScope.inDeserialization || !x.hasUndefinedArgs)) replace0(x, commit(x))
          }

        } finally {
          worklist.clear()
          replaced.clear()
          entered = false
        }
      }
    }

    /** Queries a replacement for (node, tag). Handles replacement chains. */
    private def transitiveReplacement(n0: Node, tag: Tag): Node = {
      var n = n0
      while (true) {
        replaced.get(n) match {
          case None =>
            return n
          case Some(r: Node) =>
            assert(n != r, s"transitive self-replacement of $n")
            n = r
          case Some(a: Array[Node]) =>
            assert(tag != null)
            val r = a(tag.id)
            if (r eq null) shouldNotReachHere(s"replacement for ($n, $tag) is undefined")
            n = r

          case x => shouldNotReachHere(s"bad value in replacement map: $x")
        }
      }
      shouldNotReachHere("")
    }

    /** Queries an all-tag replacement for node. Handles replacement chains. */
    private def transitiveReplacement(node: Node): Node = {
      transitiveReplacement(node, null)
    }

    def replace(src: Node, dst: Node): Unit =  {
      require(src.isCommitted)
      if (src != dst) {
        bulk {
          replace0(src, dst)
        }
      }
    }

    private def replace0(src: Node, dst: Node): Unit =  {
      assert(entered)
      if (src != dst) {
        val r = transitiveReplacement(dst)
        assert(src != r, s"self-replacement of $src")
        replaced(src) = r
        src.moveGroupInfoTo(r)
        replaceImpl(src, src hasTag Tag.XCONTROL)
        src.setReferent(r)
      }
    }

    /**
     * Decommit `node` and replace all its usages.
     * Replacement for each usage tagged by `tag` is equal to `replacementByTag(tag)`.
     *
     * @param node node, which usages are replaced
     * @param replacementByTag function that returns corresponding replacement for given tag
     */
    def replace(node: Node)(replacementByTag: PartialFunction[Tag, Node]): Unit = {
      bulk {
        val repl = createRArray(node, replacementByTag)
        replaceImpl(node, repl(Tag.XCONTROL.id) ne null)
      }
    }

    private def replaceImpl(node: Node, removeXPoint: Boolean): Unit = {
      require(node.isCommitted)

      if (removeXPoint) {
        val dst = transitiveReplacement(node, Tag.XCONTROL)
        // If we replace spinal node to dst (spinal node), we should remove dst's XPoint.
        (node, dst) match {
          case (node: SpinalNode, dst: SpinalNode) =>
            assert(node.hasXPoint == dst.hasXPoint)
            if (dst.hasXPoint) {
              assert(!dst.hasXHandler)
              dst.removeXPoint()
            }
          case _ => shouldNotReachHere()
        }
      }

      node.replaceUses { case e =>
        val dst = transitiveReplacement(node, e.sourceLabel)
        assert ((dst ne null) && (dst ne node))
        worklist += e.target
        dst
      }
      assert(node.uses.isEmpty)
      decommit(node)
    }

    private def createRArray(node: Node, replacementByTag: PartialFunction[Tag, Node]) = {
      val array: Array[Node] = Tag.VALUES map { tag =>
        if ((node hasTag tag) && (replacementByTag isDefinedAt tag)) {
          transitiveReplacement(replacementByTag(tag), tag)
        } else null
      }
      replaced(node) = array
      array
    }
  }

}
