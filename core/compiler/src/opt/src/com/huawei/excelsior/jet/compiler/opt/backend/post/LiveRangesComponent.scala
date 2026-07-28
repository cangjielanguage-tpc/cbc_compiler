/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.ScalaCollections.haveSame
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.Numbering
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

/**
  * Live ranges in post-processing (on IR with already allocated resources and fixed nodes order).
  *
  * @author conwor
  */
trait LiveRangesComponent { self: Universe =>

  /** Returns number of given `node` in backendCodeOrder */
  private def number(node: Node): Int =
    LiveRanges.backendCodeOrder.number(node.groupRoot)

  /** Returns true, iff given `x` dominates given `y`. */
  private def dominates(x: Node, y: Node): Boolean = {
    if (x == y) return true

    val xBlock = x.block
    val yBlock = y.block

    if (xBlock == yBlock) {
      number(x) < number(y)
    } else {
      val point = if (x == xBlock) xBlock else lowerPoint(x)
      point dominates yBlock
    }
  }

  private def strictDominates(x: Node, y: Node): Boolean =
    x != y && dominates(x, y)

  /** Return node from code order, where real use of given `edge` placed. */
  private def codeOrderUse(edge: Edge): Node = {
    val useNode = edge.target match {
      case _: Phi => edge.usePoint
      case x => x.groupRoot
    }
    Projection.skip(useNode match {
      case cs: Constraints => cs.owner match { // TODO: simplify constraints
        case sn: SpinalNode => sn.xHandler ensuring { _.inputs.size == 1 }
        case x => x
      }
      case x => x
    })
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Live range of given `value` node. */
  final class SSALiveRange private[LiveRangesComponent] (val value: Node) {

    /** X belongs to finalUses, if X is in this.value.uses and for each Y != X from this.value.uses: !(X dominates Y).
      * biggestUseNumber is the biggest number of all this.value uses. */
    lazy val (finalUses, biggestUseNumber) = {
      // finalUses and biggestUseNumber are optimizations for `intersect` method.
      // Instead of them we may use all this.value uses.
      // TODO: make compilation time measurements for these optimizations.

      val valueUses = ArrayBuffer.empty[Node] ++= (this.value.valueOutEdges map codeOrderUse)
      if (valueUses.isEmpty) {
        (Nil, number(value))

      } else {
        val sorted = valueUses sortBy number
        val finUses = ArrayBuffer.empty[Node]

        for (u <- sorted.reverseIterator if !finUses.exists(dominates(u, _))) {
          finUses += u
        }

        (finUses, number(sorted.last))
      }
    }

    /** Returns true, iff this range intersect with given `that` range. */
    def intersects(that: SSALiveRange): Boolean = {
      if (this == that) return false
      if (number(this.value) < number(that.value)) {
        this contains that.value
      } else {
        that contains this.value
      }
    }

    /** Returns resource, occupied by this range. */
    def resource = value.resource

    /** Returns true, iff given `node` contained in this range. */
    def contains(node: Node): Boolean = {
      if (node == value) return true
      dominates(value, node) &&
        (number(node) < biggestUseNumber) &&
        finalUses.exists { use => strictDominates(node, use) }
    }
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /** LiveRange is a group of bound SSALiveRanges allocated on the same resource. Each LiveRanges is a closure
    * over phi-function arguments.
    *
    * LiveRange components may have different types because of NOP casts optimization (in Preparation).
    * Thus there could not be unique "type" of LiveRange. All type-like properties e.g. isFP or mayBeJavaRef
    * should be implemented independently.
    * */
  final class LiveRange private[LiveRangesComponent](val ssaRanges: Iterable[SSALiveRange]) {

    assert(haveSame(ssaRanges)(_.resource))

    /** Returns iterator over SSA-values from this range. */
    def values: Iterator[Node] =
      ssaRanges.iterator map (_.value)

    /** Returns resource, at which this live range allocated. */
    def resource: Resource =
      ssaRanges.head.resource

    /** Returns true, iff this range is floating-point type. */
    def isFP: Boolean = {
      val result = ssaRanges.head.value.tpe.isFloatingPointType
      assert(values forall (_.isFP == result))
      result
    }

    /** Returns true, iff any component of this range may be traceable. */
    def mayBeTraceableRef: Boolean = values exists mayBeTraceableReference

    /** Returns true, iff this range intersected with given `that` range. */
    def intersects(that: LiveRange): Boolean =
      that.ssaRanges exists { r1 => this.ssaRanges exists { r2 => r2 intersects r1 } }


    ////////////////////////////////////////////////////////////////////
    // These methods are not efficient and used for unit-tests only now.
    // If you want to use them in performance-critical code, they should be rewritten!

    /** Returns true, iff given `node` contained in this range. */
    def contains(node: Node) = ssaRanges exists (_.contains(node))
  }


  object LiveRanges {
    private var _backendCodeOrder: Numbering[Node] = _
    private val ssaRangesCache = Maps[Node].newMMap[SSALiveRange]
    private val rangesCache = Maps[Node].newMMap[LiveRange]

    /** Enable access to LiveRanges during `action`. */
    def enableFor(action: => Unit): Unit = {
      _backendCodeOrder = Numbering(cfg.topSort.order flatMap { b =>
        assert((CodeOrder contains b) && (CodeOrder contains b.blockEnd))
        CodeOrder in b
      })

      onCommit.withCallback(_ => shouldNotReachHere("nodes commit not supported while LiveRanges built, feel free to support it")) {
        onDecommit.withCallback(_ => shouldNotReachHere("nodes decommit not supported while LiveRanges built, feel free to support it")) {
          beforeStructuralChange.withCallback(_ => shouldNotReachHere("nodes structural change not supported while LiveRanges built, feel free to support it")) {
            CodeOrder.onChange.withCallback(_ => shouldNotReachHere("code order change not supported while LiveRanges built, feel free to support it")) {
              // TODO: implement node resource allocation callbacks
              action
            }
          }
        }
      }

      _backendCodeOrder = null
      ssaRangesCache.clear()
      rangesCache.clear()
    }

    private def enabled(): Boolean = _backendCodeOrder != null

    /** Returns linear order of all nodes with rules:
      *  - reachable blocks are ordered in top-sort order;
      *  - generated nodes are ordered in consistency with generation info.
      */
    def backendCodeOrder = { assert(enabled()); _backendCodeOrder }

    /** Returns SSALiveRange of the `node`. */
    def ssa(node: Node): SSALiveRange = {
      assert(enabled())
      ssaRangesCache.getOrElseUpdate(node, { new SSALiveRange(node) })
    }

    /** Returns LiveRange of the `node`. */
    def web(node: Node): LiveRange = {
      assert(enabled())
      rangesCache.getOrElse(node, {
        val ssaRanges = ArrayBuffer.empty[SSALiveRange]
        var nodes: immutable.Set[Node] = Sets[Node].newImmSet

        def visit(n: Node): Unit = if (!nodes(n)) {
          ssaRanges += ssa(n)
          nodes += n
          if (n.isInstanceOf[Phi]) n.valueArgs foreach visit
          collect[Phi](n.valueUses) foreach visit
        }

        visit(node)
        val result = new LiveRange(ssaRanges)

        ssaRanges foreach { r => rangesCache(r.value) = result }
        result
      })
    }

    /** Returns sequence of live ranges, covered given `nodes` without repetitions. */
    def coverageOf(nodes: Node*) = (nodes map web).toSet
  }

}
