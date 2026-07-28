/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.bgcm.bulldozerpass

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}

/** Group of IR DAGs:
  *  - Supports all edges - data, memory, control.
  *  - Exclude group parts (vertices of graph are group roots).
  *  - Exclude projections (XPoints, Branch.Exit). TODO: refactor them (combine with groups).
  *
  * Supports crown processing with rules:
  *  - Node appears in crown when all it's uses removed from crown.
  *  - Phi, Block, Arg & InitialMemory nodes are not appeared in crown
  */
trait BDAG { self: Universe with BackEnd =>

  class BulldozerDAG {
    val crown: Sets[Node]#QSet = Sets[Node].newQSet

    private def allowedInCrown(node: Node): Boolean = node match {
      case _: Block | _: BlockParamNode | _: Projection => false
      case _ => true
    }

    private def aboveDeps(node: Node): Iterator[Node] = node match {
      case _ if !node.isGroupRoot =>
        // Internal group dependencies are not taken into account, external group dependencies are accounted from group root
        Iterator.empty

      case _: Projection =>
        // Similar to attached group parts. TODO: simplify
        Iterator.empty

      case _ =>
        val pointDep = node match {
          case fn: FloatingNode => Iterator.single(fn.upperPoint)
          case _ => Iterator.empty
        }
        (node.groupedArgs ++ pointDep) collect { case dep if allowedInCrown(dep.groupRoot) => dep.groupRoot }
    }

    private val belowDepsCount = Maps[Node].newMMap[Int]

    private def incBelowDeps(node: Node): Unit = {
      belowDepsCount(node) = belowDepsCount.getOrElse(node, 0) + 1
      crown -= node
    }

    private def decBelowDeps(node: Node): Unit = {
      val deps = belowDepsCount(node) - 1
      assert(deps >= 0)
      belowDepsCount(node) = deps
      if (deps == 0) {
        crown += node
      }
    }

    // Constructor
    for (node <- allNodes) {
      aboveDeps(node) foreach incBelowDeps
    }

    def processNode(node: Node): Unit = {
      crown -= node
      aboveDeps(node) foreach decBelowDeps
    }

    def processPhiArg(arg: Node): Unit = {
      val root = arg.groupRoot
      if (allowedInCrown(root)) {
        decBelowDeps(root)
      }
    }

    private def insertHintNode(lower: LowerPoint, key: Node, proto: BulldozerHint.Proto): BulldozerHint = {
      val upper = lower.inCtrl
      val hintNode = insertCodeAfter(upper) { proto(key) }

      for (n @ (_n: FloatingNode) <- upper.pinnedNodes.toList if CodeOrder contains n) {
        n atUpperPoint hintNode
      }

      hintNode
    }

    /** Inserted BGCM hint at not generated yet control point. */
    def insertHintAbove(lower: LowerPoint, key: Node, proto: BulldozerHint.Proto): BulldozerHint = {
      val hintNode = insertHintNode(lower, key, proto)
      incBelowDeps(key.groupRoot)
      if (CodeOrder contains lower) {
        incBelowDeps(hintNode.inCtrl.groupRoot)
      } else {
        incBelowDeps(hintNode)
      }
      hintNode
    }

    /** Inserted BGCM hint at already generated block start. */
    def insertHintAtAlreadyGeneratedBlockStart(block: Block, key: Node, proto: BulldozerHint.Proto): BulldozerHint = {
      val hintNode = insertHintNode(block.outCtrl, key, proto)

      val pointInOrder = (CodeOrder in block find {
        case _: Block | _: Phi | _: Catch => false // Skip special nodes
        case _: Param => shouldNotReachHere() // Let's see who want to insert something at entry block start
        case _ => true
      }).get
      CodeOrder.insertBefore(pointInOrder, hintNode)

      hintNode
    }

    /** Inserts Copy node at given `edge`. */
    def insertCopyForPhiArgument(edge: Edge): Node = {
      val Edge(arg, phi: Phi) = edge
      val controlEdge = phi.controlInput(edge)
      val upperPoint = controlEdge.source.block.blockEnd.inCtrl

      temporaryResourcesForIntermediateCopy(arg) match {
        case None =>
          val copy = Copy.withOwnValue(arg) atUpperPoint upperPoint // Currently restricted by backend technically reasons
          incBelowDeps(copy)
          incBelowDeps(upperPoint)
          edge.source = copy
          copy

        case Some(temporals) =>
          val firstCopy = Copy.withoutValue(arg, temporals) atUpperPoint upperPoint
          incBelowDeps(firstCopy)
          incBelowDeps(upperPoint)
          val secondCopy = Copy.withOwnValue(firstCopy) atUpperPoint upperPoint
          incBelowDeps(secondCopy)
          incBelowDeps(upperPoint)
          edge.source = secondCopy
          secondCopy
      }
    }

    def insertNodeClone(node: FloatingNode, upperPoint: UpperPoint) = {
      val clone = Node.cloneExact(node)
      clone.atUpperPoint(upperPoint)

      // SpoiledArgSaver nodes should have only one use (they are generated right after it).
      // TODO: kill SpoiledArgSaver nodes will kilotons of nuclear fire
      val savers = insertSpoiledArgsSavers(clone)
      for (saver <- savers) {
        aboveDeps(saver) foreach incBelowDeps
      }

      aboveDeps(clone) foreach incBelowDeps
      clone
    }

    def replaceLiveBelowDep(node: Node, replacement: Node): Unit = {
      incBelowDeps(replacement)
      decBelowDeps(node)
    }

    def moveNode(node: FloatingNode, upperPoint: UpperPoint): Unit = {
      def impl(node: FloatingNode): Unit = {
        aboveDeps(node) foreach decBelowDeps
        node.atUpperPoint(upperPoint)
        aboveDeps(node) foreach incBelowDeps
      }

      impl(node)

      // SpoiledArgSaver single use should be in the same loops with it.
      // TODO: kill SpoiledArgSaver nodes will kilotons of nuclear fire
      for (sas <- collect[SpoiledArgSaver](node.args)) {
        impl(sas)
      }
    }
  }
}
