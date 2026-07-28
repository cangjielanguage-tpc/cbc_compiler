/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.util.WhileChanged.*
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

/** Provides information about values liveness.
  *
  * @author conwor
  */
trait LivenessAnalysis { self: Universe =>

  /** Implements upward data-flow analysis to calculate liveness information. */
  abstract class LivenessEngine[N : Sets] {
    protected val emptySet: Set[N] = Sets[N].newImmSet

    val live: Maps[Node]#MMap[Set[N]] = Maps[Node].newMMap[Set[N]]
    protected def getLive(node: Node): Set[N] = live.getOrElseUpdate(node, emptySet)

    protected def getLiveAtEdge(edge: Edge): Set[N] = {
      assert(edge.isControl && edge.target.isInstanceOf[Block])
      val target = edge.target.asInstanceOf[Block]
      val liveInTarget = getLive(target) -- valuesMapping(target.phies) // remove phies
      liveInTarget ++ valuesMapping(target.phies.map(_.phiArg(edge)).filter(valuesFilter)) // add phies args
    }

    protected def processBlock(block: Block, output: Set[N], updateLive: (Node, Set[N]) => Unit): Set[N]

    protected def valuesFilter(n: Node): Boolean = true

    protected def valuesMapping(it: IterableOnce[Node]): IterableOnce[N]

    def calcLiveness(): Unit = {
      val order = cfg.topSort.order.reverse

      whileChanged { changed =>
        def updateLive(n: Node, set: Set[N]): Unit = live.get(n) match {
          case None => live(n) = set
          case Some(oldSet) if oldSet.size == set.size => // nothing changed
          case Some(_) => live(n) = set; changed()
        }

        order foreach { block =>
          val output = block.succBlockEdges.foldLeft(emptySet) { (acc, edge) =>
            acc | getLiveAtEdge(edge)
          }

          val input = processBlock(block, output, updateLive)
          def livePhies = block.phies filter { phi => valuesFilter(phi) && phi.hasValueUses }
          assert(valuesMapping(livePhies).iterator.forall(input.contains))
          updateLive(block, input)
        }
      }
    }
  }

  abstract class NodeLivenessEngine extends LivenessEngine[Node] {
    override def valuesMapping(it: IterableOnce[Node]) = it
  }

  /** Liveness of all `VALUE` tagged nodes through CFG edges. */
  case class CFGLiveness(in: collection.Map[Block, Sets[Node]#QSet]) {
    def edgeIn(blockInput: Edge): Iterator[Node] = {
      val Edge(_, target: Block) = blockInput
      in(target).iterator map {
        case phi: Phi if phi.block == target => phi.phiArg(blockInput)
        case n => n
      }
    }

    def out(block: Block): Sets[Node]#QSet = {
      val result = Sets[Node].newQSet
      for (edge <- block.succBlockEdges; succ = edge.target.asInstanceOf[Block]; node <- in(succ)) {
        result += (node match {
          case phi: Phi if phi.block == succ => phi.phiArg(edge)
          case _ => node
        })
      }
      result
    }
  }

  def calcCFGLiveness(): CFGLiveness = stage(Stage.CFGLiveness) {
    val engine: NodeLivenessEngine = new NodeLivenessEngine {
      override protected def valuesFilter(n: Node): Boolean = !n.isInstanceOf[Constant]

      val globalArgs = Maps[Block].newMMap[Set[Node]]
      def getGlobalArgs(block: Block) = globalArgs.getOrElseUpdate(block, emptySet)

      for (n <- allNodes if n.producesValue && valuesFilter(n); e <- n.valueOutEdges if e.useBlock != n.block) {
        globalArgs(e.useBlock) = getGlobalArgs(e.useBlock) + n
      }

      override protected def processBlock(block: Block, output: Set[Node], updateLive: (Node, Set[Node]) => Unit): Set[Node] = {
        val middle = block.handledXPoints.foldLeft(output)((set, xpoint) => set | getLiveAtEdge(xpoint.xEdge))
        val withoutBlockNodes = middle filterNot (n => n.block == block)
        withoutBlockNodes ++ block.phies ++ getGlobalArgs(block)
      }
    }

    engine.calcLiveness()

    def orderSetByID(set: Set[Node]): Sets[Node]#QSet = Sets[Node].newQSet(set.toArray.sortBy(_.id))
    val in: collection.Map[Block, Sets[Node]#QSet] = engine.live collect { case (b: Block, set) => (b, orderSetByID(set)) }

    CFGLiveness(in)
  }
}
