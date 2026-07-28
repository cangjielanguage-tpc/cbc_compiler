/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.Worklist
import com.huawei.excelsior.jet.util.graph.ordering.TopSort

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

/**
  * Linear order of nodes.
  *
  * @author cypok
  * @author alexm
  */
trait LinearNodeOrder { self: Universe =>

  object LinearNodeOrder {

    /** Linear order of block nodes with additional rules:
      *  - uses are ordered after defs;
      *  - memory anti-dependency;
      *  - generated nodes are ordered in consistency with generation info.
      *  - phies are ordered just after corresponding block;
      *  - block end results are skipped;
      *  - XPoints are skipped;
      *  - branch with cmp argument are ordered together.
      */
    def strictBlockOrder(block: Block, strictlyByPoints: Boolean = false): collection.Seq[Node] = {
      requireAllNodesPinned()
      val dag = DAG(block, strictlyByPoints)
      checkGraphConsistency(CheckLevels.Desirable, dag)
      val nodes = dag.topSort.order

      assert(nodes.head == block)
      checkConsistency(CheckLevels.Desirable) { verifyOrdering(block, nodes) }
      nodes
    }

    /** Linear order of all nodes with additional rules:
      *  - nodes are placed as early as possible;
      *  - reachable blocks are ordered in top-sort order;
      *  - unreachable blocks are ordered after reachable;
      *  - uses are ordered after defs (except blocks and phies);
      *  - generated nodes are ordered in consistency with generation info;
      *  - phies are ordered just after corresponding block node;
      *  - branch exits are ordered just after branch node;
      *  - XPoints are ordered just after corresponding SpinalNode.
      */
    def globalOrder(): (collection.Seq[Node], Boolean) = globalOrder(currentScope)
    def globalOrder(scope: Scope): (collection.Seq[Node], Boolean) = {
      /** TopSort order of blocks including unreachable blocks. */
      val blocksOrder: Iterator[Block] = {
        val reachable = scope.cfg.topSort.order
        (reachable.iterator ++ scope.all[Block]).distinct
      }

      val remainingNodes = Sets[Node].newQSet(scope.allNodes)
      val orderedNodes = new ArrayBuffer[Node](remainingNodes.size)

      /** Last processed control node. */
      var lastCtrl: ControlNode = null

      /** Nodes that must be processed next unconditionally. */
      val fixedNodes = Worklist[Node](blocksOrder.next())

      def suitableNode(n: Node) = {
        remainingNodes(n) &&
          (n.block == null || (lastCtrl != null && n.block == lastCtrl.block)) && // node should be floating or from current block
          (n.args forall { a => a == null || a == n || !remainingNodes(a) }) && // all arguments should be already processed
          (n.isInstanceOf[XPoint] || !n.isInstanceOf[ControlNode]) // control nodes are handled separately
      }

      /** Nodes that may be processed next. */
      val candidates = Worklist.from(scope.allNodes filter suitableNode)

      while (remainingNodes.nonEmpty) {
        val node = fixedNodes.drain find remainingNodes getOrElse {
          candidates.drain find suitableNode getOrElse {
            // all suitable candidates are processed so now we have to take next control node
            try {
              lastCtrl match {
                case p: UpperPoint => p.outCtrl
                case _: BlockEnd | _: Branch.Exit => blocksOrder.next()

                case _ if lastCtrl.block.unreachable =>
                  // During incremental GCM unreachable code may be pinned incorrectly,
                  // because pinEarly relies on dominators, which cannot be computed correctly for unreachable code
                  // (any node dominates unreachable code).
                  return (orderedNodes ++= remainingNodes, false)
              }
            } catch {
              case NonFatal(_) =>
                return (orderedNodes ++= remainingNodes, true)
            }
          }
        }

        assert(remainingNodes contains node)
        remainingNodes -= node
        orderedNodes += node

        lastCtrl = node match {
          case _: XPoint => lastCtrl
          case cn: ControlNode => cn
          case _ => lastCtrl
        }

        // Uses of node _may_ be processed next and nothing else.
        // It's correct because we process nodes as early as possible.
        candidates ++= (node match {
          case b: Block => b.uses ++ b.nodes ++ (b.points flatMap (_.projections))
          case _ => node.uses
        }).filter(suitableNode)

        node match {
          case b: Block =>
            fixedNodes ++= b.phies
            for (node <- CodeOrder in b) {
              fixedNodes ++= node.attachedArgs
              fixedNodes += node
              fixedNodes ++= node.attachedResults
            }

          case end: BlockEnd => fixedNodes ++= end.exits
          case sn: SpinalNode if sn.canThrow => fixedNodes += sn.xpoint
          case _ =>
        }
      }

      (orderedNodes, false)
    }

    /** Verification of ordering properties. Used in assert. */
    private def verifyOrdering(block: Block, nodes: IterableOnce[Node]): Unit = {
      var ctrl: Node = null
      var memory: Node = null
      val processedControlNodes = Sets[Node].newMSet
      val processedNodes = Sets[Node].newMSet

      for (node <- nodes.iterator) {
        assert (node.block == block)
        node match {
          case sn: SpinalNode => assert (sn.inCtrl == ctrl)
          case cn: ControlledNode => assert ((cn.inCtrl.block != block) || processedControlNodes.contains(cn.inCtrl))
          case _ =>
        }

        import Branch.Exit
        (node, ctrl) match {
          case (_: Exit, _: BlockEnd) => // ok
          case (_: Exit, _: Exit)     => // ok
          case (_,       _: BlockEnd) => shouldNotReachHere("Node " + node + " is ordered after block end " + ctrl)
          case (_,       _: Exit)     => shouldNotReachHere("Node " + node + " is ordered after multi-way branch exit " + ctrl)
          case _ =>
        }

        node match {
          case HasInMemory(inMem) => assert(inMem.block != block || inMem == memory || inMem == block)
          case _ =>
        }

        node match {
          case _: Block | _: Phi =>
          case _ => assert (node.args forall (n => (n == null) || (n.block != block) || !n.isGroupRoot || processedNodes(n)))
        }

        node match {
          case _: ControlNode =>
            ctrl = node
            processedControlNodes += node
          case _ =>
        }

        node match {
          case _: MemoryNode =>
            memory = node
          case _ =>
        }

        processedNodes += node
      }
    }
  }
}
