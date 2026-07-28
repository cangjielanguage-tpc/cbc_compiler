/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation

import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf.{CalledMethod, State}
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.ProfileForest.CallTreeNode

import scala.collection.mutable

private[blame] case class ProfileForest(entries: List[CallTreeNode]) {
  def +(that: ProfileForest): ProfileForest = {
    ProfileForest(entries ++ that.entries)
  }
}

private[blame] object ProfileForest {

  class CallTreeNode(val id: String, val method: Method, val bcInCaller: Int, var preinlined: Boolean, val parent: CallTreeNode) {

    var children = List.empty[CallTreeNode]
    var printId = 0
    
    private[jprof] var staticInlined = false
    private[jprof] var postInlined = false
    private[jprof] var absorbed = false

    private[jprof] var callCount: Int = 0
    private[jprof] var initialHits: Int = 0
    private[jprof] var followupHits: Int = 0
    private[jprof] var heuristicHits: Int = 0

    private[representation] def hasHits: Boolean = initialHits > 0 || followupHits > 0 || heuristicHits > 0
    private[jprof] def totalHits: Int = initialHits + followupHits
    private[jprof] def hasHeuristicHitsInSubtree: Boolean = {
      if (heuristicHits > 0) true
      else children exists (_.hasHeuristicHitsInSubtree)
    }

    override def equals(obj: Any) = obj match {
      case node: CallTreeNode => node.id == id
      case _ => false
    }

    override def hashCode() = id.hashCode()
  }

  private[jprof] def empty = ProfileForest(List.empty)

  private[jprof] def apply(states: Seq[State], calledMethods: Seq[CalledMethod]): ProfileForest = {

    val fakeEntryNode = new CallTreeNode("fakeEntryNode", Method.fakeCaller, -1, false, null)
    val calledMethodsMap = calledMethods.map(x => x.nodeId -> x).toMap // 1-to-1 mapping

    def eqNode(n: CallTreeNode, calledMethod: CalledMethod): Boolean = {
      n.method == calledMethod.method && n.bcInCaller == calledMethod.bcInCaller
    }

    val nodesMap = mutable.HashMap.empty[String, CallTreeNode] // N-to-1 mapping

    def addNode(nodeId: String): CallTreeNode = {
      nodesMap.get(nodeId) match {
        case Some(node) => return node // this node is already added
        case None => // fallthrough
      }

      val currentMethod = calledMethodsMap(nodeId)
      assert(currentMethod.nodeId == nodeId)

      val callerNode: CallTreeNode =
        if (currentMethod.callerId.nonEmpty) {
          addNode(currentMethod.callerId) // check that all callers are added and get the immediate one
        } else {
          fakeEntryNode // entry node to collect all roots as children
        }

      // check caller's children if node with the same context already exists
      val node = (callerNode.children.filter(eqNode(_, currentMethod)): @unchecked) match {
        case Nil => // there is no node with the given context, create new one
          val node = new CallTreeNode(nodeId, currentMethod.method, currentMethod.bcInCaller, currentMethod.inlined, callerNode)
          callerNode.children = node :: callerNode.children
          node

        case node :: Nil => // node with the same context is found
          assert(node.id != currentMethod.nodeId) // this is a different call path merged
          assert(node.preinlined && currentMethod.inlined || !node.preinlined && !currentMethod.inlined)
          node
      }
      node.callCount += currentMethod.callCount
      nodesMap(nodeId) = node
      node
    }

    def processState(state: State): Unit = {
      var node = addNode(state.scopeId)
      // if hits are recorded for the inlined method, find the not inlined caller and record hits for it
      while (node.preinlined) {
        node = node.parent
      }
      node.initialHits += state.initialHits
      node.followupHits += state.followupHits
      node.heuristicHits += state.heuristicHits
    }

    states foreach processState
    ProfileForest(fakeEntryNode.children)
  }
}
