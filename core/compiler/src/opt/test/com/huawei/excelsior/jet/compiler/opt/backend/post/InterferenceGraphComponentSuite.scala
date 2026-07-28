/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.post

import com.huawei.excelsior.jet.assembler.amd64.GPR._
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.common.CodeHelpers._
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.opt.ir.Resources._
import com.huawei.excelsior.jet.util.ScalaCollections

/**
  * Tests for InterferenceGraphComponent
  *
  * @author conwor
  */
class InterferenceGraphComponentSuite extends PostProcessSuite with InterferenceGraphComponent with LiveRangesComponent {

  // TODO: eliminate copy-paste between this code and LiveRangesComponentSuite
  override def beforeEach(): Unit = {
    super.beforeEach()
    tieNodesInBackendOrder = true
  }

  private var resources: Seq[Resource] = _
  private var nodes: Seq[Node] = _

  private var interferenceGraph: InterferenceGraph = _

  private def calcInterferenceGraph(): Unit = {
    interferenceGraph = new InterferenceGraph(nodes)
    interferenceGraph.appendResources(resources, { _ => true })
  }

  private def findVertex(x: Any): IGVertex = {
    val allVertices = (interferenceGraph.completeSubGraph ++ interferenceGraph.unmerged) flatMap (v => interferenceGraph.group(v) )
    (allVertices find {
      case rv @ RangeVertex(range) if range.values contains x => true
      case rv @ ResourceVertex(resource) if resource == x => true
      case _ => false
    }).get
  }

  private def allVertices() = interferenceGraph.completeSubGraph ++ interferenceGraph.unmerged

  private def checkNeighbours(x: Any)(expected: Any*): Unit = {
    val vx = findVertex(x)
    val ev = expected map findVertex

    interferenceGraph.neighboursSet(vx) should be (ev.toSet)
    for (v <- allVertices()) {
      interferenceGraph.adjacent(vx, v) should be (ev contains v)
    }
  }

  private def checkIGGroups(groups: Any*): Unit = {
    for (group <- groups) {
      group match {
        case xs: Set[_] =>
          val vxs = xs map findVertex
          vxs foreach { vx =>
            interferenceGraph.group(vx).toSet should be (vxs)
          }
          ScalaCollections.uniqueValue(vxs map interferenceGraph.delegate) match {
            case Some(x) => vxs should contain (x)
            case None => shouldNotReachHere()
          }

        case x: Any =>
          val vx = findVertex(x)
          interferenceGraph.group(vx).toSet should be (Set(vx))
          interferenceGraph.delegate(vx) should be (vx)

      }
    }
  }

  private def testWithRanges(name: String)(init: => Unit)(checks: => Unit): Unit = {
    test(name) {
      init
      LiveRanges.enableFor {
        checks
      }
    }
  }


  testWithRanges("simple code line") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s(a,a)", "d=s(b,b)", "e=s(c,b)", "ret(d)"))
  } {
    val (a, b, c, d, e) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node)
    nodes = Seq(a, b, c, d, e)
    resources = Seq(RAX, RBX, RCX)

    calcInterferenceGraph()

    checkNeighbours(a)(b)
    checkNeighbours(b)(a, c, d)
    checkNeighbours(c)(b, d)
    checkNeighbours(d)(b, c, e)
    checkNeighbours(e)(d)
    checkNeighbours(RAX)(RBX, RCX)
    checkNeighbours(RBX)(RAX, RCX)
    checkNeighbours(RCX)(RAX, RBX)

    interferenceGraph.simplify()

    // This assertion may failed, because they are based on current implementation details.
    // TODO: make more implementation-independent tests

    interferenceGraph.completeSubGraph.size should be (3)
    checkIGGroups(Set(RAX, a, c, e), Set(RBX, b), Set(RCX, d))
  }

  testWithRanges("simple code line with pre-colored nodes") {
    makeCFG(0 @@ ("a=s()", "b=s()", "c=s(a,a)", "d=s(b,b)", "defRBX=s()", "e=s(c,b)", "useRBX=s(defRBX,defRBX)", "spoilRAX=s()", "ret(d)"))
  } {
    val (a, b, c, d, e) = ("a": Node, "b": Node, "c": Node, "d": Node, "e": Node)
    nodes = Seq(a, b, c, d, e)
    resources = Seq(RAX, RBX, RCX)

    val (defRBX, spoilRAX) = ("defRBX": Node, "spoilRAX": Node)
    defRBX.resource = RBX
    spoilRAX.spoiled = Seq(RAX)

    calcInterferenceGraph()

    checkNeighbours(a)(b)
    checkNeighbours(b)(a, c, d, RBX)
    checkNeighbours(c)(b, d, RBX)
    checkNeighbours(d)(b, c, e, RAX, RBX)
    checkNeighbours(e)(d, RBX)
    checkNeighbours(RAX)(RBX, RCX, d)
    checkNeighbours(RBX)(RAX, RCX, b, c, d, e)
    checkNeighbours(RCX)(RAX, RBX)

    interferenceGraph.simplify()

    // This assertion may failed, because they are based on current implementation details.
    // TODO: make more implementation-independent tests

    interferenceGraph.completeSubGraph.size should be (4)
    checkIGGroups(Set(RAX, b, e), Set(RBX, a), Set(RCX, d), c)
  }
}
