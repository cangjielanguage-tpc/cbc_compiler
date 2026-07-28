/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.util

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.opt.backend.amd64.BackEndAmd64
import com.huawei.excelsior.jet.compiler.opt.ir.Resources._
import com.huawei.excelsior.jet.compiler.opt.middle.DCEComponent
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder

/**
  * @author liontiger
  */
class BackendGraphsSuite extends CompilerSuite with GlobalNodesBuilder with BackendGraphs with DCEComponent with BackEndAmd64 {

  var dag: GenerationDAG = _
  var b: Block = _
  var x, y, c, r: Node = _

  def makeCopy(res: ResourceSet, node: Node) = Copy.withoutValue(node, res)

  def checkCrown(values: Node*) = dag.crown.toSet should be (values.toSet)
  def checkSuccs(n: Node, values: Node*) = dag.succs(n).toSet should be (values.toSet)
  def checkPreds(n: Node, values: Node*) = dag.preds(n).toSet should be (values.toSet)

  override def beforeEach(): Unit = {
    super.beforeEach()
    disableValueNumbering()
  }
  
  def genNode(n: Node): Unit = {
    dag.tie(n)
  }

  def genAndCheckCrown(n: Node, values: Node*) = {
    genNode(n)
    checkCrown(values: _*)
  }

  def checkAndAlso(extra: => Unit): Unit = {
    makeCFG(0@@("x", "y", "c=add(x,y)", "r=ret(c)"))

    // During CFG construction fake node created for Return node argument.
    // After `c` node created and used in Return this node is dead.
    eliminateDeadCode()

    b = 0
    x = "x"
    y = "y"
    c = "c"
    r = "r"
    c.attachToGroup(r, reason = Group.AttachReason.INLINE_ADDR_MODE)

    withGCM() {
      dag = new GenerationDAG(b)
      dag.nodes should not contain c
      checkSuccs(x, r)
      checkSuccs(y, r)
      checkPreds(r, b, x, y)
      checkCrown(b)
      extra
    }
  }


  test("simple generation") {
    checkAndAlso {
      genAndCheckCrown(b, x, y)
      genAndCheckCrown(x, y)
      genAndCheckCrown(y, r)
      genAndCheckCrown(r)
    }
  }

  test("simple generation with transfers") {
    checkAndAlso {
      dag.withCallbacks {
        genAndCheckCrown(b, x, y)

        val z1 = makeCopy(raxSet, x)
        checkSuccs(x, r, z1)
        checkSuccs(z1, r)
        checkPreds(z1, b, x)
        checkPreds(r, b, x, y, z1)
        checkCrown(x, y)

        genAndCheckCrown(x, y, z1)

        val z2 = makeCopy(rdxSet, x)
        checkSuccs(x, r, z1, z2)
        checkSuccs(z2, r)
        checkPreds(z2, b, x)
        checkPreds(r, b, x, y, z1, z2)
        checkCrown(y, z1, z2)

        genAndCheckCrown(z1, y, z2)
        genAndCheckCrown(z2, y)
        genAndCheckCrown(y, r)
        genAndCheckCrown(r)
      }
    }
  }
}
