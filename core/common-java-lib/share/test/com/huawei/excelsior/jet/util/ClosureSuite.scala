/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util

import com.huawei.excelsior.jet.CommonSuite
import com.huawei.excelsior.jet.util.DSLs.IntGraphBuilderDSL

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Tests for ProcessSet.
 */
class ClosureSuite extends CommonSuite with IntGraphBuilderDSL {

  /**
   *          |---------------|
   *          |               |
   *          v               |
   *          0 ----> 1 ----> 3
   *          |       |
   *          v       v
   *  6 <---> 2 <---- 4
   *          |       |
   *          v       |
   *          5 <-----|
   */
  private val _graph = makeGraph(0 -> ((1 -> ((3 -> 0) || (4 -> (2 || 5)))) || (2 -> (5 || (6 -> 2)))))
  
  private val set = new mutable.LinkedHashSet[Int]
  private val postOrder = new ArrayBuffer[Int]

  override def beforeEach(): Unit = {
    super.beforeEach()
    set.clear()
    postOrder.clear()
  }
  
  private def checkPreAction(n: Int): Unit = {
    set should not contain (n)
  }

  private def checkPostAction(n: Int): Unit = {
    postOrder += n
    set should contain (n)
    for (x <- _graph.succs(n)) {
      set should contain (x)
    }
  }
  
  private def succs(n: Int) = {
    set should contain (n)
    _graph.succs(n)
  }
  
  private def process(from: Int*): Unit = {
    Closure.withActions(set, from)(succs)(checkPreAction)(checkPostAction)
  }
  
  private def checkResult(elems: Int*)(postElems: Int*): Unit = {
    set should have size (elems.size)
    for ((x, y) <- set zip elems) {
      x should be (y)
    }
    postOrder should equal (postElems)
  }

  test("all graph") {
    process(0)
    checkResult(0, 1, 3, 4, 2, 5, 6)(3, 5, 6, 2, 4, 1, 0)
  }
  
  test("all graph with sime starts") {
    process(0, 1, 5)
    checkResult(0, 1, 3, 4, 2, 5, 6)(3, 5, 6, 2, 4, 1, 0)
  }

  test("all graph with other starts") {
    process(6, 1)
    checkResult(6, 2, 5, 1, 3, 0, 4)(5, 2, 6, 0, 3, 4, 1)
  }

  test("sub-graph") {
    process(4)
    checkResult(4, 2, 5, 6)(5, 6, 2, 4)
  }

  test("single node graph") {
    process(5)
    checkResult(5)(5)
  }
  
}