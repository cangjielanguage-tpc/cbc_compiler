/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import com.huawei.excelsior.jet.util.graph.BiGraph

/** Tests for GraphBuilderDSL.
 */
class IntGraphBuilderDSLSuite extends CommonGraphBuilderTestsCollection {

  type NN = Int
  type G = BiGraph[Int]

  def make(start: SubGraph): Unit = makeGraph(start)
  def g = graph
  def int2n(node: Int) = node
  def int2graph(node: Int) = node2SubGraph(node)

  test("prolog test") {
    make(0 -> 1)
    g.preds(0) should be (empty)
  }
}
