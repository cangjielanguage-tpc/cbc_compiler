/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.testutils.DSLs

import com.huawei.excelsior.jet.util.DSLs.CommonGraphBuilderTestsCollection
import com.huawei.excelsior.jet.util.graph.BiGraph

/** Tests for GraphBuilderDSL.
 */
class IRBuilderDSLSuite extends CommonGraphBuilderTestsCollection with IRBuilderDSL {

  type NN = Block
  type G = BiGraph[Block]

  def make(start: SubGraph): Unit = makeCFG(start)
  def g = cfg
  def int2n(node: Int) = int2Block(node)
  def int2graph(node: Int) = int2SubGraph(node)

  test("prolog test") {
    make(0 -> 1)
    g.preds(0) should not be (empty)
  }

}
