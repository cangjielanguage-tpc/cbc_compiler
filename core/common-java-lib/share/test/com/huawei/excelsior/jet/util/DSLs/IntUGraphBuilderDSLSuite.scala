/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.util.DSLs

import com.huawei.excelsior.jet.CommonSuite

/**
  * Tests for IntUGraphBuilderDSL.
  */
class IntUGraphBuilderDSLSuite extends CommonSuite with IntUGraphBuilderDSL {

  test("empty graph") {
    makeGraph()
  }

  test("one-vertex graph") {
    makeGraph()
    checkNeighbours(42)()
  }

  test("one-edge graph") {
    makeGraph((0, 1))
    checkNeighbours(0)(1)
    checkNeighbours(1)(0)
    checkNeighbours(42)()
  }

  test("some graph") {
    makeGraph((0, 1), (0, 3), (1, 2), (1, 4), (1, 5), (4, 5), (3, 4))
    checkNeighbours(0)(1, 3)
    checkNeighbours(1)(0, 2, 4, 5)
    checkNeighbours(2)(1)
    checkNeighbours(3)(0, 4)
    checkNeighbours(4)(1, 3 ,5)
    checkNeighbours(5)(1, 4)
  }

}
